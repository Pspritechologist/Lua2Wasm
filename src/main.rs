#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
	trim_prefix_suffix,
)]

use std::rc::Rc;
use bstr::BStr;

mod types;
mod parsing;
mod debug;

mod gc {
	pub use dumpster::unsync::*;
	pub use dumpster::Trace;
}

#[derive(Debug, Clone, Copy)]
enum Operation {
	// Loads.
	LoadNil(u8, u8),
	LoadBool(u8, bool),
	LoadNum(u8, u16),
	LoadStr(u8, u16),
	LoadTab(u8),
	LoadClosure(u8, u16),
	// Tables.
	Set(u8, u8, u8),
	Get(u8, u8, u8),
	// Arithmetic.
	Add(u8, u8, u8),
	Sub(u8, u8, u8),
	Mul(u8, u8, u8),
	Div(u8, u8, u8),
	Mod(u8, u8, u8),
	Pow(u8, u8, u8),
	Neg(u8, u8),
	// Logic.
	Eq(u8, u8, u8),
	Neq(u8, u8, u8),
	Lt(u8, u8, u8),
	Lte(u8, u8, u8),
	Gt(u8, u8, u8),
	Gte(u8, u8, u8),
	Not(u8, u8),
	// Bitwise.
	BitAnd(u8, u8, u8),
	BitOr(u8, u8, u8),
	BitXor(u8, u8, u8),
	BitShL(u8, u8, u8),
	BitShR(u8, u8, u8),
	BitNot(u8, u8),
	// Misc operators.
	Concat(u8, u8, u8),
	Len(u8, u8),
	// Calling.
	Call(u8, u8, u8),
	// Ret(u8, u8),
	// Control.
	GoTo(u32, u8),
	SkpIf(u8),
	SkpIfNot(u8),
	// Meta.
	Copy(u8, u8),
}
impl Operation {
	pub fn goto(position: usize) -> Self {
		let (p32, p8) = Self::encode_goto(position);
		Self::GoTo(p32, p8)
	}

	pub fn encode_goto(position: usize) -> (u32, u8) {
		let p32 = (position & 0xFFFF_FFFF) as u32;
		let p8 = ((position >> 32) & 0xFF) as u8;
		(p32, p8)
	}

	pub fn decode_goto(p32: u32, p8: u8) -> usize {
		((p8 as usize) << 32) | (p32 as usize)
	}
}

struct ClosureProto {
	operations: Box<[Operation]>,
	number_offset: usize,
	string_offset: usize,
	closure_offset: usize,
	slots_needed: u8,
	debug: Option<debug::DebugInfo>,
}

#[derive(Default)]
struct Frame {
	pc: usize,
	stack_base: usize,
}

fn main() -> std::process::ExitCode {
	if let Err(e) = try_main() {
		eprintln!("{e}");
		return std::process::ExitCode::FAILURE;
	}

	std::process::ExitCode::SUCCESS
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
	let src = include_str!("../src/test.lua");

	// let parsed = parsing::parse_asm(src);
	let parsed = parsing::parse(src)?;
	let mut buf = String::new();
	// parsing::fmt_asm(&mut buf, &parsed)?;
	std::fs::write("out.asm", buf)?;
	
	let parsing::Parsed { parsed_func, numbers, strings, closures } = parsed;
	let strings = ConstStrings::new(strings);

	let closures: Vec<_> = std::iter::once(parsed_func).chain(closures).map(|closure| {
		ClosureProto {
			operations: closure.operations,
			number_offset: 0,
			string_offset: 0,
			closure_offset: 0,
			slots_needed: closure.frame_size,
			debug: Some(closure.debug),
		}
	}).collect();

	let luant = Rc::new(Luant {
		constants: Constants {
			numbers: numbers.to_vec(),
			strings,
			closures,
		},
		..Default::default()
	});

	let main_func = Closure {
		luant: luant.clone(),
		idx: 0,
	};

	let stack = match call_func(&main_func) {
		Ok(stack) => stack,
		Err((op_idx, e)) => {
			let span = luant.constants.closures[0].debug.as_ref()
				.and_then(|d| d.src_map().get(op_idx))
				.unwrap_or(0);

			let line = src[..span].lines().count();
			let col = src[..span].lines().last().map_or(0, |l| l.len()) + 1;
			
			Err(format!("Runtime error at {line}:{col}: {e}"))?
		}
	};

	for (reg, val) in stack.iter().enumerate() {
		print!("{reg} = ");
		match val {
			Some(val) => println!("{val:#}"),
			None => println!("null"),
		}
	}

	Ok(())
}

#[derive(Default)]
struct ConstStrings {
	data: Vec<u8>,
	map: Vec<(usize, usize)>,
}
impl ConstStrings {
	pub fn get(&self, index: usize) -> &BStr {
		let (start, end) = self.map[index];
		BStr::new(&self.data[start..end])
	}

	pub fn push(&mut self, s: &BStr) -> usize {
		let start = self.data.len();
		self.data.extend_from_slice(s);
		let end = self.data.len();
		self.map.push((start, end));
		self.map.len() - 1
	}

	pub fn new<'a>(strings: impl IntoIterator<Item=&'a BStr>) -> Self {
		let mut data = Vec::new();
		let mut map = Vec::new();

		for s in strings {
			let start = data.len();
			data.extend_from_slice(s);
			let end = data.len();
			map.push((start, end));
		}

		Self { data, map }
	}

	pub fn len(&self) -> usize {
		self.data.len()
	}
}

#[derive(Default)]
struct Constants {
	numbers: Vec<f64>,
	strings: ConstStrings,
	closures: Vec<ClosureProto>,
}

#[derive(Default)]
struct VmState {
	hasher: types::Hasher,
	gc_buf: std::cell::Cell<Vec<u8>>,
}

#[derive(Default)]
struct Luant {
	vm_state: VmState,
	constants: Constants,
	global_table: types::Table,
}

impl Luant {
	pub fn new() -> Self {
		Self::default()
	}
}

struct Closure {
	luant: Rc<Luant>,
	idx: usize,
}

fn call_func(func: &Closure) -> Result<Vec<Option<types::Value>>, (usize, Box<dyn std::error::Error>)> {
	use crate::types::{Value, ToValue, Num};

	let luant = &func.luant;
	let proto = &luant.constants.closures[func.idx];

	let mut frames: Vec<Frame> = Vec::from([Frame { pc: 0, stack_base: 0 }]);
	let mut stack: Vec<Option<Value>> = vec![None; proto.slots_needed.into()];

	let mut frame = frames.last_mut().unwrap();
	let mut registers = &mut stack[frame.stack_base..];

	let operations = &proto.operations[..];

	//TODO: Temp printing.
	let print_fn = Some(Value::Func(|args| {
		let mut values = args.iter();

		if let Some(v) = values.next() {
			match v {
				Some(v) => print!("{v}"),
				None => print!("<nil>"),
			}
		}

		for val in values {
			match val {
				Some(v) => print!("\t{v}"),
				None => print!("\t<nil>"),
			}
		}

		println!();

		Ok(vec![])
	}));
	let libs_tab = Some(Value::Table(types::Table::from_iter(luant, [
		("assert", Value::Func(|args| {
			let condition = &args[0];
			let msg = args.get(1);

			if !condition.as_ref().is_some_and(|v| v.is_truthy()) {
				if let Some(Some(Value::Str(s))) = msg {
					return Err(format!("Assertion failed: {}", s.as_str()).into());
				} else {
					return Err("Assertion failed".into());
				}
			}

			Ok(vec![])
		})),
		("assert_eq", Value::Func(|args| {
			let a = &args[0];
			let b = &args[1];
			let msg = args.get(2);

			if a != b {
				if let Some(Some(Value::Str(s))) = msg {
					return Err(format!("Assertion failed: {a:?} != {b:?}: {}", s.as_str()).into());
				} else {
					return Err(format!("Assertion failed: {a:#?} != {b:#?}").into());
				}
			}

			Ok(vec![])
		})),
	])));

	registers[0] = print_fn.clone();
	registers[1] = libs_tab.clone();
	
	macro_rules! math_op {
		($dst:expr, $a:expr, $b:expr, $op:tt) => { {
			let a = Value::coerce_num(registers[$a as usize].as_ref()).map_err(|e| (frame.pc, e))?;
			let b = Value::coerce_num(registers[$b as usize].as_ref()).map_err(|e| (frame.pc, e))?;
			let result = a.$op(*b).unwrap_or_default();
			registers[$dst as usize] = Some(result.to_value(&luant));
		} };
		($dst:expr, $a:expr, $op:tt) => { {
			let a = Value::coerce_num(registers[$a as usize].as_ref()).map_err(|e| (frame.pc, e))?;
			let result = a.$op().unwrap_or_default();
			registers[$dst as usize] = Some(result.to_value(&luant));
		} };
	}
	
	macro_rules! logical_op {
		($dst:expr, $a:expr, $b:expr, $op:tt) => { {
			let a = &registers[$a as usize];
			let b = &registers[$b as usize];
			registers[$dst as usize] = Some(Value::Bool(a $op b));
		} };
	}

	macro_rules! bitwise_op {
		($dst:expr, $a:expr, $b:expr, |$ai:ident, $bi:ident| $f:expr) => { {
			let a = &registers[$a as usize];
			let b = &registers[$b as usize];
			match (a, b) {
				(Some(Value::Num(a)), Some(Value::Num(b))) => {
					let (Some($ai), Some($bi)) = (a.as_i64(), b.as_i64()) else {
						return Err((frame.pc, "Attempted to perform bitwise operation on non-integer number".into()));
					};
					let result = { $f };
					registers[$dst as usize] = Some(result.to_value(&luant));
				}
				_ => return Err((frame.pc, "Attempted to perform bitwise operation on non-number value".into())),
			}
		} };
		($dst:expr, $a:expr, |$ai:ident| $f:expr) => { {
			let a = &registers[$a as usize];
			match a {
				Some(Value::Num(a)) => {
					let Some($ai) = a.as_i64() else {
						return Err((frame.pc, "Attempted to perform bitwise operation on non-integer number".into()));
					};
					let result = { $f };
					registers[$dst as usize] = Some(result.to_value(&luant));
				}
				_ => return Err((frame.pc, "Attempted to perform bitwise operation on non-number value".into())),
			}
		} };
	}

	loop {
		let Some(&op) = operations.get(frame.pc) else {
			break;
		};

		match op {
			Operation::LoadNil(dst, cnt) => (dst..dst + cnt).for_each(|dst| registers[dst as usize] = None),
			Operation::LoadBool(dst, val) => registers[dst as usize] = Some(Value::Bool(val)),
			Operation::LoadNum(dst, idx) => {
				let num = luant.constants.numbers[idx as usize];
				let value = Num::try_from(num).unwrap_or_default().to_value(luant);
				registers[dst as usize] = Some(value);
			},
			Operation::LoadStr(dst, idx) => {
				let s = luant.constants.strings.get(idx as usize);
				let value = s.to_value(luant);
				registers[dst as usize] = Some(value);
			},
			Operation::LoadTab(dst) => registers[dst as usize] = Some(Value::Table(Default::default())),
			Operation::LoadClosure(dst, idx) => {

			},
			Operation::Set(tab, key, val) => {
				let Some(key) = registers[key as usize].clone() else {
					return Err((frame.pc, "Attempted to use nil value as table key".into()));
				};

				let value = registers[val as usize].clone();

				let Some(Value::Table(tab)) = registers[tab as usize].as_mut() else {
					return Err((frame.pc, "Attempted to index non-table value".into()));
				};

				match value {
					Some(value) => tab.set(key, value),
					None => { tab.remove(&key); },
				}
			},
			Operation::Get(dst, tab, key) => {
				let Some(Value::Table(tab)) = registers[tab as usize].as_ref() else {
					return Err((frame.pc, "Attempted to index non-table value".into()));
				};

				let Some(key) = registers[key as usize].clone() else {
					return Err((frame.pc, "Attempted to use nil value as table key".into()));
				};

				let value = tab.get(&key);
				registers[dst as usize] = value;
			},
			Operation::Add(dst, a, b) => math_op!(dst, a, b, try_add),
			Operation::Sub(dst, a, b) => math_op!(dst, a, b, try_sub),
			Operation::Mul(dst, a, b) => math_op!(dst, a, b, try_mul),
			Operation::Div(dst, a, b) => math_op!(dst, a, b, try_div),
			Operation::Mod(dst, a, b) => math_op!(dst, a, b, try_rem),
			Operation::Pow(dst, a, b) => math_op!(dst, a, b, try_powf),
			Operation::Neg(dst, a) => math_op!(dst, a, try_neg),
			Operation::Eq(dst, a, b) => logical_op!(dst, a, b, ==),
			Operation::Neq(dst, a, b) => logical_op!(dst, a, b, !=),
			Operation::Lt(dst, a, b) => logical_op!(dst, a, b, <),
			Operation::Lte(dst, a, b) => logical_op!(dst, a, b, <=),
			Operation::Gt(dst, a, b) => logical_op!(dst, a, b, >),
			Operation::Gte(dst, a, b) => logical_op!(dst, a, b, >=),
			Operation::Not(dst, a) => 
				registers[dst as usize] = Some(Value::Bool(!registers[a as usize].as_ref().is_some_and(|v| v.is_truthy()))),
			Operation::BitAnd(dst, a, b) => bitwise_op!(dst, a, b, |a, b| a & b),
			Operation::BitOr(dst, a, b) => bitwise_op!(dst, a, b, |a, b| a | b),
			Operation::BitXor(dst, a, b) => bitwise_op!(dst, a, b, |a, b| a ^ b),
			Operation::BitShL(dst, a, b) => bitwise_op!(dst, a, b, |a, b| a << b),
			Operation::BitShR(dst, a, b) => bitwise_op!(dst, a, b, |a, b| a >> b),
			Operation::BitNot(dst, a) => bitwise_op!(dst, a, |a| !a),
			Operation::Concat(dst, a, b) => {
				let a = Value::coerce_str(registers[a as usize].as_ref(), luant).map_err(|e| (frame.pc, e));
				let b = Value::coerce_str(registers[b as usize].as_ref(), luant).map_err(|e| (frame.pc, e));
				match (a, b) {
					(Ok(a), Ok(b)) => {
						let mut buf = luant.vm_state.gc_buf.take();

						let text_len = a.len() + b.len();

						buf.clear();
						buf.reserve_exact(8 + text_len);

						buf.extend([0u8; 8]); // Placeholder for hash.

						buf.extend_from_slice(&a);
						buf.extend_from_slice(&b);

						let hash = luant.vm_state.hasher.hash_bytes(&buf[8..]);
						buf[..8].copy_from_slice(&hash.to_ne_bytes());

						// SAFETY: We just constructed `buf` to have a valid hash and valid UTF-8 data.
						let value = unsafe { types::LString::new_from_buf(buf.as_slice(), text_len) };

						registers[dst as usize] = Some(Value::Str(value));

						luant.vm_state.gc_buf.set(buf);
					}
					_ => Err("Attempted to concatenate non-string values".into()).map_err(|e| (frame.pc, e))?,
				}
			},
			Operation::Len(dst, a) => {
				let target = &registers[a as usize];
				let result = target.as_ref().and_then(|v| v.len()).ok_or_else(|| (frame.pc, "Attempted to get length of non-string/table value".into()))?;
				registers[dst as usize] = Some(result.to_value(luant));
			},
			Operation::Call(func_reg, arg_count, ret_count) => {
				let func = registers[func_reg as usize].clone();
				let args_start = func_reg as usize + 1;
				let args_end = args_start + arg_count as usize;
				let args = &registers[args_start..args_end];

				let rets = match func {
					Some(Value::Func(f)) => f(args).map_err(|e| (frame.pc, e))?,
					None => return Err((frame.pc, "Attempted to call nil value".into())),
					_ => return Err((frame.pc, "Attempted to call non-function value".into())),
				};

				for (i, ret) in rets.into_iter().enumerate().take(ret_count as usize) {
					registers[func_reg as usize + i] = ret;
				}
			},
			Operation::SkpIf(cond) => {
				let condition = registers[cond as usize].as_ref().is_some_and(|v| v.is_truthy());

				if !condition {
					if let Some(Operation::GoTo(p32, p8)) = operations.get(frame.pc + 1) {
						let position = Operation::decode_goto(*p32, *p8);
						frame.pc = position;
						continue;
					};
				} else {
					frame.pc += 1;
				}
			},
			Operation::SkpIfNot(cond) => {
				let condition = registers[cond as usize].as_ref().is_some_and(|v| v.is_truthy());

				if condition {
					if let Some(Operation::GoTo(p32, p8)) = operations.get(frame.pc + 1) {
						let position = Operation::decode_goto(*p32, *p8);
						frame.pc = position;
						continue;
					};
				} else {
					frame.pc += 1;
				}
			},
			Operation::GoTo(p32, p8) => {
				let position = Operation::decode_goto(p32, p8);
				frame.pc = position;
				continue;
			},
			Operation::Copy(dst, src) => {
				registers[dst as usize] = registers[src as usize].clone();
			},
		}
		
		frame.pc += 1;
	}

	Ok(stack)
}
