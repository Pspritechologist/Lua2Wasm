#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
	trim_prefix_suffix,
	cell_get_cloned,
)]

use std::rc::Rc;
use bstr::BStr;

mod types;
mod parsing;
mod debug;
mod globals;

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
	Ret(u8, u8),
	// Control.
	GoTo([u8; 3]),
	SkpIf(u8),
	SkpIfNot(u8),
	// Meta.
	Copy(u8, u8),
	GetUpVal(u8, u8),
	SetUpVal(u8, u8),
	GetUpTab(u8, u16),
	SetUpTab(u16, u8),
	Close(u8),
}
impl Operation {
	pub fn tmp_goto() -> Self {
		Self::GoTo([0, 0, 0])
	}

	pub fn goto(position: usize) -> Self {
		let encoded = Self::encode_goto(position);
		Self::GoTo(encoded)
	}

	pub fn encode_goto(position: usize) -> [u8; 3] {
		// Ensure the position can fit in 24 bits.
		debug_assert!(position < (1 << 24), "Goto position out of range");

		let encoded = position as u32;
		[
			((encoded >> 16) & 0xFF) as u8,
			((encoded >> 8) & 0xFF) as u8,
			(encoded & 0xFF) as u8,
		]
	}

	pub fn decode_goto(encoded: [u8; 3]) -> usize {
		let [a, b, c] = encoded;
		let combined = ((a as u32) << 16) | ((b as u32) << 8) | (c as u32);
		combined as usize
	}

	pub fn update_goto_target(&mut self, new_target: usize) {
		if let Self::GoTo(encoded) = self {
			*encoded = Self::encode_goto(new_target);
		} else {
			debug_assert!(false, "Attempted to update target of non-goto operation");
			unsafe { std::hint::unreachable_unchecked() };
		}
	}
}

struct ClosureProto {
	operations: Box<[Operation]>,
	param_count: u8,
	slots_needed: u8,
	number_offset: usize,
	string_offset: usize,
	closure_offset: usize,
	upvalues: Box<[parsing::Upvalue]>,
	debug: Option<debug::DebugInfo>,
}

struct Frame {
	pc: usize,
	stack_base: usize,
	closure_proto: usize,
	expected_to_return: u8,
}

impl Frame {
	pub fn initial() -> Self {
		Self {
			pc: 0,
			// The first slot is reserved for the main closure.
			//TODO: Handle passing arguments to main functions.
			stack_base: 2,
			closure_proto: 0,
			expected_to_return: 0,
		}
	}

	pub fn new(call_proto: usize, stack_base: usize, expected_returns: u8) -> Self {
		Self {
			pc: 0,
			stack_base,
			closure_proto: call_proto,
			expected_to_return: expected_returns,
		}
	}
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
	parsing::fmt_asm(&mut buf, &parsed)?;
	std::fs::write("out.asm", buf)?;
	
	let parsing::Parsed { parsed_func, numbers, strings, closures } = parsed;
	let strings = ConstStrings::new(strings);

	let closures: Vec<_> = std::iter::once(parsed_func).chain(closures).map(|c| {
		ClosureProto {
			operations: c.operations,
			param_count: c.param_count,
			number_offset: 0,
			string_offset: 0,
			closure_offset: 0,
			slots_needed: c.frame_size,
			upvalues: c.upvalues,
			debug: c.debug,
		}
	}).collect();

	let luant = Rc::new(Luant {
		constants: Constants {
			numbers: numbers.to_vec(),
			strings,
			closures,
		},
		..Luant::new()
	});

	let main_func = types::Closure::new(0, [types::UpvalueSlot::new(types::Upvalue::Open(0))]);

	let stack = match call_func(main_func, &luant) {
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

	pub fn new<'a>(strings: impl IntoIterator<Item = &'a BStr>) -> Self {
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

struct Luant {
	vm_state: VmState,
	constants: Constants,
	global_table: types::Table,
}

impl Luant {
	pub fn new() -> Self {
		let mut inst = Self {
			vm_state: Default::default(),
			constants: Default::default(),
			global_table: Default::default(),
		};
		inst.fill_globals();
		inst
	}
}

fn call_func(main_func: types::Closure, luant: &Rc<Luant>) -> Result<Vec<Option<types::Value>>, (usize, Box<dyn std::error::Error>)> {
	use crate::types::{Value, ToValue, Num};
		
	let luant = &**luant;
	
	let mut func_proto: &ClosureProto;
	let mut frames: Vec<Frame>;
	let mut stack: Vec<Option<Value>>;
	let mut open_upvalues: Vec<types::UpvalueSlot>;

	let mut frame;
	let mut registers;

	let mut operations;

	{
		let proto = &luant.constants.closures[main_func.proto_idx];

		func_proto = proto;
		frames = Vec::from([Frame::initial()]);
		stack = vec![None; proto.slots_needed as usize + 2];
		open_upvalues = vec![main_func.upvalues[0].clone()];
		stack[0] = Some(Value::Table(luant.global_table.clone()));
		stack[1] = Some(Value::Closure(main_func));

		frame = frames.last_mut().unwrap();
		registers = &mut stack[frame.stack_base..];

		operations = &proto.operations[..];
	}
	
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
				let idx = idx as usize + func_proto.number_offset;
				let num = luant.constants.numbers[idx];
				let value = Num::try_from(num).unwrap_or_default().to_value(luant);
				registers[dst as usize] = Some(value);
			},
			Operation::LoadStr(dst, idx) => {
				let idx = idx as usize + func_proto.string_offset;
				let s = luant.constants.strings.get(idx);
				let value = s.to_value(luant);
				registers[dst as usize] = Some(value);
			},
			Operation::LoadTab(dst) => registers[dst as usize] = Some(Value::Table(Default::default())),
			Operation::LoadClosure(dst, idx) => {
				let proto_idx = func_proto.closure_offset + (idx as usize);
				let prototype = &luant.constants.closures[proto_idx];

				let upvalues: Vec<_> = prototype.upvalues.iter().map(|&uv| match uv {
					parsing::Upvalue::ParentSlot(slot) => {
						let abs_slot = frame.stack_base + (slot as usize);
						match open_upvalues.iter().find(|uv| unsafe { uv.open_idx().unwrap_unchecked() } == abs_slot) {
							Some(upval) => upval.clone(),
							None => {
								let upval = types::UpvalueSlot::new(types::Upvalue::Open(abs_slot));
								open_upvalues.push(upval.clone());
								upval
							},
						}
					},
					parsing::Upvalue::ParentUpValue(idx) => {
						// Get the parent closure.
						let Some(Value::Closure(parent_closure)) = &stack[frame.stack_base - 1] else { unreachable!() };
						parent_closure.upvalues[idx as usize].clone()
					},
				}).collect();

				// Reset registers to avoid borrow issues.
				registers = &mut stack[frame.stack_base..];

				let closure = types::Closure::new(proto_idx, upvalues);

				registers[dst as usize] = Some(Value::Closure(closure));
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

				match func {
					Some(Value::Func(f)) => {
						// Native function pointer, simple.
						let args_start = func_reg as usize + 1;
						let args_end = args_start + arg_count as usize;
						let args = &registers[args_start..args_end];

						let rets = f(args).map_err(|e| (frame.pc, e))?;
						for (i, ret) in rets.into_iter().enumerate().take(ret_count as usize) {
							registers[func_reg as usize + i] = ret;
						}
					},
					Some(Value::Closure(c)) => {
						let closure_proto = &luant.constants.closures[c.proto_idx];
						let new_stack_base = 1 + frame.stack_base + func_reg as usize;
						let new_stack_end = new_stack_base + closure_proto.slots_needed as usize;

						let new_len = stack.len().max(new_stack_end);
						stack.resize(new_len, None);
						frames.push(Frame::new(c.proto_idx, new_stack_base, ret_count));

						func_proto = closure_proto;
						frame = frames.last_mut().unwrap();
						registers = &mut stack[frame.stack_base..new_stack_end];
						operations = &closure_proto.operations[..];

						// Zero out missing params.
						if arg_count < closure_proto.param_count {
							(arg_count..closure_proto.param_count).for_each(|i| {
								registers[i as usize] = None;
							});
						}

						// Continue to avoid incrementing the pc.
						continue;
					},
					None => return Err((frame.pc, "Attempted to call nil value".into())),
					_ => return Err((frame.pc, "Attempted to call non-function value".into())),
				}
			},
			Operation::Ret(reg, count) => {
				let ret_start = reg as usize;
				let ret_count = count as usize;

				// Close upvalues before returning from the frame
				open_upvalues.retain(|uv| !uv.close_if_above(&stack, frame.stack_base));

				let last_frame = frames.pop().unwrap();

				if frames.is_empty() {
					//TODO: Returning values and stuff.
					break;
				}

				frame = frames.last_mut().unwrap();
				func_proto = &luant.constants.closures[frame.closure_proto];

				if last_frame.expected_to_return > 0 {
					let expected_returns = last_frame.expected_to_return as usize;
					let actual_returns = ret_count.min(expected_returns);

					// Function returns are written the function's arguments.
					let dst_start = last_frame.stack_base;
					let dst_end = dst_start + actual_returns;
					let src_start = ret_start + last_frame.stack_base;
					let src_end = src_start + actual_returns;
					
					(dst_start..dst_end).zip(src_start..src_end).for_each(|(dst, src)| {
						let val = std::mem::take(&mut stack[src]);
						stack[dst] = val;
					});

					// Clear out any remaining expected return slots.
					if let Some(extra_returns) = expected_returns.checked_sub(actual_returns) {
						let clear_start = dst_start + actual_returns;
						let clear_end = clear_start + extra_returns;
						(clear_start..clear_end).for_each(|dst| {
							stack[dst] = None;
						});
					}
				}

				registers = &mut stack[frame.stack_base..];
				operations = &luant.constants.closures[frame.closure_proto].operations[..];
			},
			Operation::SkpIf(cond) => {
				let condition = registers[cond as usize].as_ref().is_some_and(|v| v.is_truthy());

				if !condition {
					if let Some(&Operation::GoTo(encoded)) = operations.get(frame.pc + 1) {
						let position = Operation::decode_goto(encoded);
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
					if let Some(&Operation::GoTo(encoded)) = operations.get(frame.pc + 1) {
						let position = Operation::decode_goto(encoded);
						frame.pc = position;
						continue;
					};
				} else {
					frame.pc += 1;
				}
			},
			Operation::GoTo(encoded) => {
				let position = Operation::decode_goto(encoded);
				frame.pc = position;
				continue;
			},
			Operation::Copy(dst, src) => {
				registers[dst as usize] = registers[src as usize].clone();
			},
			Operation::GetUpVal(dst, idx) => {
				let Some(Value::Closure(closure)) = &stack[frame.stack_base - 1] else { unreachable!() };
				let upval = &closure.upvalues[idx as usize];
				
				let value = upval.get_value(&stack);

				// Reset registers to avoid borrow issues.
				registers = &mut stack[frame.stack_base..];

				registers[dst as usize] = value;
			},
			Operation::SetUpVal(idx, src) => {
				let value = registers[src as usize].clone();

				let Some(Value::Closure(closure)) = &stack[frame.stack_base - 1] else { unreachable!() };
				let upval = &closure.upvalues[idx as usize];
				
				if let Some((idx, value)) = upval.set_value(value) {
					stack[idx] = value;
				}

				// Reset registers to avoid borrow issues.
				registers = &mut stack[frame.stack_base..];
			},
			Operation::GetUpTab(dst, key) => {
				// Indexes a field from the first Upvalue of a function (_ENV).
				let Some(Value::Closure(closure)) = &stack[frame.stack_base - 1] else { unreachable!() };
				let upval = &closure.upvalues[0];
				let Some(Value::Table(tab)) = upval.get_value(&stack) else { unsafe { std::hint::unreachable_unchecked() } };

				let idx = key as usize + func_proto.string_offset;
				let s = luant.constants.strings.get(idx);
				let key = s.to_value(luant);

				let value = tab.get(&key);

				// Reset registers to avoid borrow issues.
				registers = &mut stack[frame.stack_base..];

				registers[dst as usize] = value;
			},
			Operation::SetUpTab(key, src) => {
				// Sets a field in the first Upvalue of a function (_ENV).
				let value = registers[src as usize].clone();
				
				let Some(Value::Closure(closure)) = &stack[frame.stack_base - 1] else { unreachable!() };
				let upval = &closure.upvalues[0];
				let Some(Value::Table(mut tab)) = upval.get_value(&stack) else { unsafe { std::hint::unreachable_unchecked() } };

				let idx = key as usize + func_proto.string_offset;
				let s = luant.constants.strings.get(idx);
				let key = s.to_value(luant);

				match value {
					Some(value) => tab.set(key, value),
					None => { tab.remove(&key); },
				}

				// Reset registers to avoid borrow issues.
				registers = &mut stack[frame.stack_base..];
			},
			Operation::Close(base) => {
				let base = frame.stack_base + (base as usize);
				open_upvalues.retain(|uv| !uv.close_if_above(&stack, base));

				// Reset registers to avoid borrow issues.
				registers = &mut stack[frame.stack_base..];
			},
		}
		
		frame.pc += 1;
	}

	Ok(stack)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_goto_encoding() {
		let positions = [0, 1, 2, 12345, 54321, 0x7FFFFF, 0xFFFFFF];

		for &pos in &positions {
			let encoded = Operation::encode_goto(pos);
			let decoded_pos = Operation::decode_goto(encoded);
			assert_eq!(pos, decoded_pos, "Position mismatch for pos={pos}");
		}
	}
}
