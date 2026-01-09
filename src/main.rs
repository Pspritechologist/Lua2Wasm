#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
)]

use prelude::BStr;

mod types;
mod parsing;

mod prelude {
	pub use bstr::BStr;

	pub mod gc {
		pub use dumpster::unsync::*;
		pub use dumpster::Trace;
	}

	pub(crate) use crate::types;
	pub(crate) use types::Num;
	pub(crate) use crate::State;
}

#[derive(Debug, Clone, Copy)]
enum Operation {
	// Loads.
	LoadNull(u8),
	LoadBool(u8, bool),
	LoadNum(u8, u16),
	LoadStr(u8, u16),
	LoadBuf(u8, u16),
	LoadTab(u8),
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
	// Control.
	GoTo(u32, u8),
	SkpIf(u8),
	SkpIfNot(u8),
	// Meta.
	Copy(u8, u8),
	// Debug.
	Put(u8),
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

#[derive(Debug, Default)]
pub struct State {
	hasher: types::Hasher,
	gc_buf: Vec<u8>,
}

#[derive(Default)]
struct Frame {
	pc: usize,
	stack_base: usize,
}

fn main() {
	// let parsed = parsing::parse_asm(include_str!("../src/test.asm"));
	let parsed = parsing::parse(include_str!("../src/test.lua")).unwrap();
	let mut buf = String::new();
	parsing::fmt_asm(&mut buf, &parsed).unwrap();
	std::fs::write("out.asm", buf).unwrap();
	
	let parsing::Parsed { operations, numbers, strings, locals } = parsed;

	let stack = run_vm(
		&operations,
		locals,
		ConstStrings::new(strings),
		&numbers,
	);

	for (reg, val) in stack.iter().enumerate() {
		print!("{reg} = ");
		match val {
			Some(val) => println!("{val:#}"),
			None => println!("null"),
		}
	}
}

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
}

fn run_vm(byte_code: &[Operation], stack_size: u8, const_strs: ConstStrings, const_nums: &[f64]) -> Vec<Option<types::Value>> {
	use crate::types::{Value, ToValue};
	use crate::prelude::{Num, gc::{self, Gc}};

	let mut state = State::default();
	let mut frames: Vec<Frame> = Vec::from([Frame { pc: 0, stack_base: 0 }]);
	let mut stack: Vec<Option<Value>> = vec![None; stack_size.into()];

	let mut frame = frames.last_mut().unwrap();
	let mut registers = &mut stack[frame.stack_base..];
	
	macro_rules! math_op {
		($dst:expr, $a:expr, $b:expr, $op:tt) => { {
			let a = &registers[$a as usize];
			let b = &registers[$b as usize];
			match (a, b) {
				(Some(Value::Num(a)), Some(Value::Num(b))) => {
					let result = a.$op(**b).unwrap_or_default();
					registers[$dst as usize] = Some(result.to_value(&mut state));
				}
				_ => unimplemented!(),
			}
		} };
		($dst:expr, $a:expr, $op:tt) => { {
			let a = &registers[$a as usize];
			match a {
				Some(Value::Num(a)) => {
					let result = a.$op().unwrap_or_default();
					registers[$dst as usize] = Some(result.to_value(&mut state));
				}
				_ => unimplemented!(),
			}
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
					let (Some($ai), Some($bi)) = (a.as_int(), b.as_int()) else {
						unimplemented!();
					};
					let result = { $f };
					registers[$dst as usize] = Some(result.to_value(&mut state));
				}
				_ => unimplemented!(),
			}
		} };
		($dst:expr, $a:expr, |$ai:ident| $f:expr) => { {
			let a = &registers[$a as usize];
			match a {
				Some(Value::Num(a)) => {
					let Some($ai) = a.as_int() else {
						unimplemented!();
					};
					let result = { $f };
					registers[$dst as usize] = Some(result.to_value(&mut state));
				}
				_ => unimplemented!(),
			}
		} };
	}

	loop {
		let Some(&op) = byte_code.get(frame.pc) else {
			break;
		};

		match op {
			Operation::LoadNull(dst) => registers[dst as usize] = None,
			Operation::LoadBool(dst, val) => registers[dst as usize] = Some(Value::Bool(val)),
			Operation::LoadNum(dst, idx) => {
				let num = const_nums[idx as usize];
				let value = Num::try_from(num).unwrap_or_default().to_value(&mut state);
				registers[dst as usize] = Some(value);
			},
			Operation::LoadStr(dst, idx) => {
				let s = const_strs.get(idx as usize);
				let value = s.to_value(&mut state);
				registers[dst as usize] = Some(value);
			},
			Operation::LoadBuf(dst, size) => {
				let size = if let Some(v) = &registers[size as usize] {
					let Value::Num(n) = v else {
						unimplemented!()
					};

					n.val() as usize
				} else { 0 };

				state.gc_buf.clear();
				state.gc_buf.resize(size, 0);

				registers[dst as usize] = Some(Value::Buffer(Gc::from(state.gc_buf.as_slice())));
			},
			Operation::LoadTab(dst) => registers[dst as usize] = Some(Value::Table(Gc::default())),
			Operation::Set(tab, key, val) => {
				let Some(key) = registers[key as usize].clone() else {
					unimplemented!()
				};

				let value = registers[val as usize].clone();

				let Some(Value::Table(tab)) = registers[tab as usize].as_ref() else {
					unimplemented!()
				};

				match value {
					Some(value) => tab.borrow_mut().set(key, value),
					None => { tab.borrow_mut().remove(&key); },
				}
			},
			Operation::Get(dst, tab, key) => {
				let Some(Value::Table(tab)) = registers[tab as usize].as_ref() else {
					unimplemented!()
				};

				let Some(key) = registers[key as usize].clone() else {
					unimplemented!()
				};

				let value = tab.borrow().get(&key);
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
				let a = &registers[a as usize];
				let b = &registers[b as usize];
				match (a, b) {
					(Some(Value::Str(a)), Some(Value::Str(b))) => {
						let buf = &mut state.gc_buf;

						let text_len = a.len() + b.len();

						buf.clear();
						buf.reserve_exact(8 + text_len);

						buf.extend([0u8; 8]); // Placeholder for hash.

						buf.extend_from_slice(a);
						buf.extend_from_slice(b);

						let hash = state.hasher.hash_bytes(&buf[8..]);
						buf[..8].copy_from_slice(&hash.to_ne_bytes());

						// SAFETY: We just constructed `buf` to have a valid hash and valid UTF-8 data.
						let value = unsafe { types::LString::new_from_buf(buf.as_slice(), text_len) };

						registers[dst as usize] = Some(Value::Str(value));
					}
					_ => unimplemented!(),
				}
			},
			Operation::Len(dst, a) =>
				registers[dst as usize] = registers[a as usize].as_ref().map(|v| v.len().unwrap().to_value(&mut state)),
			Operation::SkpIf(cond) => {
				let condition = registers[cond as usize].as_ref().is_some_and(|v| v.is_truthy());

				if !condition {
					if let Some(Operation::GoTo(p32, p8)) = byte_code.get(frame.pc + 1) {
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
					if let Some(Operation::GoTo(p32, p8)) = byte_code.get(frame.pc + 1) {
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
			Operation::Put(val) => {
				match &registers[val as usize] {
					Some(v) => println!("{v:#}"),
					None => println!("<null>"),
				}
			},
		}
		
		frame.pc += 1;
	}

	stack
}
