#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
)]

mod types;

mod prelude {
	pub mod gc {
		pub use dumpster::unsync::*;
		pub use dumpster::Trace;
	}

	pub(crate) use crate::types;
	pub(crate) use crate::State;

	#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
	pub struct Num(real_float::Finite<f64>);
	impl Num {
		pub fn new(value: real_float::Finite<f64>) -> Self { Self(value) }
	}
	impl TryFrom<f64> for Num {
		type Error = real_float::InfiniteError;
		fn try_from(value: f64) -> Result<Self, Self::Error> {
			real_float::Finite::try_new(value).map(Self)
		}
	}
	impl std::ops::Deref for Num {
		type Target = real_float::Finite<f64>;
		fn deref(&self) -> &Self::Target { &self.0 }
	}
	impl std::ops::DerefMut for Num {
		fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
	}
	unsafe impl<V: dumpster::Visitor> dumpster::TraceWith<V> for Num {
		fn accept(&self, _visitor: &mut V) -> Result<(), ()> { Ok(()) }
	}
}

#[derive(Debug, Clone, Copy)]
enum Operation {
	LoadNull(u8),
	LoadBool(u8, bool),
	LoadNum(u8, u16),
	LoadStr(u8, u16),
	LoadBuf(u8, u16),
	LoadTab(u8),
	Set(u8, u8, u8),
	Get(u8, u8),
	Copy(u8, u8),
	Add(u8, u8, u8),
	Sub(u8, u8, u8),
	Mul(u8, u8, u8),
	Div(u8, u8, u8),
	Mod(u8, u8, u8),
	Pow(u8, u8, u8),
	Eq(u8, u8, u8),
	Neq(u8, u8, u8),
	Lt(u8, u8, u8),
	Lte(u8, u8, u8),
	Gt(u8, u8, u8),
	Gte(u8, u8, u8),
	GoTo(u32, u8),
	SkpIf(u8),
	SkpIfNot(u8),
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
	let (byte_code, locals, strings, nums) = parse_asm(include_str!("../src/test.asm"));
	let stack = run_vm(
		&byte_code,
		locals,
		ConstStrings::new(strings),
		&nums,
	);

	for (reg, val) in stack.iter().enumerate() {
		print!("{reg} = ");
		match val {
			Some(val) => println!("{val:#}"),
			None => println!("null"),
		}
	}
}

fn parse_asm(src: &str) -> (Vec<Operation>, u8, Vec<&str>, Vec<f64>) {
	let mut ops = Vec::new();
	let mut max_reg = 0;
	let mut numbers = Vec::new();

	let mut lines = src.lines();

	for op in &mut lines.by_ref() {
		let mut parts = op.split_whitespace();

		let Some(op) = parts.next() else {
			continue;
		};

		if op.starts_with('#') {
			continue;
		}

		if op == "-DATA-" {
			break;
		}

		macro_rules! args {
			($a:ty$(, $b:ty$(, $c:ty)?)?) => {
				(
					parts.next().map(|p| p.parse::<$a>().unwrap()).unwrap_or_default()
					$(,
						parts.next().map(|p| p.parse::<$b>().unwrap()).unwrap_or_default()
						$(, parts.next().map(|p| p.parse::<$c>().unwrap()).unwrap_or_default() )?
					)?
				)
			};
		}

		macro_rules! simple_op {
			($name:ident) => {
				simple_op!($name u8, u8, u8)
			};
			($name:ident $a:ty $(, $b:ty $(, $c:ty)?)?) => {
				{
					#[allow(unused_parens)]
					let (a $(${ignore($b)}, b $(${ignore($c)}, c)?)?) = args!($a$(, $b$(, $c)?)?);
					max_reg = max_reg.max(a);
					Operation::$name(a $(${ignore($b)}, b $(${ignore($c)}, c)?)?)
				}
			};
		}

		ops.push(match op {
			"loadnull" => simple_op!(LoadNull u8),
			"loadbool" => simple_op!(LoadBool u8, bool),
			"loadnum" => {
				let (reg, num) = args!(u8, f64);

				max_reg = max_reg.max(reg);

				let index = numbers.iter().position(|n| *n == num).unwrap_or_else(|| {
					let idx = numbers.len();
					numbers.push(num);
					idx
				});

				Operation::LoadNum(reg, index as u16)
			},
			"loadstr" => simple_op!(LoadStr u8, u16),
			"loadbuf" => simple_op!(LoadBuf u8, u16),
			"loadtab" => simple_op!(LoadTab u8),
			"set" => simple_op!(Set u8, u8, u8),
			"get" => simple_op!(Get u8, u8),
			"copy" => simple_op!(Copy u8, u8),
			"add" => simple_op!(Add),
			"sub" => simple_op!(Sub),
			"mul" => simple_op!(Mul),
			"div" => simple_op!(Div),
			"mod" => simple_op!(Mod),
			"pow" => simple_op!(Pow),
			"eq" => simple_op!(Eq),
			"neq" => simple_op!(Neq),
			"lt" => simple_op!(Lt),
			"lte" => simple_op!(Lte),
			"gt" => simple_op!(Gt),
			"gte" => simple_op!(Gte),
			"skpif" => {
				let cond = args!(u8);
				max_reg = max_reg.max(cond);
				Operation::SkpIf(cond)
			},
			"skpifnot" => {
				let cond = args!(u8);
				max_reg = max_reg.max(cond);
				Operation::SkpIfNot(cond)
			},
			"goto" => {
				let offset = args!(isize);
				let position = ops.len().strict_add_signed(offset);
				let (p32, p8) = ((position & 0xFFFF_FFFF) as u32, (position >> 32) as u8);
				Operation::GoTo(p32, p8)
			},
			op => panic!("Unknown operation: {op}"),
		});
	}

	let mut strings = Vec::new();
	strings.push("");
	for line in lines {
		strings.push(line);
	}

	(ops, max_reg + 1, strings, numbers)
}

struct ConstStrings {
	data: Vec<u8>,
	map: Vec<(usize, usize)>,
}
impl ConstStrings {
	pub fn get(&self, index: usize) -> &str {
		let (start, end) = self.map[index];
		std::str::from_utf8(&self.data[start..end]).unwrap()
	}

	pub fn push(&mut self, s: &str) -> usize {
		let start = self.data.len();
		self.data.extend_from_slice(s.as_bytes());
		let end = self.data.len();
		self.map.push((start, end));
		self.map.len() - 1
	}

	pub fn new<'a>(strings: impl IntoIterator<Item=&'a str>) -> Self {
		let mut data = Vec::new();
		let mut map = Vec::new();

		for s in strings {
			let start = data.len();
			data.extend_from_slice(s.as_bytes());
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
	}
	
	macro_rules! logical_op {
		($dst:expr, $a:expr, $b:expr, $op:tt) => { {
			let a = &registers[$a as usize];
			let b = &registers[$b as usize];
			registers[$dst as usize] = Some(Value::Bool(a $op b));
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
			Operation::Get(dst, tab) => {
				let Some(Value::Table(tab)) = registers[tab as usize].as_ref() else {
					unimplemented!()
				};

				let Some(key) = registers[dst as usize].clone() else {
					unimplemented!()
				};

				let value = tab.borrow().get(&key);
				registers[dst as usize] = value;
			},
			Operation::Copy(dst, src) => {
				registers[dst as usize] = registers[src as usize].clone();
			},
			Operation::Add(dst, a, b) => math_op!(dst, a, b, try_add),
			Operation::Sub(dst, a, b) => math_op!(dst, a, b, try_sub),
			Operation::Mul(dst, a, b) => math_op!(dst, a, b, try_mul),
			Operation::Div(dst, a, b) => math_op!(dst, a, b, try_div),
			Operation::Mod(dst, a, b) => math_op!(dst, a, b, try_rem),
			Operation::Pow(dst, a, b) => math_op!(dst, a, b, try_powf),
			Operation::Eq(dst, a, b) => logical_op!(dst, a, b, ==),
			Operation::Neq(dst, a, b) => logical_op!(dst, a, b, !=),
			Operation::Lt(dst, a, b) => logical_op!(dst, a, b, <),
			Operation::Lte(dst, a, b) => logical_op!(dst, a, b, <=),
			Operation::Gt(dst, a, b) => logical_op!(dst, a, b, >),
			Operation::Gte(dst, a, b) => logical_op!(dst, a, b, >=),
			Operation::SkpIf(cond) => {
				let Some(Value::Bool(condition)) = &registers[cond as usize] else {
					unimplemented!()
				};

				if !*condition {
					if let Some(Operation::GoTo(p32, p8)) = byte_code.get(frame.pc + 1) {
						let position = (( *p8 as usize) << 32) | (*p32 as usize);
						frame.pc = position;
						continue;
					};
				} else {
					frame.pc += 1;
				}
			},
			Operation::SkpIfNot(cond) => {
				let Some(Value::Bool(condition)) = &registers[cond as usize] else {
					unimplemented!()
				};

				if *condition {
					if let Some(Operation::GoTo(p32, p8)) = byte_code.get(frame.pc + 1) {
						let position = (( *p8 as usize) << 32) | (*p32 as usize);
						frame.pc = position;
						continue;
					};
				} else {
					frame.pc += 1;
				}
			},
			Operation::GoTo(p32, p8) => {
				let position = ((p8 as usize) << 32) | (p32 as usize);
				frame.pc = position;
				continue;
			},
		}
		
		frame.pc += 1;
	}

	stack
}
