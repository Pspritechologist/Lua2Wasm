use datatypes::Frame;
use bytecode::Operation;

use crate::Luant;
use crate::parsing::Upvalue; // Don't love this being used here.
use types::*;

pub(crate) mod bytecode;
pub(crate) mod datatypes;
pub use datatypes::LuantState;
pub mod debug;
pub mod types;
mod globals;

pub struct Function {
	luant: Luant,
	closure: types::Closure,
	frames: Vec<Frame>,
	stack: Vec<Option<types::Value>>,
	open_upvalues: Vec<types::UpvalueSlot>,
}

impl Function {
	pub fn new(luant: Luant, closure: types::Closure) -> Self {
		Self {
			luant,
			closure,
			frames: vec![],
			stack: vec![],
			open_upvalues: vec![],
		}
	}

	pub fn run(&mut self) -> Result<&[Option<Value>], Box<dyn std::error::Error>> {
		self.init();

		execute_ops(
			&self.luant.borrow(),
			&mut self.frames,
			&mut self.stack,
			&mut self.open_upvalues,
			0..,
		).unwrap().map_err(|(_, e)| e)
	}

	fn init(&mut self) {
		let (luant, closure) = (&self.luant, &self.closure);

		let initial_stack_size = luant.borrow().constants.closures[closure.proto_idx].slots_needed;
		self.stack.resize(initial_stack_size as usize + 2, None);

		self.stack[0] = Some(types::Value::Table(luant.borrow().global_table.clone()));
		self.stack[1] = Some(types::Value::Closure(closure.clone()));

		self.open_upvalues.resize_with(1, || closure.upvalues[0].clone());

		self.frames.resize_with(1, Frame::initial);
	}
}

struct ThroughState<'a> {
	luant: &'a LuantState,
	frames: &'a mut Vec<Frame>,
	stack: &'a mut Vec<Option<Value>>,
	open_upvalues: &'a mut Vec<types::UpvalueSlot>,
}
impl ThroughState<'_> {
	fn get_slots(&self, range: std::ops::Range<usize>) -> &[Option<Value>] {
		&self.stack[range]
	}
}

fn execute_ops<'a>(
	luant: &LuantState,
	frames: &mut Vec<Frame>,
	stack: &'a mut Vec<Option<Value>>,
	open_upvalues: &mut Vec<types::UpvalueSlot>,
	execution_range: impl Iterator,
) -> Option<Result<&'a [Option<Value>], (usize, Box<dyn std::error::Error>)>> {
	let mut func_proto = {
		let frame = frames.last().unwrap();
		&luant.constants.closures[frame.closure_proto]	
	};
	let mut frame = frames.last_mut().unwrap();
	let mut registers = &mut stack[frame.stack_base..];
	let mut operations = &func_proto.operations[..];
	
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
						return Some(Err((frame.pc, "Attempted to perform bitwise operation on non-integer number".into())));
					};
					let result = { $f };
					registers[$dst as usize] = Some(result.to_value(&luant));
				}
				_ => return Some(Err((frame.pc, "Attempted to perform bitwise operation on non-number value".into()))),
			}
		} };
		($dst:expr, $a:expr, |$ai:ident| $f:expr) => { {
			let a = &registers[$a as usize];
			match a {
				Some(Value::Num(a)) => {
					let Some($ai) = a.as_i64() else {
						return Some(Err((frame.pc, "Attempted to perform bitwise operation on non-integer number".into())));
					};
					let result = { $f };
					registers[$dst as usize] = Some(result.to_value(&luant));
				}
				_ => return Some(Err((frame.pc, "Attempted to perform bitwise operation on non-number value".into()))),
			}
		} };
	}

	for _ in execution_range {
		let res = try {
			let Some(&op) = operations.get(frame.pc) else {
				debug_assert!(false, "Program counter out of bounds");
				unsafe { std::hint::unreachable_unchecked(); }
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
						Upvalue::ParentSlot(slot) => {
							let abs_slot = frame.stack_base + (slot as usize);
							match open_upvalues.iter().find(|uv| unsafe { uv.open_idx().unwrap_unchecked() } == abs_slot) {
								Some(upval) => upval.clone(),
								None => {
									let upval = types::UpvalueSlot::new(types::UpvalueRef::Open(abs_slot));
									open_upvalues.push(upval.clone());
									upval
								},
							}
						},
						Upvalue::ParentUpValue(idx) => {
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
						return Some(Err((frame.pc, "Attempted to use nil value as table key".into())));
					};

					let value = registers[val as usize].clone();

					let Some(Value::Table(tab)) = registers[tab as usize].as_mut() else {
						return Some(Err((frame.pc, "Attempted to index non-table value".into())));
					};

					match value {
						Some(value) => tab.set(key, value),
						None => { tab.remove(&key); },
					}
				},
				Operation::Get(dst, tab, key) => {
					let Some(Value::Table(tab)) = registers[tab as usize].as_ref() else {
						return Some(Err((frame.pc, "Attempted to index non-table value".into())));
					};

					let Some(key) = registers[key as usize].clone() else {
						return Some(Err((frame.pc, "Attempted to use nil value as table key".into())));
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
							let args_range = args_start..args_end;

							let rets = {
								let state = ThroughState { luant, frames, stack, open_upvalues };
								let res = f(state, args_range).map_err(|e| (0, e));
								frame = frames.last_mut().unwrap();
								registers = &mut stack[frame.stack_base..];
								res
							}?;

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
						None => return Some(Err((frame.pc, "Attempted to call nil value".into()))),
						_ => return Some(Err((frame.pc, "Attempted to call non-function value".into()))),
					}
				},
				Operation::Ret(reg, count) => {
					let ret_start = reg as usize;
					let ret_count = count as usize;

					// Close upvalues before returning from the frame
					open_upvalues.retain(|uv| !uv.close_if_above(stack, frame.stack_base));

					let last_frame = frames.pop().unwrap();

					if frames.is_empty() {
						let start = ret_start + last_frame.stack_base;
						let end = start + ret_count;
						return Some(Ok(&stack[start..end]));
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
					
					let value = upval.get_value(stack);

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
					let Some(Value::Table(tab)) = upval.get_value(stack) else { unsafe { std::hint::unreachable_unchecked() } };

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
					let Some(Value::Table(mut tab)) = upval.get_value(stack) else { unsafe { std::hint::unreachable_unchecked() } };

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
					open_upvalues.retain(|uv| !uv.close_if_above(stack, base));

					// Reset registers to avoid borrow issues.
					registers = &mut stack[frame.stack_base..];
				},
			}
			
			frame.pc += 1;
		};

		if let Err(e) = res {
			return Some(Err(e));
		}
	}
	
	None
}
