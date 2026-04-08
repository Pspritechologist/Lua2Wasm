use crate::{
	object::{ClosureRef, InstructionSink, ValueExt},
	bytecode::{Loc, Operation as Op, RetKind},
	parsing::expressions::{Const, Expr},
};
use value::Value;
use wasm_encoder::{BlockType, MemArg, ValType};
use super::LuaModuleState;

pub trait LuaInstSinkExt {
	fn lua_str(&mut self, state: &mut LuaModuleState, idx: usize) -> &mut Self;
	fn expr(&mut self, state: &mut LuaModuleState, expr: Expr) -> &mut Self;
	fn loc_get(&mut self, state: &mut LuaModuleState, loc: Loc) -> &mut Self;
	fn loc_set(&mut self, state: &mut LuaModuleState, loc: Loc) -> &mut Self;
	fn operations(&mut self, state: &mut LuaModuleState, ops: impl IntoIterator<Item = Op>) -> &mut Self;
}

impl LuaInstSinkExt for InstructionSink<'_> {
	fn lua_str(&mut self, state: &mut LuaModuleState, idx: usize) -> &mut Self {
		let string = state.strings[idx];
		self.static_str(&mut state.module_state, string.sym, string.len)
	}

	fn expr(&mut self, state: &mut LuaModuleState, expr: Expr) -> &mut Self {
		expr.push(state, self);
		self
	}

	fn loc_get(&mut self, state: &mut LuaModuleState, loc: Loc) -> &mut Self {
		loc.push_get(state, self);
		self
	}

	fn loc_set(&mut self, state: &mut LuaModuleState, loc: Loc) -> &mut Self {
		loc.push_set(state, self);
		self
	}

	fn operations(&mut self, state: &mut LuaModuleState, ops: impl IntoIterator<Item = Op>) -> &mut Self {
		let mut ops = ops.into_iter();
		while let Some(op) = ops.next() {
			compile_op(state, &mut ops, self, op);
		}
		self
	}
}

impl Loc {
	pub fn push_get(self, state: &mut LuaModuleState, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_get(state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			Loc::Global(idx) => {
				seq.global_get(state.module_state.global_table)
					.lua_str(state, idx)
					.call(state.module_state.extern_fns.table_get_name);
			},
		};
	}

	pub fn push_set(self, state: &mut LuaModuleState, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_set(state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			Loc::Global(idx) => {
				seq.global_get(state.module_state.global_table)
					.lua_str(state, idx)
					.call(state.module_state.extern_fns.table_set_name);
			},
		};
	}
}

impl Expr {
	pub fn push(self, state: &mut LuaModuleState, seq: &mut InstructionSink) {
		match self {
			Expr::Constant(Const::Nil) => Value::nil().push(seq),
			Expr::Constant(Const::Bool(b)) => Value::bool(b).push(seq),
			Expr::Constant(Const::Number(n)) => Value::float(n.val()).push(seq),
			Expr::Constant(Const::String(idx)) => {
				seq.lua_str(state, idx);
			},
			Expr::Slot(idx) => Loc::Slot(idx).push_get(state, seq),
			Expr::UpValue(idx) => Loc::UpValue(idx).push_get(state, seq),
			Expr::Global(idx) => Loc::Global(idx).push_get(state, seq),
			Expr::VarRet => {
				seq.i32_const(0)
					.i32_eq()
					.if_(BlockType::Result(ValType::I64))
					.i64_const(Value::nil().as_i64())
					.else_()
					.global_get(state.module_state.shtack_ptr)
					.i64_load(MemArg { align: 3, offset: 0, memory_index: state.module_state.shtack_mem })
					.end();
			},
			Expr::VarArgs => todo!(),
		};
	}
}

fn compile_op(state: &mut LuaModuleState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink, op: Op) {
	match op {
		Op::Add(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.add).loc_set(state, dst); },
		Op::Sub(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.sub).loc_set(state, dst); },
		Op::Mul(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.mul).loc_set(state, dst); },
		Op::Div(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.div).loc_set(state, dst); },
		Op::Mod(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.modulo).loc_set(state, dst); },
		Op::Pow(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.pow).loc_set(state, dst); },
		Op::Neg(dst, lhs) => { seq.expr(state, lhs).call(state.module_state.extern_fns.neg).loc_set(state, dst); },

		Op::Eq(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.eq).loc_set(state, dst); },
		Op::Neq(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.neq).loc_set(state, dst); },
		Op::Lt(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.lt).loc_set(state, dst); },
		Op::Lte(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.lte).loc_set(state, dst); },
		Op::Gt(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.gt).loc_set(state, dst); },
		Op::Gte(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.gte).loc_set(state, dst); },
		Op::Not(dst, lhs) => { seq.expr(state, lhs).call(state.module_state.extern_fns.not).loc_set(state, dst); },

		Op::BitAnd(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.bit_and).loc_set(state, dst); },
		Op::BitOr(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.bit_or).loc_set(state, dst); },
		Op::BitXor(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.bit_xor).loc_set(state, dst); },
		Op::BitShL(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.bit_sh_l).loc_set(state, dst); },
		Op::BitShR(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.bit_sh_r).loc_set(state, dst); },
		Op::BitNot(dst, lhs) => { seq.expr(state, lhs).call(state.module_state.extern_fns.bit_not).loc_set(state, dst); },

		Op::Concat(dst, lhs, rhs) => { seq.expr(state, lhs).expr(state, rhs).call(state.module_state.extern_fns.concat).loc_set(state, dst); },
		Op::Len(dst, lhs) => { seq.expr(state, lhs).call(state.module_state.extern_fns.len).loc_set(state, dst); },
		
		Op::Copy(dst, val) => { val.push(state, seq); dst.push_set(state, seq); },
		Op::LoadClosure(dst, idx) => { seq.push_function(&mut state.module_state, state.closures[idx].unwrap().sym); dst.push_set(state, seq); },
		Op::LoadTab(dst) => { seq.call(state.module_state.extern_fns.table_load); dst.push_set(state, seq); },
		Op::Get(dst, tab, key) => { tab.push(state, seq); key.push(state, seq); seq.call(state.module_state.extern_fns.table_get); dst.push_set(state, seq); },
		Op::Set(tab, key, val) => { tab.push(state, seq); key.push(state, seq); val.push(state, seq); seq.call(state.module_state.extern_fns.table_set); },
		Op::StartIf(cond) => compile_if(state, ops, seq, cond),
		Op::StartLoop => compile_loop(state, ops, seq),
		Op::Break => { seq.br(1); }, // A depth of 1 points to the outer block, for breaking.
		Op::BreakIfNot(cond) => {
			seq.expr(state, cond).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(1);
		},
		Op::Continue => { seq.br(0); }, // A depth of 0 points to the inner block, for continuing.
		Op::ContIfNot(cond) => {
			seq.expr(state, cond).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(0);
		},
		Op::Ret { ret_slot, ret_cnt } => {
			for (i, s) in (ret_slot..ret_slot + ret_cnt).enumerate() {
				seq.global_get(state.module_state.shtack_ptr)
					.loc_get(state, Loc::Slot(s))
					.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
			}
			seq.i32_const(ret_cnt.into()).return_();
		},
		Op::Call { func_slot, arg_cnt, ret_kind } => {
			for i in 0..arg_cnt {
				seq.global_get(state.module_state.shtack_ptr)
					.loc_get(state, Loc::Slot(func_slot + i + 1))
					.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
			}
			seq.i32_const(arg_cnt.into())
				.loc_get(state, Loc::Slot(func_slot))
				.call_as_lua_fn(&mut state.module_state);

			match ret_kind {
				RetKind::None => { seq.drop(); },
				RetKind::Single(loc) => {
					seq.i32_const(0)
						.i32_eq()
						.if_(BlockType::Result(ValType::I64))
						.i64_const(Value::nil().as_i64())
						.else_()
						.global_get(state.module_state.shtack_ptr)
						.i64_load(MemArg { align: 3, offset: 0, memory_index: state.module_state.shtack_mem })
						.loc_set(state, loc);
				},
				RetKind::Many => todo!(),
			}
		},

		Op::Close(_) => todo!(),

		Op::ElseIf(..) | Op::Else | Op::EndIf | Op::EndLoop  => unreachable!(),
	}
}

fn compile_if(state: &mut LuaModuleState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink, cond: Expr) {
	cond.push(state, seq);
	seq.call(state.module_state.extern_fns.get_truthy);

	state.loop_depth += 1;

	seq.if_(BlockType::Empty);
	let next = loop {
		let op = ops.next().unwrap();

		if op == Op::EndIf { break None; }
		if op == Op::Else { seq.else_(); break Some(None); }
		if op == Op::ElseIf(cond) { seq.else_(); break Some(Some(cond)); }

		compile_op(state, ops, seq, op);
	};

	match next {
		None => (),
		Some(None) => loop {
			let op = ops.next().unwrap();
			if op == Op::EndIf { break; }
			compile_op(state, ops, seq, op)
		},
		Some(Some(cond)) => compile_if(state, ops, seq, cond),
	}

	state.loop_depth -= 1;

	seq.end();
}

fn compile_loop(state: &mut LuaModuleState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink) {
	// The outer block, for breaks.
	seq.block(BlockType::Empty);
	// The inner block, for looping.
	seq.loop_(BlockType::Empty);

	let old_depth = state.loop_depth;
	state.loop_depth = 0;
	while let Some(op) = ops.next() {
		if op == Op::EndLoop { break; }
		compile_op(state, ops, seq, op);
	}

	seq.end(); // End the loop block.
	seq.end(); // End the outer block.

	state.loop_depth = old_depth;
	
	// A jump back to the start of the loop will have been compiled already.
}
