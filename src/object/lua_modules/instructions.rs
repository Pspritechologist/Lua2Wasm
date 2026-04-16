use crate::{
	bytecode::{Loc, Operation as Op, RetKind}, object::{InstructionSink, ValueExt, lua_modules::LuaFunctionState}, parsing::expressions::{Const, Expr}
};
use value::Value;
use wasm_encoder::{BlockType, MemArg, ValType};
use super::LuaModuleState;

pub trait LuaInstSinkExt {
	fn lua_str(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, idx: usize) -> &mut Self;
	fn expr(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, expr: Expr) -> &mut Self;
	fn loc_get(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, loc: Loc) -> &mut Self;
	fn loc_set(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, loc: Loc) -> &mut Self;
	fn operations(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: impl IntoIterator<Item = Op>) -> &mut Self;
}

impl LuaInstSinkExt for InstructionSink<'_> {
	fn lua_str(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, idx: usize) -> &mut Self {
		let string = state.strings[idx];
		self.static_str(&mut state.module_state, string.sym, string.len)
	}

	fn expr(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, expr: Expr) -> &mut Self {
		expr.push(state, f_state, self);
		self
	}

	fn loc_get(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, loc: Loc) -> &mut Self {
		loc.push_get(state, f_state, self);
		self
	}

	fn loc_set(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, loc: Loc) -> &mut Self {
		loc.push_set(state, f_state, self);
		self
	}

	fn operations(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: impl IntoIterator<Item = Op>) -> &mut Self {
		let mut ops = ops.into_iter();
		while let Some(op) = ops.next() {
			compile_op(state, f_state, &mut ops, self, op);
		}
		self
	}
}

impl Loc {
	pub(super) fn push_get(self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_get(f_state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			Loc::Capture(idx) => todo!(),
			Loc::Global(idx) => {
				seq.global_get(state.module_state.global_table)
					.lua_str(state, f_state, idx)
					.call(state.module_state.extern_fns.table_get_name);
			},
		};
	}

	pub(super) fn push_set(self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_set(f_state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			Loc::Capture(idx) => todo!(),
			Loc::Global(idx) => {
				seq.global_get(state.module_state.global_table)
					.lua_str(state, f_state, idx)
					.call(state.module_state.extern_fns.table_set_name);
			},
		};
	}
}

impl Expr {
	pub(super) fn push(self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &mut InstructionSink) {
		match self {
			Expr::Constant(Const::Nil) => Value::nil().push(seq),
			Expr::Constant(Const::Bool(b)) => Value::bool(b).push(seq),
			Expr::Constant(Const::Number(n)) => Value::float(n.val()).push(seq),
			Expr::Constant(Const::String(idx)) => {
				seq.lua_str(state, f_state, idx);
			},
			Expr::Slot(idx) => Loc::Slot(idx).push_get(state, f_state, seq),
			Expr::UpValue(idx) => Loc::UpValue(idx).push_get(state, f_state, seq),
			Expr::Capture(idx) => Loc::Capture(idx).push_get(state, f_state, seq),
			Expr::Global(idx) => Loc::Global(idx).push_get(state, f_state, seq),
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

fn compile_op(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink, op: Op) {
	match op {
		Op::Add(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.add).loc_set(state, f_state, dst); },
		Op::Sub(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.sub).loc_set(state, f_state, dst); },
		Op::Mul(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.mul).loc_set(state, f_state, dst); },
		Op::Div(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.div).loc_set(state, f_state, dst); },
		Op::Mod(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.modulo).loc_set(state, f_state, dst); },
		Op::Pow(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.pow).loc_set(state, f_state, dst); },
		Op::Neg(dst, lhs) => { seq.expr(state, f_state, lhs).call(state.module_state.extern_fns.neg).loc_set(state, f_state, dst); },

		Op::Eq(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.eq).loc_set(state, f_state, dst); },
		Op::Neq(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.neq).loc_set(state, f_state, dst); },
		Op::Lt(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.lt).loc_set(state, f_state, dst); },
		Op::Lte(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.lte).loc_set(state, f_state, dst); },
		Op::Gt(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.gt).loc_set(state, f_state, dst); },
		Op::Gte(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.gte).loc_set(state, f_state, dst); },
		Op::Not(dst, lhs) => { seq.expr(state, f_state, lhs).call(state.module_state.extern_fns.not).loc_set(state, f_state, dst); },

		Op::BitAnd(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.bit_and).loc_set(state, f_state, dst); },
		Op::BitOr(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.bit_or).loc_set(state, f_state, dst); },
		Op::BitXor(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.bit_xor).loc_set(state, f_state, dst); },
		Op::BitShL(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.bit_sh_l).loc_set(state, f_state, dst); },
		Op::BitShR(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.bit_sh_r).loc_set(state, f_state, dst); },
		Op::BitNot(dst, lhs) => { seq.expr(state, f_state, lhs).call(state.module_state.extern_fns.bit_not).loc_set(state, f_state, dst); },

		Op::Concat(dst, lhs, rhs) => { seq.expr(state, f_state, lhs).expr(state, f_state, rhs).call(state.module_state.extern_fns.concat).loc_set(state, f_state, dst); },
		Op::Len(dst, lhs) => { seq.expr(state, f_state, lhs).call(state.module_state.extern_fns.len).loc_set(state, f_state, dst); },
		
		Op::Copy(dst, val) => { val.push(state, f_state, seq); dst.push_set(state, f_state, seq); },
		Op::LoadClosure(dst, idx) => { seq.push_function(&mut state.module_state, state.closures[idx].unwrap().sym); dst.push_set(state, f_state, seq); },
		Op::LoadTab(dst) => { seq.call(state.module_state.extern_fns.table_load); dst.push_set(state, f_state, seq); },
		Op::Get(dst, tab, key) => { tab.push(state, f_state, seq); key.push(state, f_state, seq); seq.call(state.module_state.extern_fns.table_get); dst.push_set(state, f_state, seq); },
		Op::Set(tab, key, val) => { tab.push(state, f_state, seq); key.push(state, f_state, seq); val.push(state, f_state, seq); seq.call(state.module_state.extern_fns.table_set); },
		Op::StartIf(cond) => compile_if(state, f_state, ops, seq, cond),
		Op::StartLoop => compile_loop(state, f_state, ops, seq),
		Op::Break => { seq.br(1); }, // A depth of 1 points to the outer block, for breaking.
		Op::BreakIf(cond) => {
			seq.expr(state, f_state, cond).call(state.module_state.extern_fns.get_truthy).br_if(1);
		},
		Op::BreakIfNot(cond) => {
			seq.expr(state, f_state, cond).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(1);
		},
		Op::Continue => { seq.br(0); }, // A depth of 0 points to the inner block, for continuing.
		Op::ContIfNot(cond) => {
			seq.expr(state, f_state, cond).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(0);
		},
		Op::Ret { ret_slot, ret_cnt } => {
			for (i, s) in (ret_slot..ret_slot + ret_cnt).enumerate() {
				seq.global_get(state.module_state.shtack_ptr)
					.loc_get(state, f_state, Loc::Slot(s))
					.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
			}
			seq.i32_const(ret_cnt.into()).return_();
		},
		Op::Call { func_slot, arg_cnt, ret_kind } => compile_call(state, f_state, seq, func_slot, arg_cnt, ret_kind),

		Op::Close(_) => todo!(),

		Op::ElseIf(..) | Op::Else | Op::EndIf | Op::EndLoop  => unreachable!(),
	}
}

fn compile_call(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &mut InstructionSink, func_slot: u8, arg_cnt: u8, ret_kind: RetKind) {
	for i in 0..arg_cnt + 1 {
		// Copy the function and the args into the shadow stack.
		seq.global_get(state.module_state.shtack_ptr)
			.loc_get(state, f_state, Loc::Slot(func_slot + i))
			.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
	}
	seq.i32_const(arg_cnt.into())
		.loc_get(state, f_state, Loc::Slot(func_slot))
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
				.loc_set(state, f_state, loc);
		},
		RetKind::Many => todo!(),
	}
}

fn compile_if(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink, cond: Expr) {
	cond.push(state, f_state, seq);
	seq.call(state.module_state.extern_fns.get_truthy);

	f_state.loop_depth += 1;

	seq.if_(BlockType::Empty);
	let next = loop {
		let op = ops.next().unwrap();

		if op == Op::EndIf { break None; }
		if op == Op::Else { seq.else_(); break Some(None); }
		if op == Op::ElseIf(cond) { seq.else_(); break Some(Some(cond)); }

		compile_op(state, f_state, ops, seq, op);
	};

	match next {
		None => (),
		Some(None) => loop {
			let op = ops.next().unwrap();
			if op == Op::EndIf { break; }
			compile_op(state, f_state, ops, seq, op)
		},
		Some(Some(cond)) => compile_if(state, f_state, ops, seq, cond),
	}

	f_state.loop_depth -= 1;

	seq.end();
}

fn compile_loop(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink) {
	// The outer block, for breaks.
	seq.block(BlockType::Empty);
	// The inner block, for looping.
	seq.loop_(BlockType::Empty);

	let old_depth = f_state.loop_depth;
	f_state.loop_depth = 0;
	while let Some(op) = ops.next() {
		if op == Op::EndLoop { break; }
		compile_op(state, f_state, ops, seq, op);
	}

	seq.end(); // End the loop block.
	seq.end(); // End the outer block.

	f_state.loop_depth = old_depth;
	
	// A jump back to the start of the loop will have been compiled already.
}
