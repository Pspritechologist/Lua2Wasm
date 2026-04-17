use crate::bytecode::{Loc, Operation as Op, RetKind};
use crate::object::{InstructionSink, IntoPushedValue, ModuleState, StringRef, lua_modules::LuaFunctionState};
use crate::parsing::expressions::{Const, Expr};
use value::Value;
use wasm_encoder::{BlockType, MemArg, ValType};
use super::LuaModuleState;

use crate::object::instructions::operations::*;

pub trait LuaInstSinkExt {
	fn loc_set(&mut self, value: impl IntoPushedValue, state: &mut ModuleState, f_state: &LuaFunctionState, strings: &[StringRef], loc: Loc) -> &mut Self;
	fn operations(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: impl IntoIterator<Item = Op>) -> &mut Self;
}

impl LuaInstSinkExt for InstructionSink<'_> {
	fn loc_set(&mut self, value: impl IntoPushedValue, state: &mut ModuleState, f_state: &LuaFunctionState, strings: &[StringRef], loc: Loc) -> &mut Self {
		loc.push_set(state, f_state, strings, self, value);
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

pub struct PushExpr<'a>(Expr, &'a LuaFunctionState, &'a [StringRef]);
impl IntoPushedValue for PushExpr<'_> {
	fn push(self, state: &mut ModuleState, seq: &mut InstructionSink) {
		let PushExpr(expr, f_state, strings) = self;

		match expr {
			Expr::Constant(Const::Nil) => Value::nil().push(state, seq),
			Expr::Constant(Const::Bool(b)) => Value::bool(b).push(state, seq),
			Expr::Constant(Const::Number(n)) => Value::float(n.val()).push(state, seq),
			Expr::Constant(Const::String(idx)) => strings[idx].push(state, seq),
			Expr::Slot(idx) => Loc::Slot(idx).push_get(state, f_state, strings, seq),
			Expr::UpValue(idx) => Loc::UpValue(idx).push_get(state, f_state, strings, seq),
			Expr::Capture(idx) => Loc::Capture(idx).push_get(state, f_state, strings, seq),
			Expr::Global(idx) => Loc::Global(idx).push_get(state, f_state, strings, seq),
			Expr::VarRet => {
				todo!() //TODO
				// seq.i32_const(0)
				// 	.i32_eq()
				// 	.if_(BlockType::Result(ValType::I64))
				// 	.i64_const(Value::nil().as_i64())
				// 	.else_()
				// 	.global_get(state.module_state.shtack_ptr)
				// 	.i64_load(MemArg { align: 3, offset: 0, memory_index: state.module_state.shtack_mem })
				// 	.end();
			},
			Expr::VarArgs => todo!(),
		};
	}
}

pub struct PushLoc<'a>(Loc, &'a mut LuaFunctionState, &'a [StringRef]);
impl IntoPushedValue for PushLoc<'_> {
	fn push(self, state: &mut ModuleState, seq: &mut InstructionSink) {
		let PushLoc(loc, f_state, strings) = self;
		loc.push_get(state, f_state, strings, seq);
	}
}

impl Loc {
	pub(super) fn push_get(self, state: &mut ModuleState, f_state: &LuaFunctionState, strings: &[StringRef], seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_get(f_state.locals[&idx]); },
			Loc::UpValue(idx) => {
				seq.local_get(f_state.local_shtack_fn)
					.i32_const(idx.into())
					.call(state.extern_fns.read_upvalue);
			},
			Loc::Capture(idx) => {
				seq.local_get(f_state.local_shtack_base)
					.local_get(f_state.local_arg_count)
					.i32_add()
					.i64_load(MemArg { align: 3, offset: idx.into(), memory_index: state.shtack_mem });
			},
			Loc::Global(idx) => {
				// The _ENV Upvalue is always 0.
				Loc::UpValue(0).push_get(state, f_state, strings, seq);
				seq.push(state, strings[idx])
					.call(state.extern_fns.table_get_name);
			},
		};
	}

	pub(super) fn push_set(self, state: &mut ModuleState, f_state: &LuaFunctionState, strings: &[StringRef], seq: &mut InstructionSink, value: impl IntoPushedValue) {
		match self {
			Loc::Slot(idx) => seq.push(state, value).local_set(f_state.locals[&idx]),
			Loc::UpValue(idx) => {
				seq.local_get(f_state.local_shtack_fn)
					.i32_const(idx.into())
					.push(state, value)
					.call(state.extern_fns.write_upvalue)
			},
			Loc::Capture(idx) => {
				seq.local_get(f_state.local_shtack_base)
					.local_get(f_state.local_arg_count)
					.i32_add()
					.push(state, value)
					.i64_store(MemArg { align: 3, offset: idx.into(), memory_index: state.shtack_mem })
			},
			Loc::Global(idx) => {
				seq.push(state, value)
					.global_get(state.global_table)
					.push(state, strings[idx])
					.call(state.extern_fns.table_set_name)
			},
		};
	}
}

fn compile_op(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink, op: Op) {
	match op {
		Op::Add(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.add), &mut state.module_state, f_state, &state.strings, dst),
		Op::Sub(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.sub), &mut state.module_state, f_state, &state.strings, dst),
		Op::Mul(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.mul), &mut state.module_state, f_state, &state.strings, dst),
		Op::Div(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.div), &mut state.module_state, f_state, &state.strings, dst),
		Op::Mod(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.modulo), &mut state.module_state, f_state, &state.strings, dst),
		Op::Pow(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.pow), &mut state.module_state, f_state, &state.strings, dst),
		Op::Neg(dst, lhs) => seq.loc_set(UnOp(PushExpr(lhs, f_state, &state.strings), state.module_state.extern_fns.neg), &mut state.module_state, f_state, &state.strings, dst),

		Op::Eq(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.eq), &mut state.module_state, f_state, &state.strings, dst),
		Op::Neq(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.neq), &mut state.module_state, f_state, &state.strings, dst),
		Op::Lt(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.lt), &mut state.module_state, f_state, &state.strings, dst),
		Op::Lte(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.lte), &mut state.module_state, f_state, &state.strings, dst),
		Op::Gt(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.gt), &mut state.module_state, f_state, &state.strings, dst),
		Op::Gte(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.gte), &mut state.module_state, f_state, &state.strings, dst),
		Op::Not(dst, lhs) => seq.loc_set(UnOp(PushExpr(lhs, f_state, &state.strings), state.module_state.extern_fns.not), &mut state.module_state, f_state, &state.strings, dst),

		Op::BitAnd(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.bit_and), &mut state.module_state, f_state, &state.strings, dst),
		Op::BitOr(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.bit_or), &mut state.module_state, f_state, &state.strings, dst),
		Op::BitXor(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.bit_xor), &mut state.module_state, f_state, &state.strings, dst),
		Op::BitShL(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.bit_sh_l), &mut state.module_state, f_state, &state.strings, dst),
		Op::BitShR(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.bit_sh_r), &mut state.module_state, f_state, &state.strings, dst),
		Op::BitNot(dst, lhs) => seq.loc_set(UnOp(PushExpr(lhs, f_state, &state.strings), state.module_state.extern_fns.bit_not), &mut state.module_state, f_state, &state.strings, dst),

		Op::Concat(dst, lhs, rhs) => seq.loc_set(BinOp(PushExpr(lhs, f_state, &state.strings), PushExpr(rhs, f_state, &state.strings), state.module_state.extern_fns.concat), &mut state.module_state, f_state, &state.strings, dst),
		Op::Len(dst, lhs) => seq.loc_set(UnOp(PushExpr(lhs, f_state, &state.strings), state.module_state.extern_fns.len), &mut state.module_state, f_state, &state.strings, dst),
		
		Op::Copy(dst, val) => seq.loc_set(PushExpr(val, f_state, &state.strings), &mut state.module_state, f_state, &state.strings, dst),
		Op::LoadClosure(dst, idx) => seq.loc_set(LuaClosure {
			sym: state.closures[idx].as_ref().unwrap().0.sym,
			upvalues: &state.closures[idx].as_ref().unwrap().1,
			local_shtack_base: f_state.local_shtack_base,
			local_shtack_fn: f_state.local_shtack_fn,
		}, &mut state.module_state, f_state, &state.strings, dst),
		Op::LoadTab(dst) => seq.loc_set(NewTable, &mut state.module_state, f_state, &state.strings, dst),
		Op::Get(dst, tab, key) => seq.loc_set(GetTabField::new(PushExpr(tab, f_state, &state.strings), PushExpr(key, f_state, &state.strings)), &mut state.module_state, f_state, &state.strings, dst),
		Op::Set(tab, key, value) => seq.push(&mut state.module_state, SetTabField::new(PushExpr(tab, f_state, &state.strings), PushExpr(key, f_state, &state.strings), PushExpr(value, f_state, &state.strings))),
		Op::StartIf(cond) => compile_if(state, f_state, ops, seq, cond),
		Op::StartLoop => compile_loop(state, f_state, ops, seq),
		Op::Break => seq.br(1), // A depth of 1 points to the outer block, for breaking.
		Op::BreakIf(cond) => seq.push(&mut state.module_state, PushExpr(cond, f_state, &state.strings)).call(state.module_state.extern_fns.get_truthy).br_if(1),
		Op::BreakIfNot(cond) => seq.push(&mut state.module_state, PushExpr(cond, f_state, &state.strings)).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(1),
		Op::Continue => seq.br(0), // A depth of 0 points to the inner block, for continuing.
		Op::ContIfNot(cond) => seq.push(&mut state.module_state, PushExpr(cond, f_state, &state.strings)).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(0),
		Op::Ret { ret_slot, ret_cnt } => {
			for (i, s) in (ret_slot..ret_slot + ret_cnt).enumerate() {
				seq.local_get(f_state.local_shtack_base)
					.push(&mut state.module_state, PushLoc(Loc::Slot(s), f_state, &state.strings))
					.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
			}
			seq.i32_const(ret_cnt.into()).return_()
		},
		Op::Call { func_slot, arg_cnt, ret_kind } => compile_call(state, f_state, seq, func_slot, arg_cnt, ret_kind),

		Op::Close(_) => todo!(),

		Op::ElseIf(..) | Op::Else | Op::EndIf | Op::EndLoop  => unreachable!(),
	};
}

fn compile_call<'a, 's>(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &'a mut InstructionSink<'s>, func_slot: u8, arg_cnt: u8, ret_kind: RetKind) -> &'a mut InstructionSink<'s> {
	let get_next_shtack_base = {
		let shtack_base = f_state.local_shtack_base;
		let frame_size = f_state.local_frame_size;
		move |seq: &mut InstructionSink| { seq.local_get(shtack_base).local_get(frame_size).i32_add(); }
	};

	for i in 0..arg_cnt + 1 {
		// Copy the function and the args into the shadow stack.
		get_next_shtack_base(seq);
		seq.push(&mut state.module_state, PushLoc(Loc::Slot(func_slot + i), f_state, &state.strings))
			.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
	}

	// Get the arg count.
	seq.i32_const(arg_cnt.into());
	// Get the shtack base.
	get_next_shtack_base(seq);
	seq.i32_const(1).i32_add();
	// Finally get the function itself and call it.
	seq.push(&mut state.module_state, PushLoc(Loc::Slot(func_slot), f_state, &state.strings))
		.call_as_lua_fn(&mut state.module_state);

	match ret_kind {
		RetKind::None => seq.drop(),
		RetKind::Single(loc) => {
			seq.loc_set(|state: &mut ModuleState, seq: &mut InstructionSink<'_>| {
				seq.i32_const(0)
					.i32_eq()
					.if_(BlockType::Result(ValType::I64))
					.i64_const(Value::nil().as_i64())
					.else_();
				get_next_shtack_base(seq);
				seq.i32_const(1).i32_add();
				seq.i64_load(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem });
			}, &mut state.module_state, f_state, &state.strings, loc)
		},
		RetKind::Many => todo!(),
	}
}

fn compile_if<'s, 'a>(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &'a mut InstructionSink<'s>, cond: Expr) -> &'a mut InstructionSink<'s> {
	PushExpr(cond, f_state, &state.strings).push(&mut state.module_state, seq);
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
		Some(Some(cond)) => { compile_if(state, f_state, ops, seq, cond); } ,
	}

	f_state.loop_depth -= 1;

	seq.end()
}

fn compile_loop<'a, 's>(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &'a mut InstructionSink<'s>) -> &'a mut InstructionSink<'s> {
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

	seq
}
