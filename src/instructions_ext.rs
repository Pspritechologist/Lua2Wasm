use value::Value;
use wasm_encoder::{BlockType, InstructionSink, MemArg, ValType};

use crate::{State, bytecode::Loc, parsing::expressions::{Const, Expr}};

pub trait InstructionsExt<'a>: Sized {
	fn get_sink(&mut self) -> &mut InstructionSink<'a>;

	fn static_str(&mut self, state: &State, idx: usize) -> &mut InstructionSink<'a> {
		let seq = self.get_sink();
		let (addr, len) = state.strings[idx];
		seq.i32_const(addr.cast_signed())
			.i32_const(len.cast_signed())
			.call(state.extern_fns.static_str)
	}

	fn const_val(&mut self, value: Value) -> &mut InstructionSink<'a> {
		let seq = self.get_sink();
		value.push(seq);
		seq
	}

	fn expr(&mut self, state: &State, expr: Expr) -> &mut InstructionSink<'a> {
		let seq = self.get_sink();
		expr.push(state, seq);
		seq
	}

	fn loc_get(&mut self, state: &State, loc: Loc) -> &mut InstructionSink<'a> {
		let seq = self.get_sink();
		loc.push_get(state, seq);
		seq
	}

	fn loc_set(&mut self, state: &mut State, loc: Loc) -> &mut InstructionSink<'a> {
		let seq = self.get_sink();
		loc.push_set(state, seq);
		seq
	}
}

impl<'a> InstructionsExt<'a> for InstructionSink<'a> {
	fn get_sink(&mut self) -> &mut InstructionSink<'a> { self }
}

pub trait ValueExt {
	fn push(self, seq: &mut InstructionSink);
}
impl ValueExt for Value {
	fn push(self, seq: &mut InstructionSink) {
		seq.i64_const(self.as_i64());
	}
}

impl Loc {
	pub fn push_get(self, state: &State, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_get(state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			Loc::Global(idx) => {
				seq.global_get(state.global_table)
					.static_str(state, idx)
					.call(state.extern_fns.table_get_name);
			},
		};
	}

	pub fn push_set(self, state: &mut State, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_set(state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			Loc::Global(idx) => {
				seq.global_get(state.global_table)
					.static_str(state, idx)
					.call(state.extern_fns.table_set_name);
			},
		};
	}
}

impl Expr {
	pub fn push(self, state: &State, seq: &mut InstructionSink) {
		match self {
			Expr::Constant(Const::Nil) => Value::nil().push(seq),
			Expr::Constant(Const::Bool(b)) => Value::bool(b).push(seq),
			Expr::Constant(Const::Number(n)) => Value::float(n.val()).push(seq),
			Expr::Constant(Const::String(idx)) => {
				let (addr, len) = state.strings[idx];
				Value::string(addr, len).push(seq);
			},
			Expr::Slot(idx) => Loc::Slot(idx).push_get(state, seq),
			Expr::UpValue(idx) => Loc::UpValue(idx).push_get(state, seq),
			Expr::Global(idx) => Loc::Global(idx).push_get(state, seq),
			Expr::VarRet => {
				seq.i32_const(0)
					// .binop(BinaryOp::I32Eq)
					.i32_eq()
					.if_(BlockType::Result(ValType::I64))
					.i64_const(Value::nil().as_i64())
					.else_()
					.global_get(state.shtack_ptr)
					.i64_load(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem })
					.end();
			},
			Expr::VarArgs => todo!(),
		};
	}
}
