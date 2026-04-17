use crate::{
	bytecode::{Loc, Operation as Op, RetKind}, object::{InstructionSink, IntoPushedValue, lua_modules::LuaFunctionState}, parsing::{Upvalue, expressions::{Const, Expr}}
};
use value::Value;
use wasm_encoder::{BlockType, MemArg, ValType};
use super::LuaModuleState;

trait Reborrow {
	type Reborrowed<'b> where Self: 'b;
    fn rb(&mut self) -> Self::Reborrowed<'_>;
}
impl<A> Reborrow for &mut A {
	type Reborrowed<'b> = &'b mut A where Self: 'b;
	fn rb(&mut self) -> Self::Reborrowed<'_> {
		&mut **self
	}
}
impl<'a, A, B> Reborrow for (&'a mut A, &'a mut B) {
	type Reborrowed<'b> = (&'b mut A, &'b mut B) where Self: 'b;
    fn rb(&mut self) -> Self::Reborrowed<'_> {
        (&mut *self.0, &mut *self.1)
    }
}
impl<'a, A, B, C> Reborrow for (&'a mut A, &'a mut B, &'a mut C) {
	type Reborrowed<'b> = (&'b mut A, &'b mut B, &'b mut C) where Self: 'b;
	fn rb(&mut self) -> Self::Reborrowed<'_> {
		(&mut *self.0, &mut *self.1, &mut *self.2)
	}
}

pub trait LuaInstSinkExt {
	fn lua_str(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, idx: usize) -> &mut Self;
	fn loc_set<V>(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, loc: Loc, value: V) -> &mut Self
	where V: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>;
	fn operations(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: impl IntoIterator<Item = Op>) -> &mut Self;
	fn push<P: IntoPushedValue>(&mut self, state: P::State<'_>, value: P) -> &mut Self;
}

impl LuaInstSinkExt for InstructionSink<'_> {
	fn lua_str(&mut self, state: &mut LuaModuleState, _f_state: &mut LuaFunctionState, idx: usize) -> &mut Self {
		let string = state.strings[idx];
		self.static_str(&mut state.module_state, string.sym, string.len)
	}

	fn loc_set<V>(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, loc: Loc, value: V) -> &mut Self
	where V: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)> {
		loc.push_set(state, f_state, self, value);
		self
	}

	fn operations(&mut self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: impl IntoIterator<Item = Op>) -> &mut Self {
		let mut ops = ops.into_iter();
		while let Some(op) = ops.next() {
			compile_op(state, f_state, &mut ops, self, op);
		}
		self
	}

	fn push<P: IntoPushedValue>(&mut self, state: P::State<'_>, value: P) -> &mut Self {
		value.push(state, self);
		self
	}
}

impl IntoPushedValue for Expr {
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, (state, f_state): Self::State<'_>, seq: &mut InstructionSink) {
		match self {
			Expr::Constant(Const::Nil) => Value::nil().push((), seq),
			Expr::Constant(Const::Bool(b)) => Value::bool(b).push((), seq),
			Expr::Constant(Const::Number(n)) => Value::float(n.val()).push((), seq),
			Expr::Constant(Const::String(idx)) => {
				seq.lua_str(state, f_state, idx);
			},
			Expr::Slot(idx) => Loc::Slot(idx).push_get(state, f_state, seq),
			Expr::UpValue(idx) => Loc::UpValue(idx).push_get(state, f_state, seq),
			Expr::Capture(idx) => Loc::Capture(idx).push_get(state, f_state, seq),
			Expr::Global(idx) => Loc::Global(idx).push_get(state, f_state, seq),
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
impl IntoPushedValue for Loc {
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, (state, f_state): Self::State<'_>, seq: &mut InstructionSink) {
		self.push_get(state, f_state, seq);
	}
}
impl<T, F: FnOnce(&mut LuaModuleState, &mut LuaFunctionState, &mut InstructionSink) -> T> IntoPushedValue for F {
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, (state, f_state): Self::State<'_>, seq: &mut InstructionSink) {
		self(state, f_state, seq);
	}
}

pub struct NewTable;
impl IntoPushedValue for NewTable {
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, (state, _f_state): Self::State<'_>, seq: &mut InstructionSink) {
		seq.call(state.module_state.extern_fns.table_load);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BinOp<L, R>(pub L, pub R, pub crate::object::linking::Symbol);
impl<L: IntoPushedValue, R: IntoPushedValue> IntoPushedValue for BinOp<L, R>
where
	L: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
	R: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
{
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, mut state: Self::State<'_>, seq: &mut InstructionSink) {
		let Self(lhs, rhs, f) = self;
		seq.push(state.rb(), lhs)
			.push(state, rhs)
			.call(f);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnOp<L>(pub L, pub crate::object::linking::Symbol);
impl<L: IntoPushedValue> IntoPushedValue for UnOp<L>
where L: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
{
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, mut state: Self::State<'_>, seq: &mut InstructionSink) {
		let Self(lhs, f) = self;
		seq.push(state.rb(), lhs)
			.call(f);
	}
}

#[derive(Debug, Clone, Copy)]
pub struct LuaStaticFunction(crate::object::ClosureRef);
impl IntoPushedValue for LuaStaticFunction {
	type State<'a> = &'a mut crate::object::ModuleState;
	fn push(self, state: Self::State<'_>, seq: &mut InstructionSink) {
		let LuaStaticFunction(closure) = self;
		seq.push_function_ptr(state, closure.sym)
			.call(state.extern_fns.static_function);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LuaClosure(pub usize);
impl IntoPushedValue for LuaClosure {
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, (state, f_state): Self::State<'_>, seq: &mut InstructionSink) {
		let LuaClosure(idx) = self;
		let (closure, upvalues) = state.closures[idx].as_ref().unwrap();
		seq.push_function_ptr(&mut state.module_state, closure.sym)
			.i32_const(upvalues.len() as i32)
			.call(state.module_state.extern_fns.new_closure);

		for (i, &upval) in upvalues.iter().enumerate() {
			match upval {
				Upvalue::ParentSlot(idx) => {
					seq.i32_const(i as i32)
						.local_get(f_state.local_shtack_base)
						.i32_const(idx.into())
						.i32_add()
						.call(state.module_state.extern_fns.set_nth_upvalue);
				},
				Upvalue::ParentUpValue(idx) => {
					seq.local_get(f_state.local_shtack_fn)
						.i32_const(idx.into())
						.i32_const(i as i32)
						.call(state.module_state.extern_fns.take_nth_upvalue_into);
				},
			}
		}

		seq.call(state.module_state.extern_fns.finalize_closure);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WasmLocal(pub u32);
impl IntoPushedValue for WasmLocal {
	type State<'a> = ();
	fn push(self, (): Self::State<'_>, seq: &mut InstructionSink) {
		seq.local_get(self.0);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GetTabField<T, K> {
	pub tab: T,
	pub key: K,
}
impl<T: IntoPushedValue, K: IntoPushedValue> IntoPushedValue for GetTabField<T, K>
where
	T: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
	K: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
{
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, mut state: Self::State<'_>, seq: &mut InstructionSink) {
		let Self { tab, key } = self;
		seq.push(state.rb(), tab)
			.push(state.rb(), key)
			.call(state.0.module_state.extern_fns.table_get);
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SetTabField<T, K, V> {
	pub tab: T,
	pub key: K,
	pub value: V,
}
impl<T: IntoPushedValue, K: IntoPushedValue, V: IntoPushedValue> IntoPushedValue for SetTabField<T, K, V>
where
	T: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
	K: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
	V: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)>,
{
	type State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState);
	fn push(self, mut state: Self::State<'_>, seq: &mut InstructionSink) {
		let Self { tab, key, value } = self;
		seq.push(state.rb(), tab)
			.push(state.rb(), key)
			.push(state.rb(), value)
			.call(state.0.module_state.extern_fns.table_set);
	}
}

impl Loc {
	pub(super) fn push_get(self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &mut InstructionSink) {
		match self {
			Loc::Slot(idx) => { seq.local_get(f_state.locals[&idx]); },
			Loc::UpValue(idx) => {
				seq.local_get(f_state.local_shtack_fn)
					.i32_const(idx.into())
					.call(state.module_state.extern_fns.read_upvalue);
			},
			Loc::Capture(idx) => {
				seq.local_get(f_state.local_shtack_base)
					.local_get(f_state.local_arg_count)
					.i32_add()
					.i64_load(MemArg { align: 3, offset: idx.into(), memory_index: state.module_state.shtack_mem });
			},
			Loc::Global(idx) => {
				// The _ENV Upvalue is always 0.
				Loc::UpValue(0).push_get(state, f_state, seq);
				seq.lua_str(state, f_state, idx)
					.call(state.module_state.extern_fns.table_get_name);
			},
		};
	}

	pub(super) fn push_set<V>(self, state: &mut LuaModuleState, f_state: &mut LuaFunctionState, seq: &mut InstructionSink, value: V)
	where V: for<'a> IntoPushedValue<State<'a> = (&'a mut LuaModuleState, &'a mut LuaFunctionState)> {
		match self {
			Loc::Slot(idx) => seq.push((&mut *state, &mut *f_state), value).local_set(f_state.locals[&idx]),
			Loc::UpValue(idx) => {
				seq.local_get(f_state.local_shtack_fn)
					.i32_const(idx.into())
					.push((&mut *state, &mut *f_state), value)
					.call(state.module_state.extern_fns.write_upvalue)
			},
			Loc::Capture(idx) => {
				seq.local_get(f_state.local_shtack_base)
					.local_get(f_state.local_arg_count)
					.i32_add()
					.push((&mut *state, &mut *f_state), value)
					.i64_store(MemArg { align: 3, offset: idx.into(), memory_index: state.module_state.shtack_mem })
			},
			Loc::Global(idx) => {
				seq.push((&mut *state, &mut *f_state), value)
					.global_get(state.module_state.global_table)
					.lua_str(state, f_state, idx)
					.call(state.module_state.extern_fns.table_set_name)
			},
		};
	}
}

fn compile_op(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &mut InstructionSink, op: Op) {
	match op {
		Op::Add(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.add)),
		Op::Sub(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.sub)),
		Op::Mul(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.mul)),
		Op::Div(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.div)),
		Op::Mod(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.modulo)),
		Op::Pow(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.pow)),
		Op::Neg(dst, lhs) => seq.loc_set(state, f_state, dst, UnOp(lhs, state.module_state.extern_fns.neg)),

		Op::Eq(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.eq)),
		Op::Neq(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.neq)),
		Op::Lt(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.lt)),
		Op::Lte(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.lte)),
		Op::Gt(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.gt)),
		Op::Gte(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.gte)),
		Op::Not(dst, lhs) => seq.loc_set(state, f_state, dst, UnOp(lhs, state.module_state.extern_fns.not)),

		Op::BitAnd(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.bit_and)),
		Op::BitOr(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.bit_or)),
		Op::BitXor(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.bit_xor)),
		Op::BitShL(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.bit_sh_l)),
		Op::BitShR(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.bit_sh_r)),
		Op::BitNot(dst, lhs) => seq.loc_set(state, f_state, dst, UnOp(lhs, state.module_state.extern_fns.bit_not)),

		Op::Concat(dst, lhs, rhs) => seq.loc_set(state, f_state, dst, BinOp(lhs, rhs, state.module_state.extern_fns.concat)),
		Op::Len(dst, lhs) => seq.loc_set(state, f_state, dst, UnOp(lhs, state.module_state.extern_fns.len)),
		
		Op::Copy(dst, val) => seq.loc_set(state, f_state, dst, val),
		Op::LoadClosure(dst, idx) => seq.loc_set(state, f_state, dst, LuaClosure(idx)),
		Op::LoadTab(dst) => seq.loc_set(state, f_state, dst, NewTable),
		Op::Get(dst, tab, key) => seq.loc_set(state, f_state, dst, GetTabField { tab, key }),
		Op::Set(tab, key, value) => seq.push((&mut *state, &mut *f_state), SetTabField { tab, key, value }),
		Op::StartIf(cond) => compile_if(state, f_state, ops, seq, cond),
		Op::StartLoop => compile_loop(state, f_state, ops, seq),
		Op::Break => seq.br(1), // A depth of 1 points to the outer block, for breaking.
		Op::BreakIf(cond) => seq.push((&mut *state, &mut *f_state), cond).call(state.module_state.extern_fns.get_truthy).br_if(1),
		Op::BreakIfNot(cond) => seq.push((&mut *state, &mut *f_state), cond).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(1),
		Op::Continue => seq.br(0), // A depth of 0 points to the inner block, for continuing.
		Op::ContIfNot(cond) => seq.push((&mut *state, &mut *f_state), cond).call(state.module_state.extern_fns.get_truthy).i32_eqz().br_if(0),
		Op::Ret { ret_slot, ret_cnt } => {
			for (i, s) in (ret_slot..ret_slot + ret_cnt).enumerate() {
				seq.local_get(f_state.local_shtack_base)
					.push((&mut *state, &mut *f_state), Loc::Slot(s))
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
		seq.push((&mut *state, &mut *f_state), Loc::Slot(func_slot + i))
			.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.module_state.shtack_mem });
	}

	// Get the arg count.
	seq.i32_const(arg_cnt.into());
	// Get the shtack base.
	get_next_shtack_base(seq);
	seq.i32_const(1).i32_add();
	// Finally get the function itself and call it.
	seq.push((&mut *state, &mut *f_state), Loc::Slot(func_slot))
		.call_as_lua_fn(&mut state.module_state);

	match ret_kind {
		RetKind::None => seq.drop(),
		RetKind::Single(loc) => {
			seq.loc_set(state, f_state, loc, |state: &mut LuaModuleState, _f_state: &mut _, seq: &mut InstructionSink<'_>| {
				seq.i32_const(0)
					.i32_eq()
					.if_(BlockType::Result(ValType::I64))
					.i64_const(Value::nil().as_i64())
					.else_();
				get_next_shtack_base(seq);
				seq.i32_const(1).i32_add();
				seq.i64_load(MemArg { align: 3, offset: 0, memory_index: state.module_state.shtack_mem });
			})
		},
		RetKind::Many => todo!(),
	}
}

fn compile_if<'s, 'a>(state: &mut LuaModuleState, f_state: &mut LuaFunctionState, ops: &mut impl Iterator<Item=Op>, seq: &'a mut InstructionSink<'s>, cond: Expr) -> &'a mut InstructionSink<'s> {
	cond.push((state, f_state), seq);
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
