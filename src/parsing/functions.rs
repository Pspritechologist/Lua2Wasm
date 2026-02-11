use crate::vm::debug::{DebugInfo, SrcMap};
use super::{debug::InfoCollector, VariableScope, ParseScope, Named, LexerExt, Error, Op, expect_tok};
use luant_lexer::{Lexer, Token, IdentKey};
use bstr::BStr;
use walrus::{FunctionBuilder, LocalId, Module, ValType, ir::BinaryOp};

#[derive(Debug)]
pub struct FuncState<'a, 's> {
	constants: &'a mut super::ConstantMap<'s>,
	module: &'a mut Module,
	reg_map: Vec<Option<LocalId>>,
	builder: FunctionBuilder,
	max_slot_use: u8,
	cur_slot_use: u8,

	debug_info: InfoCollector
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
	pub operations: Box<[Op]>,
	pub debug: Option<DebugInfo>,
	pub frame_size: u8,
	pub param_count: u8,
	pub upvalues: Box<[Upvalue]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueTag {
	Nil = 0,
	Bool = 1,
	Number = 2,
	String = 3,
	Table = 4,
	Closure = 5,
}
impl ValueTag {
	fn try_from_u8(n: u8) -> Option<Self> {
		match n {
			0 => Some(ValueTag::Nil),
			1 => Some(ValueTag::Bool),
			2 => Some(ValueTag::Number),
			3 => Some(ValueTag::String),
			4 => Some(ValueTag::Table),
			5 => Some(ValueTag::Closure),
			_ => None,
		}
	}

	fn from_u8(n: u8) -> Self {
		Self::try_from_u8(n).unwrap_or_else(|| {
			debug_assert!(false, "Invalid value tag");
			unsafe { std::hint::unreachable_unchecked(); }
		})
	}

	fn as_u8(self) -> u8 {
		self as u8
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Value {
	// Bit layout:
	// [0-3]: Tag
	// [4-63]: Data
	data: [u8; 8],
}
impl Value {
	fn as_i64(self) -> i64 {
		i64::from_le_bytes(self.data)
	}
	fn from_i64(n: i64) -> Self {
		Self { data: n.to_le_bytes() }
	}

	fn set_tag(&mut self, tag: ValueTag) {
		self.data[0] = (self.data[0] & 0xF0) | (tag as u8 & 0x0F);
	}
	fn get_tag(&self) -> ValueTag {
		ValueTag::from_u8(self.data[0] & 0x0F)
	}

	fn nil() -> Self {
		Self { data: [0u8; 8] }
	}
	fn int(n: i64) -> Self {
		let mut v = Self::from_i64(n);
		v.set_tag(ValueTag::Number);
		v
	}
	fn float(n: f64) -> Self {
		let mut v = Self { data: n.to_le_bytes() };
		v.set_tag(ValueTag::Number);
		v
	}
	fn bool(b: bool) -> Self {
		let mut v = Self::from_i64(if b { 1 } else { 0 });
		v.set_tag(ValueTag::Bool);
		v
	}
}

impl<'a, 's> FuncState<'a, 's> {
	pub fn new(module: &'a mut Module, builder: FunctionBuilder, constants: &'a mut super::ConstantMap<'s>) -> Self {
		Self {
			constants,
			module,
			reg_map: Vec::new(),
			builder,
			max_slot_use: 0,
			cur_slot_use: 0,
			debug_info: InfoCollector::default(),
		}
	}

	pub fn into_inner(self) -> (Vec<Op>, SrcMap, u8) {
		// (self.operations, self.debug_info.into_map(), self.max_slot_use)
		todo!()
	}

	pub fn emit(&mut self, scope: &mut (impl ParseScope<'s> + ?Sized), op: Op, src_index: usize) {
		let module = &mut self.module;
		let reg_map = &mut self.reg_map;
		let mut builder = self.builder.func_body();

		macro_rules! reg {
			($val:expr) => { {
				let r = usize::from($val);
				reg_map.get(r).copied().flatten().unwrap_or_else(|| {
					let local = module.locals.add(ValType::I64);
					reg_map.resize(r + 1, None);
					reg_map[r] = Some(local);
					local
				})
			} };
		}

		macro_rules! bin_op {
			($dst:ident, $lhs:ident, $rhs:ident, $op:ident) => { {
				let (dst, lhs, rhs) = (reg!($dst), reg!($lhs), reg!($rhs));
				builder.local_get(lhs).local_get(rhs).binop(BinaryOp::$op).local_set(dst);
			} }
		}

		match op {
			Op::LoadNil(dst, len) => {
				for i in dst..dst + len {
					builder.i64_const(Value::nil().as_i64()).local_set(reg!(i)); // Nil is represented as 0.
				}
			},
			Op::LoadBool(dst, b) => {
				builder.i64_const(Value::bool(b).as_i64()).local_set(reg!(dst));
			},
			Op::LoadNum(dst, idx) => {
				let n = self.constants.numbers[idx as usize];
				builder.i64_const(Value::float(n).as_i64()).local_set(reg!(dst));
			},
			Op::LoadStr(dst, idx) => todo!(),
			Op::LoadTab(_) => todo!(),
			Op::LoadClosure(_, _) => todo!(),
			Op::Set(_, _, _) => todo!(),
			Op::Get(_, _, _) => todo!(),
			Op::Add(d, a, b) => bin_op!(d, a, b, I64Add),
			Op::Sub(d, a, b) => bin_op!(d, a, b, I64Sub),
			Op::Mul(d, a, b) => bin_op!(d, a, b, I64Mul),
			Op::Div(d, a, b) => bin_op!(d, a, b, I64DivS),
			Op::Mod(d, a, b) => bin_op!(d, a, b, I64RemS),
			Op::Pow(d, a, b) => todo!(),
			// Integer negation is represented as `0 - x`
			Op::Neg(d, a) => { builder.i64_const(0).local_get(reg!(a)).binop(BinaryOp::I64Sub).local_set(reg!(d)); },
			Op::Eq(d, a, b) | Op::Neq(d, a, b) => {
				let (dst, lhs, rhs) = (reg!(d), reg!(a), reg!(b));
				builder
					// First determine the type tag of the LHS.
					.local_get(lhs)
					.local_get(rhs)
					.binop(BinaryOp::I64Eq)
					// Do a trivial equality check.
					.if_else(ValType::I64, |then| { then.i64_const(Value::bool(true).as_i64()); }, |el| {
						el.local_get(lhs)
							.i64_const(0x0F)
							.binop(BinaryOp::I64And)
							// Check if the type is of String as that's the only type that doesn't use bitwise comparison.
							.i64_const(ValueTag::String.as_u8().into())
							.binop(BinaryOp::I64Eq)
							.if_else(ValType::I64, |then| {
								//TODO: String comparison.
							}, |el| { el.i64_const(Value::bool(false).as_i64()); });
					});
				
				if matches!(op, Op::Neq(_, _, _)) {
					// Invert the result for Neq.
					builder.binop(BinaryOp::I64Eq);
				}

				builder.local_set(dst);
			},
			Op::Lt(_, _, _) => todo!(),
			Op::Lte(_, _, _) => todo!(),
			Op::Gt(_, _, _) => todo!(),
			Op::Gte(_, _, _) => todo!(),
			Op::Not(_, _) => todo!(),
			Op::BitAnd(_, _, _) => todo!(),
			Op::BitOr(_, _, _) => todo!(),
			Op::BitXor(_, _, _) => todo!(),
			Op::BitShL(_, _, _) => todo!(),
			Op::BitShR(_, _, _) => todo!(),
			Op::BitNot(_, _) => todo!(),
			Op::Concat(_, _, _) => todo!(),
			Op::Len(_, _) => todo!(),
			Op::Call(_, _, _) => todo!(),
			Op::Ret(_, _) => todo!(),
			Op::GoTo(_) => todo!(),
			Op::SkpIf(_) => todo!(),
			Op::SkpIfNot(_) => todo!(),
			Op::Copy(_, _) => todo!(),
			Op::GetUpVal(_, _) => todo!(),
			Op::SetUpVal(_, _) => todo!(),
			Op::GetUpTab(_, _) => todo!(),
			Op::SetUpTab(_, _) => todo!(),
			Op::Close(_) => todo!(),
		}
		
		self.debug_info.emit(src_index);
	}
	pub fn emit_closing_goto(&mut self, scope: &mut (impl ParseScope<'s> + ?Sized), base: u8, position: usize, src_index: usize) {
		self.emit(scope, Op::Close(base), src_index);
		self.emit(scope, Op::goto(position), src_index);
	}
	pub fn emit_flat_goto(&mut self, scope: &mut (impl ParseScope<'s> + ?Sized), position: usize, src_index: usize) {
		self.emit(scope, Op::goto(position), src_index);
	}

	pub fn ops(&self) -> &[Op] {
		// &self.operations
		todo!()
	}

	pub fn ops_mut(&mut self) -> &mut [Op] {
		// &mut self.operations
		todo!()
	}

	pub fn reserve_slot(&mut self) -> u8 {
		let reg = self.cur_slot_use;
		self.cur_slot_use += 1;

		self.max_slot_use = self.max_slot_use.max(self.cur_slot_use);

		reg
	}

	pub fn slots_used(&self) -> u8 {
		self.cur_slot_use
	}

	pub fn set_slots_used(&mut self, used: u8) {
		self.cur_slot_use = used;
		self.max_slot_use = self.max_slot_use.max(self.cur_slot_use);
	}
	
	pub fn update_max_slots_used(&mut self, used: u8) {
		self.max_slot_use = self.max_slot_use.max(used);
	}
	
	pub fn number_idx(&mut self, n: f64) -> u16 {
		if let Some(idx) = self.constants.numbers.iter().position(|&num| num == n) {
			idx
		} else {
			let idx = self.constants.numbers.len();
			self.constants.numbers.push(n);
			idx
		}.try_into().expect("Too many numbers consts :(")
	}

	pub fn string_idx(&mut self, s: &'s [u8]) -> u16 {
		let s = BStr::new(s);
		if let Some(&idx) = self.constants.string_indexes.get(s) {
			idx
		} else {
			let idx = self.constants.strings.len();
			self.constants.strings.push(s);
			self.constants.string_indexes.insert(s, idx);
			idx
		}.try_into().expect("Too many string consts :(")
	}

	pub fn push_closure(&mut self, func: ParsedFunction) -> u16 {
		let idx = self.constants.closures.len();
		self.constants.closures.push(func);
		// This is + 1 because 0 is the 'main' function and unnameable.
		//TODO: This assumes the main function will always be at index 0. Is that fine?
		(idx + 1).try_into().expect("Too many closures :(")
	}
}

/// A lot like a [`RootScope`], but for a nested closure.\
/// This acts to separate a closure from its outer scope, ensuring locals
/// and such are only accessed as UpValues.
struct ClosureScope<'a, 's> {
	outer_scope: &'a mut dyn ParseScope<'s>,
	upvalues: slotmap::SparseSecondaryMap<IdentKey, (u8, Upvalue)>,
}
impl<'a, 's> ClosureScope<'a, 's> {
	fn new(outer_scope: &'a mut dyn ParseScope<'s>, lexer: &mut Lexer<'s>) -> Self {
		Self {
			outer_scope,
			// The 0th Upvalue of a function is always the global environment.
			upvalues: FromIterator::from_iter([(lexer.get_ident("_ENV"), (0, Upvalue::ParentUpValue(0)))]),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Upvalue {
	ParentSlot(u8),
	ParentUpValue(u8),
}

impl<'s> ParseScope<'s> for ClosureScope<'_, 's> {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { unreachable!() }

	fn new_label(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _label: IdentKey, _label_pos: usize) -> Result<(), Error<'s>> {
		unreachable!()
	}
	
	fn emit_goto(&mut self, _state: &mut FuncState<'_, 's>, _label: IdentKey, _span: usize, _closed_base: Option<u8>) -> Result<(), Error<'s>> {
		unreachable!()
	}

	fn merge_missing_labels(&mut self, _other: Vec<(IdentKey, usize, Option<u8>)>) {
		unreachable!()
	}

	fn label_exists(&mut self, _label: IdentKey) -> bool { false }

	fn emit_break(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Break statement not within a loop".into())
	}
	fn emit_continue(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Continue statement not within a loop".into())
	}

	fn new_local(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _name: IdentKey) -> Result<u8, Error<'s>> {
		unreachable!()
	}

	fn resolve_name(&mut self, state: &mut FuncState<'_, 's>, name: IdentKey, _is_capturing: bool) -> Result<Named, Error<'s>> {
		if let Some(&(idx, _)) = self.upvalues.get(name) {
			return Ok(Named::UpValue(idx));
		}

		Ok(match self.outer_scope.resolve_name(state, name, true)? {
			Named::Local(slot) => {
				let upval_idx = self.upvalues.len().try_into().expect("Too many upvalues :(");
				self.upvalues.insert(name, (upval_idx, Upvalue::ParentSlot(slot)));
				Named::UpValue(upval_idx)
			},
			Named::UpValue(idx) => {
				let upval_idx = self.upvalues.len().try_into().expect("Too many upvalues :(");
				self.upvalues.insert(name, (upval_idx, Upvalue::ParentUpValue(idx)));
				Named::UpValue(upval_idx)
			},
			Named::Global(name) => Named::Global(name),
		})
	}

	fn total_locals(&mut self) -> u8 { 0 }
}

pub fn parse_function<'s>(lexer: &mut Lexer<'s>, mut scope: impl ParseScope<'s>, state: &mut FuncState<'_, 's>, name: Option<super::IdentKey>, span: usize) -> Result<ParsedFunction, Error<'s>> {
	let mut builder = FunctionBuilder::new(&mut state.module.types, &[ValType::I32, ValType::I32], &[ValType::I32]);
	builder.name("owo".into());

	let mut closure_state = FuncState::new(state.module, builder, state.constants);
	let mut closure_scope = VariableScope::new(ClosureScope::new(&mut scope, lexer));

	expect_tok!(lexer, Token::ParenOpen)?;

	let mut params = 0u8;

	let mut tok = lexer.next_must()?;
	loop {
		let ident = match tok {
			Token::Identifier(ident) => ident,
			Token::VarArgs => todo!(),
			Token::ParenClose => break,
			tok => Err(format!("Expected parameter name, found {tok:?}"))?,
		};

		closure_scope.new_local(lexer, &mut closure_state, ident)?;
		params = params.checked_add(1).ok_or("Too many function parameters")?;

		match lexer.next_must()? {
			Token::Comma => tok = lexer.next_must()?,
			Token::ParenClose => break,
			tok => Err(format!("Expected ',' or ')', found {tok:?}"))?,
		}
	}

	loop {
		let tok = lexer.next_must()?;
		if tok == Token::End { break; }
		super::parse_stmt(tok, lexer, &mut closure_scope, &mut closure_state)?;
	}

	// Ensure the function ends with a return.
	if !matches!(closure_state.ops().last(), Some(Op::Ret(..))) {
		closure_state.emit(&mut closure_scope, Op::Ret(0, 0), lexer.src_index());
	}

	let upvalues = closure_scope.into_inner(&mut closure_state, lexer)?.upvalues;
	let upvalues = {
		let mut buf = Box::new_uninit_slice(upvalues.len());
		let mut assert_buf = Vec::new();

		for (_, (idx, upval)) in upvalues.into_iter() {
			buf[usize::from(idx)].write(upval);

			if cfg!(debug_assertions) { assert_buf.push(idx); }
		}

		if cfg!(debug_assertions) {
			let initial_len = assert_buf.len();
			assert_buf.sort_unstable();
			assert_buf.dedup();
			assert_eq!(initial_len, assert_buf.len(), "Upvalue indices were not unique");
		}

		// SAFETY: All elements have been initialized.
		unsafe { buf.assume_init() }
	};

	let debug = DebugInfo::new_closure(
		closure_state.debug_info.into_map(),
		name.map(|i| lexer.resolve_ident(i)),
		span,
	);

	let parsed = ParsedFunction {
		param_count: params,
		// operations: closure_state.operations.into_boxed_slice(),
		operations: todo!(),
		debug: Some(debug),
		frame_size: closure_state.max_slot_use,
		upvalues,
	};

	Ok(parsed)
}
