use crate::vm::debug::{DebugInfo, SrcMap};
use super::{debug::InfoCollector, VariableScope, ParseScope, Named, LexerExt, Error, Op, expect_tok};
use luant_lexer::{Lexer, Token, IdentKey};
use bstr::BStr;

#[derive(Debug)]
pub struct FuncState<'a, 's> {
	constants: &'a mut super::ConstantMap<'s>,
	operations: Vec<Op>,
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

impl<'a, 's> FuncState<'a, 's> {
	pub fn new(constants: &'a mut super::ConstantMap<'s>) -> Self {
		Self {
			constants,
			operations: Vec::new(),
			max_slot_use: 0,
			cur_slot_use: 0,
			debug_info: InfoCollector::default(),
		}
	}

	pub fn into_inner(self) -> (Vec<Op>, SrcMap, u8) {
		(self.operations, self.debug_info.into_map(), self.max_slot_use)
	}

	pub fn emit(&mut self, op: Op, src_index: usize) {
		self.operations.push(op);
		self.debug_info.emit(src_index);
	}
	pub fn emit_closing_goto(&mut self, base: u8, position: usize, src_index: usize) {
		self.emit(Op::Close(base), src_index);
		self.emit(Op::goto(position), src_index);
	}
	pub fn emit_flat_goto(&mut self, position: usize, src_index: usize) {
		self.emit(Op::goto(position), src_index);
	}

	pub fn ops(&self) -> &[Op] {
		&self.operations
	}

	pub fn ops_mut(&mut self) -> &mut [Op] {
		&mut self.operations
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
	let mut closure_state = FuncState::new(state.constants);
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
		closure_state.emit(Op::Ret(0, 0), lexer.src_index());
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
		operations: closure_state.operations.into_boxed_slice(),
		debug: Some(debug),
		frame_size: closure_state.max_slot_use,
		upvalues,
	};

	Ok(parsed)
}
