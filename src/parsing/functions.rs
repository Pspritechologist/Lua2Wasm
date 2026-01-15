use crate::{prelude::BStr, debug::{DebugInfo, SrcMap}};
use super::{debug::InfoCollector, scopes::{RootScope, ParseScope}, LexerExt, Error, Op, expect_tok};
use luant_lexer::{Lexer, Token};

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
	pub debug: DebugInfo,
	pub frame_size: u8,
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
	
	pub fn number_idx(&mut self, n: f64) -> u16 {
		if let Some(idx) = self.constants.numbers.iter().position(|&num| num == n) {
			idx
		} else {
			let idx = self.constants.numbers.len();
			self.constants.numbers.push(n);
			idx
		}.try_into().expect("Too many numbers consts :(")
	}

	pub fn string_idx(&mut self, s: &'s BStr) -> u16 {
		if let Some(&idx) = self.constants.string_indexes.get(s) {
			idx
		} else {
			let idx = self.constants.strings.len();
			self.constants.strings.push(s);
			self.constants.string_indexes.insert(s, idx);
			idx
		}.try_into().expect("Too many string consts :(")
	}
}
	
pub fn parse_function<'s>(lexer: &mut Lexer<'s>, scope: impl ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<ParsedFunction, Error<'s>> {
	let span = lexer.src_index();

	let name = match lexer.next_must()? {
		Token::Identifier(name) => {
			expect_tok!(lexer, Token::ParenOpen)?;
			Some(name)
		},
		Token::ParenOpen => None,
		tok => return Err(format!("Expected function name or '(', found {tok:?}").into()),
	};

	expect_tok!(lexer, Token::ParenOpen)?;

	//TODO: Parse params.

	expect_tok!(lexer, Token::ParenClose)?;

	let mut state = FuncState::new(state.constants);
	let mut root_scope = RootScope::new_root();

	loop {
		let tok = lexer.next_must()?;
		if tok == Token::End { break; }
		super::parse_stmt(tok, lexer, &mut root_scope, &mut state)?;
	}

	root_scope.finalize(&mut state, lexer)?;

	let parsed = ParsedFunction {
		operations: state.operations.into_boxed_slice(),
		debug: crate::debug::DebugInfo::new_closure(state.debug_info.into_map(), name.map(|i| lexer.resolve_ident(i)), span),
		frame_size: state.max_slot_use,
	};

	Ok(parsed)
}
