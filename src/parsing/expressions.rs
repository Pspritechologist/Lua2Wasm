use crate::parsing::LexerExt;
use crate::prelude::BStr;
use super::{Error, ParseState, Op, IdentKey, expect_tok};
use luant_lexer::{Lexer, Token};

mod postfix_ops;
mod prefix;
mod pratt;

pub use prefix::{PlaceExpr, IdentExpr};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Const<'s> {
	Number(crate::types::Num),
	String(&'s BStr),
	Bool(bool),
	Nil,
}

impl<'s> Const<'s> {
	pub fn is_truthy(self) -> bool {
		!matches!(self, Const::Bool(false) | Const::Nil)
	}

	pub fn is_null(self) -> bool {
		matches!(self, Const::Nil)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expr<'s> {
	Constant(Const<'s>),
	Local(IdentKey),
	Temp(u8),
}

impl<'s> Expr<'s> {
	#[inline]
	pub fn to_temp(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<u8, Error<'s>> {
		match self.as_temp() {
			Some(t) => Ok(t),
			None => {
				let temp = state.new_temp();
				self.set_to_slot(lexer, state, temp)?;
				Ok(temp)
			},
		}
	}

	#[inline]
	pub fn as_temp(self) -> Option<u8> {
		match self {
			Expr::Temp(t) => Some(t),
			Expr::Local(_) | Expr::Constant(_) => None,
		}
	}

	#[inline]
	pub fn to_new_local(self, lexer: &Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), name: IdentKey) -> Result<u8, Error<'s>> {
		let local = state.new_local(lexer, name)?;
		self.set_to_slot(lexer, state, local)?;
		Ok(local)
	}

	#[inline]
	pub fn to_slot(self, lexer: &Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<u8, Error<'s>> {
		match self {
			Expr::Constant(_) => {
				let temp = state.new_temp();
				self.set_to_slot(lexer, state, temp)?;
				Ok(temp)
			},
			Expr::Local(ident_key) => state.local(lexer, ident_key),
			Expr::Temp(temp) => Ok(temp),
		}
	}

	#[inline]
	pub fn set_to_slot(self, lexer: &Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), slot: u8) -> Result<(), Error<'s>> {
		match self {
			Expr::Temp(temp) => state.emit(Op::Copy(slot, temp)),
			Expr::Local(ident) => {
				let local_slot = state.local(lexer, ident)?;
				state.emit(Op::Copy(slot, local_slot));
			},
			Expr::Constant(Const::Number(n)) => {
				let num_idx = state.number_idx(n.val());
				state.emit(Op::LoadNum(slot, num_idx));
			},
			Expr::Constant(Const::String(s)) => {
				let str_idx = state.string_idx(s);
				state.emit(Op::LoadStr(slot, str_idx));
			},
			Expr::Constant(Const::Bool(b)) => state.emit(Op::LoadBool(slot, b)),
			Expr::Constant(Const::Nil) => state.emit(Op::LoadNil(slot, 1)),
		}

		Ok(())
	}
}

pub fn parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<Expr<'s>, Error<'s>> {
	pratt::parse_expr(head, lexer, state, 0)
}

pub fn parse_table_init<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<u8, Error<'s>> {
	assert_eq!(head, Token::BraceOpen);

	let tab_slot = state.new_temp();
	state.emit(Op::LoadTab(tab_slot));

	if lexer.next_if(Token::BraceClose)?.is_some() {
		return Ok(tab_slot);
	}

	let [key_slot, val_slot] = [state.new_temp(), state.new_temp()];

	let mut array_index = 0;
	loop {
		enum KeyOrArray<'s> {
			Key(Expr<'s>),
			Array(Expr<'s>),
		}

		// parse field or array value.
		let first = match lexer.next_must()? {
			Token::Identifier(field) => match lexer.next_if(Token::Assign)? {
				Some(_) => KeyOrArray::Key(Expr::Constant(Const::String(lexer.resolve_ident(field)))),
				None => KeyOrArray::Array(parse_expr(head, lexer, state)?),
			},
			Token::BracketOpen => {
				let field = parse_expr(lexer.next_must()?, lexer, state)?;
				expect_tok!(lexer, Token::BracketClose)?;
				expect_tok!(lexer, Token::Assign)?;
				KeyOrArray::Key(field)
			},
			head => KeyOrArray::Array(parse_expr(head, lexer, state)?),
		};

		match first {
			KeyOrArray::Key(expr) => {
				expr.set_to_slot(lexer, state, key_slot)?;
				parse_expr(lexer.next_must()?, lexer, state)?.set_to_slot(lexer, state, val_slot)?;
				state.emit(Op::Set(tab_slot, key_slot, val_slot));
			},
			KeyOrArray::Array(entry) => {
				array_index += 1; // Lua is 1-indexed.

				entry.set_to_slot(lexer, state, val_slot)?;

				let num_idx = state.number_idx(array_index as f64);
				state.emit(Op::LoadNum(key_slot, num_idx));
				state.emit(Op::Set(tab_slot, key_slot, val_slot));
			},
		}

		if lexer.next_if(Token::Comma)?.is_none() {
			expect_tok!(lexer, Token::BraceClose)?;
			break;
		}
	}

	Ok(tab_slot)
}
