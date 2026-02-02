use super::{Error, ParseScope, Named, LexerExt, Op, IdentKey, expect_tok};
use super::functions::FuncState;
use crate::BStr;
use luant_lexer::{Lexer, Token};

type Num = real_float::Finite<f64>;

mod postfix_ops;
mod prefix;
mod pratt;

pub use prefix::{PlaceExpr, IdentExpr};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Const<'s> {
	Number(Num),
	String(&'s BStr),
	Bool(bool),
	Nil,
}

impl<'s> Const<'s> {
	pub fn string(bytes: &'s (impl AsRef<[u8]> + ?Sized)) -> Self {
		Const::String(BStr::new(bytes.as_ref()))
	}

	pub fn is_truthy(self) -> bool {
		!matches!(self, Const::Bool(false) | Const::Nil)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expr<'s> {
	Constant(Const<'s>),
	Local(u8),
	UpValue(u8),
	Global(IdentKey),
	Temp(u8),
}

impl<'s> Expr<'s> {
	pub fn from_named(name: Named) -> Self {
		match name {
			Named::Local(slot) => Expr::Local(slot),
			Named::UpValue(idx) => Expr::UpValue(idx),
			Named::Global(ident) => Expr::Global(ident),
		}
	}

	pub fn as_const(self) -> Option<Const<'s>> {
		match self {
			Expr::Constant(c) => Some(c),
			_ => None,
		}
	}

	#[inline]
	pub fn to_temp(self, lexer: &mut Lexer<'s>, state: &mut FuncState<'_, 's>) -> Result<u8, Error<'s>> {
		match self.as_temp() {
			Some(t) => Ok(t),
			None => {
				let temp = state.reserve_slot();
				self.set_to_slot(lexer, state, temp)?;
				Ok(temp)
			},
		}
	}

	#[inline]
	pub fn as_temp(self) -> Option<u8> {
		match self {
			Expr::Temp(t) => Some(t),
			Expr::Local(_) | Expr::UpValue(_) |
			Expr::Global(_) | Expr::Constant(_) => None,
		}
	}

	#[inline]
	pub fn to_new_local(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<u8, Error<'s>> {
		let local = scope.new_local(lexer, state, name)?;
		self.set_to_slot(lexer, state, local)?;
		Ok(local)
	}

	#[inline]
	pub fn to_slot(self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>) -> Result<u8, Error<'s>> {
		match self {
			Expr::Constant(_) |
			Expr::UpValue(_) |
			Expr::Global(_) => {
				let temp = state.reserve_slot();
				self.set_to_slot(lexer, state, temp)?;
				Ok(temp)
			},
			Expr::Local(slot) => Ok(slot),
			Expr::Temp(temp) => Ok(temp),
		}
	}

	#[inline]
	pub fn set_to_slot(self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, slot: u8) -> Result<(), Error<'s>> {
		match self {
			Expr::Temp(temp) => if temp != slot {
				state.emit(Op::Copy(slot, temp), lexer.src_index())
			},
			Expr::Local(local_slot) => {
				if local_slot != slot {
					state.emit(Op::Copy(slot, local_slot), lexer.src_index());
				}
			},
			Expr::UpValue(ident) => {
				state.emit(Op::GetUpVal(slot, ident), lexer.src_index());
			},
			Expr::Global(ident) => {
				let str_idx = state.string_idx(lexer.resolve_ident(ident).as_bytes());
				state.emit(Op::GetUpTab(slot, str_idx), lexer.src_index());
			},
			Expr::Constant(Const::Number(n)) => {
				let num_idx = state.number_idx(n.val());
				state.emit(Op::LoadNum(slot, num_idx), lexer.src_index());
			},
			Expr::Constant(Const::String(s)) => {
				let str_idx = state.string_idx(s);
				state.emit(Op::LoadStr(slot, str_idx), lexer.src_index());
			},
			Expr::Constant(Const::Bool(b)) => state.emit(Op::LoadBool(slot, b), lexer.src_index()),
			Expr::Constant(Const::Nil) => state.emit(Op::LoadNil(slot, 1), lexer.src_index()),
		}

		Ok(())
	}
}

pub fn can_start_expr(tok: Token) -> bool {
	use Token::*;
	matches! { tok,
		BraceOpen | ParenOpen | VarArgs | Function | Number(_) | Identifier(_) | String(_) | True | False | Nil |
		Minus | BitNot | Len | Not
	}
}

pub fn parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Expr<'s>, Error<'s>> {
	// let slots_used = state.slots_used();
	let expr = pratt::parse_expr(head, lexer, scope, state, 0)?;
	// state.set_slots_used(slots_used);
	Ok(expr)
}

pub fn parse_table_init<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<u8, Error<'s>> {
	assert_eq!(head, Token::BraceOpen);

	let tab_slot = state.reserve_slot();
	state.emit(Op::LoadTab(tab_slot), lexer.src_index());

	if lexer.next_if(Token::BraceClose)?.is_some() {
		return Ok(tab_slot);
	}

	let [key_slot, val_slot] = [state.reserve_slot(), state.reserve_slot()];

	let mut array_index = 0;
	loop {
		enum KeyOrArray<'s> {
			Key(Expr<'s>),
			Array(Expr<'s>),
		}

		let span = lexer.src_index();

		// parse field or array value.
		let first = match lexer.next_must()? {
			Token::Identifier(field) => match lexer.next_if(Token::Assign)? {
				Some(_) => KeyOrArray::Key(Expr::Constant(Const::string(lexer.resolve_ident(field)))),
				None => KeyOrArray::Array(parse_expr(head, lexer, scope, state)?),
			},
			Token::BracketOpen => {
				let field = parse_expr(lexer.next_must()?, lexer, scope, state)?;
				expect_tok!(lexer, Token::BracketClose)?;
				expect_tok!(lexer, Token::Assign)?;
				KeyOrArray::Key(field)
			},
			head => KeyOrArray::Array(parse_expr(head, lexer, scope, state)?),
		};

		match first {
			KeyOrArray::Key(expr) => {
				expr.set_to_slot(lexer, state, key_slot)?;
				parse_expr(lexer.next_must()?, lexer, scope, state)?.set_to_slot(lexer, state, val_slot)?;
				state.emit(Op::Set(tab_slot, key_slot, val_slot), span);
			},
			KeyOrArray::Array(entry) => {
				array_index += 1; // Lua is 1-indexed.

				entry.set_to_slot(lexer, state, val_slot)?;

				let num_idx = state.number_idx(array_index as f64);
				state.emit(Op::LoadNum(key_slot, num_idx), span);

				state.emit(Op::Set(tab_slot, key_slot, val_slot), span);
			},
		}

		if lexer.next_if(Token::Comma)?.is_none() {
			expect_tok!(lexer, Token::BraceClose)?;
			break;
		}
	}

	Ok(tab_slot)
}
