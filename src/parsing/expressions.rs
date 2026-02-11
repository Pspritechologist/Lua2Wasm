use super::{Error, ParseScope, Named, LexerExt, Op, IdentKey, expect_tok};
use super::functions::FuncState;
use luant_lexer::{Lexer, Token};

type Num = real_float::Finite<f64>;

mod postfix_ops;
mod prefix;
mod pratt;

pub use prefix::{PlaceExpr, IdentExpr};
use real_float::Finite;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Const {
	Number(Num),
	String(usize),
	Bool(bool),
	Nil,
}

impl Const {
	pub fn is_truthy(self) -> bool {
		!matches!(self, Const::Bool(false) | Const::Nil)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expr {
	Constant(Const),
	Local(u8),
	UpValue(u8),
	Global(IdentKey),
	Temp(u8),
}

impl Expr {
	pub fn from_named(name: Named) -> Self {
		match name {
			Named::Local(slot) => Expr::Local(slot),
			Named::UpValue(idx) => Expr::UpValue(idx),
			Named::Global(ident) => Expr::Global(ident),
		}
	}

	pub fn as_const(self) -> Option<Const> {
		match self {
			Expr::Constant(c) => Some(c),
			_ => None,
		}
	}

	#[inline]
	pub fn to_temp<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<u8, Error<'s>> {
		match self.as_temp() {
			Some(t) => Ok(t),
			None => {
				let temp = state.reserve_slot();
				self.set_to_slot(lexer, scope, state, temp)?;
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
	pub fn to_new_local<'s>(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<u8, Error<'s>> {
		let local = scope.new_local(lexer, state, name)?;
		self.set_to_slot(lexer, scope, state, local)?;
		Ok(local)
	}

	#[inline]
	pub fn to_slot<'s>(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<u8, Error<'s>> {
		match self {
			Expr::Constant(_) |
			Expr::UpValue(_) |
			Expr::Global(_) => {
				let temp = state.reserve_slot();
				self.set_to_slot(lexer, scope, state, temp)?;
				Ok(temp)
			},
			Expr::Local(slot) => Ok(slot),
			Expr::Temp(temp) => Ok(temp),
		}
	}

	#[inline]
	pub fn set_to_slot<'s>(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, slot: u8) -> Result<(), Error<'s>> {
		match self {
			Expr::Temp(temp) => if temp != slot {
				state.emit(scope, Op::Copy(slot, temp), lexer.src_index())
			},
			Expr::Local(local_slot) => {
				if local_slot != slot {
					state.emit(scope, Op::Copy(slot, local_slot), lexer.src_index());
				}
			},
			Expr::UpValue(ident) => {
				state.emit(scope, Op::GetUpVal(slot, ident), lexer.src_index());
			},
			Expr::Global(ident) => {
				let str_idx = state.string_idx(lexer.resolve_ident(ident), true)?;
				state.emit(scope, Op::GetUpTab(slot, str_idx), lexer.src_index());
			},
			Expr::Constant(Const::Number(n)) => {
				state.emit(scope, Op::LoadNum(slot, n), lexer.src_index());
			},
			Expr::Constant(Const::String(s)) => {
				state.emit(scope, Op::LoadStr(slot, s), lexer.src_index());
			},
			Expr::Constant(Const::Bool(b)) => state.emit(scope, Op::LoadBool(slot, b), lexer.src_index()),
			Expr::Constant(Const::Nil) => state.emit(scope, Op::LoadNil(slot, 1), lexer.src_index()),
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

pub fn parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Expr, Error<'s>> {
	let expr = pratt::parse_expr(head, lexer, scope, state, 0)?;
	Ok(expr)
}

pub fn parse_table_init<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<u8, Error<'s>> {
	assert_eq!(head, Token::BraceOpen);

	let tab_slot = state.reserve_slot();
	state.emit(scope, Op::LoadTab(tab_slot), lexer.src_index());

	if lexer.next_if(Token::BraceClose)?.is_some() {
		return Ok(tab_slot);
	}

	let [key_slot, val_slot] = [state.reserve_slot(), state.reserve_slot()];

	let mut array_index = 0usize;
	loop {
		enum KeyOrArray {
			Key(Expr),
			Array(Expr),
		}

		let span = lexer.src_index();

		// parse field or array value.
		let first = match lexer.next_must()? {
			Token::Identifier(field) => match lexer.next_if(Token::Assign)? {
				Some(_) => KeyOrArray::Key(Expr::Constant(Const::String(state.string_idx(lexer.resolve_ident(field), true)?))),
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
				expr.set_to_slot(lexer, scope, state, key_slot)?;
				parse_expr(lexer.next_must()?, lexer, scope, state)?.set_to_slot(lexer, scope, state, val_slot)?;
				state.emit(scope, Op::Set(tab_slot, key_slot, val_slot), span);
			},
			KeyOrArray::Array(entry) => {
				array_index += 1; // Lua is 1-indexed.

				entry.set_to_slot(lexer, scope, state, val_slot)?;

				state.emit(scope, Op::LoadNum(key_slot, Finite::new(array_index as f64)), span);

				state.emit(scope, Op::Set(tab_slot, key_slot, val_slot), span);
			},
		}

		if lexer.next_if([Token::Comma, Token::LineTerm])?.is_none() {
			expect_tok!(lexer, Token::BraceClose)?;
			break;
		}
	}

	Ok(tab_slot)
}
