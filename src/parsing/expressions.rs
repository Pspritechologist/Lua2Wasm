use crate::bytecode::Loc;

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
	CallRet,
	VarArgs,
}

impl From<Loc> for Expr {
	fn from(loc: Loc) -> Self { Self::from_loc(loc) }
}
impl From<Const> for Expr {
	fn from(c: Const) -> Self { Self::Constant(c) }
}

impl Expr {
	pub fn is_multi_valued(self) -> bool {
		matches!(self, Expr::CallRet | Expr::VarArgs)
	}

	pub fn from_loc(loc: Loc) -> Self {
		match loc {
			Loc::Local(slot) => Expr::Local(slot),
			Loc::UpValue(idx) => Expr::UpValue(idx),
			Loc::Global(ident) => Expr::Global(ident),
			Loc::Temp(temp) => Expr::Temp(temp),
		}
	}
	
	pub fn as_loc(self) -> Option<Loc> {
		Some(match self {
			Expr::Local(slot) => Loc::Local(slot),
			Expr::UpValue(idx) => Loc::UpValue(idx),
			Expr::Global(ident) => Loc::Global(ident),
			Expr::Temp(temp) => Loc::Temp(temp),
			Expr::Constant(_) | Expr::CallRet |
			Expr::VarArgs => return None,
		})
	}

	pub fn try_to_loc(self) -> Result<Loc, Error<'static>> {
		self.as_loc().ok_or_else(|| "Expected expression to be a location".into())
	}

	pub fn to_loc(self) -> Loc {
		self.as_loc().expect("Expected expression to be a location")
	}

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
	pub fn to_temp<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Loc, Error<'s>> {
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
	pub fn as_temp(self) -> Option<Loc> {
		match self {
			Expr::Temp(t) => Some(Loc::Temp(t)),
			Expr::Local(_) | Expr::UpValue(_) |
			Expr::Global(_) | Expr::Constant(_) |
			Expr::CallRet | Expr::VarArgs => None,
		}
	}

	#[inline]
	pub fn to_new_local<'s>(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<Loc, Error<'s>> {
		let local = scope.new_local(lexer, state, name)?;
		self.set_to_slot(lexer, scope, state, local)?;
		Ok(local)
	}

	#[inline]
	pub fn to_slot<'s>(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Loc, Error<'s>> {
		match self {
			Expr::Constant(_) |
			Expr::UpValue(_) |
			Expr::Global(_) |
			Expr::CallRet |
			Expr::VarArgs => {
				let temp = state.reserve_slot();
				self.set_to_slot(lexer, scope, state, temp)?;
				Ok(temp)
			},
			Expr::Local(_) |
			Expr::Temp(_) => Ok(self.to_loc()),
		}
	}

	#[inline]
	pub fn set_to_slot<'s>(self, lexer: &Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, slot: Loc) -> Result<(), Error<'s>> {
		state.emit(scope, Op::Copy(slot, self), lexer.src_index());
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

pub fn parse_table_init<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Loc, Error<'s>> {
	assert_eq!(head, Token::BraceOpen);

	let tab_slot = state.reserve_slot();
	state.emit(scope, Op::LoadTab(tab_slot), lexer.src_index());

	if lexer.next_if(Token::BraceClose)?.is_some() {
		return Ok(tab_slot);
	}

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
			KeyOrArray::Key(key) => {
				let val = parse_expr(lexer.next_must()?, lexer, scope, state)?;
				state.emit(scope, Op::Set(tab_slot.into(), key, val), span);
			},
			KeyOrArray::Array(entry) => {
				array_index += 1; // Lua is 1-indexed.

				let key = Expr::Constant(Const::Number(Finite::new(array_index as f64)));
				state.emit(scope, Op::Set(tab_slot.into(), key.into(), entry), span);
			},
		}

		if lexer.next_if([Token::Comma, Token::LineTerm])?.is_none() {
			expect_tok!(lexer, Token::BraceClose)?;
			break;
		}
	}

	Ok(tab_slot)
}
