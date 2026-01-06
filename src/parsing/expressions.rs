use crate::parsing::LexerExt;
use super::{Error, ParseState, ParseStateExt, Op, IdentKey, LexExtras, expect_tok};
use luant_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
	Left, Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InfixOp {
	Add, Sub,
	Mul, Div, DivInt,
	Mod, Pow,
	Concat,
	Eq, Neq,
	Lt, Lte,
	Gt, Gte,
	And, Or,
	BitAnd, BitOr,
	BitXor,
	Shl, Shr,
}
impl InfixOp {
	fn assoc(&self) -> Assoc {
		match self {
			InfixOp::Pow | InfixOp::Concat => Assoc::Right,
			_ => Assoc::Left,
		}
	}

	fn prec(&self) -> u8 {
		match self {
			InfixOp::Or => 1,
			InfixOp::And => 2,
			InfixOp::Eq | InfixOp::Neq | InfixOp::Lt | InfixOp::Lte | InfixOp::Gt | InfixOp::Gte => 3,
			InfixOp::BitOr => 4,
			InfixOp::BitXor => 5,
			InfixOp::BitAnd => 6,
			InfixOp::Shl | InfixOp::Shr => 7,
			InfixOp::Concat => 8,
			InfixOp::Add | InfixOp::Sub => 9,
			InfixOp::Mul | InfixOp::Div | InfixOp::DivInt | InfixOp::Mod => 10,
			// Unary => 11,
			InfixOp::Pow => 12,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrefixOp {
	Neg, BitNot, Len, Not,
}
impl PrefixOp {
	fn prec(&self) -> u8 {
		11
	}
}

pub enum Const<'s> {
	Number(f64),
	String(&'s str),
	Bool(bool),
	Null,
}

pub enum Expr<'s> {
	Constant(Const<'s>),
	Local(IdentKey),
}

impl<'s> Expr<'s> {
	pub fn is_truthy(&self) -> Option<bool> {
		match self {
			Expr::Constant(Const::Bool(b)) => Some(*b),
			Expr::Constant(Const::Null) => Some(false),
			_ => None,
		}
	}

	pub fn is_null(&self) -> bool {
		matches!(self, Expr::Constant(Const::Null))
	}

	pub fn set_to_slot(&self, lexer: &Lexer<'s>, state: &mut impl ParseState<'s>, slot: u8) -> Result<(), Error<'s>> {
		match self {
			Expr::Local(ident) => {
				let local_slot = state.local(lexer, *ident)?;
				state.emit(Op::Copy(slot, local_slot));
			},
			Expr::Constant(Const::Number(n)) => {
				let num_idx = state.number_idx(*n);
				state.emit(Op::LoadNum(slot, num_idx));
			},
			Expr::Constant(Const::String(s)) => {
				let str_idx = state.string_idx(s);
				state.emit(Op::LoadStr(slot, str_idx));
			},
			Expr::Constant(Const::Bool(b)) => state.emit(Op::LoadBool(slot, *b)),
			Expr::Constant(Const::Null) => state.emit(Op::LoadNull(slot)),
		}

		Ok(())
	}
}

pub fn try_parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<Expr<'s>, Error<'s>> {
	let expr = match head {
		Token::BraceOpen => Expr::Local(parse_table_init(head, lexer, state)?),
		Token::ParenOpen => {
			let expr = try_parse_expr(lexer.next_must()?, lexer, state)?;
			expect_tok!(lexer, Token::ParenClose)?;
			expr
		},
		Token::VarArgs => todo!(),
		Token::Minus => todo!(),
		Token::BitNot => todo!(),
		Token::Len => todo!(),
		Token::Function => todo!(),
		Token::Not => todo!(),
		Token::Number(n) => Expr::Constant(Const::Number(n)),
		Token::Identifier(ident) => Expr::Local(ident),
		Token::String(s) => Expr::Constant(Const::String(s)), //TODO: Escapes.
		Token::RawString(s) => Expr::Constant(Const::String(s)),
		Token::True => Expr::Constant(Const::Bool(true)),
		Token::False => Expr::Constant(Const::Bool(false)),
		Token::Nil => Expr::Constant(Const::Null),
		tok => return Err(format!("Expected expression, found {tok:?}").into()),
	};

	Ok(expr)
}

pub fn parse_table_init<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<IdentKey, Error<'s>> {
	assert_eq!(head, Token::BraceOpen);

	let tab_temp = state.new_temporary(lexer);
	let tab_slot = state.local(lexer, tab_temp)?;
	state.emit(Op::LoadTab(tab_slot));

	if lexer.next_if(Token::BraceClose)?.is_some() {
		return Ok(tab_temp);
	}

	let [key_temp, val_temp] = state.temporaries(lexer);
	let [key_slot, val_slot] = [state.local(lexer, key_temp)?, state.local(lexer, val_temp)?];

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
				None => KeyOrArray::Array(try_parse_expr(head, lexer, state)?),
			},
			Token::BracketOpen => {
				let field = try_parse_expr(lexer.next_must()?, lexer, state)?;
				expect_tok!(lexer, Token::BracketClose)?;
				expect_tok!(lexer, Token::Assign)?;
				KeyOrArray::Key(field)
			},
			_ => KeyOrArray::Array(try_parse_expr(head, lexer, state)?),
		};

		match first {
			KeyOrArray::Key(expr) => {
				expr.set_to_slot(lexer, state, key_slot)?;
				try_parse_expr(lexer.next_must()?, lexer, state)?.set_to_slot(lexer, state, val_slot)?;
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

	Ok(tab_temp)
}
