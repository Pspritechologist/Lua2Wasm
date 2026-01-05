use crate::parsing::LexerExt;
use super::{Error, ParseState, Op, IdentKey, LexExtras, expect_tok};
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
	Local(u8),
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

	pub fn to_slot(&self, state: &mut impl ParseState<'s>) -> u8 {
		match self {
			Expr::Local(slot) => *slot,
			Expr::Constant(Const::Number(n)) => {
				let slot = state.new_slot();
				let num_idx = state.number_idx(*n);
				state.emit(Op::LoadNum(slot, num_idx));
				slot
			},
			Expr::Constant(Const::String(s)) => {
				let slot = state.new_slot();
				let str_idx = state.string_idx(s);
				state.emit(Op::LoadStr(slot, str_idx));
				slot
			},
			Expr::Constant(Const::Bool(b)) => {
				let slot = state.new_slot();
				state.emit(Op::LoadBool(slot, *b));
				slot
			},
			Expr::Constant(Const::Null) => {
				let slot = state.new_slot();
				state.emit(Op::LoadNull(slot));
				slot
			},
		}
	}
}

pub fn try_parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<Expr<'s>, Error<'s>> {

	
	let expr = match lexer.next_must()? {
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
		Token::Identifier(i) => todo!(),
		Token::String(s) => Expr::Constant(Const::String(s)), //TODO: Escapes.
		Token::RawString(s) => Expr::Constant(Const::String(s)),
		Token::True => Expr::Constant(Const::Bool(true)),
		Token::False => Expr::Constant(Const::Bool(false)),
		Token::Nil => Expr::Constant(Const::Null),
		tok => return Err(format!("Expected expression, found {tok:?}").into()),
	};

	Ok(expr)
}

pub fn parse_table_init<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<u8, Error<'s>> {
	assert_eq!(head, Token::BraceOpen);

	let dst = state.new_slot();
	state.emit(Op::LoadTab(dst));

	loop {
		let tok = lexer.next_must()?;
		if tok == Token::BraceClose {
			break;
		}

		let mut array_index = 0;

		enum KeyOrArray<'s> {
			Key(Expr<'s>),
			Array(Expr<'s>),
		}

		// parse field or array value.
		let first = match head {
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
				let key = expr.to_slot(state);
				let value = try_parse_expr(lexer.next_must()?, lexer, state)?.to_slot(state);
				state.emit(Op::Set(dst, key, value));
			},
			KeyOrArray::Array(entry) => {
				array_index += 1; // Lua is 1-indexed.

				let index_slot = state.new_slot();
				let value = entry.to_slot(state);

				let num_idx = state.number_idx(array_index as f64);
				state.emit(Op::LoadNum(index_slot, num_idx));
				state.emit(Op::Set(dst, index_slot, value));
			},
		}

		if lexer.next_if(Token::Comma)?.is_none() {
			expect_tok!(lexer, Token::BraceClose)?;
			break;
		}
	}

	Ok(dst)
}
