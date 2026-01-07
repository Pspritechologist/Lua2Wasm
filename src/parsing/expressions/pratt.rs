use crate::parsing::LexerExt;
use super::{Error, ParseState, ParseStateExt, Op, IdentKey, expect_tok};
use super::{Expr, Const, parse_table_init};
use luant_lexer::{Lexer, Token};

pub fn parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), prec: u8) -> Result<Expr<'s>, Error<'s>> {
	let expr = match PrefixOp::from_token(head) {
		// Handle prefix operators.
		Some(prefix_op) => prefix_op.parse_prefix(lexer, state)?,
		// Otherwise handle an atom.
		None => match head {
			Token::BraceOpen => Expr::Temp(parse_table_init(head, lexer, state)?),
			Token::ParenOpen => {
				let expr = parse_expr(lexer.next_must()?, lexer, state, 0)?;
				expect_tok!(lexer, Token::ParenClose)?;
				expr
			},
			Token::VarArgs => todo!(),
			Token::Function => todo!(),
			Token::Number(n) => Expr::Constant(Const::Number(n)),
			Token::Identifier(ident) => Expr::Local(ident),
			Token::String(s) => Expr::Constant(Const::String(s)), //TODO: Escapes.
			Token::RawString(s) => Expr::Constant(Const::String(s)),
			Token::True => Expr::Constant(Const::Bool(true)),
			Token::False => Expr::Constant(Const::Bool(false)),
			tok => return Err(format!("Expected expression, found {tok:?}").into()),
		},
	};

	let mut expr = expr;

	loop {
		let Some(next_op) = lexer.next_if_map(|tok| InfixOp::from_token(tok).filter(|op| prec < op.prec()))? else {
			return Ok(expr);
		};

		expr = next_op.parse_infix(lexer, state, expr)?;
	}
}

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
	Shl, Shr,
}
impl InfixOp {
	fn parse_infix<'s>(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), left: Expr<'s>) -> Result<Expr<'s>, Error<'s>> {
		let right = parse_expr(lexer.next_must()?, lexer, state, self.prec())?;

		macro_rules! std_infix {
			($op:ident) => { {
				let dst = match (left, right) {
					(Expr::Temp(t), _) | (_, Expr::Temp(t)) => t,
					_ => state.new_register(),
				};

				let lhs = left.to_slot(lexer, state)?;
				let rhs = right.to_slot(lexer, state)?;
				state.emit(Op::$op(dst, lhs, rhs));
				Expr::Temp(dst)
			} };
		}

		Ok(match self {
			InfixOp::Add => std_infix!(Add),
			InfixOp::Sub => std_infix!(Sub),
			InfixOp::Mul => std_infix!(Mul),
			InfixOp::Div => std_infix!(Div),
			InfixOp::DivInt => todo!(),
			InfixOp::Mod => std_infix!(Mod),
			InfixOp::Pow => std_infix!(Pow),
			InfixOp::Concat => todo!(),
			InfixOp::Eq => std_infix!(Eq),
			InfixOp::Neq => std_infix!(Neq),
			InfixOp::Lt => std_infix!(Lt),
			InfixOp::Lte => std_infix!(Lte),
			InfixOp::Gt => std_infix!(Gt),
			InfixOp::Gte => std_infix!(Gte),
			InfixOp::BitAnd => std_infix!(BitAnd),
			InfixOp::BitOr => std_infix!(BitOr),
			InfixOp::Shl => std_infix!(BitShL),
			InfixOp::Shr => std_infix!(BitShR),
			InfixOp::And => todo!(),
			InfixOp::Or => todo!(),
		})
	}

	fn from_token(tok: Token) -> Option<Self> {
		Some(match tok {
			Token::Plus => InfixOp::Add,
			Token::Minus => InfixOp::Sub,
			Token::Mult => InfixOp::Mul,
			Token::Div => InfixOp::Div,
			Token::DivFloor => InfixOp::DivInt,
			Token::Mod => InfixOp::Mod,
			Token::Pow => InfixOp::Pow,
			Token::Concat => InfixOp::Concat,
			Token::Equals => InfixOp::Eq,
			Token::NotEquals => InfixOp::Neq,
			Token::LessThan => InfixOp::Lt,
			Token::LessThanEquals => InfixOp::Lte,
			Token::GreaterThan => InfixOp::Gt,
			Token::GreaterThanEquals => InfixOp::Gte,
			Token::And => InfixOp::And,
			Token::Or => InfixOp::Or,
			Token::BitAnd => InfixOp::BitAnd,
			Token::BitOr => InfixOp::BitOr,
			Token::BitShiftL => InfixOp::Shl,
			Token::BitShiftR => InfixOp::Shr,
			_ => return None,
		})
	}

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
			InfixOp::BitAnd => 5,
			InfixOp::Shl | InfixOp::Shr => 6,
			InfixOp::Concat => 7,
			InfixOp::Add | InfixOp::Sub => 8,
			InfixOp::Mul | InfixOp::Div | InfixOp::DivInt | InfixOp::Mod => 9,
			// Unary => 10,
			InfixOp::Pow => 11,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrefixOp {
	Neg, BitNot, Len, Not,
}
impl PrefixOp {
	fn parse_prefix<'s>(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<Expr<'s>, Error<'s>> {
		let right = parse_expr(lexer.next_must()?, lexer, state, self.prec())?;

		let dst = match right {
			Expr::Temp(t) => t,
			_ => state.new_register(),
		};

		let right = right.to_slot(lexer, state)?;

		match self {
			PrefixOp::Neg => state.emit(Op::Neg(dst, right)),
			PrefixOp::BitNot => state.emit(Op::BitNot(dst, right)),
			PrefixOp::Len => state.emit(Op::Len(dst, right)),
			PrefixOp::Not => state.emit(Op::Not(dst, right)),
		}
		
		Ok(Expr::Temp(dst))
	}

	fn from_token(tok: Token) -> Option<Self> {
		Some(match tok {
			Token::Minus => PrefixOp::Neg,
			Token::BitNot => PrefixOp::BitNot,
			Token::Len => PrefixOp::Len,
			Token::Not => PrefixOp::Not,
			_ => return None,
		})
	}

	fn assoc(&self) -> Assoc {
		Assoc::Right // All prefix operators are right associative
	}

	fn prec(&self) -> u8 {
		10
	}
}
