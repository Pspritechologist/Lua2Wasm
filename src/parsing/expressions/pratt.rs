use crate::parsing::LexerExt;
use crate::types::Num;
use super::{Error, ParseScope, Op, IdentKey, expect_tok};
use super::{Expr, Const, FuncState, parse_table_init};
use super::postfix_ops::{CallType, IndexType};
use luant_lexer::{Lexer, Token};

pub fn parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, prec: u8) -> Result<Expr<'s>, Error<'s>> {
	// If the matches in this list are modified, be sure to update `super::can_start_expr` as well.
	let expr = match PrefixOp::from_token(head) {
		// Handle prefix operators.
		Some(prefix_op) => prefix_op.parse_prefix(lexer, scope, state)?,
		// Otherwise handle an atom.
		None => match head {
			Token::BraceOpen => Expr::Temp(parse_table_init(head, lexer, scope, state)?),
			Token::ParenOpen => {
				let expr = parse_expr(lexer.next_must()?, lexer, scope, state, 0)?;
				expect_tok!(lexer, Token::ParenClose)?;
				expr
			},
			Token::VarArgs => todo!(),
			Token::Function => todo!(),
			Token::Number(n) => Expr::Constant(Const::Number(Num::new(n))),
			Token::Identifier(ident) => Expr::from_named(scope.resolve_name(state, ident, false)?),
			Token::String(s) => Expr::Constant(Const::String(s)),
			Token::True => Expr::Constant(Const::Bool(true)),
			Token::False => Expr::Constant(Const::Bool(false)),
			Token::Nil => Expr::Constant(Const::Nil),
			tok => return Err(format!("Expected expression, found {tok:?}").into()),
		},
	};

	let mut expr = expr;

	loop {
		let Some(next_op) = lexer.next_if_map(|tok| InfixOp::from_token(tok).filter(|op| prec < op.prec()))? else {
			return Ok(expr);
		};

		expr = next_op.parse_infix(lexer, scope, state, expr)?;
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InfixOp<'s> {
	Add, Sub,
	Mul, Div, DivInt,
	Mod, Pow,
	Concat,
	Eq, Neq,
	Lt, Lte,
	Gt, Gte,
	And, Or,
	BitAnd, BitOr, BitXor,
	Shl, Shr,
	Call(CallType<'s>),
	Index(IndexType),
}
impl<'s> InfixOp<'s> {
	fn parse_infix(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, left: Expr<'s>) -> Result<Expr<'s>, Error<'s>> {
		let mut std_infix = |make_op: fn(u8, u8, u8) -> Op| {
			let span = lexer.src_index();

			let initial_slots_used = state.slots_used();

			let lhs = left.to_slot(lexer, state)?;

			let pre_rhs_slots_used = state.slots_used();
			let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;
			state.set_slots_used(pre_rhs_slots_used);

			if let (Expr::Constant(a), Expr::Constant(b)) = (left, right) &&
				let Some(res) = self.fold_const(lexer, a, b) {
				return Ok(Expr::Constant(res));
			}

			let rhs = right.to_slot(lexer, state)?;

			state.set_slots_used(initial_slots_used);

			let dst = state.reserve_slot();
			
			state.emit(make_op(dst, lhs, rhs), span);

			Ok(Expr::Temp(dst))
		};

		match self {
			InfixOp::Add => std_infix(Op::Add),
			InfixOp::Sub => std_infix(Op::Sub),
			InfixOp::Mul => std_infix(Op::Mul),
			InfixOp::Div => std_infix(Op::Div),
			InfixOp::DivInt => todo!(),
			InfixOp::Mod => std_infix(Op::Mod),
			InfixOp::Pow => std_infix(Op::Pow),
			InfixOp::Concat => std_infix(Op::Concat),
			InfixOp::Eq => std_infix(Op::Eq),
			InfixOp::Neq => std_infix(Op::Neq),
			InfixOp::Lt => std_infix(Op::Lt),
			InfixOp::Lte => std_infix(Op::Lte),
			InfixOp::Gt => std_infix(Op::Gt),
			InfixOp::Gte => std_infix(Op::Gte),
			InfixOp::BitAnd => std_infix(Op::BitAnd),
			InfixOp::BitOr => std_infix(Op::BitOr),
			InfixOp::BitXor => std_infix(Op::BitXor),
			InfixOp::Shl => std_infix(Op::BitShL),
			InfixOp::Shr => std_infix(Op::BitShR),
			InfixOp::Call(call) => call.parse_call_args(lexer, scope, state, left)?.handle_call(lexer, scope, state, 1),
			InfixOp::Index(index) => index.parse_index(lexer, scope, state)?.handle_index(lexer, state, left),
			//TODO: Fold consts here.
			InfixOp::And => {
				let dst = left.to_temp(lexer, state)?;

				state.emit(Op::SkpIf(dst), lexer.src_index());
				let goto_pos = state.ops().len();
				state.emit(Op::tmp_goto(), lexer.src_index()); // Placeholder
				
				let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;
				right.set_to_slot(lexer, state, dst)?;

				let end_pos = state.ops().len();
				state.ops_mut()[goto_pos] = Op::goto(end_pos);

				Ok(Expr::Temp(dst))
			},
			InfixOp::Or => {
				let dst = left.to_temp(lexer, state)?;

				state.emit(Op::SkpIfNot(dst), lexer.src_index());
				let goto_pos = state.ops().len();
				state.emit(Op::tmp_goto(), lexer.src_index()); // Placeholder
				
				let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;
				right.set_to_slot(lexer, state, dst)?;

				let end_pos = state.ops().len();
				state.ops_mut()[goto_pos] = Op::goto(end_pos);

				Ok(Expr::Temp(dst))
			},
		}
	}

	fn from_token(tok: Token<'s>) -> Option<Self> {
		Some(match tok {
			Token::Plus => InfixOp::Add,
			Token::Minus => InfixOp::Sub,
			Token::Mul => InfixOp::Mul,
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
			Token::BitNot => InfixOp::BitXor,
			Token::BitShiftL => InfixOp::Shl,
			Token::BitShiftR => InfixOp::Shr,
			tok => CallType::from_token(tok).map(InfixOp::Call)
				.or_else(|| IndexType::from_token(tok).map(InfixOp::Index))?,
		})
	}

	fn fold_const(self, lexer: &mut Lexer<'s>, a: Const<'s>, b: Const<'s>) -> Option<Const<'s>> {
		use Const::*;
		Some(match (self, a, b) {
			(InfixOp::Add, Number(a), Number(b)) => Number(Num::new(a.try_add(*b).ok()?)),
			(InfixOp::Sub, Number(a), Number(b)) => Number(Num::new(a.try_sub(*b).ok()?)),
			(InfixOp::Mul, Number(a), Number(b)) => Number(Num::new(a.try_mul(*b).ok()?)),
			(InfixOp::Div, Number(a), Number(b)) => Number(Num::new(a.try_div(*b).ok()?)),
			(InfixOp::DivInt, Number(a), Number(b)) => Number(Num::new(a.try_div(*b).ok()?)),
			(InfixOp::Mod, Number(a), Number(b)) => Number(Num::new(a.try_rem(*b).ok()?)),
			(InfixOp::Pow, Number(a), Number(b)) => Number(Num::new(a.try_powf(*b).ok()?)),
			(InfixOp::Eq, a, b) => Bool(a == b),
			(InfixOp::Neq, a, b) => Bool(a != b),
			(InfixOp::Lt, Number(a), Number(b)) => Bool(a < b),
			(InfixOp::Lte, Number(a), Number(b)) => Bool(a <= b),
			(InfixOp::Gt, Number(a), Number(b)) => Bool(a > b),
			(InfixOp::Gte, Number(a), Number(b)) => Bool(a >= b),
			(InfixOp::Concat, String(_), String(_)) => return None, //TODO: Can't represent dynamic strings yet.
			(InfixOp::And, a, b) => Bool(a.is_truthy() && b.is_truthy()),
			(InfixOp::Or, a, b) => Bool(a.is_truthy() || b.is_truthy()),
			(InfixOp::BitAnd, Number(a), Number(b)) => Number(Num::try_from((a.as_i64()? & b.as_i64()?) as f64).ok()?),
			(InfixOp::BitOr, Number(a), Number(b)) => Number(Num::try_from((a.as_i64()? | b.as_i64()?) as f64).ok()?),
			(InfixOp::BitXor, Number(a), Number(b)) => Number(Num::try_from((a.as_i64()? ^ b.as_i64()?) as f64).ok()?),
			(InfixOp::Shl, Number(a), Number(b)) => Number(Num::try_from((a.as_i64()? << b.as_i64()?) as f64).ok()?),
			(InfixOp::Shr, Number(a), Number(b)) => Number(Num::try_from((a.as_i64()? >> b.as_i64()?) as f64).ok()?),
			_ => return None,
		})
	}

	fn prec(self) -> u8 {
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
			InfixOp::Call(_) | InfixOp::Index(_) => 13,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrefixOp {
	Neg, BitNot, Len, Not,
}
impl PrefixOp {
	fn parse_prefix<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Expr<'s>, Error<'s>> {
		let span = lexer.src_index();

		let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;

		if let Expr::Constant(a) = right && let Some(res) = self.fold_const(a) {
			return Ok(Expr::Constant(res));
		}

		let dst = right.to_temp(lexer, state)?;

		match self {
			PrefixOp::Neg => state.emit(Op::Neg(dst, dst), span),
			PrefixOp::BitNot => state.emit(Op::BitNot(dst, dst), span),
			PrefixOp::Len => state.emit(Op::Len(dst, dst), span),
			PrefixOp::Not => state.emit(Op::Not(dst, dst), span),
		}
		
		Ok(Expr::Temp(dst))
	}

	fn from_token(tok: Token) -> Option<Self> {
		// If the matches in this list are modified, be sure to update `super::can_start_expr` as well.
		Some(match tok {
			Token::Minus => PrefixOp::Neg,
			Token::BitNot => PrefixOp::BitNot,
			Token::Len => PrefixOp::Len,
			Token::Not => PrefixOp::Not,
			_ => return None,
		})
	}

	fn fold_const<'s>(self, a: Const<'s>) -> Option<Const<'s>> {
		use Const::*;
		Some(match (self, a) {
			(PrefixOp::Neg, Number(a)) => Number(Num::new(a.try_neg().ok()?)),
			(PrefixOp::Not, a) => Bool(!a.is_truthy()),
			(PrefixOp::Len, String(s)) => Number(Num::from(s.len())),
			(PrefixOp::BitNot, Number(a)) => Number(Num::try_from((!a.as_i64()?) as f64).ok()?),
			_ => return None,
		})
	}

	fn prec(self) -> u8 {
		11
	}
}
