use crate::bytecode::{Loc, RetKind};
use crate::parsing::LexerExt;
use super::{Error, ParseScope, Op, expect_tok};
use super::{Expr, Const, FuncState, parse_table_init};
use super::postfix_ops::{CallType, IndexType};
use luant_lexer::{Lexer, Token};

type Num = real_float::Finite<f64>;

pub fn parse_expr<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, prec: u8) -> Result<Expr, Error<'s>> {
	// If the matches in this list are modified, be sure to update `super::can_start_expr` as well.
	let expr = match PrefixOp::from_token(head) {
		// Handle prefix operators.
		Some(prefix_op) => prefix_op.parse_prefix(lexer, scope, state)?,
		// Otherwise handle an atom.
		None => match head {
			Token::BraceOpen => parse_table_init(head, lexer, scope, state)?.into(),
			Token::ParenOpen => {
				let expr = parse_expr(lexer.next_must()?, lexer, scope, state, 0)?;
				expect_tok!(lexer, Token::ParenClose)?;
				expr
			},
			Token::VarArgs => Expr::VarArgs,
			Token::Function => todo!(),
			Token::Number(n) => Expr::Constant(Const::Number(n)),
			Token::Identifier(ident) => Expr::from_named(scope.resolve_name(lexer, state, ident, false)?),
			Token::String((raw, s)) => Expr::Constant(Const::String(state.string_idx(s, raw)?)),
			Token::True => Expr::Constant(Const::Bool(true)),
			Token::False => Expr::Constant(Const::Bool(false)),
			Token::Nil => Expr::Constant(Const::Nil),
			tok => return Err(format!("Expected expression, found {tok:?}").into()),
		},
	};

	let mut expr = expr;

	loop {
		let Some(next_op) = lexer.try_next_if_map(|tok| {
			InfixOp::from_token(tok, state).map(|o| o.filter(|op| prec < op.prec()))
		})? else {
			return Ok(expr);
		};

		expr = next_op.parse_infix(lexer, scope, state, expr)?;
	}
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
	BitAnd, BitOr, BitXor,
	Shl, Shr,
	Call(CallType),
	Index(IndexType),
}
impl InfixOp {
	fn parse_infix<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, left: Expr) -> Result<Expr, Error<'s>> {
		let mut std_infix = |make_op: fn(Loc, Expr, Expr) -> Op| {
			let span = lexer.src_index();

			let initial_slots_used = state.slots_used();

			let pre_rhs_slots_used = state.slots_used();
			let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;
			state.set_slots_used(pre_rhs_slots_used);

			if let (Expr::Constant(a), Expr::Constant(b)) = (left, right) &&
				let Some(res) = self.fold_const(state, a, b) {
				return Ok(Expr::Constant(res));
			}

			state.set_slots_used(initial_slots_used);

			let dst = state.reserve_slot();
			
			state.emit(scope, make_op(dst, left, right), span);

			Ok(dst.into())
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
			InfixOp::Call(call) => {
				let loc = state.reserve_slot();
				call.parse_call_args(lexer, scope, state, left)?
					.handle_call(lexer, scope, state, RetKind::Single(loc))
			},
			InfixOp::Index(index) => index.parse_index(lexer, scope, state)?.handle_index(lexer, scope, state, left),
			
			InfixOp::And | InfixOp::Or => todo!(),
			// InfixOp::And => {
			// 	let dst = left.to_temp(lexer, scope, state)?;

			// 	state.emit(scope, Op::SkpIf(dst), lexer.src_index());
			// 	let goto_pos = state.ops().len();
			// 	state.emit(scope, Op::tmp_goto(), lexer.src_index()); // Placeholder
				
			// 	let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;
			// 	right.set_to_slot(lexer, scope, state, dst)?;

			// 	let end_pos = state.ops().len();
			// 	state.ops_mut()[goto_pos] = Op::goto(end_pos);

			// 	Ok(Expr::Temp(dst))
			// },
			// InfixOp::Or => {
			// 	let dst = left.to_temp(lexer, scope, state)?;

			// 	state.emit(scope, Op::SkpIfNot(dst), lexer.src_index());
			// 	let goto_pos = state.ops().len();
			// 	state.emit(scope, Op::tmp_goto(), lexer.src_index()); // Placeholder
				
			// 	let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;
			// 	right.set_to_slot(lexer, scope, state, dst)?;

			// 	let end_pos = state.ops().len();
			// 	state.ops_mut()[goto_pos] = Op::goto(end_pos);

			// 	Ok(Expr::Temp(dst))
			// },
		}
	}

	fn from_token<'s>(tok: Token<'s>, state: &mut FuncState<'_, 's>) -> Result<Option<Self>, Error<'s>> {
		Ok(Some(match tok {
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
			tok => return Ok(CallType::from_token(tok, state)?.map(InfixOp::Call)
				.or_else(|| IndexType::from_token(tok).map(InfixOp::Index))),
		}))
	}

	fn fold_const<'s>(self, state: &mut FuncState<'_, 's>, a: Const, b: Const) -> Option<Const> {
		use Const::*;
		Some(match (self, a, b) {
			(InfixOp::Add, Number(a), Number(b)) => Number(a.try_add(b).ok()?),
			(InfixOp::Sub, Number(a), Number(b)) => Number(a.try_sub(b).ok()?),
			(InfixOp::Mul, Number(a), Number(b)) => Number(a.try_mul(b).ok()?),
			(InfixOp::Div, Number(a), Number(b)) => Number(a.try_div(b).ok()?),
			(InfixOp::DivInt, Number(a), Number(b)) => Number(a.try_div(b).ok()?),
			(InfixOp::Mod, Number(a), Number(b)) => Number(a.try_rem(b).ok()?),
			(InfixOp::Pow, Number(a), Number(b)) => Number(a.try_powf(b).ok()?),
			(InfixOp::Eq, a, b) => Bool(a == b),
			(InfixOp::Neq, a, b) => Bool(a != b),
			(InfixOp::Lt, Number(a), Number(b)) => Bool(a < b),
			(InfixOp::Lte, Number(a), Number(b)) => Bool(a <= b),
			(InfixOp::Gt, Number(a), Number(b)) => Bool(a > b),
			(InfixOp::Gte, Number(a), Number(b)) => Bool(a >= b),
			(InfixOp::Concat, String(a), String(b)) => {
				let (a, b) = (state.get_string(a), state.get_string(b));
				let new_string = [&**a, &**b].concat();
				let idx = state.string_idx_owned(new_string);
				Const::String(idx)
			},
			(InfixOp::And, a, b) => Bool(a.is_truthy() && b.is_truthy()),
			(InfixOp::Or, a, b) => Bool(a.is_truthy() || b.is_truthy()),
			(InfixOp::BitAnd, Number(a), Number(b)) => Number(Num::try_new((num_as_int(a)? & num_as_int(b)?) as f64).ok()?),
			(InfixOp::BitOr, Number(a), Number(b)) => Number(Num::try_new((num_as_int(a)? | num_as_int(b)?) as f64).ok()?),
			(InfixOp::BitXor, Number(a), Number(b)) => Number(Num::try_new((num_as_int(a)? ^ num_as_int(b)?) as f64).ok()?),
			(InfixOp::Shl, Number(a), Number(b)) => Number(Num::try_new((num_as_int(a)? << num_as_int(b)?) as f64).ok()?),
			(InfixOp::Shr, Number(a), Number(b)) => Number(Num::try_new((num_as_int(a)? >> num_as_int(b)?) as f64).ok()?),
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
	fn parse_prefix<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<Expr, Error<'s>> {
		let span = lexer.src_index();

		let right = parse_expr(lexer.next_must()?, lexer, scope, state, self.prec())?;

		if let Expr::Constant(a) = right && let Some(res) = self.fold_const(state, a) {
			return Ok(Expr::Constant(res));
		}

		let dst = state.reserve_slot();

		match self {
			PrefixOp::Neg => state.emit(scope, Op::Neg(dst, right), span),
			PrefixOp::BitNot => state.emit(scope, Op::BitNot(dst, right), span),
			PrefixOp::Len => state.emit(scope, Op::Len(dst, right), span),
			PrefixOp::Not => state.emit(scope, Op::Not(dst, right), span),
		}
		
		Ok(dst.into())
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

	fn fold_const<'s>(self, state: &mut FuncState<'_, 's>, a: Const) -> Option<Const> {
		use Const::*;
		Some(match (self, a) {
			(PrefixOp::Neg, Number(a)) => Number(a.try_neg().ok()?),
			(PrefixOp::Not, a) => Bool(!a.is_truthy()),
			(PrefixOp::Len, String(s)) => Number(Num::new(state.get_string(s).len() as f64)),
			(PrefixOp::BitNot, Number(a)) => Number(Num::try_new((!num_as_int(a)?) as f64).ok()?),
			_ => return None,
		})
	}

	fn prec(self) -> u8 {
		11
	}
}

fn num_as_int(num: Num) -> Option<i64> {
	let num = num.val();
	let i = num as i64;
	if i as f64 != num { None } else { Some(i) }
}
