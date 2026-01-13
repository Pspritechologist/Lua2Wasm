use crate::parsing::LexerExt;
use super::{Error, ParseState, IdentKey, Op, expect_tok};
use super::Expr;
use super::postfix_ops::{CallType, IndexType};
use luant_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy)]
pub enum PlaceExpr<'s> {
	Name(IdentKey),
	Index {
		target: Expr<'s>,
		index: Expr<'s>,
	},
}

impl<'s> PlaceExpr<'s> {
	pub fn set_expr(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), expr: Expr<'s>) -> Result<(), Error<'s>> {
		match self {
			PlaceExpr::Name(ident) => {
				let slot = state.local(lexer, ident)?;
				expr.set_to_slot(lexer, state, slot)
			},
			PlaceExpr::Index { target, index } => {
				let table_slot = target.to_slot(lexer, state)?;
				let index_slot = index.to_slot(lexer, state)?;
				let value_slot = expr.to_slot(lexer, state)?;

				state.emit(Op::Set(table_slot, index_slot, value_slot));

				Ok(())
			},
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum IdentExpr<'s> {
	Place(PlaceExpr<'s>),
	Call(Expr<'s>),
}

impl<'s> IdentExpr<'s> {
	pub fn parse(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<IdentExpr<'s>, Error<'s>> {
		let expr = match head {
			Token::ParenOpen => {
				let expr = super::parse_expr(lexer.next_must()?, lexer, state)?;
				expect_tok!(lexer, Token::ParenClose)?;
				expr
			},
			Token::Identifier(ident) => Expr::Local(ident),
			tok => return Err(format!("Expected expression, found {tok:?}").into()),
		};
	
		let Some(mut next_op) = lexer.next_if_map(TrailingOp::from_token)? else {
			return match head {
				Token::Identifier(ident) => Ok(IdentExpr::Place(PlaceExpr::Name(ident))),
				_ => Err("Expected place expression".into()),
			}
		};
	
		let mut expr = expr;
	
		loop {
			match next_op {
				TrailingOp::Call(call_type) => {
					expr = call_type.parse_call_args(lexer, state, expr)?.handle_call(lexer, state, 1)?;
					next_op = match lexer.next_if_map(TrailingOp::from_token)? {
						Some(next_op) => next_op,
						None => return Ok(IdentExpr::Call(expr)),
					}
				},
				TrailingOp::Index(index_type) => {
					let index = index_type.parse_index(lexer, state)?;
					next_op = match lexer.next_if_map(TrailingOp::from_token)? {
						Some(next_op) => {
							expr = index.handle_index(lexer, state, expr)?;
							next_op
						},
						None => return Ok(IdentExpr::Place(PlaceExpr::Index {
							target: expr,
							index: index.to_key()?,
						})),
					}
				},
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TrailingOp<'s> {
	Call(CallType<'s>),
	Index(IndexType),
}
impl<'s> TrailingOp<'s> {
	fn from_token(tok: Token<'s>) -> Option<Self> {
		CallType::from_token(tok).map(TrailingOp::Call)
			.or_else(|| IndexType::from_token(tok).map(TrailingOp::Index))
	}
}
