use super::{LexerExt, Error, ParseScope, Named, IdentKey, Op, expect_tok};
use super::{Expr, FuncState};
use super::postfix_ops::{CallType, IndexType, ParsedCall};
use luant_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy)]
pub enum PlaceExpr<'s> {
	Name(IdentKey),
	Index {
		span: usize,
		target: Expr<'s>,
		index: Expr<'s>,
	},
}

impl<'s> PlaceExpr<'s> {
	pub fn set_expr(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, expr: Expr<'s>) -> Result<(), Error<'s>> {
		match self {
			PlaceExpr::Name(ident) => {
				match scope.resolve_name(state, ident)? {
					Named::Local(slot) => expr.set_to_slot(lexer, state, slot)?,
					Named::UpValue(idx) => {
						let src = expr.to_slot(lexer, state)?;
						state.emit(Op::SetUpVal(idx, src), lexer.src_index())
					},
					Named::Global(name) => {
						let src = expr.to_slot(lexer, state)?;
						let idx = state.string_idx(lexer.resolve_ident(name).as_bytes());
						state.emit(Op::SetUpTab(idx, src), lexer.src_index())
					},
				}

				Ok(())
			},
			PlaceExpr::Index { target, index, span } => {
				let table_slot = target.to_slot(lexer, state)?;
				let index_slot = index.to_slot(lexer, state)?;
				let value_slot = expr.to_slot(lexer, state)?;

				state.emit(Op::Set(table_slot, index_slot, value_slot), span);

				Ok(())
			},
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum IdentExpr<'s> {
	Place(PlaceExpr<'s>),
	Call(ParsedCall),
}

impl<'s> IdentExpr<'s> {
	pub fn parse(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<IdentExpr<'s>, Error<'s>> {
		let expr = match head {
			Token::ParenOpen => {
				let expr = super::parse_expr(lexer.next_must()?, lexer, scope, state)?;
				expect_tok!(lexer, Token::ParenClose)?;
				expr
			},
			Token::Identifier(ident) => match scope.resolve_name(state, ident)? {
				Named::Local(slot) => Expr::Local(slot),
				Named::UpValue(idx) => Expr::UpValue(idx),
				Named::Global(name) => Expr::Global(name),
			},
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
					let parsed_call = call_type.parse_call_args(lexer, scope, state, expr)?;
					match lexer.next_if_map(TrailingOp::from_token)? {
						Some(found_op) => {
							next_op = found_op;
							expr = parsed_call.handle_call(lexer, scope, state, 1)?;
						},
						None => return Ok(IdentExpr::Call(parsed_call)),
					}
				},
				TrailingOp::Index(index_type) => {
					let index = index_type.parse_index(lexer, scope, state)?;
					let span = lexer.src_index();
					next_op = match lexer.next_if_map(TrailingOp::from_token)? {
						Some(next_op) => {
							expr = index.handle_index(lexer, state, expr)?;
							next_op
						},
						None => return Ok(IdentExpr::Place(PlaceExpr::Index {
							target: expr,
							index: index.to_key()?,
							span,
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
