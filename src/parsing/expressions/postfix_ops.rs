use crate::parsing::LexerExt;
use super::{Error, ParseState, Op, expect_tok};
use super::{Expr, Const};
use luant_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallType<'s> {
	Args, Method, Table, String(&'s bstr::BStr),
}
impl<'s> CallType<'s> {
	pub fn parse_call_args(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), left: Expr<'s>) -> Result<Expr<'s>, Error<'s>> {
		match self {
			CallType::Args => todo!(),
			CallType::Method => todo!(),
			CallType::Table => todo!(),
			CallType::String(_) => todo!(),
		}
	}

	pub fn from_token(tok: Token<'s>) -> Option<Self> {
		Some(match tok {
			Token::ParenOpen => CallType::Args,
			Token::Colon => CallType::Method,
			Token::BraceOpen => CallType::Table,
			Token::String(s) => CallType::String(s),
			_ => return None,
		})
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexType {
	Index, Field,
}
impl IndexType {
	/// Note that this function expects the leading open bracket or dot to have already been consumed.
	pub fn parse_index<'s>(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), target: Expr<'s>) -> Result<Expr<'s>, Error<'s>> {
		let index_expr = match self {
			IndexType::Index => {
				let index_expr = super::parse_expr(lexer.next_must()?, lexer, state)?;
				expect_tok!(lexer, Token::BracketClose)?;
				index_expr
			},
			IndexType::Field => {
				let Token::Identifier(ident) = lexer.next_must()? else {
					return Err("Expected identifier after '.'".into());
				};
				let ident_name = lexer.resolve_ident(ident);
				Expr::Constant(Const::String(ident_name))
			},
		};

		let dst = index_expr.to_temp(lexer, state)?;

		let table_slot = target.to_slot(lexer, state)?;
		let index_slot = index_expr.to_slot(lexer, state)?;

		state.emit(Op::Get(dst, table_slot, index_slot));

		Ok(Expr::Temp(dst))
	}

	pub fn from_token(tok: Token) -> Option<Self> {
		Some(match tok {
			Token::BracketOpen => IndexType::Index,
			Token::Dot => IndexType::Field,
			_ => return None,
		})
	}
}
