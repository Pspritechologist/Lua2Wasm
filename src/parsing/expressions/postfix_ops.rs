use crate::parsing::LexerExt;
use super::{Error, ParseState, Op, expect_tok};
use super::{Expr, Const};
use luant_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy)]
pub struct ParsedCall {
	pub func_reg: u8,
	pub arg_count: u8,
}
impl ParsedCall {
	pub fn handle_call<'s>(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), ret_count: u8) -> Result<Expr<'s>, Error<'s>> {
		state.emit(Op::Call(self.func_reg, self.arg_count, ret_count));
		Ok(Expr::Temp(self.func_reg))
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallType<'s> {
	Args, Method, Table, String(&'s bstr::BStr),
}
impl<'s> CallType<'s> {
	pub fn parse_call_args(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), left: Expr<'s>) -> Result<ParsedCall, Error<'s>> {
		let func_reg = state.new_temp();
		left.set_to_slot(lexer, state, func_reg)?;

		Ok(match self {
			CallType::Args => {
				let mut arg_count = 0;

				// Parse arguments until we hit the closing parenthesis.
				loop {
					arg_count += 1;

					_ = state.new_temp(); // Reserve a temp for the argument.
					let arg_expr = super::parse_expr(lexer.next_must()?, lexer, state)?;
					arg_expr.set_to_slot(lexer, state, func_reg + arg_count)?;
					
					match lexer.next_must()? {
						Token::Comma => continue,
						Token::ParenClose => break,
						tok => Err(format!("Expected ',' or ')', found {tok:?}"))?,
					}
				}

				ParsedCall { func_reg, arg_count }
			},
			CallType::Method => todo!(),
			CallType::Table => todo!(),
			CallType::String(_) => todo!(),
		})
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

#[derive(Debug, Clone, Copy)]
pub struct ParsedIndex<'s> {
	index_expr: Expr<'s>,
}
impl<'s> ParsedIndex<'s> {
	/// Emits the `get` operation for this index operation.
	pub fn handle_index(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized), target: Expr<'s>) -> Result<Expr<'s>, Error<'s>> {
		let target_slot = target.to_slot(lexer, state)?;
		let index_slot = self.index_expr.to_slot(lexer, state)?;
		let result_reg = state.new_temp();
		state.emit(Op::Get(result_reg, target_slot, index_slot));
		Ok(Expr::Temp(result_reg))
	}
	
	pub fn to_key(&self) -> Result<Expr<'s>, Error<'s>> {
		Ok(self.index_expr)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexType {
	Index, Field,
}
impl IndexType {
	/// Parses the key expression for the index operation and returns it.
	/// Use 'handle_index' to emit the `get` operation if desired.\
	/// Note that this function expects the leading open bracket or dot to have already been consumed.
	pub fn parse_index<'s>(self, lexer: &mut Lexer<'s>, state: &mut (impl ParseState<'s> + ?Sized)) -> Result<ParsedIndex<'s>, Error<'s>> {
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
		
		Ok(ParsedIndex {
			index_expr,
		})
	}

	pub fn from_token(tok: Token) -> Option<Self> {
		Some(match tok {
			Token::BracketOpen => IndexType::Index,
			Token::Dot => IndexType::Field,
			_ => return None,
		})
	}
}
