use crate::parsing::LexerExt;
use super::{Error, ParseScope, Op, expect_tok};
use super::{Expr, Const, FuncState};
use luant_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy)]
pub struct ParsedCall {
	pub span: usize,
	pub func_reg: u8,
	pub arg_count: u8,
}
impl ParsedCall {
	pub fn handle_call<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, ret_count: u8) -> Result<Expr<'s>, Error<'s>> {
		state.emit(scope, Op::Call(self.func_reg, self.arg_count, ret_count), self.span);
		// The first return value is stored over top the first argument.
		//TODO: Multiple returns.
		Ok(Expr::Temp(self.func_reg + 1))
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallType<'s> {
	Args, Method, Table, String(&'s bstr::BStr),
}
impl<'s> CallType<'s> {
	pub fn parse_call_args(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, left: Expr<'s>) -> Result<ParsedCall, Error<'s>> {
		let span = lexer.src_index();

		Ok(match self {
			CallType::Args => {
				let mut head = lexer.next_must()?;

				let func_reg = state.reserve_slot();
				left.set_to_slot(lexer, scope, state, func_reg)?;

				// Special case zero arg calls.
				if head == Token::ParenClose {
					return Ok(ParsedCall {
						func_reg,
						arg_count: 0,
						span,
					});
				}

				let mut arg_count = 0;
				let initial_slots_used = state.slots_used();

				// Parse arguments until we hit the closing parenthesis.
				loop {
					arg_count += 1;
					state.set_slots_used(initial_slots_used + arg_count);

					let arg_expr = super::parse_expr(head, lexer, scope, state)?;
					arg_expr.set_to_slot(lexer, scope, state, func_reg + arg_count)?;
					
					match lexer.next_must()? {
						Token::Comma => head = lexer.next_must()?,
						Token::ParenClose => break,
						tok => Err(format!("Expected ',' or ')', found {tok:?}"))?,
					}
				}

				ParsedCall { func_reg, arg_count, span }
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
	span: usize,
}
impl<'s> ParsedIndex<'s> {
	/// Emits the `get` operation for this index operation.
	pub fn handle_index(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, target: Expr<'s>) -> Result<Expr<'s>, Error<'s>> {
		let target_slot = target.to_slot(lexer, scope, state)?;
		let index_slot = self.index_expr.to_slot(lexer, scope, state)?;
		let result_reg = state.reserve_slot();
		state.emit(scope, Op::Get(result_reg, target_slot, index_slot), self.span);
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
	pub fn parse_index<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<ParsedIndex<'s>, Error<'s>> {
		let span = lexer.src_index();

		let index_expr = match self {
			IndexType::Index => {
				let index_expr = super::parse_expr(lexer.next_must()?, lexer, scope, state)?;
				expect_tok!(lexer, Token::BracketClose)?;
				index_expr
			},
			IndexType::Field => {
				let Token::Identifier(ident) = lexer.next_must()? else {
					return Err("Expected identifier after '.'".into());
				};
				let ident_name = lexer.resolve_ident(ident);
				Expr::Constant(Const::string(ident_name))
			},
		};
		
		Ok(ParsedIndex {
			index_expr,
			span,
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
