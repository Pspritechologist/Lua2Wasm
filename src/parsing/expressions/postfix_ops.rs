use crate::bytecode::{Loc, RetKind};
use crate::parsing::LexerExt;
use super::{Error, ParseScope, Op, expect_tok};
use super::{Expr, Const, FuncState};
use camento_lexer::{Lexer, Token};

#[derive(Debug, Clone, Copy)]
pub struct ParsedCall {
	pub span: usize,
	pub func_slot: u8,
	pub arg_count: u8,
}
impl ParsedCall {
	pub fn handle_call<'s>(self, _lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, ret_count: RetKind) -> Result<Expr, Error<'s>> {
		state.emit(scope, Op::Call {
			func_slot: self.func_slot,
			arg_cnt: self.arg_count,
			ret_kind: ret_count,
		}, self.span);

		Ok(match ret_count {
			RetKind::None => Expr::Constant(Const::Nil),
			RetKind::Single(loc) => loc.into(),
			RetKind::Many => Expr::VarRet,
		})
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallType {
	Args, Method, Table, String(usize),
}
impl CallType {
	pub fn parse_call_args<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, left: Expr) -> Result<ParsedCall, Error<'s>> {
		let span = lexer.src_index();

		Ok(match self {
			CallType::Args => {
				let func_slot = state.reserve_slot_u8();
				left.set_to_slot(lexer, scope, state, Loc::Slot(func_slot))?;

				let arg_count = Self::parse_params(lexer, scope, state, func_slot + 1)?;

				ParsedCall { func_slot, arg_count, span }
			},
			CallType::Method => {
				let method_name = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
				expect_tok!(lexer, Token::ParenOpen)?;
				let method_name_idx = state.string_idx(camento_lexer::String::raw(lexer.resolve_ident(method_name).into()))?;

				let func_slot = state.reserve_slot_u8();
				
				let obj_slot = state.reserve_slot();
				left.set_to_slot(lexer, scope, state, obj_slot)?;

				state.emit(scope, Op::Get(Loc::Slot(func_slot), obj_slot.into(), Const::String(method_name_idx).into()), span);

				let arg_count = 1 + Self::parse_params(lexer, scope, state, func_slot + 2)?;

				ParsedCall { func_slot, arg_count, span }
			},
			CallType::Table => {
				let func_slot = state.reserve_slot_u8();
				left.set_to_slot(lexer, scope, state, Loc::Slot(func_slot))?;

				// This will be in the slot immediately following the function.
				let arg_slot = super::parse_table_init(Token::BraceOpen, lexer, scope, state)?;
				debug_assert_eq!(arg_slot, Loc::Slot(func_slot + 1), "Expected table argument to be placed in the slot immediately following the function slot");

				ParsedCall { func_slot, arg_count: 1, span }
			},
			CallType::String(idx) => {
				let func_slot = state.reserve_slot_u8();
				left.set_to_slot(lexer, scope, state, Loc::Slot(func_slot))?;

				Expr::Constant(Const::String(idx)).to_slot(lexer, scope, state)?;

				ParsedCall { func_slot, arg_count: 1, span }
			},
		})
	}

	pub fn from_token<'s>(tok: Token<'s>, state: &mut FuncState<'_, 's>) -> Result<Option<Self>, Error<'s>> {
		Ok(Some(match tok {
			Token::ParenOpen => CallType::Args,
			Token::Colon => CallType::Method,
			Token::BraceOpen => CallType::Table,
			Token::String(s) => CallType::String(state.string_idx(s)?),
			_ => return Ok(None),
		}))
	}

	fn parse_params<'s>(lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, args_base_slot: u8) -> Result<u8, Error<'s>> {
		let mut head = lexer.next_must()?;

		// Special case zero arg calls.
		if head == Token::ParenClose {
			return Ok(0);
		}

		let mut arg_count = 0;
		let initial_slots_used = state.slots_used();

		// Parse arguments until we hit the closing parenthesis.
		loop {
			let arg_expr = super::parse_expr(head, lexer, scope, state)?;
			arg_expr.set_to_slot(lexer, scope, state, Loc::Slot(args_base_slot + arg_count))?;
			
			arg_count += 1;
			state.set_slots_used(initial_slots_used + arg_count);

			match lexer.next_must()? {
				Token::Comma => head = lexer.next_must()?,
				Token::ParenClose => break,
				tok => Err(format!("Expected ',' or ')', found {tok:?}"))?,
			}
		}

		Ok(arg_count)
	}
}

#[derive(Debug, Clone, Copy)]
pub struct ParsedIndex {
	index_expr: Expr,
	span: usize,
}
impl<'s> ParsedIndex {
	/// Emits the `get` operation for this index operation.
	pub fn handle_index(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>, target: Expr) -> Result<Expr, Error<'s>> {
		let target_slot = target.to_slot(lexer, scope, state)?;
		let index_slot = self.index_expr.to_slot(lexer, scope, state)?;
		let result_reg = state.reserve_slot();
		state.emit(scope, Op::Get(result_reg, target_slot.into(), index_slot.into()), self.span);
		Ok(result_reg.into())
	}
	
	pub fn to_key(self) -> Result<Expr, Error<'s>> {
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
	pub fn parse_index<'s>(self, lexer: &mut Lexer<'s>, scope: &mut (impl ParseScope<'s> + ?Sized), state: &mut FuncState<'_, 's>) -> Result<ParsedIndex, Error<'s>> {
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
				Expr::Constant(Const::String(state.string_idx(camento_lexer::String::raw(ident_name.into()))?))
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
