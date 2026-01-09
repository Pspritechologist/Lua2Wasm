use crate::Operation as Op;
use luant_lexer::{IdentKey, LexExtras, Lexer, Token};

use state::{ParseState, ParseStateExt, RootState, VariableScope};
use expressions::*;
use loops::*;

mod asm;
mod state;
mod expressions;
mod loops;

pub use asm::{parse_asm, fmt_asm};
pub use state::Parsed;

pub trait LexerExt<'s> {
	fn next_must(&mut self) -> Result<Token<'s>, Error<'s>>;
}
impl<'s> LexerExt<'s> for Lexer<'s> {
	fn next_must(&mut self) -> Result<Token<'s>, Error<'s>> {
		self.next().transpose()?.ok_or_else(|| "Unexpected end of input".into())
	}
}

macro_rules! expect_tok {
	($lexer:expr, $pat:pat) => {
		match $lexer.next_must()? {
			$pat => Ok(()),
			tok => Err(format!("Expected {}, found {tok:?}", stringify!($pat))),
		}
	};
	($lexer:expr, $pat:pat $(if $guard:expr)? => $val:expr) => {
		match $lexer.next_must()? {
			$pat $(if $guard)? => Ok($val),
			tok => Err(format!("Expected {}, found {tok:?}", stringify!($pat))),
		}
	};
}
use expect_tok;

type Error<'s> = Box<dyn std::error::Error + 's>;

pub fn parse(src: &str) -> Result<Parsed<'_>, Error<'_>> {
	let mut lexer = luant_lexer::lexer(src);
	let mut state = RootState::default();

	while let Some(tok) = lexer.next().transpose()? {
		parse_stmt(tok, &mut lexer, &mut state)?;
	}

	state.finalize(&lexer)
}

fn parse_stmt<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<(), Error<'s>> {
	match head {
		Token::LineTerm => (),
		Token::Label => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let pos = state.get_ops().len();
			state.label(lexer, label, pos)?;
			expect_tok!(lexer, Token::Label)?;
		},
		Token::Function => todo!(),
		Token::Local => {
			//TODO: This uses two Vecs in the worst case and just isn't very shwifty.

			let first_ident = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let mut rest_idents = Vec::new();
			
			match lexer.peek()? {
				Some(Token::Assign) => (),
				Some(Token::Comma) => {
					while lexer.next_if(Token::Comma)?.is_some() {
						rest_idents.push(expect_tok!(lexer, Token::Identifier(ident) => ident)?);
					}
				},
				_ => return state.new_local(lexer, first_ident).map(|_| ()),
			}

			if lexer.next_if(Token::Assign)?.is_none() {
				state.new_local(lexer, first_ident)?;
				rest_idents.into_iter()
					.try_for_each(|ident| state.new_local(lexer, ident).map(|_| ()))?;
				return Ok(());
			}

			let first_expr = parse_expr(lexer.next_must()?, lexer, state)?;
			let mut rest_exprs = Vec::new();
			while lexer.next_if(Token::Comma)?.is_some() {
				rest_exprs.push(parse_expr(lexer.next_must()?, lexer, state)?);
			}

			first_expr.to_new_local(lexer, state, first_ident)?;
			let mut ident_iter = rest_idents.into_iter();
			rest_exprs.into_iter()
				.zip(ident_iter.by_ref())
				.try_for_each(|(expr, ident)| expr.to_new_local(lexer, state, ident).map(|_| ()))?;
			
			ident_iter.try_for_each(|ident| state.new_local(lexer, ident).map(|_| ()))?;
		},
		Token::Return => todo!(),
		Token::Do => parse_do_block(head, lexer, state)?,
		Token::If => parse_if_statement(head, lexer, state)?,
		Token::Else => todo!(),
		Token::While => todo!(),
		Token::For => todo!(),
		Token::Break => state.emit_break()?,
		Token::Goto => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let goto_op_pos = state.get_ops().len();
			let goto_target = state.find_label(label, goto_op_pos);
			state.emit(Op::goto(goto_target));
		},
		Token::Identifier(ident) => {
			//TODO: Temp obviously.
			
			// Check if this is an assignment (single or multi)
			let mut rest_idents = Vec::new();
			
			// Collect additional identifiers if there are commas
			while lexer.next_if(Token::Comma)?.is_some() {
				rest_idents.push(expect_tok!(lexer, Token::Identifier(ident) => ident)?);
			}
			
			if lexer.next_if(Token::Assign)?.is_some() {
				let first_expr = parse_expr(lexer.next_must()?, lexer, state)?;
				let mut rest_exprs = Vec::new();
				while lexer.next_if(Token::Comma)?.is_some() {
					rest_exprs.push(parse_expr(lexer.next_must()?, lexer, state)?);
				}

				// Assign first expression to first identifier
				let slot = state.local(lexer, ident)?;
				first_expr.set_to_slot(lexer, state, slot)?;
				
				// Assign remaining expressions to remaining identifiers
				let mut ident_iter = rest_idents.into_iter();
				rest_exprs.into_iter()
					.zip(ident_iter.by_ref())
					.try_for_each(|(expr, ident)| {
						let slot = state.local(lexer, ident)?;
						expr.set_to_slot(lexer, state, slot)
					})?;
				
				return Ok(());
			}

			assert_eq!(lexer.resolve_ident(ident), "print");
			expect_tok!(lexer, Token::ParenOpen)?;
			let arg_expr = parse_expr(lexer.next_must()?, lexer, state)?;
			expect_tok!(lexer, Token::ParenClose)?;
			
			let slot = arg_expr.to_slot(lexer, state)?;
			state.emit(Op::Put(slot));
		},
		tok => return Err(format!("Expected statement, found {tok:?}").into()),
	}

	Ok(())
}

fn parse_do_block<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut dyn ParseState<'s>) -> Result<(), Error<'s>> {
	assert_eq!(head, Token::Do);

	let mut state = VariableScope::new(state);

	loop {
		let tok = lexer.next_must()?;
		if tok == Token::End {
			break;
		}

		parse_stmt(tok, lexer, &mut state)?;
	}

	Ok(())
}

fn parse_if_statement<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, outer_state: &mut dyn ParseState<'s>) -> Result<(), Error<'s>> {
	assert_eq!(head, Token::If);

	let mut if_end_jump_positions = Vec::new();

	'outer: loop {
		let cond = parse_expr(lexer.next_must()?, lexer, outer_state)?.to_slot(lexer, outer_state)?;
		outer_state.emit(Op::SkpIf(cond));
		let next_branch_jump_pos = outer_state.get_ops().len();
		outer_state.emit(Op::GoTo(0, 0)); // Placeholder

		expect_tok!(lexer, Token::Then)?;

		let mut if_state = VariableScope::new(&mut *outer_state);

		let end_tok = loop {
			let tok = lexer.next_must()?;
			if matches!(tok, Token::Else | Token::ElseIf | Token::End) {
				break tok;
			}

			parse_stmt(tok, lexer, &mut if_state)?;
		};

		if matches!(end_tok, Token::Else | Token::ElseIf) {
			if_end_jump_positions.push(if_state.get_ops().len());
			if_state.emit(Op::GoTo(0, 0)); // Placeholder
		}

		let end_pos = if_state.get_ops().len();
		if_state.get_ops_mut()[next_branch_jump_pos] = Op::goto(end_pos);

		let outer_state = if_state.into_inner();

		match end_tok {
			Token::Else => {
				let mut else_state = VariableScope::new(&mut *outer_state);
				loop {
					let tok = lexer.next_must()?;
					if tok == Token::End {
						break 'outer;
					}

					parse_stmt(tok, lexer, &mut else_state)?;
				}
			},
			Token::ElseIf => continue 'outer,
			Token::End => break 'outer,
			_ => unreachable!(),
		}
	}

	let end_jump_op = Op::goto(outer_state.get_ops().len());
	if_end_jump_positions.into_iter()
		.for_each(|pos| outer_state.get_ops_mut()[pos] = end_jump_op);

	Ok(())
}

#[test]
fn test() {
	let src = include_str!("../test.lua");
	// for tok in luant_lexer::lexer(src) {
	// 	println!("{:?}", tok.unwrap());
	// }
	println!("{:?}", parse(src).unwrap());
}
