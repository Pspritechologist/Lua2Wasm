use crate::Operation as Op;
use luant_lexer::{IdentKey, LexExtras, Lexer, Token};

use state::{ParseState, ParseStateExt, RootState, VariableScope};
use expressions::*;
use loops::*;

mod asm;
mod state;
mod expressions;
mod loops;

pub use asm::parse_asm;
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
			fn parse_next<'s>(lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>, ident: IdentKey) -> Result<Expr<'s>, Error<'s>> {
				let next_tok = lexer.next_if(|tok| matches!(tok, Token::Comma | Token::Equals))?;
				match next_tok {
					Some(Token::Comma) => {
						let next_ident = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
						let value = parse_next(lexer, state, ident)?;
						Ok(Expr::Constant(Const::Null))
					},
					Some(Token::Equals) => {
						let temp = state.new_temporary(lexer);
						let slot = state.local(lexer, temp)?;

						let expr = try_parse_expr(lexer.next_must()?, lexer, state)?;
						expr.set_to_slot(lexer, state, slot)?;

						Ok(expr)
					},
					None => Ok(Expr::Constant(Const::Null)),
					_ => unreachable!(),
				}
			}

			// fn parse_next_expr<'s>(lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<(), Error<'s>> {

			// }
		},
		Token::Return => todo!(),
		Token::Do => parse_do_block(head, lexer, state)?,
		Token::If => todo!(),
		Token::Else => todo!(),
		Token::While => todo!(),
		Token::For => todo!(),
		Token::Break => state.emit_break()?,
		Token::Goto => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let goto_op_pos = state.get_ops().len();
			let goto_target = state.find_label(label, goto_op_pos);
			let (p32, p8) = ((goto_target >> 8) as u32, (goto_target & 0xFF) as u8);
			state.emit(Op::GoTo(p32, p8));
		},
		Token::Identifier(ident) => {
			//TODO: Temp obviously.
			assert_eq!(lexer.resolve_ident(ident), "print");
			expect_tok!(lexer, Token::ParenOpen)?;
			let arg_expr = try_parse_expr(lexer.next_must()?, lexer, state)?;
			expect_tok!(lexer, Token::ParenClose)?;
			
			let temp = state.new_temporary(lexer);
			let slot = state.local(lexer, temp)?;
			arg_expr.set_to_slot(lexer, state, slot)?;
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

#[test]
fn test() {
	let src = include_str!("../test.lua");
	// for tok in luant_lexer::lexer(src) {
	// 	println!("{:?}", tok.unwrap());
	// }
	println!("{:?}", parse(src).unwrap());
}
