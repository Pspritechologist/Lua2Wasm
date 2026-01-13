use crate::Operation as Op;
use luant_lexer::{IdentKey, Lexer, Token};

use state::{ParseState, RootState, VariableScope};
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

pub trait ParseSrc {
	fn bytes(&self) -> &[u8];
}
impl ParseSrc for [u8] {
	fn bytes(&self) -> &[u8] { self }
}
impl ParseSrc for str {
	fn bytes(&self) -> &[u8] { self.as_bytes() }
}

pub fn parse<S: ParseSrc + ?Sized>(src: &S) -> Result<Parsed<'_>, Error<'_>> {
	let mut lexer = luant_lexer::lexer(src.bytes());
	let mut state = RootState::default();

	//TODO: Temp printing.
	let print = lexer.get_ident("print");
	assert_eq!(state.new_local(&lexer, print)?, 0);
	let libs = lexer.get_ident("lib");
	assert_eq!(state.new_local(&lexer, libs)?, 1);

	let mut parse = || {
		while let Some(tok) = lexer.next().transpose()? {
			parse_stmt(tok, &mut lexer, &mut state)?;
		}
		Ok::<_, Error>(())
	};

	parse().map_err(|e| {
		let (l, c) = lexer.current_pos();
		format!("Error at {l}:{c}: {e}")
	})?;

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
				Some(Token::Function) => todo!(),
				Some(Token::Assign) => (),
				Some(Token::Comma) => {
					while lexer.next_if(Token::Comma)?.is_some() {
						rest_idents.push(expect_tok!(lexer, Token::Identifier(ident) => ident)?);
					}
				},
				_ => Err("Expected '=', ',' or 'function'")?,
			}

			if lexer.next_if(Token::Assign)?.is_none() {
				// No assignments.
				state.new_local(lexer, first_ident)?;
				rest_idents.into_iter()
					.try_for_each(|ident| state.new_local(lexer, ident).map(|_| ()))?;
			} else {
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
				
				ident_iter.try_for_each(
					|ident| Expr::Constant(Const::Nil).to_new_local(lexer, state, ident).map(|_| ())
				)?;
			}
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
		head @ (Token::Identifier(_) | Token::ParenOpen) => {
			use expressions::IdentExpr;
			
			// Check if this is an assignment (single or multi)
			match IdentExpr::parse(head, lexer, state)? {
				IdentExpr::Place(first_place) => {
					let mut rest_places = Vec::new();

					while lexer.next_if(Token::Comma)?.is_some() {
						let IdentExpr::Place(place_expr) = IdentExpr::parse(lexer.next_must()?, lexer, state)? else {
							Err("Expected place expression")?
						};
						rest_places.push(place_expr);
					}

					expect_tok!(lexer, Token::Assign)?;

					let first_expr = parse_expr(lexer.next_must()?, lexer, state)?;
					let mut rest_exprs = Vec::new();

					while lexer.next_if(Token::Comma)?.is_some() {
						rest_exprs.push(parse_expr(lexer.next_must()?, lexer, state)?);
					}

					first_place.set_expr(lexer, state, first_expr)?;

					let mut place_iter = rest_places.into_iter();
					rest_exprs.into_iter()
						.zip(place_iter.by_ref())
						.try_for_each(|(expr, place)| place.set_expr(lexer, state, expr))?;
					
					place_iter.try_for_each(|place| place.set_expr(lexer, state, Expr::Constant(Const::Nil)))?;
				},
				IdentExpr::Call(_) => (),
			}

		},
		tok => return Err(format!("Expected statement, found {tok:?}").into()),
	}

	state.free_temps();

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
