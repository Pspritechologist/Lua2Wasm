use crate::Operation as Op;
use luant_lexer::{IdentKey, Lexer, Token};

use functions::FuncState;
use scopes::{ParseScope, RootScope, VariableScope};
use expressions::*;

mod asm;
mod functions;
mod scopes;
mod expressions;
mod loops;
mod debug;

pub use asm::{parse_asm, fmt_asm};

#[derive(Debug)]
pub struct Parsed<'s> {
	pub parsed_func: functions::ParsedFunction,
	pub numbers: Box<[f64]>,
	pub strings: Box<[&'s bstr::BStr]>,
	pub closures: Box<[functions::ParsedFunction]>,
}

pub trait ParseSrc {
	fn bytes(&self) -> &[u8];
}
impl ParseSrc for [u8] {
	fn bytes(&self) -> &[u8] { self }
}
impl ParseSrc for str {
	fn bytes(&self) -> &[u8] { self.as_bytes() }
}

pub fn parse<'s, S: ParseSrc + ?Sized>(src: &'s S) -> Result<Parsed<'s>, Error<'s>> {
	let mut constants = ConstantMap::default();
	let mut state = functions::FuncState::new(&mut constants);
	let mut lexer = luant_lexer::lexer(src.bytes());
	let mut root_scope = RootScope::new_root();

	//TODO: Temp printing.
	let print = lexer.get_ident("print");
	assert_eq!(root_scope.new_local(&lexer, &mut state, print)?, 0);
	let libs = lexer.get_ident("lib");
	assert_eq!(root_scope.new_local(&lexer, &mut state, libs)?, 1);

	let mut parse = || {
		while let Some(tok) = lexer.next().transpose()? {
			parse_stmt(tok, &mut lexer, &mut root_scope, &mut state)?;
		}
		Ok::<_, Error>(())
	};

	parse().map_err(|e| {
		let (l, c) = lexer.current_pos();
		format!("Error at {l}:{c}: {e}")
	})?;

	root_scope.finalize(&mut state, &lexer)?;

	let (ops, src_map, used_regs) = state.into_inner();

	let debug = crate::debug::DebugInfo::new_file(
		src_map,
		None::<&str>,
	);

	let parsed_func = functions::ParsedFunction {
		operations: ops.into_boxed_slice(),
		param_count: 0,
		frame_size: used_regs,
		debug: Some(debug),
	};

	Ok(Parsed {
		parsed_func,
		numbers: constants.numbers.into_boxed_slice(),
		strings: constants.strings.into_boxed_slice(),
		closures: constants.closures.into_boxed_slice(),
	})
}

#[derive(Debug, Clone, Default)]
pub struct ConstantMap<'s> {
	numbers: Vec<f64>,
	string_indexes: hashbrown::HashMap<&'s bstr::BStr, usize>,
	strings: Vec<&'s bstr::BStr>,
	closures: Vec<functions::ParsedFunction>,
}

trait LexerExt<'s> {
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

fn parse_stmt<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut impl ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	match head {
		Token::LineTerm => (),
		Token::Label => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let pos = state.ops().len();
			scope.new_label(lexer, state, label, pos)?;
			expect_tok!(lexer, Token::Label)?;
		},
		Token::Function => todo!(),
		Token::Local => {
			if lexer.next_if(Token::Function)?.is_some() {
				let span = lexer.src_index();
				let name = expect_tok!(lexer, Token::Identifier(ident) => ident)?;

				let dst = scope.new_local(lexer, state, name)?;

				let closure = functions::parse_function(lexer, &mut *scope, state, Some(name), span)?;
				let idx = state.push_closure(closure);

				state.emit(Op::LoadClosure(dst, idx), span);
			} else {
				//TODO: This uses two Vecs in the worst case and just isn't very shwifty.
				//TODO: Globals/Upvalues.

				let first_ident = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
				let mut rest_idents = Vec::new();
				
				if lexer.peek()? == Some(Token::Comma) {
					while lexer.next_if(Token::Comma)?.is_some() {
						rest_idents.push(expect_tok!(lexer, Token::Identifier(ident) => ident)?);
					}
				}

				if lexer.next_if(Token::Assign)?.is_none() {
					// No assignments.
					scope.new_local(lexer, state, first_ident)?;
					rest_idents.into_iter()
						.try_for_each(|ident| scope.new_local(lexer, state, ident).map(|_| ()))?;
				} else {
					let first_expr = parse_expr(lexer.next_must()?, lexer, scope, state)?;
					let mut rest_exprs = Vec::new();
					while lexer.next_if(Token::Comma)?.is_some() {
						rest_exprs.push(parse_expr(lexer.next_must()?, lexer, scope, state)?);
					}
		
					first_expr.to_new_local(lexer, scope, state, first_ident)?;

					let mut ident_iter = rest_idents.into_iter();
					
					rest_exprs.into_iter()
						.zip(ident_iter.by_ref())
						.try_for_each(|(expr, ident)| expr.to_new_local(lexer, scope, state, ident).map(|_| ()))?;
					
					ident_iter.try_for_each(
						|ident| Expr::Constant(Const::Nil).to_new_local(lexer, scope, state, ident).map(|_| ())
					)?;
				}
			}
		},
		Token::Return => {
			if lexer.peek()?.is_none_or(|tok| !expressions::can_start_expr(tok)) {
				state.emit(Op::Ret(0, 0), lexer.src_index());
			} else {
				let start_reg = scope.local_count();

				let mut ret_count = 0;
				loop {
					ret_count += 1;
					
					let expr = parse_expr(lexer.next_must()?, lexer, scope, state)?;
					expr.set_to_slot(lexer, scope, state, start_reg + ret_count - 1)?;
					
					if lexer.next_if(Token::Comma)?.is_none() {
						break;
					}
				}

				state.update_max_slots_used(start_reg + ret_count);
				
				let ret = Op::Ret(start_reg, ret_count);
				state.emit(ret, lexer.src_index());
			}
		},
		Token::Do => parse_do_block(lexer, scope, state)?,
		Token::If => parse_if_statement(lexer, scope, state)?,
		Token::While => loops::parse_while(lexer, scope, state)?,
		Token::For => todo!(),
		Token::Break => scope.emit_break(state, lexer.src_index())?,
		Token::Goto => {
			let span = lexer.src_index();
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let goto_op_pos = state.ops().len();
			let goto_target = scope.find_label(label, goto_op_pos);
			state.emit(Op::goto(goto_target), span);
		},
		head @ (Token::Identifier(_) | Token::ParenOpen) => {
			use expressions::IdentExpr;
			
			// Check if this is an assignment (single or multi)
			match IdentExpr::parse(head, lexer, scope, state)? {
				IdentExpr::Place(first_place) => {
					let mut rest_places = Vec::new();

					while lexer.next_if(Token::Comma)?.is_some() {
						let IdentExpr::Place(place_expr) = IdentExpr::parse(lexer.next_must()?, lexer, scope, state)? else {
							Err("Expected place expression")?
						};
						rest_places.push(place_expr);
					}

					expect_tok!(lexer, Token::Assign)?;

					let first_expr = parse_expr(lexer.next_must()?, lexer, scope, state)?;
					let mut rest_exprs = Vec::new();

					while lexer.next_if(Token::Comma)?.is_some() {
						rest_exprs.push(parse_expr(lexer.next_must()?, lexer, scope, state)?);
					}

					first_place.set_expr(lexer, scope, state, first_expr)?;

					let mut place_iter = rest_places.into_iter();
					rest_exprs.into_iter()
						.zip(place_iter.by_ref())
						.try_for_each(|(expr, place)| place.set_expr(lexer, scope, state, expr))?;
					
					place_iter.try_for_each(|place| place.set_expr(lexer, scope, state, Expr::Constant(Const::Nil)))?;
				},
				IdentExpr::Call(func_slot) => {
					func_slot.handle_call(lexer, scope, state, 0)?;
				},
			}
		},
		tok => return Err(format!("Expected statement, found {tok:?}").into()),
	}

	state.set_slots_used(scope.local_count());

	Ok(())
}

fn parse_do_block<'s>(lexer: &mut Lexer<'s>, scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	let mut scope = VariableScope::new(scope);

	loop {
		let tok = lexer.next_must()?;
		if tok == Token::End {
			break;
		}

		parse_stmt(tok, lexer, &mut scope, state)?;
	}

	scope.finalize_root();

	Ok(())
}

fn parse_if_statement<'s>(lexer: &mut Lexer<'s>, outer_scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	let mut if_end_jump_positions = Vec::new();

	'outer: loop {
		let span = lexer.src_index();
		let cond = parse_expr(lexer.next_must()?, lexer, outer_scope, state)?.to_slot(lexer, outer_scope, state)?;
		state.emit(Op::SkpIf(cond), span);
		let next_branch_jump_pos = state.ops().len();
		state.emit(Op::GoTo(0, 0), span); // Placeholder

		expect_tok!(lexer, Token::Then)?;

		let mut if_scope = VariableScope::new(&mut *outer_scope);

		let end_tok = loop {
			let tok = lexer.next_must()?;
			if matches!(tok, Token::Else | Token::ElseIf | Token::End) {
				break tok;
			}

			parse_stmt(tok, lexer, &mut if_scope, state)?;
		};

		if matches!(end_tok, Token::Else | Token::ElseIf) {
			if_end_jump_positions.push(state.ops().len());
			state.emit(Op::GoTo(0, 0), lexer.src_index()); // Placeholder
		}

		let end_pos = state.ops().len();
		state.ops_mut()[next_branch_jump_pos] = Op::goto(end_pos);

		let outer_scope = if_scope.finalize_root();

		match end_tok {
			Token::Else => {
				let mut else_scope = VariableScope::new(&mut *outer_scope);
				loop {
					let tok = lexer.next_must()?;
					if tok == Token::End {
						else_scope.finalize_root();
						break 'outer;
					}

					parse_stmt(tok, lexer, &mut else_scope, state)?;
				}
			},
			Token::ElseIf => continue 'outer,
			Token::End => break 'outer,
			_ => unreachable!(),
		}
	}

	let end_jump_op = Op::goto(state.ops().len());
	if_end_jump_positions.into_iter()
		.for_each(|pos| state.ops_mut()[pos] = end_jump_op);

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
