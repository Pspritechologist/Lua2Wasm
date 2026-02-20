use std::borrow::Cow;

use crate::bytecode::{Loc, Operation as Op, RetKind};
use crate::debug::DebugInfo;
use luant_lexer::{IdentKey, LexInterner, Lexer, Token};

use functions::FuncState;
use scopes::{ParseScope, Named, RootScope, VariableScope};
use expressions::*;

mod functions;
mod scopes;
pub mod expressions;
mod loops;
mod debug;

pub use functions::{ParsedFunction, Upvalue};

#[derive(Debug)]
pub struct Parsed<'s> {
	pub parsed_func: functions::ParsedFunction,
	pub strings: Box<[Cow<'s, bstr::BStr>]>,
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

pub fn parse<'s, S: ParseSrc + ?Sized>(src: &'s S) -> Result<(Parsed<'s>, LexInterner<'s>), Error<'s>> {
	let mut constants = ConstantMap::default();
	let mut state = functions::FuncState::new(&mut constants);
	let mut lexer = luant_lexer::lexer(src.bytes());
	let mut root_scope = RootScope::new_root(&mut lexer);

	let mut parse = || {
		while let Some((tok, trivia)) = lexer.next_with_trivia().transpose()? {
			parse_stmt(trivia, tok, &mut lexer, &mut root_scope, &mut state)?;
		}
		Ok::<_, Error>(())
	};

	parse().map_err(|e| {
		let (l, c) = lexer.current_pos();
		format!("Error at {l}:{c}: {e}")
	})?;

	root_scope.finalize(&mut state, &lexer)?;

	let (ops, debug, used_regs) = state.into_inner();

	let debug = debug.into_debug_info(
		lexer.interner(), None::<&str>, 0, false
	);

	let parsed_func = functions::ParsedFunction {
		operations: ops.into_boxed_slice(),
		param_count: 0,
		frame_size: used_regs,
		debug: Some(debug),
		// The global table is always loaded in the very first stack slot.
		upvalues: Box::new([Upvalue::ParentSlot(0)]),
		exported: None,
	};

	Ok((Parsed {
		parsed_func,
		strings: constants.strings.into_boxed_slice(),
		closures: constants.closures.into_boxed_slice(),
	}, lexer.into_interner()))
}

#[derive(Debug, Clone, Default)]
pub struct ConstantMap<'s> {
	string_indexes: hashbrown::HashMap<Cow<'s, bstr::BStr>, usize>,
	strings: Vec<Cow<'s, bstr::BStr>>,
	closures: Vec<functions::ParsedFunction>,
}

trait LexerExt<'s> {
	fn next_must(&mut self) -> Result<Token<'s>, Error<'s>>;
	fn next_must_with_trivia(&mut self) -> Result<(Token<'s>, Vec<&'s [u8]>), Error<'s>>;
}
impl<'s> LexerExt<'s> for Lexer<'s> {
	fn next_must(&mut self) -> Result<Token<'s>, Error<'s>> {
		self.next().transpose()?.ok_or_else(|| "Unexpected end of input".into())
	}

	fn next_must_with_trivia(&mut self) -> Result<(Token<'s>, Vec<&'s [u8]>), Error<'s>> {
		self.next_with_trivia().transpose()?.ok_or_else(|| "Unexpected end of input".into())
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

pub type Error<'s> = Box<dyn std::error::Error + 's>;

fn parse_stmt<'s>(trivia: Vec<&'s [u8]>, head: Token<'s>, lexer: &mut Lexer<'s>, scope: &mut impl ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	match head {
		Token::LineTerm => (),
		Token::Label => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let pos = state.ops().len();
			scope.new_label(lexer, state, label, pos)?;
			expect_tok!(lexer, Token::Label)?;
		},
		Token::Function => {
			use bstr::{ByteSlice, BString};

			let span = lexer.src_index();
			let name = expect_tok!(lexer, Token::Identifier(ident) => ident)?;

			let mut closure = functions::parse_function(lexer, &mut *scope, state, Some(name), span)?;

			if let Some(attr) = trivia.into_iter().find_map(|s| s.strip_prefix(b"export")) {
				let name = lexer.resolve_ident(name).to_string();
				closure.exported = Some((name, BString::from(attr.trim())));
			}

			let idx = state.push_closure(closure);

			state.emit(scope, Op::LoadClosure(Loc::Global(name), idx), span);
		},
		Token::Local => {
			if lexer.next_if(Token::Function)?.is_some() {
				use bstr::{ByteSlice, BString};

				let span = lexer.src_index();
				let name = expect_tok!(lexer, Token::Identifier(ident) => ident)?;

				let dst = scope.new_local(lexer, state, name)?;

				let mut closure = functions::parse_function(lexer, &mut *scope, state, Some(name), span)?;

				if let Some(attr) = trivia.into_iter().find_map(|s| s.strip_prefix(b"export")) {
					let name = lexer.resolve_ident(name).to_string();
					closure.exported = Some((name, BString::from(attr.trim())));
				}

				let idx = state.push_closure(closure);

				state.emit(scope, Op::LoadClosure(dst, idx), span);
			} else {
				//TODO: This uses two Vecs in the worst case and just isn't very shwifty.
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
				state.emit(scope, Op::empty_ret(), lexer.src_index());
			} else {
				let start_reg = scope.total_locals();

				let mut ret_count = 0;
				loop {
					ret_count += 1;
					
					let expr = parse_expr(lexer.next_must()?, lexer, scope, state)?;
					expr.set_to_slot(lexer, scope, state, Loc::Slot(start_reg + ret_count - 1))?;
					
					if lexer.next_if(Token::Comma)?.is_none() {
						break;
					}
				}

				state.update_max_slots_used(start_reg + ret_count);
				
				let ret = Op::ret(start_reg, ret_count);
				state.emit(scope, ret, lexer.src_index());
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
			scope.emit_goto(state, label, span, None)?;
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
					func_slot.handle_call(lexer, scope, state, RetKind::None)?;
				},
			}
		},
		tok => return Err(format!("Expected statement, found {tok:?}").into()),
	}

	state.set_slots_used(scope.total_locals());

	Ok(())
}

fn parse_do_block<'s>(lexer: &mut Lexer<'s>, scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	let mut scope = VariableScope::new(scope);

	loop {
		let (tok, trivia) = lexer.next_must_with_trivia()?;
		if tok == Token::End {
			break;
		}

		parse_stmt(trivia, tok, lexer, &mut scope, state)?;
	}

	if scope.needs_closing() {
		let base = scope.local_base();
		state.emit(&mut scope, Op::Close(base), lexer.src_index());
	}

	scope.finalize_scope();

	Ok(())
}

fn parse_if_statement<'s>(lexer: &mut Lexer<'s>, outer_scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	let if_span = lexer.src_index();
	let if_cond = parse_expr(lexer.next_must()?, lexer, outer_scope, state)?;

	// Start the if instruction block.
	state.emit(outer_scope, Op::StartIf(if_cond), if_span);

	'outer: loop {
		expect_tok!(lexer, Token::Then)?;

		let mut if_scope = VariableScope::new(&mut *outer_scope);

		let end_tok = loop {
			let (tok, trivia) = lexer.next_must_with_trivia()?;
			if matches!(tok, Token::Else | Token::ElseIf | Token::End) {
				break tok;
			}

			parse_stmt(trivia, tok, lexer, &mut if_scope, state)?;
		};

		if if_scope.needs_closing() {
			// Otherwise, emit a closing operation for the If block if needed.
			let base = if_scope.local_base();
			state.emit(&mut if_scope, Op::Close(base), lexer.src_index());
		}

		let outer_scope = if_scope.finalize_scope();

		match end_tok {
			Token::Else => {
				// Start the else instruction block.
				state.emit(outer_scope, Op::Else, lexer.src_index());

				let mut else_scope = VariableScope::new(&mut *outer_scope);
				loop {
					let (tok, trivia) = lexer.next_must_with_trivia()?;
					if tok == Token::End {
						if else_scope.needs_closing() {
							let base = else_scope.local_base();
							// Emit a closing operation for the else block if needed.
							state.emit(&mut else_scope, Op::Close(base), lexer.src_index());
						}
						else_scope.finalize_scope();
						break 'outer;
					}

					parse_stmt(trivia, tok, lexer, &mut else_scope, state)?;
				}
			},
			Token::ElseIf => {
				let elif_span = lexer.src_index();
				let elif_cond = parse_expr(lexer.next_must()?, lexer, outer_scope, state)?;
				state.emit(outer_scope, Op::ElseIf(elif_cond), elif_span);
				continue 'outer
			},
			Token::End => break 'outer,
			_ => unreachable!(),
		}
	}

	// End the if instruction block.
	state.emit(outer_scope, Op::EndIf, lexer.src_index());

	Ok(())
}
