use crate::parsing::{LexerExt, scopes::BreakCond};
use super::{Error, ParseScope, Op, VariableScope, FuncState, expect_tok};
use camento_lexer::{IdentKey, Lexer, Token};

struct LoopScope<'a, 's> {
	parent: &'a mut dyn ParseScope<'s>,
}
impl<'a, 's> LoopScope<'a, 's> {
	fn new(parent: &'a mut dyn ParseScope<'s>) -> Self {
		Self {
			parent,
		}
	}
}
impl<'a, 's> ParseScope<'s> for LoopScope<'a, 's> {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { self.parent }

	fn emit_break(&mut self, state: &mut FuncState<'_, 's>, span: usize, cond: BreakCond) -> Result<(), Error<'s>> {
		state.emit(self, match cond {
			BreakCond::Unconditional => Op::Break,
			BreakCond::IfTrue(cond) => Op::BreakIf(cond),
			BreakCond::IfFalse(cond) => Op::BreakIfNot(cond),
		}, span);
		Ok(())
	}
}

pub fn parse_while<'s>(lexer: &mut Lexer<'s>, scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	state.emit(scope, Op::StartLoop, lexer.src_index());

	let cond = super::parse_expr(lexer.next_must()?, lexer, scope, state)?;

	let mut scope = VariableScope::new(LoopScope::new(scope), state);

	match cond.as_const().map(|c| c.is_truthy()) {
		Some(true) => (), // This is an infinite loop and no check need be compiled.
		Some(false) | //TODO: Don't compile unreachable loops.
		None => {
			let span = lexer.src_index();
			scope.emit_break(state, span, BreakCond::IfFalse(cond))?;
		}
	}

	expect_tok!(lexer, Token::Do)?;

	loop {
		let (tok, trivia) = lexer.next_must_with_trivia()?;
		if matches!(tok, Token::End) {
			break;
		}

		super::parse_stmt(trivia, tok, lexer, &mut scope, state)?;
	}

	let mut loop_scope = scope.finalize_scope(state);

	// Jump back to the start of the loop.
	state.emit(&mut loop_scope.parent, Op::Continue, lexer.src_index());

	state.emit(&mut loop_scope.parent, Op::EndLoop, lexer.src_index());

	Ok(())
}

pub fn parse_repeat<'s>(lexer: &mut Lexer<'s>, scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	state.emit(scope, Op::StartLoop, lexer.src_index());

	let mut scope = VariableScope::new(LoopScope::new(scope), state);

	loop {
		let (tok, trivia) = lexer.next_must_with_trivia()?;
		if matches!(tok, Token::Until) {
			break;
		}

		super::parse_stmt(trivia, tok, lexer, &mut scope, state)?;
	}
	
	let cond = super::parse_expr(lexer.next_must()?, lexer, &mut scope, state)?;
	scope.emit_break(state, lexer.src_index(), BreakCond::IfTrue(cond))?;

	let mut loop_scope = scope.finalize_scope(state);

	// Jump back to the start of the loop.
	state.emit(&mut loop_scope.parent, Op::Continue, lexer.src_index());

	state.emit(&mut loop_scope.parent, Op::EndLoop, lexer.src_index());

	Ok(())
}
