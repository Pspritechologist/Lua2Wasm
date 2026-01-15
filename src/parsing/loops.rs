use crate::parsing::LexerExt;
use super::{Error, ParseScope, Op, VariableScope, FuncState, expect_tok};
use luant_lexer::{Lexer, Token};

struct LoopScope<'a, 's> {
	parent: &'a mut dyn ParseScope<'s>,
	start_pos: usize,
	end_jumps: Vec<usize>,
}
impl<'a, 's> LoopScope<'a, 's> {
	fn new(parent: &'a mut dyn ParseScope<'s>, start_pos: usize) -> Self {
		Self {
			parent,
			start_pos,
			end_jumps: Vec::new(),
		}
	}

	fn patch_end_jumps(&mut self, state: &mut FuncState<'_, 's>) {
		let end_op = Op::goto(state.ops().len());
		let ops = state.ops_mut();

		for &jump_pos in &self.end_jumps {
			debug_assert!(matches!(ops[jump_pos], Op::GoTo(_, _)), "Expected GoTo at position {jump_pos}, found {:?}", ops[jump_pos]);
			ops[jump_pos] = end_op;
		}
	}
}
impl<'a, 's> ParseScope<'s> for LoopScope<'a, 's> {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { self.parent }

	fn emit_break(&mut self, state: &mut FuncState<'_, 's>, span: usize) -> Result<(), Error<'s>> {
		let break_pos = state.ops().len();
		state.emit(Op::GoTo(0, 0), span); // Placeholder
		self.end_jumps.push(break_pos);
		Ok(())
	}

	fn emit_continue(&mut self, state: &mut FuncState<'_, 's>, span: usize) -> Result<(), Error<'s>> {
		state.emit(Op::goto(self.start_pos), span);
		Ok(())
	}
}

pub fn parse_while<'s>(lexer: &mut Lexer<'s>, scope: &mut dyn ParseScope<'s>, state: &mut FuncState<'_, 's>) -> Result<(), Error<'s>> {
	let start = state.ops().len();
	
	let initial_slots_used = state.slots_used();
	let cond = super::parse_expr(lexer.next_must()?, lexer, scope, state)?;

	let mut scope = VariableScope::new(LoopScope::new(scope, start));

	match cond.as_const().map(|c| c.is_truthy()) {
		Some(true) => (), // This is an infinite loop and no check need be compiled.
		Some(false) | //TODO: Don't compile unreachable loops.
		None => {
			let span = lexer.src_index();
			let cond = cond.to_slot(lexer, &mut scope, state)?;
			state.emit(Op::SkpIf(cond), span);
			scope.emit_break(state, span)?;
			
			state.set_slots_used(initial_slots_used);
		}
	}

	expect_tok!(lexer, Token::Do)?;

	loop {
		let tok = lexer.next_must()?;
		if matches!(tok, Token::End) {
			break;
		}

		super::parse_stmt(tok, lexer, &mut scope, state)?;
	}

	let mut loop_scope = scope.finalize_root();

	// Jump back to the start of the loop.
	state.emit(Op::goto(loop_scope.start_pos), lexer.src_index());

	// Patch in jumps to the end of the loop (breaks).
	loop_scope.patch_end_jumps(state);

	Ok(())
}
