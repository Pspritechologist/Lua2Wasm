use crate::parsing::LexerExt;
use super::{Error, ParseState, Op, VariableScope, expect_tok};
use luant_lexer::{Lexer, Token};

struct LoopState<'a, 's> {
	parent: &'a mut dyn ParseState<'s>,
	start_pos: usize,
	end_jumps: Vec<usize>,
}
impl<'a, 's> LoopState<'a, 's> {
	fn new(parent: &'a mut dyn ParseState<'s>, start_pos: usize) -> Self {
		Self {
			parent,
			start_pos,
			end_jumps: Vec::new(),
		}
	}

	fn patch_end_jumps(&mut self) {
		let end_pos = self.parent.get_ops().len();
		let (p32, p8) = ((end_pos >> 8) as u32, (end_pos & 0xFF) as u8);

		let ops = self.parent.get_ops_mut();

		for &jump_pos in &self.end_jumps {
			if let Op::GoTo(ref mut to32, ref mut to8) = self.parent.get_ops_mut()[jump_pos] {
				*to32 = p32;
				*to8 = p8;
			} else {
				panic!("Expected GoTo at position {jump_pos}, found {:?}", self.parent.get_ops()[jump_pos]);
			}
		}
	}
}
impl<'a, 's> ParseState<'s> for LoopState<'a, 's> {
	fn parent(&mut self) -> &mut dyn ParseState<'s> { self.parent }
	fn get_ops(&self) -> &[Op] { self.parent.get_ops() }
	fn get_ops_mut(&mut self) -> &mut [Op] { self.parent.get_ops_mut() }

	fn emit_break(&mut self) -> Result<(), Error<'s>> {
		let break_pos = self.get_ops().len();
		self.emit(Op::GoTo(0, 0)); // Placeholder
		self.end_jumps.push(break_pos);
		Ok(())
	}

	fn emit_continue(&mut self) -> Result<(), Error<'s>> {
		let pos = self.start_pos;
		let (p32, p8) = ((pos >> 8) as u32, (pos & 0xFF) as u8);
		self.emit(Op::GoTo(p32, p8));
		Ok(())
	}
}

pub fn parse_while<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut dyn ParseState<'s>) -> Result<(), Error<'s>> {
	assert!(matches!(head, Token::While));

	let start = state.get_ops().len();
	let mut state = VariableScope::new(LoopState::new(state, start));

	// let cond = super::try_parse_expr(lexer.next_must()?, lexer, &mut state)?.set_to_slot(&mut state);

	expect_tok!(lexer, Token::Do)?;

	// Condition check
	// state.emit(op);

	loop {
		let tok = lexer.next_must()?;
		if matches!(tok, Token::End) {
			break;
		}

		// super::parse_stmt(tok, lexer, &mut state)?;
	}

	let LoopState { parent: state, end_jumps, .. } = state.into_inner();

	let end_pos = state.get_ops().len();
	end_jumps.into_iter().for_each(|pos| {
		let Op::GoTo(p32, p8) = &mut state.get_ops_mut()[pos] else {
			panic!("Expected GoTo at position {pos}, found {:?}", state.get_ops()[pos]);
		};
		*p32 = (end_pos >> 8) as u32;
		*p8 = (end_pos & 0xFF) as u8;
	});

	Ok(())
}
