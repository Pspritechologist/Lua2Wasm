use crate::parsing::LexerExt;

use super::{Error, ParseState, Op, IdentKey, LexExtras};
use luant_lexer::{Lexer, Token};

struct LoopState<P> {
	parent: P,
	start_pos: usize,
	end_jumps: Vec<usize>,
}
impl<'s, P: ParseState<'s>> LoopState<P> {
	fn new(parent: P, start_pos: usize) -> Self {
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
impl<'s, P: ParseState<'s>> ParseState<'s> for LoopState<P> {
	fn emit(&mut self, op: Op) { self.parent.emit(op); }
	fn get_ops(&self) -> &[Op] { self.parent.get_ops() }
	fn get_ops_mut(&mut self) -> &mut [Op] { self.parent.get_ops_mut() }
	fn number_idx(&mut self, n: f64) -> usize { self.parent.number_idx(n) }
	fn string_idx(&mut self, lex_state: LexExtras<'s>, s: &'s str) -> usize { self.parent.string_idx(lex_state, s) }
	fn label(&mut self, lex_state: LexExtras<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s>> { self.parent.label(lex_state, label, pos) }
	fn find_label(&mut self, lex_state: LexExtras<'s>, label: IdentKey, pos: usize) -> usize { self.parent.find_label(lex_state, label, pos) }
	fn new_slot(&mut self) -> u8 { self.parent.new_slot() }

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

pub fn parse_while<'s>(head: Token, lexer: &mut Lexer<'s>, mut state: impl ParseState<'s>) -> Result<(), Error<'s>> {
	assert!(matches!(head, Token::While));

	let start = state.get_ops().len();
	let mut state = LoopState::new(state, start);

	let reg = super::parse_expr(lexer, &mut state)?;

	// Condition check
	// state.emit(op);

	loop {
		let tok = lexer.next_must()?;
		if matches!(tok, Token::End) {
			break;
		}

		// super::parse_stmt(tok, lexer, &mut state)?;
	}

	let LoopState { parent: state, end_jumps, .. } = state;

	Ok(())
}
