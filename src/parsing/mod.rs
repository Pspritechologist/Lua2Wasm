use crate::Operation as Op;
use luant_lexer::{IdentKey, LexExtras, Lexer, Token};
use hashbrown::HashMap;

use expressions::*;
use loops::*;

mod asm;
mod expressions;
mod loops;

pub use asm::parse_asm;
use slotmap::SparseSecondaryMap;

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

#[auto_impl::auto_impl(&mut)]
trait ParseState<'s> {
	fn emit(&mut self, op: Op) { self.parent().emit(op) }
	fn get_ops(&self) -> &[Op];
	fn get_ops_mut(&mut self) -> &mut [Op];
	fn number_idx(&mut self, n: f64) -> u16 { self.parent().number_idx(n) }
	fn string_idx(&mut self, s: &'s str) -> u16 { self.parent().string_idx(s) }
	fn label(&mut self, lexer: &Lexer<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s>> { self.parent().label(lexer, label, pos) }
	fn find_label(&mut self, label: IdentKey, pos: usize) -> usize { self.parent().find_label(label, pos) }
	fn label_exists(&mut self, label: IdentKey) -> bool { self.parent().label_exists(label) }
	fn new_slot(&mut self) -> u8 { self.parent().new_slot() }
	fn drop_slot(&mut self, slot: u8) { self.parent().drop_slot(slot) }
	fn emit_break(&mut self) -> Result<(), Error<'s>> { self.parent().emit_break() }
	fn emit_continue(&mut self) -> Result<(), Error<'s>> { self.parent().emit_continue() }

	fn parent(&'_ mut self) -> &'_ mut dyn ParseState<'s>;
}

#[derive(Debug)]
pub struct Parsed<'s> {
	operations: Vec<Op>,
	numbers: Vec<f64>,
	strings: Vec<&'s str>,
}

pub fn parse(src: &str) -> Result<Parsed<'_>, Error<'_>> {
	#[derive(Default, Clone)]
	struct RootState<'s> {
		operations: Vec<Op>,

		locals: SparseSecondaryMap<IdentKey, u8>,
		numbers: Vec<f64>,
		string_indexes: HashMap<&'s str, usize>,
		strings: Vec<&'s str>,
		labels: SparseSecondaryMap<IdentKey, usize>,
		missing_labels: Vec<(IdentKey, usize)>,
	}

	impl<'s> ParseState<'s> for RootState<'s> {
		fn parent(&mut self) -> &mut dyn ParseState<'s> { unreachable!() }

		fn emit(&mut self, op: Op) {
			self.operations.push(op);
		}
		fn get_ops(&self) -> &[Op] {
			&self.operations
		}
		fn get_ops_mut(&mut self) -> &mut [Op] {
			&mut self.operations
		}

		fn number_idx(&mut self, n: f64) -> u16 {
			if let Some(idx) = self.numbers.iter().position(|&num| num == n) {
				idx
			} else {
				let idx = self.numbers.len();
				self.numbers.push(n);
				idx
			}.try_into().expect("Too many numbers consts :(")
		}
		fn string_idx(&mut self, s: &'s str) -> u16 {
			if let Some(&idx) = self.string_indexes.get(s) {
				idx
			} else {
				let idx = self.strings.len();
				self.strings.push(s);
				self.string_indexes.insert(s, idx);
				idx
			}.try_into().expect("Too many string consts :(")
		}
		fn label(&mut self, lexer: &Lexer<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s> > {
			if self.label_exists(label) {
				return Err(format!("Label '{}' already defined", lexer.resolve_ident(label)).into());
			}
			self.labels.insert(label, pos);
			Ok(())
		}
		fn find_label(&mut self, label: IdentKey, pos: usize) -> usize {
			if let Some(&pos) = self.labels.get(label) {
				return pos;
			}

			// Add to the list to fill in at a later date.
			self.missing_labels.push((label, pos));
			0
		}
		fn label_exists(&mut self, label: IdentKey) -> bool {
			self.labels.contains_key(label)
		}

		fn new_slot(&mut self) -> u8 {
			9
		}
		fn drop_slot(&mut self,slot:u8) {
			assert_eq!(slot, 9);
		}

		fn emit_break(&mut self) -> Result<(), Error<'s>> {
			Err("Break statement not within a loop".into())
		}
		fn emit_continue(&mut self) -> Result<(), Error<'s>> {
			Err("Continue statement not within a loop".into())
		}
	}

	let mut lexer = luant_lexer::lexer(src);
	let mut state = RootState::default();

	while let Some(tok) = lexer.next().transpose()? {
		parse_stmt(tok, &mut lexer, &mut state)?;
	}

	Ok(Parsed {
		operations: state.operations,
		numbers: state.numbers,
		strings: state.strings,
	})
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
		Token::Local => todo!(),
		Token::Return => todo!(),
		Token::Do => parse_do_block(head, lexer, state)?,
		Token::If => todo!(),
		Token::Else => todo!(),
		Token::While => todo!(),
		Token::For => todo!(),
		Token::Break => state.emit_break()?,
		Token::Goto => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			let goto_pos = state.get_ops().len();
			let pos = state.find_label(label, goto_pos);
			let (p32, p8) = ((pos >> 8) as u32, (pos & 0xFF) as u8);
			state.emit(Op::GoTo(p32, p8));
		},
		Token::Identifier(_) => todo!(),
		tok => return Err(format!("Expected statement, found {tok:?}").into()),
	}

	Ok(())
}

fn parse_do_block<'s>(head: Token<'s>, lexer: &mut Lexer<'s>, state: &mut impl ParseState<'s>) -> Result<(), Error<'s>> {
	struct ScopeState<'a, 's> {
		parent: &'a mut dyn ParseState<'s>,
		labels: SparseSecondaryMap<IdentKey, usize>,
	}

	impl<'s> ParseState<'s> for ScopeState<'_, 's> {
		fn parent(&mut self) -> &mut dyn ParseState<'s> { self.parent }
		fn get_ops(&self) -> &[Op] { self.parent.get_ops() }
		fn get_ops_mut(&mut self) -> &mut [Op] { self.parent.get_ops_mut() }

		fn label(&mut self, lexer: &Lexer<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s> > {
			if self.label_exists(label) {
				return Err(format!("Label '{}' already defined", lexer.resolve_ident(label)).into());
			}
			self.labels.insert(label, pos);
			Ok(())
		}
		fn find_label(&mut self, label: IdentKey, pos: usize) -> usize {
			self.labels.get(label).copied().unwrap_or_else(|| self.parent().find_label(label, pos))
		}
		fn label_exists(&mut self, label: IdentKey) -> bool {
			self.labels.contains_key(label) || self.parent().label_exists(label)
		}
	}

	assert_eq!(head, Token::Do);

	let mut state = ScopeState {
		parent: state,
		labels: SparseSecondaryMap::new(),
	};

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
