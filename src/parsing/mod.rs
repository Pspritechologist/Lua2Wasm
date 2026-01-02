mod loops;
use crate::Operation as Op;
use luant_lexer::{IdentKey, LexExtras, Lexer, Token};
use hashbrown::HashMap;

mod asm;

pub use asm::parse_asm;
use slotmap::SparseSecondaryMap;

pub trait LexerExt<'src> {
	fn next_must(&mut self) -> Result<Token, Error<'src>>;
}
impl<'src> LexerExt<'src> for Lexer<'src> {
	fn next_must(&mut self) -> Result<Token, Error<'src>> {
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

type Error<'s> = Box<dyn std::error::Error + 's>;

#[auto_impl::auto_impl(&mut)]
trait ParseState<'s> {
	fn emit(&mut self, op: Op);
	fn get_ops(&self) -> &[Op];
	fn get_ops_mut(&mut self) -> &mut [Op];
	fn number_idx(&mut self, n: f64) -> usize;
	fn string_idx(&mut self, lex_state: LexExtras<'s>, s: &'s str) -> usize;
	fn label(&mut self, lex_state: LexExtras<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s>>;
	fn find_label(&mut self, lex_state: LexExtras<'s>, label: IdentKey, pos: usize) -> usize;
	fn new_slot(&mut self) -> u8;
	fn emit_break(&mut self) -> Result<(), Error<'s>>;
	fn emit_continue(&mut self) -> Result<(), Error<'s>>;
}

pub fn parse(src: &str) -> Result<(), Error<'_>> {
	#[derive(Default, Clone)]
	struct RootState<'s> {
		operations: Vec<Op>,

		numbers: Vec<f64>,
		string_indexes: HashMap<&'s str, usize>,
		strings: Vec<&'s str>,
		labels: SparseSecondaryMap<IdentKey, usize>,
		missing_labels: Vec<(IdentKey, usize)>,
	}

	impl<'s> ParseState<'s> for RootState<'s> {
		fn emit(&mut self, op: Op) {
			self.operations.push(op);
		}
		fn get_ops(&self) -> &[Op] {
			&self.operations
		}
		fn get_ops_mut(&mut self) -> &mut [Op] {
			&mut self.operations
		}

		fn number_idx(&mut self, n: f64) -> usize {
			if let Some(idx) = self.numbers.iter().position(|&num| num == n) {
				return idx;
			}
			let idx = self.numbers.len();
			self.numbers.push(n);
			idx
		}
		fn string_idx(&mut self, _lex_state: LexExtras<'s>, s: &'s str) -> usize {
			if let Some(&idx) = self.string_indexes.get(s) {
				return idx;
			}
			let idx = self.strings.len();
			self.strings.push(s);
			self.string_indexes.insert(s, idx);
			idx
		}
		fn label(&mut self, lex_state: LexExtras<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s> > {
			if self.labels.contains_key(label) {
				return Err(format!("Label '{}' already defined", lex_state.resolve_ident(label)).into());
			}
			self.labels.insert(label, pos);
			Ok(())
		}
		fn find_label(&mut self, _lex_state: LexExtras<'s>, label: IdentKey, pos: usize) -> usize {
			if let Some(&pos) = self.labels.get(label) {
				return pos;
			}

			// Add to the list to fill in at a later date.
			self.missing_labels.push((label, pos));
			0
		}

		fn new_slot(&mut self) -> u8 {
			9
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

	Ok(())
}

fn parse_stmt<'s>(head: Token, lexer: &mut Lexer<'s>, state: impl ParseState<'s>) -> Result<(), Error<'s>> {
	match head {
		Token::LineTerm => (),
		Token::Label => todo!(),
		Token::Function => todo!(),
		Token::Local => todo!(),
		Token::Return => todo!(),
		Token::Do => todo!(),
		Token::If => todo!(),
		Token::Else => todo!(),
		Token::While => todo!(),
		Token::For => todo!(),
		Token::Break => todo!(),
		Token::Goto => {
			let label = expect_tok!(lexer, Token::Identifier(ident) => ident)?;
			// state.labels.
		},
		Token::Identifier(_) => todo!(),
		tok => return Err(format!("Expected statement, found {tok:?}").into()),
	}

	Ok(())
}

fn parse_expr<'s>(lexer: &mut Lexer<'s>, state: impl ParseState<'s>) -> Result<u8, Error<'s>> {
	Ok(0)
}

#[test]
fn test() {
	let src = include_str!("../test.lua");
	parse(src).unwrap();
}
