use crate::{bytecode::Loc, debug::{DebugInfo, SrcMap}};
use super::{debug::InfoCollector, VariableScope, ParseScope, Named, LexerExt, Error, Op, expect_tok};
use luant_lexer::{Lexer, Token, IdentKey};
use bstr::BStr;

#[derive(Debug)]
pub struct FuncState<'a, 's> {
	constants: &'a mut super::ConstantMap<'s>,
	
	operations: Vec<Op>,
	max_slot_use: u8,
	cur_slot_use: u8,

	debug_info: InfoCollector
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
	pub operations: Box<[Op]>,
	pub debug: Option<DebugInfo>,
	pub frame_size: u8,
	pub param_count: u8,
	pub upvalues: Box<[Upvalue]>,
	pub exported: Option<(String, bstr::BString)>,
}

impl<'a, 's> FuncState<'a, 's> {
	pub fn new(constants: &'a mut super::ConstantMap<'s>) -> Self {
		Self {
			constants,
			operations: Vec::new(),
			max_slot_use: 0,
			cur_slot_use: 0,
			debug_info: InfoCollector::default(),
		}
	}

	pub fn into_inner(self) -> (Vec<Op>, InfoCollector, u8) {
		(self.operations, self.debug_info, self.max_slot_use)
	}

	pub fn emit(&mut self, _scope: &mut (impl ParseScope<'s> + ?Sized), op: Op, src_index: usize) {
		self.operations.push(op);
		self.debug_info.emit(src_index);
	}

	pub fn declare_local(&mut self, name: IdentKey) {
		self.debug_info.add_local(name);
	}

	pub fn ops(&self) -> &[Op] {
		&self.operations
	}

	pub fn ops_mut(&mut self) -> &mut [Op] {
		&mut self.operations
	}

	pub fn reserve_slot(&mut self) -> Loc {
		let reg = self.cur_slot_use;
		self.cur_slot_use += 1;

		self.max_slot_use = self.max_slot_use.max(self.cur_slot_use);

		Loc::Slot(reg)
	}

	pub fn reserve_slot_u8(&mut self) -> u8 {
		let reg = self.cur_slot_use;
		self.cur_slot_use += 1;

		self.max_slot_use = self.max_slot_use.max(self.cur_slot_use);

		reg
	}

	pub fn slots_used(&self) -> u8 {
		self.cur_slot_use
	}

	pub fn set_slots_used(&mut self, used: u8) {
		self.cur_slot_use = used;
		self.max_slot_use = self.max_slot_use.max(self.cur_slot_use);
	}
	
	pub fn update_max_slots_used(&mut self, used: u8) {
		self.max_slot_use = self.max_slot_use.max(used);
	}

	pub fn string_idx(&mut self, s: &'s (impl AsRef<[u8]> + ?Sized), raw: bool) -> Result<usize, Error<'s>> {
		let s = BStr::new(s);

		//TODO:
		// Technically I think I'm meant to replace any newline-style char within long-form strings with raw newlines?
		// Seems like a lot of work...

		let s = if raw || !s.contains(&b'\\') { s.into() } else {
			let mut unescaped = bstr::BString::default();
			let mut chars = s.iter().cloned().peekable();
			while let Some(c) = chars.next() {
				if c == b'\\' {
					// It's technically impossible for a non-raw string to end with a single backslash.
					match chars.next().ok_or("Trailing backslash in string")? {
						b'a' => unescaped.push(b'a'),   // bell.
						b'b' => unescaped.push(b'a'),   // back space.
						b'f' => unescaped.push(b'a'),   // form feed.
						b'n' => unescaped.push(b'\n'),  // newline.
						b'r' => unescaped.push(b'\r'),  // carriage return.
						b't' => unescaped.push(b'\t'),  // horizontal tab.
						b'v' => unescaped.push(b'a'),   // vertical tab.
						b'\\' => unescaped.push(b'\\'), // backslash.
						b'"' => unescaped.push(b'"'),   // double quote.
						b'\'' => unescaped.push(b'\''), // single quote.
						b'\n' => unescaped.push(b'\n'), // line continuation ('Short strings' can span multiple lines if each line ends with a backslash).
						b'z' => while chars.next_if(|c| c.is_ascii_whitespace()).is_some() { }, // skip following whitespace.
						b'x' => { // hex byte sequences.
							let hi = chars.next().ok_or("Unexpected end of string in hex escape")?.to_ascii_lowercase();
							let lo = chars.next().ok_or("Unexpected end of string in hex escape")?.to_ascii_lowercase();
							if !hi.is_ascii_hexdigit() || !lo.is_ascii_hexdigit() {
								return Err(format!("Invalid hex escape sequence: \\x{}{}", hi as char, lo as char).into());
							}

							let hi = hi - if hi.is_ascii_digit() { b'0' } else { b'a' - 10 };
							let lo = lo - if lo.is_ascii_digit() { b'0' } else { b'a' - 10 };
							let byte = hi * 16 + lo;

							unescaped.push(byte);
						},
						digit if digit.is_ascii_digit() => { // decimal byte sequences.
							let (a, b, c) = (
								digit,
								chars.next().ok_or("Unexpected end of string in decimal escape")?,
								chars.next().ok_or("Unexpected end of string in decimal escape")?,
							);

							if !b.is_ascii_digit() || !c.is_ascii_digit() {
								return Err(format!("Invalid decimal escape sequence: \\{}{}{}", a as char, b as char, c as char).into());
							}

							let a = a - b'0';
							let b = b - b'0';
							let c = c - b'0';
							let byte = a * 100 + b * 10 + c;

							unescaped.push(byte);
						},
						b'u' => { // Unicode code points.
							if chars.next() != Some(b'{') {
								return Err("Expected '{' after \\u in Unicode escape".into());
							}

							let mut codepoint = 0u32;

							let mut next_digit = || {
								let c = chars.next().ok_or("Unexpected end of string in Unicode escape")?.to_ascii_lowercase();
								if c == b'}' {
									return Ok::<_, Error>(false);
								}

								if !c.is_ascii_hexdigit() {
									return Err(format!("Invalid character '{}' in Unicode escape", c as char).into());
								}

								let digit = (c - if c.is_ascii_digit() { b'0' } else { b'a' - 10 }) as u32;
								codepoint = codepoint * 16 + digit;

								Ok(true)
							};

							// A limit of three hex digits.
							if next_digit()? && next_digit()? && next_digit()? {
								return Err("Too many hex digits in Unicode escape".into());
							}

							// Lua allows codepoints less than 2^31
							if codepoint >= 2^31 {
								return Err(format!("Unicode code point out of range: U+{codepoint:X}").into());
							}

							let ch = char::from_u32(codepoint).ok_or(format!("Invalid Unicode code point: U+{codepoint:X}"))?;

							unescaped.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
						},
						other => return Err(format!("Invalid escape sequence: \\{}", other as char).into()),
					}
				} else {
					if c == b'\n' {
						return Err("Unescaped newline in string".into());
					}

					unescaped.push(c);
				}
			}

			std::borrow::Cow::Owned(unescaped)
		};

		if let Some(&idx) = self.constants.string_indexes.get(&s) {
			Ok(idx)
		} else {
			let idx = self.constants.strings.len();
			self.constants.strings.push(s.clone());
			self.constants.string_indexes.insert(s, idx);
			Ok(idx)
		}
	}

	pub fn string_idx_owned(&mut self, s: impl Into<Vec<u8>>) -> usize {
		let s = std::borrow::Cow::Owned(bstr::BString::new(s.into()));
		if let Some(&idx) = self.constants.string_indexes.get(&s) {
			idx
		} else {
			let idx = self.constants.strings.len();
			self.constants.strings.push(s.clone());
			self.constants.string_indexes.insert(s, idx);
			idx
		}
	}

	pub fn get_string(&self, idx: usize) -> &BStr {
		self.constants.strings.get(idx).expect("Invalid string index")
	}

	pub fn push_closure(&mut self, func: ParsedFunction) -> usize {
		let idx = self.constants.closures.len();
		self.constants.closures.push(func);
		// This is + 1 because 0 is the 'main' function and unnameable.
		//TODO: This assumes the main function will always be at index 0. Is that fine?
		idx + 1
	}
}

/// A lot like a [`RootScope`], but for a nested closure.\
/// This acts to separate a closure from its outer scope, ensuring locals
/// and such are only accessed as UpValues.
struct ClosureScope<'a, 's> {
	outer_scope: &'a mut dyn ParseScope<'s>,
	upvalues: slotmap::SparseSecondaryMap<IdentKey, (u8, Upvalue)>,
}
impl<'a, 's> ClosureScope<'a, 's> {
	fn new(outer_scope: &'a mut dyn ParseScope<'s>, lexer: &mut Lexer<'s>) -> Self {
		Self {
			outer_scope,
			// The 0th Upvalue of a function is always the global environment.
			upvalues: FromIterator::from_iter([(lexer.get_ident("_ENV"), (0, Upvalue::ParentUpValue(0)))]),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Upvalue {
	ParentSlot(u8),
	ParentUpValue(u8),
}

impl<'s> ParseScope<'s> for ClosureScope<'_, 's> {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { unreachable!() }

	fn new_label(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _label: IdentKey, _label_pos: usize) -> Result<(), Error<'s>> {
		unreachable!()
	}
	
	fn emit_goto(&mut self, _state: &mut FuncState<'_, 's>, _label: IdentKey, _span: usize, _closed_base: Option<u8>) -> Result<(), Error<'s>> {
		unreachable!()
	}

	fn merge_missing_labels(&mut self, _other: Vec<(IdentKey, usize, Option<u8>)>) {
		unreachable!()
	}

	fn label_exists(&mut self, _label: IdentKey) -> bool { false }

	fn emit_break(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Break statement not within a loop".into())
	}
	fn emit_continue(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Continue statement not within a loop".into())
	}

	fn new_local(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _name: IdentKey) -> Result<Loc, Error<'s>> {
		unreachable!()
	}

	fn resolve_name(&mut self, state: &mut FuncState<'_, 's>, name: IdentKey, _is_capturing: bool) -> Result<Named, Error<'s>> {
		if let Some(&(idx, _)) = self.upvalues.get(name) {
			return Ok(Named::UpValue(idx));
		}

		Ok(match self.outer_scope.resolve_name(state, name, true)? {
			Named::Local(slot) => {
				let upval_idx = self.upvalues.len().try_into().expect("Too many upvalues :(");
				self.upvalues.insert(name, (upval_idx, Upvalue::ParentSlot(slot)));
				Named::UpValue(upval_idx)
			},
			Named::UpValue(idx) => {
				let upval_idx = self.upvalues.len().try_into().expect("Too many upvalues :(");
				self.upvalues.insert(name, (upval_idx, Upvalue::ParentUpValue(idx)));
				Named::UpValue(upval_idx)
			},
			Named::Global(name) => Named::Global(name),
		})
	}

	fn total_locals(&mut self) -> u8 { 0 }
}

pub fn parse_function<'s>(lexer: &mut Lexer<'s>, mut scope: impl ParseScope<'s>, state: &mut FuncState<'_, 's>, name: Option<super::IdentKey>, span: usize) -> Result<ParsedFunction, Error<'s>> {
	let mut closure_state = FuncState::new(state.constants);
	let mut closure_scope = VariableScope::new(ClosureScope::new(&mut scope, lexer));

	expect_tok!(lexer, Token::ParenOpen)?;

	let mut params = 0u8;

	let mut tok = lexer.next_must()?;
	loop {
		let ident = match tok {
			Token::Identifier(ident) => ident,
			Token::VarArgs => todo!(),
			Token::ParenClose => break,
			tok => Err(format!("Expected parameter name, found {tok:?}"))?,
		};

		closure_scope.new_local(lexer, &mut closure_state, ident)?;
		params = params.checked_add(1).ok_or("Too many function parameters")?;

		match lexer.next_must()? {
			Token::Comma => tok = lexer.next_must()?,
			Token::ParenClose => break,
			tok => Err(format!("Expected ',' or ')', found {tok:?}"))?,
		}
	}

	loop {
		let (tok, trivia) = lexer.next_must_with_trivia()?;
		if tok == Token::End { break; }
		super::parse_stmt(trivia, tok, lexer, &mut closure_scope, &mut closure_state)?;
	}

	// Ensure the function ends with a return.
	if !matches!(closure_state.ops().last(), Some(Op::Ret {.. })) {
		closure_state.emit(&mut closure_scope, Op::empty_ret(), lexer.src_index());
	}

	let upvalues = closure_scope.into_inner(&mut closure_state, lexer)?.upvalues;
	let upvalues = {
		let mut buf = Box::new_uninit_slice(upvalues.len());
		let mut assert_buf = Vec::new();

		for (_, (idx, upval)) in upvalues.into_iter() {
			buf[usize::from(idx)].write(upval);

			if cfg!(debug_assertions) { assert_buf.push(idx); }
		}

		if cfg!(debug_assertions) {
			let initial_len = assert_buf.len();
			assert_buf.sort_unstable();
			assert_buf.dedup();
			assert_eq!(initial_len, assert_buf.len(), "Upvalue indices were not unique");
		}

		// SAFETY: All elements have been initialized.
		unsafe { buf.assume_init() }
	};

	let name = name.map(|i| lexer.resolve_ident(i));
	let debug = closure_state.debug_info.into_debug_info(
		lexer.interner(), name, span, false
	);

	let parsed = ParsedFunction {
		param_count: params,
		operations: closure_state.operations.into_boxed_slice(),
		debug: Some(debug),
		frame_size: closure_state.max_slot_use,
		upvalues,
		exported: None,
	};

	Ok(parsed)
}
