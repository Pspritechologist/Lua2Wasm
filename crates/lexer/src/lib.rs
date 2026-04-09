use logos::{FilterResult, Logos};
use bstr::{ByteSlice, BStr};
use lexical_parse_float::FromLexicalWithOptions;

pub use logos;

const FLOAT_PARSE_FORMAT: u128 = lexical_parse_float::NumberFormatBuilder::new()
	.no_special(true)
	.build_strict();

pub fn parse_num(s: impl AsRef<[u8]>) -> Result<real_float::Finite<f64>, lexical_parse_float::Error> {
	f64::from_lexical_with_options::<FLOAT_PARSE_FORMAT>(s.as_ref(), &Default::default()).map(real_float::Finite::new)
}

pub trait TokPat<'s> {
	fn matches(self, tok: Token<'s>) -> bool;
}

impl<'s> TokPat<'s> for Token<'_> {
	fn matches(self, tok: Token<'s>) -> bool {
		self == tok
	}
}

impl<'s, const LEN: usize> TokPat<'s> for [Token<'s>; LEN] {
	fn matches(self, tok: Token<'s>) -> bool {
		self.contains(&tok)
	}
}

impl<'s> TokPat<'s> for &[Token<'s>] {
	fn matches(self, tok: Token<'s>) -> bool {
		self.contains(&tok)
	}
}

impl<'s, F: FnOnce(Token<'s>) -> bool> TokPat<'s> for F {
	fn matches(self, tok: Token<'s>) -> bool {
		self(tok)
	}
}

pub struct Lexer<'s> {
	inner: logos::Lexer<'s, Token<'s>>,
	#[expect(clippy::type_complexity)]
	peeked: Option<(Token<'s>, std::ops::Range<usize>, Vec<&'s [u8]>)>,
}

impl<'s> Lexer<'s> {
	#[expect(clippy::type_complexity)]
	pub fn next_with_trivia(&mut self) -> Option<Result<(Token<'s>, Vec<&'s [u8]>), LexError<'s>>> {
		if let Some((tok, _, trivia)) = self.peeked.take() {
			return Some(Ok((tok, trivia)));
		}
		
		let mut trivia = Vec::new();
		loop {
			match self.inner.next() {
				Some(Ok(Token::DocComment(comment))) => trivia.push(comment),
				Some(Ok(tok)) => return Some(Ok((tok, trivia))),
				Some(Err(e)) => return Some(Err(e)),
				None => return None,
			}
		}
	}

	pub fn peek(&mut self) -> Result<Option<Token<'s>>, LexError<'s>> {
		if self.peeked.is_none() {
			let span = self.current_span();
			self.peeked = self.next_with_trivia().transpose()?.map(|(tok, trivia)| (tok, span, trivia));
		}

		Ok(self.peeked.as_ref().map(|(tok, _, _)| *tok))
	}

	pub fn next_if(&mut self, pat: impl TokPat<'s>) -> Result<Option<Token<'s>>, LexError<'s>> {
		if let Some(tok) = self.peek()? && pat.matches(tok) {
			self.peeked = None;
			return Ok(Some(tok));
		}

		Ok(None)
	}

	pub fn next_if_map<O>(&mut self, pat: impl FnOnce(Token<'s>) -> Option<O>) -> Result<Option<O>, LexError<'s>> {
		if let Some(tok) = self.peek()? && let Some(out) = pat(tok) {
			self.peeked = None;
			return Ok(Some(out));
		}

		Ok(None)
	}

	pub fn try_next_if_map<O, E>(&mut self, pat: impl FnOnce(Token<'s>) -> Result<Option<O>, E>) -> Result<Option<O>, E>
	where E: From<LexError<'s>> {
		if let Some(tok) = self.peek()? && let Some(out) = pat(tok)? {
			self.peeked = None;
			return Ok(Some(out));
		}

		Ok(None)
	}

	pub fn get_ident<'a: 's>(&mut self, ident: &'a (impl AsRef<str> + ?Sized)) -> IdentKey {
		self.inner.extras.intern_ident(ident.as_ref())
	}
	/// Returns the identifier string associated with the given IdentKey.
	pub fn resolve_ident(&self, key: impl Into<IdentKey>) -> &'s str {
		self.inner.extras.resolve_ident(key.into())
	}

	pub fn current_span(&self) -> std::ops::Range<usize> {
		self.peeked.as_ref().map(|(_, span, _)| span.clone()).unwrap_or_else(|| self.inner.span())
	}

	pub fn src_index(&self) -> usize {
		self.current_span().end
	}

	pub fn current_pos(&self) -> (usize, usize) {
		let span = self.current_span();
		let src = self.inner.source();

		let line = src[..span.start].chars().filter(|&c| c == '\n').count() + 1;
		let col = src[..span.start].rfind(b"\n").map(|pos| span.start - pos).unwrap_or(span.start + 1);

		(line, col)
	}

	pub fn interner(&self) -> &LexInterner<'s> {
		&self.inner.extras
	}

	pub fn into_interner(self) -> LexInterner<'s> {
		self.inner.extras
	}
}

impl<'s> Iterator for Lexer<'s> {
	type Item = Result<Token<'s>, LexError<'s>>;
	fn next(&mut self) -> Option<Self::Item> {
		self.peeked.take()
			.map(|(tok, _, _)| Ok(tok))
			.or_else(|| self.inner.by_ref().find(|t| !matches!(t, Ok(Token::DocComment(_)))))
	}
}

pub fn lexer(src: &[u8]) -> Lexer<'_> {
	Lexer {
		inner: Token::lexer(src),
		peeked: None,
	}
}

slotmap::new_key_type! {
	pub struct IdentKey;
}

#[derive(Debug, Default)]
pub struct LexInterner<'s> {
	idents: slotmap::SlotMap<IdentKey, &'s str>,
	ident_table: hashbrown::HashMap<&'s str, IdentKey>,
}
impl<'s> LexInterner<'s> {
	pub fn resolve_ident(&self, key: IdentKey) -> &'s str {
		self.idents.get(key).expect("Invalid Label key")
	}

	pub fn intern_ident(&mut self, ident: &'s str) -> IdentKey {
		if let Some(&key) = self.ident_table.get(ident) {
			return key;
		}

		let key = self.idents.insert(ident);
		self.ident_table.insert(ident, key);
		key
	}

	unsafe fn handle_ident(&mut self, ident: &'s [u8]) -> IdentKey {
		debug_assert!(ident.is_utf8(), "Identifiers must be valid UTF-8");

		let ident = unsafe { str::from_utf8_unchecked(ident) };
		self.intern_ident(ident)
	}
}

fn handle_comment<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> FilterResult<&'s [u8], LexError<'s>> {
	// We need to determine if it's a block comment or not.
	let blk_comment_filler = lex.slice()
		.strip_prefix(b"--[")
		.and_then(|s| s.strip_suffix(b"["));

	if let Some(equals) = blk_comment_filler {
		// A multi-line block comment, we need special rules to determine the end.
		let end_pat = [b"]", equals, b"]"].concat();
		let Some(end) = lex.remainder().find(&end_pat) else {
			return FilterResult::Error(LexError::UnmatchedBlockComment);
		};

		lex.bump(end + end_pat.len());

		return FilterResult::Skip;
	}

	// A single-line comment, the token rules have already matched the entire thing.
	match lex.slice().strip_prefix(b"---@") {
		Some(contents) => FilterResult::Emit(contents),
		None => FilterResult::Skip,
	}
}

fn handle_string<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> String<'s> {
	let slice = lex.slice();
	let s = &slice[1..slice.len() - 1]; // Remove quotes
	String::new(false, BStr::new(s))
}

fn handle_block_string<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<String<'s>, LexError<'s>> {
	// let equals = lex.slice().matches('=').count();
	let equals = lex.slice().len() - 2; // "[==[".len() == 4
	let end_pat = format!("]{}]", "=".repeat(equals)); //TODO: Highly inefficient.
	let end = lex.remainder().find(&end_pat).ok_or(LexError::UnmatchedMultiLineString)?;

	let str_end = end - end_pat.len();
	let s = &lex.remainder()[..str_end];
	// Multiline-strings ignore the first newline.
	// let s = s.strip_prefix(b"\r\n")
	// 	.or_else(|| s.strip_prefix(b"\n"))
	// 	.or_else(|| s.strip_prefix(b"\r"))
	// 	.unwrap_or(s);
	let mut lines = s.lines();
	let s = if let Some(l) = lines.next() && l.is_empty() {
		lines.as_bytes()
	} else {
		s
	};

	lex.bump(end + end_pat.len());
	
	Ok(String::new(true, BStr::new(s)))
}

#[derive(Debug, Clone, Copy, PartialEq, Logos, strum::IntoStaticStr)]
#[logos(extras = LexInterner<'s>)]
#[logos(skip(r"\s+"))]
#[logos(error(LexError<'s>, |lex| LexError::InvalidToken(BStr::new(lex.slice()))))]
#[logos(utf8 = false)]
pub enum Token<'s> {
	#[regex(r"---@.*", handle_comment, allow_greedy=true)]
	#[regex(r"--.*", handle_comment, allow_greedy=true)]
	#[regex(r"--\[=*\[", handle_comment)]
	DocComment(&'s [u8]),

	#[token(";")] LineTerm,
	#[token("[")] BracketOpen,
	#[token("]")] BracketClose,
	#[token("{")] BraceOpen,
	#[token("}")] BraceClose,
	#[token("(")] ParenOpen,
	#[token(")")] ParenClose,
	#[token(",")] Comma,

	#[token("::")] Label,
	
	#[token("...")] VarArgs,

	#[token("+")] Plus,
	#[token("-")] Minus,
	#[token("*")] Mul,
	#[token("/")] Div,
	#[token("//")] DivFloor,
	#[token("^")] Pow,
	#[token("%")] Mod,

	#[token("&")] BitAnd,
	#[token("|")] BitOr,
	#[token("~")] BitNot,
	#[token("<<")] BitShiftL,
	#[token(">>")] BitShiftR,

	#[token("#")] Len,
	#[token("..")] Concat,

	#[token("=")] Assign,
	#[token(".")] Dot,
	#[token(":")] Colon,

	#[token("function")] Function,
	#[token("local")] Local,
	#[token("global")] Global,
	#[token("return")] Return,

	#[token("and")] And,
	#[token("or")] Or,
	#[token("not")] Not,

	#[token("==")] Equals,
	#[token("~=")] NotEquals,
	#[token("<")] LessThan,
	#[token("<=")] LessThanEquals,
	#[token(">")] GreaterThan,
	#[token(">=")] GreaterThanEquals,

	#[token("do")] Do,
	#[token("end")] End,
	#[token("if")] If,
	#[token("then")] Then,
	#[token("else")] Else,
	#[token("elseif")] ElseIf,
	#[token("while")] While,
	#[token("for")] For,
	#[token("in")] In,
	#[token("repeat")] Repeat,
	#[token("until")] Until,
	#[token("break")] Break,
	#[token("goto")] Goto,

	#[regex(
		r"[-+]?\d+(\.\d*)?([eE][-+]?\d+)?",
		|lex| parse_num(lex.slice()), 
	)]
	Number(real_float::Finite<f64>),

	#[regex(
		r"[\p{XID_Start}_][\p{XID_Continue}_]*",
		// SAFETY: Lexing will only pick up valid UTF-8 sequences for identifiers.
		|lex| unsafe { lex.extras.handle_ident(lex.slice()) },
	)]
	Identifier(IdentKey),

	/// The boolean represents if the string is raw (i.e. block string) or not.
	#[regex(br#""([^"]|\\(s:.))*""#, handle_string)]
	#[regex(br#"'([^']|\\(s:.))*'"#, handle_string)]
	#[regex(r"\[=*\[", handle_block_string)]
	String(String<'s>),

	#[token("true")] True,
	#[token("false")] False,
	#[token("nil")] Nil,
}

/// Represents a Lua string literal from a src file.
/// `raw` is true if it's a block string, false if it's either a single or double-quoted string.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct String<'s> {
	raw: bool,
	data: &'s BStr,
}

/// Possible errors that can occur while parsing a Lua string literal.
#[derive(Debug, Clone, Copy)]
pub enum StringParseError {
	/// An escape sequence that is not recognized by Lua.
	InvalidEscapeSequence(u8),
	/// A hex escape sequence that ended before consuming two hex digits.
	UnexpectedEndOfHexSequence,
	/// A hex escape sequence that contained non-hex characters.
	InvalidHexSequence(u8, u8),
	/// A decimal escape sequence that ended before consuming three digits.
	UnexpectedEndOfDecimalSequence,
	/// A decimal escape sequence that contained values out of the 0-9 range.
	InvalidDecimalSequence(u8, u8, u8),
	/// A Unicode escape sequence that did not start with a '{' after the '\u'.
	PoorlyFormedUnicodeEscape,
	/// A Unicode escape sequence that ended before consuming a closing '}'.
	UnexpectedEndOfUnicodeEscape,
	/// A Unicode escape sequence that contained more than three characters before ending with a '}'.
	UnexpectedContinuationOfUnicodeEscape,
	/// A Unicode escape sequence that contained an invalid byte.
	InvalidCharUnicodeEscape(u8),
	/// A Unicode escape sequence that resolves to a code point that is not valid in Unicode.
	InvalidUnicodeCodepoint(u32),
	/// This error should never occur in practice as an unescaped newline would not be
	/// parsed as a string. Non-raw Lua strings cannot contain new lines.
	UnescapedNewline,
	/// A backslash at the end of the string with no character following it.\
	/// As with [`Self::UnescapedNewline`], this error should never occur in practice as a
	/// trailing backslash would cause the string to fail to lex in the first place.
	TrailingBackslash,
}

impl std::error::Error for StringParseError { }
impl std::fmt::Display for StringParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use StringParseError::*;

		match *self {
			InvalidEscapeSequence(c) => write!(f, "Invalid escape sequence: \\{}", c as char),
			UnexpectedEndOfHexSequence => write!(f, "Unexpected end of string during hex escape"),
			InvalidHexSequence(hi, lo) => write!(f, "Invalid hex escape sequence: \\x{}{}", hi as char, lo as char),
			UnexpectedEndOfDecimalSequence => write!(f, "Unexpected end of string during decimal escape"),
			InvalidDecimalSequence(a, b, c) => write!(f, "Invalid decimal escape sequence: \\{}{}{}", a as char, b as char, c as char),
			UnescapedNewline => write!(f, "Unescaped newline in string"),
			PoorlyFormedUnicodeEscape => write!(f, "Expected '{{' after \\u in Unicode escape"),
			UnexpectedEndOfUnicodeEscape => write!(f, "Unexpected end of string during Unicode escape"),
			UnexpectedContinuationOfUnicodeEscape => write!(f, "Expected end of Unicode escape sequence after 3 digits"),
			InvalidCharUnicodeEscape(c) => write!(f, "Invalid character '{}' in Unicode escape", c as char),
			InvalidUnicodeCodepoint(c) => write!(f, "Invalid Unicode code point: U+{c:X}"),
			TrailingBackslash => write!(f, "Trailing backslash in string"),
		}
	}
}

impl<'s> String<'s> {
	pub fn new(raw: bool, data: &'s BStr) -> Self {
		Self { raw, data }
	}
	pub fn raw(data: &'s BStr) -> Self {
		Self::new(true, data)
	}

	pub fn is_raw(&self) -> bool {
		self.raw
	}

	pub fn data(&self) -> &'s BStr {
		self.data
	}

	pub fn parse_data(&self) -> Result<std::borrow::Cow<'s, BStr>, StringParseError> {
		use StringParseError::*;

		if self.raw || !self.data.contains(&b'\\') {
			return Ok(self.data.into());
		}

		let mut buf = bstr::BString::from(Vec::with_capacity(self.data.len()));
		let mut chars = self.data.iter().cloned().peekable();

		//TODO:
		// Technically I think I'm meant to replace any newline-style char within long-form strings with raw newlines?
		// Seems like a lot of work...

		while let Some(c) = chars.next() {
			if c == b'\\' {
				// It's technically impossible for a non-raw string to end with a single backslash.
				match chars.next().ok_or(TrailingBackslash)? {
					b'a' => buf.push(b'\x07'), // bell.
					b'b' => buf.push(b'\x08'), // back space.
					b'f' => buf.push(b'\x0b'), // form feed.
					b'n' => buf.push(b'\n'),   // newline.
					b'r' => buf.push(b'\r'),   // carriage return.
					b't' => buf.push(b'\t'),   // horizontal tab.
					b'v' => buf.push(b'\x0b'), // vertical tab.
					b'\\' => buf.push(b'\\'),  // backslash.
					b'"' => buf.push(b'"'),    // double quote.
					b'\'' => buf.push(b'\''),  // single quote.
					b'\n' => buf.push(b'\n'),  // line continuation ('Short strings' can span multiple lines if each line ends with a backslash).
					b'z' => while chars.next_if(|c| c.is_ascii_whitespace()).is_some() { }, // skip following whitespace.
					b'x' => { // hex byte sequences.
						let hi = chars.next().ok_or(UnexpectedEndOfHexSequence)?.to_ascii_lowercase();
						let lo = chars.next().ok_or(UnexpectedEndOfHexSequence)?.to_ascii_lowercase();
						if !hi.is_ascii_hexdigit() || !lo.is_ascii_hexdigit() {
							return Err(InvalidHexSequence(hi, lo));
						}

						let hi = hi - if hi.is_ascii_digit() { b'0' } else { b'a' - 10 };
						let lo = lo - if lo.is_ascii_digit() { b'0' } else { b'a' - 10 };
						let byte = hi * 16 + lo;

						buf.push(byte);
					},
					digit if digit.is_ascii_digit() => { // decimal byte sequences.
						let (a, b, c) = (
							digit,
							chars.next().ok_or(UnexpectedEndOfDecimalSequence)?,
							chars.next().ok_or(UnexpectedEndOfDecimalSequence)?,
						);

						if !b.is_ascii_digit() || !c.is_ascii_digit() {
							return Err(InvalidDecimalSequence(a, b, c));
						}

						let a = a - b'0';
						let b = b - b'0';
						let c = c - b'0';
						let byte = a * 100 + b * 10 + c;

						buf.push(byte);
					},
					b'u' => { // Unicode code points.
						if chars.next() != Some(b'{') {
							return Err(PoorlyFormedUnicodeEscape);
						}

						let mut codepoint = 0u32;

						let mut next_digit = || {
							let c = chars.next().ok_or(UnexpectedEndOfUnicodeEscape)?.to_ascii_lowercase();
							if c == b'}' {
								return Ok(false);
							}

							if !c.is_ascii_hexdigit() {
								return Err(InvalidCharUnicodeEscape(c));
							}

							let digit = (c - if c.is_ascii_digit() { b'0' } else { b'a' - 10 }) as u32;
							codepoint = codepoint * 16 + digit;

							Ok(true)
						};

						// A limit of three hex digits.
						if next_digit()? && next_digit()? && next_digit()? {
							return Err(UnexpectedContinuationOfUnicodeEscape);
						}

						// Lua allows codepoints less than 2^31
						if codepoint >= 2^31 {
							return Err(InvalidUnicodeCodepoint(codepoint));
						}

						let ch = char::from_u32(codepoint).ok_or(InvalidUnicodeCodepoint(codepoint))?;

						buf.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
					},
					other => return Err(InvalidEscapeSequence(other)),
				}
			} else {
				if c == b'\n' {
					return Err(UnescapedNewline);
				}

				buf.push(c);
			}
		}

		Ok(buf.into())
	}
}

impl std::fmt::Display for Token<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::Number(val) => write!(f, "Number({})", val.val()),
			Token::Identifier(name) => write!(f, "Identifier({name:?})"),
			Token::String(String { data, .. }) => write!(f, "String('{data}')"),
			_ => write!(f, "{}", self.tok_type()),
		}
	}
}

impl Token<'_> {
	fn tok_type(&self) -> &'static str {
		self.into()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum LexError<'src> {
	NumberParsing(lexical_parse_float::Error),
	UnmatchedBlockComment,
	UnmatchedMultiLineString,
	InvalidToken(&'src BStr),
	#[default]
	Unknown,
}

impl std::error::Error for LexError<'_> { }

impl From<lexical_parse_float::Error> for LexError<'_> {
	fn from(e: lexical_parse_float::Error) -> Self {
		LexError::NumberParsing(e)
	}
}

impl std::fmt::Display for LexError<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LexError::NumberParsing(e) => write!(f, "Error parsing num literal: {e}"),
			LexError::UnmatchedBlockComment => write!(f, "Unmatched block comment"),
			LexError::UnmatchedMultiLineString => write!(f, "Unmatched multi-line string"),
			LexError::InvalidToken(tok) => write!(f, "Invalid token: {tok}"),
			LexError::Unknown => write!(f, "Unknown lexing error"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_lexer_basic() {
		let src = b"
			-- This is a comment
			local x = 42;
			local str = \"Hello, World!\";
			::my_label::
			if x >= 10 then
				x = x + 1;
			end
		";

		let mut lexer = lexer(src);

		let expected_tokens = vec![
			Token::Local,
			Token::Identifier(lexer.get_ident("x")),
			Token::Assign,
			Token::Number(real_float::Finite::new(42.0)),
			Token::LineTerm,
			Token::Local,
			Token::Identifier(lexer.get_ident("str")),
			Token::Assign,
			Token::String(String::new(false, BStr::new(b"Hello, World!"))),
			Token::LineTerm,
			Token::Label,
			Token::Identifier(lexer.get_ident("my_label")),
			Token::Label,
			Token::If,
			Token::Identifier(lexer.get_ident("x")),
			Token::GreaterThanEquals,
			Token::Number(real_float::Finite::new(10.0)),
			Token::Then,
			Token::Identifier(lexer.get_ident("x")),
			Token::Assign,
			Token::Identifier(lexer.get_ident("x")),
			Token::Plus,
			Token::Number(real_float::Finite::new(1.0)),
			Token::LineTerm,
			Token::End,
		];

		for (i, expected) in expected_tokens.into_iter().enumerate() {
			let token = lexer.next().unwrap().unwrap();
			assert_eq!(token, expected, "Expected {expected:?}, got {token:?} at {i}");
		}

		assert!(lexer.next().is_none());
	}

	#[test]
	fn test_lexer_non_utf8() {
		let src = b"
			local s = \"Hello, \xFF World!\";
		";

		let mut lexer = lexer(src);

		let expected_tokens = vec![
			Token::Local,
			Token::Identifier(lexer.get_ident("s")),
			Token::Assign,
			Token::String(String::new(false, BStr::new(b"Hello, \xFF World!"))),
			Token::LineTerm,
		];

		for expected in expected_tokens {
			let token = lexer.next().unwrap().unwrap();
			assert_eq!(token, expected);
		}

		assert!(lexer.next().is_none());
	}

	#[test]
	fn test_lexer_non_utf8_identifier() {
		let src = b"
			local var_\xFF_ = 100;
		";

		let mut lexer = lexer(src);

		let expected_tokens = vec![
			Token::Local,
			Token::Identifier(lexer.get_ident("var_")),
			// This is where the rest of the above identifier is, but it's invalid UTF-8.
			// The broken ident will leave some rubbish tokens following.
			Token::Identifier(lexer.get_ident("_")),
			//FIXME: I've no idea why this is happening, but it doesn't matter for now.
			Token::Identifier(lexer.get_ident("_")),
			Token::Assign,
			Token::Number(real_float::Finite::new(100.0)),
			Token::LineTerm,
		];

		for expected in expected_tokens {
			match lexer.next().unwrap() {
				Ok(tok) => assert_eq!(tok, expected),
				Err(e) => assert_eq!(e, LexError::InvalidToken(BStr::new(b"\xFF"))),
			}
		}

		assert!(lexer.next().is_none());
	}
}
