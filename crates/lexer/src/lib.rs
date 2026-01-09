use logos::Logos;

pub use logos;

pub trait TokPat<'s> {
	fn matches(self, tok: Token<'s>) -> bool;
}

impl<'s> TokPat<'s> for Token<'_> {
	fn matches(self, tok: Token<'s>) -> bool {
		self == tok
	}
}

impl<'s, F: FnOnce(Token<'s>) -> bool> TokPat<'s> for F {
	fn matches(self, tok: Token<'s>) -> bool {
		self(tok)
	}
}

pub struct Lexer<'s> {
	inner: logos::Lexer<'s, Token<'s>>,
	peeked: Option<Token<'s>>,
}

impl<'s> Lexer<'s> {
	pub fn peek(&mut self) -> Result<Option<Token<'s>>, LexError<'s>> {
		if self.peeked.is_none() {
			self.peeked = self.inner.next().transpose()?;
		}

		Ok(self.peeked)
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

	// Returns the identifier string associated with the given IdentKey.
	pub fn resolve_ident(&self, key: impl Into<IdentKey>) -> &'s str {
		self.inner.extras.resolve_ident(key.into())
	}

	/// Returns an new IdentKey not associated with any identifier.
	pub fn new_key(&mut self) -> IdentKey {
		self.inner.extras.idents.insert("")
	}
}

impl<'s> Iterator for Lexer<'s> {
	type Item = Result<Token<'s>, LexError<'s>>;
	fn next(&mut self) -> Option<Self::Item> {
		self.peeked.take()
			.map(Ok)
			.or_else(|| self.inner.next())
	}
}

pub fn lexer(src: &str) -> Lexer<'_> {
	Lexer {
		inner: Token::lexer(src),
		peeked: None,
	}
}

slotmap::new_key_type! {
	pub struct IdentKey;
}

#[derive(Default)]
pub struct LexExtras<'s> {
	idents: slotmap::SlotMap<IdentKey, &'s str>,
	ident_table: hashbrown::HashMap<&'s str, IdentKey>,
}
impl<'s> LexExtras<'s> {
	pub fn resolve_ident(&self, key: IdentKey) -> &'s str {
		self.idents.get(key).expect("Invalid Label key")
	}

	fn handle_ident(&mut self, ident: &'s str) -> IdentKey {
		if let Some(&key) = self.ident_table.get(ident) {
			return key;
		}

		let key = self.idents.insert(ident);
		self.ident_table.insert(ident, key);
		key
	}
}

fn handle_comment<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<logos::Skip, LexError<'s>> {
	if !lex.slice().starts_with("--[") {
		// Single-line comment
		let end = lex.remainder().find('\n').unwrap_or(lex.remainder().len());
		lex.bump(end);
		return Ok(logos::Skip);
	}

	// let equals = lex.slice().matches('=').count();
	let equals = lex.slice().len() - 4; // "--[==[".len() == 6
	let end_pat = format!("]{}]", "=".repeat(equals));
	let end = lex.remainder().find(&end_pat).ok_or(LexError::UnmatchedBlockComment)?;
	lex.bump(end + end_pat.len());

	Ok(logos::Skip)
}

fn handle_block_string<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<&'s str, LexError<'s>> {
	// let equals = lex.slice().matches('=').count();
	let equals = lex.slice().len() - 2; // "[==[".len() == 4
	let end_pat = format!("]{}]", "=".repeat(equals));
	let end = lex.remainder().find(&end_pat).ok_or(LexError::UnmatchedMultiLineString)?;

	let str_end = end - end_pat.len();
	let s = &lex.remainder()[..str_end];
	// Multiline-strings ignore the first newline.
	let s = s.strip_prefix('\n').unwrap_or(s);

	lex.bump(end + end_pat.len());
	Ok(s)
}

#[derive(Debug, Clone, Copy, PartialEq, Logos, strum::IntoStaticStr)]
#[logos(extras = LexExtras<'s>)]
#[logos(skip(r"\s+"))]
#[logos(skip(r"--(\[(=*)\[)?", handle_comment))]
// #[logos(skip(r"--\[=*\[", handle_block_comment))]
// #[logos(skip(r"--[^\n]*"))]
#[logos(error(LexError<'s>, |lex| LexError::InvalidToken(lex.slice())))]
pub enum Token<'s> {
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
	#[token("break")] Break,
	#[token("goto")] Goto,

	#[regex(
		r"[-+]?\d+(\.\d*)?([eE][-+]?\d+)?",
		|lex| lex.slice().parse::<f64>().map(real_float::Finite::new)
	)]
	Number(real_float::Finite<f64>),

	#[regex(
		r"[\p{XID_Start}_][\p{XID_Continue}_]*",
		|lex| lex.extras.handle_ident(lex.slice())
	)]
	Identifier(IdentKey),

	#[regex(r#""([^"]|\.)*""#, |lex| &lex.slice()[1..lex.slice().len() - 1])]
	#[regex(r"'([^']|\.)*'", |lex| &lex.slice()[1..lex.slice().len() - 1])]
	String(&'s str),
	#[regex(r"\[=*\[", handle_block_string)]
	RawString(&'s str),

	#[token("true")] True,
	#[token("false")] False,
	#[token("nil")] Nil,
}

impl std::fmt::Display for Token<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::Number(val) => write!(f, "Number({})", val.val()),
			Token::Identifier(name) => write!(f, "Identifier({name:?})"),
			Token::String(s) => write!(f, "String('{s}')"),
			Token::RawString(s) => write!(f, "MlString([[{s}]])"),
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
	NumberInvalid,
	UnmatchedBlockComment,
	UnmatchedMultiLineString,
	InvalidToken(&'src str),
	#[default]
	Unknown,
}

impl std::error::Error for LexError<'_> { }

impl From<std::num::ParseFloatError> for LexError<'_> {
	fn from(_: std::num::ParseFloatError) -> Self {
		LexError::NumberInvalid
	}
}

impl std::fmt::Display for LexError<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LexError::NumberInvalid => write!(f, "Invalid float literal"),
			LexError::UnmatchedBlockComment => write!(f, "Unmatched block comment"),
			LexError::UnmatchedMultiLineString => write!(f, "Unmatched multi-line string"),
			LexError::InvalidToken(tok) => write!(f, "Invalid token: {tok}"),
			LexError::Unknown => write!(f, "Unknown lexing error"),
		}
	}
}
