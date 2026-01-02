use logos::Logos;

pub use logos;

pub type Lexer<'src> = logos::Lexer<'src, Token>;

pub fn lexer(src: &str) -> Lexer<'_> {
	Token::lexer(src)
}

fn handle_block_comment<'s>(lex: &mut logos::Lexer<'s, Token>) -> Result<logos::Skip, LexError<'s>> {
	let equals = lex.slice().chars().filter(|&c| c == '=').count();
	let end_pat = format!("]{}]", "=".repeat(equals));
	let end = dbg!(lex.remainder()).find(&end_pat).ok_or(LexError::UnmatchedBlockComment)?;
	lex.bump(end + end_pat.len());

	Ok(logos::Skip)
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

#[derive(Debug, Clone, Copy, PartialEq, Logos, strum::IntoStaticStr)]
#[logos(extras = LexExtras<'s>)]
#[logos(skip(r"\s+"))]
#[logos(skip(r"--\[=*\[", handle_block_comment))]
#[logos(skip(r"--[^\n]*"))]
#[logos(error(LexError<'s>, |lex| LexError::InvalidToken(lex.slice())))]
pub enum Token {
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
	#[token("*")] Mult,
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
	#[token("else")] Else,
	#[token("while")] While,
	#[token("for")] For,
	#[token("break")] Break,
	#[token("goto")] Goto,

	#[regex(r"[-+]?\d+(\.\d*)?([eE][-+]?\d+)?", |lex| lex.slice().parse())]
	Number(f64),

	#[regex(r"[\p{XID_Start}_][\p{XID_Continue}_]*", |lex| {
		let ident = lex.slice();
		lex.extras.handle_ident(ident)
	})]
	Identifier(IdentKey),

	#[token("true")] True,
	#[token("false")] False,
	#[token("nil")] Nil,
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::Number(val) => write!(f, "Number({})", val),
			Token::Identifier(name) => write!(f, "Identifier({:?})", name),
			_ => write!(f, "{}", self.tok_type()),
		}
	}
}

impl Token {
	fn tok_type(&self) -> &'static str {
		self.into()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum LexError<'src> {
	NumberInvalid,
	UnmatchedBlockComment,
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
			LexError::InvalidToken(tok) => write!(f, "Invalid token: {tok}"),
			LexError::Unknown => write!(f, "Unknown lexing error"),
		}
	}
}
