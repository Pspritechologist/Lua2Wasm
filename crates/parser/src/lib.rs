use bstr::BStr;
use chumsky::{extra::SimpleState, inspector::Inspector, prelude::*};
use luant_lexer::{Token, IdentKey};
use real_float::Finite;

type Span = std::ops::Range<usize>;
type Error<'s> = Rich<'s, Token<'s>, Span>;
type State<'s> = SimpleState<ParseState<'s>>;
type Context = ();
type Extra<'s, S = State<'s>> = chumsky::extra::Full<Error<'s>, S, Context>;

// Some trait aliases.
trait ValueInput<'s>: chumsky::input::ValueInput<'s, Token = Token<'s>, Span = Span> { }
impl<'s, T: chumsky::input::ValueInput<'s, Token = Token<'s>, Span = Span>> ValueInput<'s> for T { }

trait LuantParser<'s, I: ValueInput<'s>, O, S: Inspector<'s, I> = State<'s>>: Clone + Parser<'s, I, O, Extra<'s, S>> { }
impl<'s, I: ValueInput<'s>, O, P: Clone + Parser<'s, I, O, Extra<'s>>> LuantParser<'s, I, O> for P { }

trait MapExtraExt<'s> {
	fn parse_state(&mut self) -> &mut ParseState<'s>;
	fn expr(&mut self, expr: impl Into<Expression<'s>>) -> ExprIdx;
	fn resolve(&mut self, key: IdentKey) -> &'s str;
}
impl<'s, I: ValueInput<'s>> MapExtraExt<'s> for chumsky::input::MapExtra<'s, '_, I, Extra<'s>> {
	fn parse_state(&mut self) -> &mut ParseState<'s> {
		&mut *self.state()
	}
	fn expr(&mut self, expr: impl Into<Expression<'s>>) -> ExprIdx {
		self.parse_state().expr(expr)
	}
	fn resolve(&mut self, key: IdentKey) -> &'s str {
		self.parse_state().resolve(key)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprIdx(usize);
impl ExprIdx {
	pub fn as_usize(self) -> usize {
		self.0
	}
	
	pub const NIL: Self = Self(0);
	pub const TRUE: Self = Self(1);
	pub const FALSE: Self = Self(2);
	pub const VAR_ARGS: Self = Self(3);
}

#[derive(Debug, Default)]
struct ParseState<'s> {
	expressions: Vec<Expression<'s>>,
	interner: luant_lexer::LexInterner<'s>,
}
impl<'s> ParseState<'s> {
	fn new(interner: luant_lexer::LexInterner<'s>) -> Self {
		Self {
			expressions: vec![
				Expression::Nil,
				Expression::Boolean(true),
				Expression::Boolean(false),
				Expression::VarArgs,
			],
			interner,
		}
	}

	fn expr(&mut self, expr: impl Into<Expression<'s>>) -> ExprIdx {
		let len = self.expressions.len();
		self.expressions.push(expr.into());
		ExprIdx(len)
	}

	fn resolve(&self, key: IdentKey) -> &'s str {
		self.interner.resolve_ident(key)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
	Or, And,
	Add, Sub, Mul, Div, IntDiv, Pow, Mod,
	BitAnd, BitXor, BitOr, ShiftRight, ShiftLeft,
	Concat,
	Less, LessEq, Greater, GreaterEq,
	Equal, NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
	Neg, Not, Len, BitNot,
}

#[derive(Debug, Clone)]
pub struct Parsed<'s> {
	pub expressions: Box<[Expression<'s>]>,
	pub statements: Box<[Statement]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Assignment {
		targets: Box<[ExprIdx]>,
		values: Box<[ExprIdx]>,
	},
	FunctionCall(FunctionCall),
	Label(IdentKey),
	Break,
	Goto(IdentKey),
	Do(Box<[Statement]>),
	While {
		condition: ExprIdx,
		body: Box<[Statement]>,
	},
	Repeat {
		body: Box<[Statement]>,
		condition: ExprIdx,
	},
	If {
		condition: ExprIdx,
		then_branch: Box<[Statement]>,
		else_branch: Option<ElseBranch>,
	},
	For {
		ident: IdentKey,
		start: ExprIdx,
		end: ExprIdx,
		step: Option<ExprIdx>,
		body: Box<[Statement]>,
	},
	GenericFor {
		idents: Box<[IdentKey]>,
		args: Box<[ExprIdx]>,
		body: Box<[Statement]>,
	},
	FunctionDecl {
		destination: ExprIdx,
		method_name: Option<IdentKey>,
		func: FunctionDef,
	},
	VarFunctionDecl {
		global: bool,
		name: IdentKey,
		func: FunctionDef,
	},
	VarDecl {
		global: bool,
		attrib: Option<IdentKey>,
		idents: Box<[(IdentKey, Option<IdentKey>)]>,
		values: Box<[ExprIdx]>,
	},
	GlobalSet(Option<IdentKey>),
	Return(Box<[ExprIdx]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElseBranch {
	pub body: Box<[Statement]>,
	pub next: Option<(ExprIdx, Option<Box<ElseBranch>>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDef {
	pub params: Box<[IdentKey]>,
	pub var_arg: Option<Option<IdentKey>>,
	pub body: Box<[Statement]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
	pub target: ExprIdx,
	pub method_name: Option<IdentKey>,
	pub args: Box<[ExprIdx]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'s> {
	Nil,
	Boolean(bool),
	Number(Finite<f64>),
	String(&'s BStr),
	VarArgs,
	Closure(FunctionDef),
	Ident(IdentKey),
	Index(ExprIdx, ExprIdx),
	Call(FunctionCall),
	Table(Box<[(ExprIdx, ExprIdx)]>),
	BinaryOp(ExprIdx, BinaryOp, ExprIdx),
	UnaryOp(UnaryOp, ExprIdx),
}
impl<'s> From<()> for Expression<'s> {
	fn from((): ()) -> Self { Self::Nil }
}
impl<'s> From<bool> for Expression<'s> {
	fn from(b: bool) -> Self { Self::Boolean(b) }
}
impl<'s> From<f64> for Expression<'s> {
	fn from(n: f64) -> Self { Self::Number(Finite::new(n)) }
}
impl<'s> From<Finite<f64>> for Expression<'s> {
	fn from(n: Finite<f64>) -> Self { Self::Number(n) }
}
impl<'s> From<&'s str> for Expression<'s> {
	fn from(s: &'s str) -> Self { Self::String(BStr::new(s)) }
}
impl<'s> From<&'s BStr> for Expression<'s> {
	fn from(s: &'s BStr) -> Self { Self::String(s) }
}
impl<'s> From<FunctionDef> for Expression<'s> {
	fn from(f: FunctionDef) -> Self { Self::Closure(f) }
}
impl<'s> From<IdentKey> for Expression<'s> {
	fn from(id: IdentKey) -> Self { Self::Ident(id) }
}
impl<'s> From<(ExprIdx, ExprIdx)> for Expression<'s> {
	fn from((target, index): (ExprIdx, ExprIdx)) -> Self { Self::Index(target, index) }
}
impl<'s> From<FunctionCall> for Expression<'s> {
	fn from(call: FunctionCall) -> Self { Self::Call(call) }
}
impl<'s> From<(ExprIdx, BinaryOp, ExprIdx)> for Expression<'s> {
	fn from((left, op, right): (ExprIdx, BinaryOp, ExprIdx)) -> Self {
		Self::BinaryOp(left, op, right)
	}
}
impl<'s> From<(UnaryOp, ExprIdx)> for Expression<'s> {
	fn from((op, expr): (UnaryOp, ExprIdx)) -> Self {
		Self::UnaryOp(op, expr)
	}
}

pub fn parse<'s>(src: &'s [u8]) -> Result<Parsed<'s>, Vec<Error<'s>>> {
	let mut lexer = luant_lexer::lexer(src);
	let tokens = {
		let mut tokens = Vec::new();
		while let Some(tok) = lexer.next().transpose().unwrap() {
			let span = lexer.current_span();
			tokens.push((tok, span));
		}
		tokens
	};

	let input = chumsky::input::IterInput::new(tokens.into_iter(), src.len()..src.len());

	let interner = lexer.into_interner();

	let mut state = ParseState::new(interner).into();
	let statements = program().parse_with_state(input, &mut state).into_result()?;

	let expressions = state.0.expressions.into_boxed_slice();

	Ok(Parsed {
		expressions,
		statements,
	})
}

fn program<'s, I: ValueInput<'s>>() -> impl LuantParser<'s, I, Box<[Statement]>> {
	let expr = expression();
	block(statement(expr.clone()), expr)
		.then_ignore(end())
		.labelled("program")
}

fn ident<'s, I: ValueInput<'s>>() -> impl LuantParser<'s, I, IdentKey> {
	select! { Token::Identifier(name) => name }.labelled("identifier")
}

fn statement<'s, I: ValueInput<'s>>(expr: impl LuantParser<'s, I, ExprIdx> + 's) -> impl LuantParser<'s, I, Statement> {
	let exp_list = expr.clone()
		.separated_by(just(Token::Comma))
		.collect::<Vec<_>>()
		.labelled("expression list");

	let var_expr = recursive(|rec_var_expr| {
		let prefix_exp = prefix_expr(expr.clone(), rec_var_expr.clone());
		var_expr(expr.clone(), prefix_exp)
	});

	let prefix_expr = prefix_expr(expr.clone(), var_expr.clone());

	let assignment = var_expr.separated_by(just(Token::Comma))
		.collect::<Vec<_>>()
		.then_ignore(just(Token::Assign))
		.then(exp_list.clone())
		.map(|(targets, values)| Statement::Assignment {
			targets: targets.into_boxed_slice(),
			values: values.into_boxed_slice(),
		})
		.labelled("assignment");

	let function_call = prefix_expr
		.then(call_args(expr.clone()))
		.map(|(target, (method_name, args))| {
			Statement::FunctionCall(FunctionCall {
				target,
				method_name,
				args,
			})
		})
		.labelled("function call");

	let label = ident()
		.delimited_by(just(Token::Label), just(Token::Label))
		.map(Statement::Label)
		.labelled("label");

	let break_stmt = just(Token::Break)
		.to(Statement::Break)
		.labelled("break");

	let goto = just(Token::Goto)
		.ignore_then(ident())
		.map(Statement::Goto)
		.labelled("goto");

	recursive(|stmt| {
		let block = block(stmt.clone(), expr.clone());

		let do_block = just(Token::Do)
			.ignore_then(block.clone())
			.then_ignore(just(Token::End))
			.map(Statement::Do)
			.labelled("do block");

		let while_loop = just(Token::While)
			.ignore_then(expr.clone())
			.then_ignore(just(Token::Do))
			.then(block.clone())
			.then_ignore(just(Token::End))
			.map(|(condition, body)| Statement::While { condition, body })
			.labelled("while loop");

		let repeat_loop = just(Token::Repeat)
			.ignore_then(block.clone())
			.then_ignore(just(Token::Until))
			.then(expr.clone())
			.map(|(body, condition)| Statement::Repeat { body, condition })
			.labelled("repeat loop");

		let if_stmt = {
			let else_branch = recursive(|else_branch| {
				let elseif_branch = just(Token::ElseIf)
					.ignore_then(expr.clone())
					.then_ignore(just(Token::Then))
					.then(block.clone())
					.then(else_branch)
					.map(|((cond, body), next): (_, Option<ElseBranch>)| ElseBranch {
						body,
						next: Some((cond, next.map(Box::new))),
					})
					.labelled("elseif branch");

				let else_only = just(Token::Else)
					.ignore_then(block.clone())
					.then_ignore(just(Token::End))
					.map(|body| ElseBranch { body, next: None })
					.labelled("else branch");

				choice((
					elseif_branch.map(Some),
					else_only.map(Some),
					just(Token::End).to(None),
				))
			});

			just(Token::If)
				.ignore_then(expr.clone())
				.then_ignore(just(Token::Then))
				.then(block.clone())
				.then(else_branch)
				.map(|((condition, then_branch), else_branch)| Statement::If {
					condition,
					then_branch,
					else_branch,
				})
				.labelled("if statement")
			};

		let numeric_for = just(Token::For)
			.ignore_then(ident())
			.then_ignore(just(Token::Assign))
			.then(expr.clone().then_ignore(just(Token::Comma)))
			.then(expr.clone())
			.then(just(Token::Comma).ignore_then(expr.clone()).or_not())
			.then_ignore(just(Token::Do))
			.then(block.clone())
			.then_ignore(just(Token::End))
			.map(|((((ident, start), end), step), body)| Statement::For {
				ident, start, end, step, body,
			});
		
		let generic_for = just(Token::For)
			.ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
			.then_ignore(just(Token::In))
			.then(exp_list.clone())
			.then_ignore(just(Token::Do))
			.then(block.clone())
			.then_ignore(just(Token::End))
			.map(|((idents, args), body)| Statement::GenericFor {
				idents: idents.into_boxed_slice(),
				args: args.into_boxed_slice(),
				body,
			});

		let function_decl = {
			let func_name = ident().map_with(|i, e| e.expr(i))
				.foldl_with(just(Token::Dot).ignore_then(ident()).repeated(), |base, field, e| {
					let field = e.resolve(field);
					let field = e.expr(field);
					e.expr((base, field))
				})
				.then(just(Token::Colon).ignore_then(ident()).or_not())
				.labelled("function name");

			just(Token::Function)
				.ignore_then(func_name)
				.then(function_def(stmt.clone(), expr.clone()))
				.map(|((base, method_name), func)| {
					Statement::FunctionDecl {
						destination: base,
						method_name,
						func,
					}
				})
				.labelled("function declaration")
		};

		let var_func_decl = just(Token::Local)
			.to(false)
			.or(just(Token::Global).to(true))
			.then_ignore(just(Token::Function))
			.then(ident())
			.then(function_def(stmt.clone(), expr.clone()))
			.map(|((global, name), func)| Statement::VarFunctionDecl {
				global, name, func,
			})
			.labelled("function declaration");
		
		let attr = ident()
			.delimited_by(just(Token::LessThan), just(Token::GreaterThan))
			.labelled("attribute");

		let attr_list = attr.clone().or_not()
			.then(ident()
				.then(attr.clone().or_not())
				.separated_by(just(Token::Comma))
				.collect::<Vec<_>>())
			.labelled("variable list");
		
		let var_decl = just(Token::Local)
			.to(false)
			.or(just(Token::Global).to(true))
			.then(attr_list.clone())
			.then(just(Token::Assign)
				.ignore_then(exp_list.clone())
				.or_not())
			.map(|((global, (attrib, var_attrs)), values)| {
				Statement::VarDecl {
					global,
					attrib,
					idents: var_attrs.into_boxed_slice(),
					values: values.unwrap_or_default().into_boxed_slice(),
				}
			});

		let global_set = just(Token::Global)
			.ignore_then(attr.clone().or_not())
			.map(Statement::GlobalSet)
			.labelled("global set");

		choice((
			assignment.boxed(),
			function_call.boxed(),
			label.boxed(),
			break_stmt.boxed(),
			goto.boxed(),
			do_block.boxed(),
			while_loop.boxed(),
			repeat_loop.boxed(),
			if_stmt.boxed(),
			numeric_for.boxed(),
			generic_for.boxed(),
			function_decl.boxed(),
			var_func_decl.boxed(),
			var_decl.boxed(),
			global_set.boxed(),
		))
	})
}

fn block<'s, I: ValueInput<'s>>(stmt: impl LuantParser<'s, I, Statement>, expr: impl LuantParser<'s, I, ExprIdx>) -> impl LuantParser<'s, I, Box<[Statement]>> {
	let exp_list = expr.separated_by(just(Token::Comma))
		.collect::<Vec<_>>()
		.map(|r| r.into_boxed_slice())
		.labelled("expression list");
	let ret = just(Token::Return)
		.ignore_then(exp_list.or_not())
		.map(|r| Statement::Return(r.unwrap_or_default()))
		.then_ignore(just(Token::LineTerm).or_not())
		.labelled("return statement");
	
	stmt
		.padded_by(just(Token::LineTerm).repeated())
		.repeated()
		.collect::<Vec<_>>()
		.then(ret.or_not())
		.map(|(mut stmt, ret)| {
			if let Some(ret) = ret { stmt.push(ret); }
			stmt.into_boxed_slice()
		})
		.labelled("block")
}

fn expression<'s, I: ValueInput<'s>>() -> impl LuantParser<'s, I, ExprIdx> {
	recursive(|expr| {
		let lits = select! {
			// These four have fixed indices for efficiency.
			Token::Nil => ExprIdx(0),
			Token::True => ExprIdx(1),
			Token::False => ExprIdx(2),
			Token::VarArgs => ExprIdx(3),

			Token::Number(n) = e => e.parse_state().expr(n),
			Token::String(s) = e => e.parse_state().expr(s),
		};

		let prefix_exp = recursive(|prefix_exp| {
			let var_expr = var_expr(expr.clone(), prefix_exp.clone());
			prefix_expr(expr.clone(), var_expr)
		});

		let atoms = choice((
			lits,
			table_constructor(expr.clone()),
			just(Token::Function)
				.ignore_then(function_def(statement(expr.clone()), expr))
				.map_with(|f, e| e.parse_state().expr(f)),
			prefix_exp,
		));

		let bin = |tok| just(tok).labelled("binary operator");
		let un = |tok| just(tok).labelled("unary operator");

		atoms.pratt({
			use chumsky::pratt::{infix, prefix, left, right};
			(
				infix(left(1), bin(Token::Or), |l, _, r, e| e.expr((l, BinaryOp::Or, r))),
				infix(left(2), bin(Token::And), |l, _, r, e| e.expr((l, BinaryOp::And, r))),

				infix(left(3), bin(Token::Equals), |l, _, r, e| e.expr((l, BinaryOp::Equal, r))),
				infix(left(3), bin(Token::NotEquals), |l, _, r, e| e.expr((l, BinaryOp::NotEqual, r))),
				infix(left(3), bin(Token::LessThan), |l, _, r, e| e.expr((l, BinaryOp::Less, r))),
				infix(left(3), bin(Token::LessThanEquals), |l, _, r, e| e.expr((l, BinaryOp::LessEq, r))),
				infix(left(3), bin(Token::GreaterThan), |l, _, r, e| e.expr((l, BinaryOp::Greater, r))),
				infix(left(3), bin(Token::GreaterThanEquals), |l, _, r, e| e.expr((l, BinaryOp::GreaterEq, r))),
				
				infix(left(4), bin(Token::BitOr), |l, _, r, e| e.expr((l, BinaryOp::BitOr, r))),
				infix(left(5), bin(Token::BitNot), |l, _, r, e| e.expr((l, BinaryOp::BitXor, r))),
				infix(left(6), bin(Token::BitAnd), |l, _, r, e| e.expr((l, BinaryOp::BitAnd, r))),
				
				infix(left(7), bin(Token::BitShiftL), |l, _, r, e| e.expr((l, BinaryOp::ShiftLeft, r))),
				infix(left(7), bin(Token::BitShiftR), |l, _, r, e| e.expr((l, BinaryOp::ShiftRight, r))),

				infix(right(8), bin(Token::Concat), |l, _, r, e| e.expr((l, BinaryOp::Concat, r))),

				infix(left(9), bin(Token::Plus), |l, _, r, e| e.expr((l, BinaryOp::Add, r))),
				infix(left(9), bin(Token::Minus), |l, _, r, e| e.expr((l, BinaryOp::Sub, r))),

				infix(left(10), bin(Token::Mul), |l, _, r, e| e.expr((l, BinaryOp::Mul, r))),
				infix(left(10), bin(Token::Div), |l, _, r, e| e.expr((l, BinaryOp::Div, r))),
				infix(left(10), bin(Token::DivFloor), |l, _, r, e| e.expr((l, BinaryOp::IntDiv, r))),
				infix(left(10), bin(Token::Mod), |l, _, r, e| e.expr((l, BinaryOp::Mod, r))),

				prefix(11, un(Token::Minus), |_, r, e| e.expr((UnaryOp::Neg, r))),
				prefix(11, un(Token::Not), |_, r, e| e.expr((UnaryOp::Not, r))),
				prefix(11, un(Token::Len), |_, r, e| e.expr((UnaryOp::Len, r))),
				prefix(11, un(Token::BitNot), |_, r, e| e.expr((UnaryOp::BitNot, r))),

				infix(right(12), bin(Token::Pow), |l, _, r, e| e.expr((l, BinaryOp::Pow, r))),
			)
		}).labelled("expression")
	})
}

fn var_expr<'s, I: ValueInput<'s>>(expr: impl LuantParser<'s, I, ExprIdx>, prefix_expr: impl LuantParser<'s, I, ExprIdx>) -> impl LuantParser<'s, I, ExprIdx> {
	choice((
		prefix_expr.clone()
			.then(expr.clone().delimited_by(just(Token::BracketOpen), just(Token::BracketClose)))
			.map_with(|(t, i), e| e.expr((t, i)))
			.labelled("index"),
		prefix_expr.clone()
			.then_ignore(just(Token::Dot))
			.then(ident())
			.map_with(|(t, i), e| {
				let i = e.resolve(i);
				let i = e.expr(i);
				e.expr((t, i))
			})
			.labelled("field access"),
		ident().map_with(|i, e| e.expr(i)),
	))
}

fn prefix_expr<'s, I: ValueInput<'s>>(expr: impl LuantParser<'s, I, ExprIdx>, var_expr: impl LuantParser<'s, I, ExprIdx>) -> impl LuantParser<'s, I, ExprIdx> {
	choice((
		just(Token::ParenOpen)
			.ignore_then(expr.clone())
			.then_ignore(just(Token::ParenClose))
			.labelled("wrapped expression"),
		var_expr.clone()
			.then(call_args(expr.clone()))
			.map_with(|(target, (method_name, args)), e| e.expr(FunctionCall {
				target,
				method_name,
				args,
			}))
			.labelled("call"),
		var_expr.clone(),
	))
}

fn call_args<'s, I: ValueInput<'s>>(expr: impl LuantParser<'s, I, ExprIdx>) -> impl LuantParser<'s, I, (Option<IdentKey>, Box<[ExprIdx]>)> {
	let expr_list = expr.clone()
		.separated_by(just(Token::Comma))
		.allow_trailing()
		.collect::<Vec<_>>()
		.map(|args| args.into_boxed_slice())
		.labelled("argument list");

	let lit_args = select! {
		Token::String(s) = e => e.expr(s),
	}.map(|s| vec![s].into_boxed_slice());

	let args = choice((
		expr_list.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
		lit_args,
		table_constructor(expr.clone()).map(|t| vec![t].into_boxed_slice()),
	)).labelled("call arguments");

	just(Token::Colon)
		.ignore_then(ident())
		.or_not()
		.then(args)
}

// Assumes the 'Function' token (as well as the name, if any) has already been consumed
// and that the following token is an opening parenthesis.
fn function_def<'s, I: ValueInput<'s>>(stmt: impl LuantParser<'s, I, Statement>, expr: impl LuantParser<'s, I, ExprIdx>) -> impl LuantParser<'s, I, FunctionDef> {
	let var_args = just(Token::VarArgs)
		.ignore_then(ident().or_not());

	let param_list = ident()
		.separated_by(just(Token::Comma))
		.allow_trailing()
		.collect::<Vec<_>>()
		.then(var_args.or_not())
		.labelled("param list");

	param_list.delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
		.then(block(stmt, expr).labelled("function body"))
		.then_ignore(just(Token::End))
		.map(|((params, var_arg), body)| FunctionDef {
			params: params.into_boxed_slice(),
			var_arg,
			body,
		})
		.labelled("function definition")
}

fn table_constructor<'s, I: ValueInput<'s>>(expr: impl LuantParser<'s, I, ExprIdx>) -> impl LuantParser<'s, I, ExprIdx> {
	let field_sep = just(Token::Comma).or(just(Token::LineTerm)).labelled("field separator");
	let val_entry = expr.clone().delimited_by(just(Token::BracketOpen), just(Token::BracketClose))
		.then_ignore(just(Token::Assign))
		.then(expr.clone());
	let ident_entry = ident().then_ignore(just(Token::Assign)).then(expr.clone()).map_with(|(key, v), e| {
		let key = e.resolve(key);
		(e.expr(key), v)
	});

	//TODO: This seems wildly silly.
	let incr = std::cell::Cell::new(0usize);
	let array_entry = expr.map_with(move |val, e| {
		incr.set(incr.get() + 1);
		(e.expr(incr.get() as f64), val)
	});

	choice((val_entry, ident_entry, array_entry)).labelled("table entry")
		.separated_by(field_sep)
		.allow_trailing()
		.collect()
		.delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
		.map_with(|fields: Vec<_>, e| {
			e.expr(Expression::Table(fields.into_boxed_slice()))
		})
		.labelled("table constructor")
}
