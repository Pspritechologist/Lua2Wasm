mod lowering;

use anyhow::{Result, anyhow};
use luant_lexer::IdentKey;
use luant_parser::{ElseBranch, ExprIdx, ExprMap, Expression, FunctionCall, FunctionDef, Statement};
use slotmap::SlotMap;
use walrus::{FunctionBuilder, Module};

struct State<'s> {
	exprs: ExprMap<'s>,
	module: Module,
	builder: FunctionBuilder,
	interner: luant_lexer::LexInterner<'s>,
}

pub fn compile(src: impl AsRef<[u8]>) -> Result<()> {
	let src = src.as_ref();
	let parsed = {
		match luant_parser::parse(src) {
			Ok(ast) => ast,
			Err(e) => {
				let (lc, _) = e.span();
				return Err(anyhow!("Syntax error at {lc} - {}", e.reason()));
			}
		}
	};

	let luant_parser::Parsed { expressions, statements, interner } = parsed;

	let mut module = Module::with_config(walrus::ModuleConfig::new());
	let mut state = State {
		builder: FunctionBuilder::new(&mut module.types, &[], &[]),
		exprs: expressions,
		interner,
		module,
	};

	for stat in statements {
		compile_stat(&mut state, stat)?;
	}

	Ok(())
}

fn compile_stat(state: &mut State, stat: Statement) -> Result<()> {
	match stat {
		Statement::Assignment { targets, values } => todo!(),
		Statement::FunctionCall(function_call) => todo!(),
		Statement::Label(ident_key) => todo!(),
		Statement::Break => todo!(),
		Statement::Goto(ident_key) => todo!(),
		Statement::Do(statements) => todo!(),
		Statement::While { condition, body } => todo!(),
		Statement::Repeat { body, condition } => todo!(),
		Statement::If { condition, then_branch, else_branch } => todo!(),
		Statement::For { ident, start, end, step, body } => todo!(),
		Statement::GenericFor { idents, args, body } => todo!(),
		Statement::FunctionDecl { destination, method_name, func } => todo!(),
		Statement::VarFunctionDecl { global, name, func } => todo!(),
		Statement::VarDecl { global, attrib, idents, values } => todo!(),
		Statement::GlobalSet(ident_key) => todo!(),
		Statement::Return(items) => todo!(),
	}
}
