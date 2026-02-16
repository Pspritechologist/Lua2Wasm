use std::{borrow::Cow, collections::BTreeMap};

use anyhow::Result;
use bytecode::Operation as Op;
use value::Value;
use walrus::{ConstExpr, DataId, DataKind, FunctionBuilder, FunctionId, InstrSeqBuilder, LocalId, MemoryId, Module, ValType, ir::BinaryOp};

use crate::{bytecode::Loc, parsing::expressions::{Const, Expr}};

pub mod parsing;
mod bytecode;
mod debug;

pub fn compile(src: impl AsRef<[u8]>) {
	let parsed = parsing::parse(src.as_ref()).unwrap();
}

pub struct State {
	module: Module,
	memory: MemoryId,
	locals: BTreeMap<u8, LocalId>,
	strings: Box<[(i32, i32)]>,
	extern_fns: ExternFns,
}

struct ExternFns {
	print: FunctionId,
	add: FunctionId,
	sub: FunctionId,
	mul: FunctionId,
	div: FunctionId,
	eq: FunctionId,
	call: FunctionId,
	get_truthy: FunctionId,
}

trait ValueExt {
	fn push(self, seq: &mut InstrSeqBuilder);
}
impl ValueExt for Value {
	fn push(self, seq: &mut InstrSeqBuilder) {
		seq.i64_const(self.as_i64());
	}
}

impl Loc {
	fn push_get(self, state: &mut State, seq: &mut InstrSeqBuilder) {
		match self {
			Loc::Slot(idx) => seq.local_get(state.locals[&idx]),
			Loc::UpValue(idx) => todo!(),
			Loc::Global(ident_key) => todo!(),
		};
	}

	fn push_set(self, state: &mut State, seq: &mut InstrSeqBuilder) {
		match self {
			Loc::Slot(idx) => seq.local_set(state.locals[&idx]),
			Loc::UpValue(idx) => todo!(),
			Loc::Global(ident_key) => todo!(),
		};
	}
}

pub fn lower(parsed: parsing::Parsed) -> Result<Vec<u8>> {
	let mut module = walrus::ModuleConfig::default()
		.parse(include_bytes!("../target/wasm32-unknown-unknown/release/wasm_scratch.wasm"))?;

	let mut skipped = 0;
	while let Some(e) = { module.exports.iter().nth(skipped) } {
		if e.name.starts_with("__luant") {
			module.exports.delete(e.id());
		} else {
			skipped += 1;
		}
	}

	let main_fn = parsed.parsed_func;

	let mut operations = main_fn.operations.iter().copied();

	let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
	builder.name("<main>".into());

	macro_rules! extern_fn {
		($ns:ident::$name:ident($($in:ident),*) $(-> $($ret:ident),+)?) => { {
			module.funcs.by_name(stringify!($name)).expect(concat!("Failed to find extern fn: ", stringify!($name)))
		} };
	}

	macro_rules! extern_fns {
		($(
			$field:ident: $ns:ident::$name:ident($($in:ident),*) $(-> $($ret:ident),+)?
		);+ $(;)?) => {
			ExternFns {
				$( $field: extern_fn!($ns::$name($($in),*) $(-> $($ret),+)?), )+
			}
		};
	}

	let memory = module.memories.add_local(false, false, 0, None, None);

	let extern_fns = extern_fns! {
		print: extern_mod::__luant_print(I64);
		add: extern_mod::__luant_add(I64, I64) -> I64;
		sub: extern_mod::__luant_sub(I64, I64) -> I64;
		mul: extern_mod::__luant_mul(I64, I64) -> I64;
		div: extern_mod::__luant_div(I64, I64) -> I64;
		eq: extern_mod::__luant_eq(I64, I64) -> I64;
		call: extern_mod::__luant_call(i32, i32) -> i32;
		get_truthy: extern_mod::__luant_get_truthy(i64) -> i32;
	};

	let mut offset = 0xFFF000;
	let strings = parsed.strings.into_iter().map(|s| {
		let kind = DataKind::Active { memory, offset: ConstExpr::Value(walrus::ir::Value::I32(offset)) };

		let (ptr, len) = (offset, s.len() as i32);
		offset += s.len() as i32;

		module.data.add(kind, s.into_owned().into());

		(ptr, len)
	}).collect();

	let mut state = State {
		memory,
		module,
		locals: Default::default(),
		strings,
		extern_fns,
	};

	let seq = &mut builder.func_body();
	
	(0..main_fn.frame_size).for_each(|slot| {
		let local = state.module.locals.add(ValType::I64);
		state.locals.insert(slot, local);
		if let Some(debug) = main_fn.debug.as_ref() && let Some(name) = debug.get_local_name(slot) {
			state.module.locals.get_mut(local).name = Some(name.to_string());
		}
	});

	while let Some(op) = operations.next() {
		compile_op(&mut state, &mut operations, seq, op);
	}

	let main_fn = builder.finish(vec![], &mut state.module.funcs);
	state.module.exports.add("main", main_fn);

	Ok(state.module.emit_wasm())
}

fn compile_op(state: &mut State, ops: &mut impl Iterator<Item=Op>, seq: &mut InstrSeqBuilder, op: Op) {
	match op {
		Op::Add(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.add); dst.push_set(state, seq); },
		Op::Sub(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.sub); dst.push_set(state, seq); },
		Op::Mul(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.mul); dst.push_set(state, seq); },
		Op::Div(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.div); dst.push_set(state, seq); },
		Op::Eq(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.eq); dst.push_set(state, seq); },
		Op::Copy(dst, val) => { val.push(state, seq); dst.push_set(state, seq); },
		Op::StartIf(cond) => compile_if(state, ops, seq, cond),
		// Op::Call(dst, , )
		_ => todo!("{op:?}"),
	}
}

fn compile_if(state: &mut State, ops: &mut impl Iterator<Item=Op>, seq: &mut InstrSeqBuilder, cond: Expr) {
	cond.push(state, seq);
	seq.call(state.extern_fns.get_truthy);
	let (consequent, next) = {
		let mut seq = seq.dangling_instr_seq(None);
		let next = loop {
			let op = ops.next().unwrap();

			if op == Op::EndIf { break None; }
			if op == Op::Else { break Some(None); }
			if op == Op::ElseIf(cond) { break Some(Some(cond)); }
			
			compile_op(state, ops, &mut seq, op)
		};

		(seq.id(), next)
	};

	let alternative = {
		let mut seq = seq.dangling_instr_seq(None);

		match next {
			None => (),
			Some(None) => loop {
				let op = ops.next().unwrap();
				if op == Op::EndIf { break; }
				compile_op(state, ops, &mut seq, op)
			},
			Some(Some(cond)) => compile_if(state, ops, &mut seq, cond),
		}

		seq.id()
	};

	seq.instr(walrus::ir::IfElse {
		consequent,
		alternative,
	});
}

impl Expr {
	fn push(self, state: &mut State, seq: &mut InstrSeqBuilder) {
		match self {
			Expr::Constant(Const::Nil) => Value::nil().push(seq),
			Expr::Constant(Const::Bool(b)) => Value::bool(b).push(seq),
			Expr::Constant(Const::Number(n)) => Value::float(n.val()).push(seq),
			Expr::Constant(Const::String(idx)) => {
				let (addr, len) = state.strings[idx];
				Value::string(addr, len).push(seq);
			},
			Expr::Slot(idx) => Loc::Slot(idx).push_get(state, seq),
			Expr::UpValue(idx) => Loc::UpValue(idx).push_get(state, seq),
			Expr::Global(ident) => Loc::Global(ident).push_get(state, seq),
			Expr::CallRet => todo!(),
			Expr::VarArgs => todo!(),
		};
	}
}
