use std::{borrow::Cow, collections::BTreeMap};

use anyhow::Result;
use bytecode::Operation as Op;
use luant_lexer::LexInterner;
use value::Value;
use walrus::{ConstExpr, DataId, DataKind, ElementItems, ElementKind, FunctionBuilder, FunctionId, GlobalId, InstrSeqBuilder, LocalId, MemoryId, Module, RefType, ValType, ir::{BinaryOp, LoadKind, MemArg, StoreKind}};

use crate::{bytecode::{Loc, RetKind}, parsing::expressions::{Const, Expr}};

pub mod parsing;
mod bytecode;
mod debug;

pub fn compile(src: impl AsRef<[u8]>) {
	let parsed = parsing::parse(src.as_ref()).unwrap();
}

pub struct State<'s> {
	module: Module,
	memory: MemoryId,
	locals: BTreeMap<u8, LocalId>,
	arg_ptr: GlobalId,
	strings: Box<[(i32, i32)]>,
	closures: Box<[Option<FunctionId>]>,
	extern_fns: ExternFns,
	interner: LexInterner<'s>
}

struct ExternFns {
	// print: FunctionId,
	add: FunctionId,
	sub: FunctionId,
	mul: FunctionId,
	div: FunctionId,
	eq: FunctionId,
	get_fn: FunctionId,
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
			Loc::Slot(idx) => { seq.local_get(state.locals[&idx]); },
			Loc::UpValue(idx) => todo!(),
			// Loc::Global(ident_key) if state.interner.resolve_ident(ident_key) == "print" => {
				
			// },
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

pub fn lower<'s>(parsed: parsing::Parsed<'s>, interner: LexInterner<'s>) -> Result<Vec<u8>> {
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

	// let memory = module.memories.add_local(false, false, 0, None, None);
	let memory = module.get_memory_id()?;

	let extern_fns = extern_fns! {
		// print: extern_mod::__luant_print(I64);
		add: extern_mod::__luant_add(I64, I64) -> I64;
		sub: extern_mod::__luant_sub(I64, I64) -> I64;
		mul: extern_mod::__luant_mul(I64, I64) -> I64;
		div: extern_mod::__luant_div(I64, I64) -> I64;
		eq: extern_mod::__luant_eq(I64, I64) -> I64;
		get_fn: extern_mod::__luant_get_fn(i64) -> i32;
		get_truthy: extern_mod::__luant_get_truthy(i64) -> i32;
	};

	let mut offset = 0xFFF700;
	let strings = parsed.strings.into_iter().map(|s| {
		let kind = DataKind::Active { memory, offset: ConstExpr::Value(walrus::ir::Value::I32(offset)) };

		let (ptr, len) = (offset, s.len() as i32);
		offset += s.len() as i32;

		module.data.add(kind, s.into_owned().into());

		(ptr, len)
	}).collect();

	let arg_ptr = module.globals.add_local(ValType::I32, false, false, ConstExpr::Value(walrus::ir::Value::I32(0xFF0000)));
	module.globals.get_mut(arg_ptr).name = Some("__var_args_ptr".into());

	let mut state = State {
		memory,
		locals: Default::default(),
		arg_ptr,
		strings,
		closures: vec![None; parsed.closures.len() + 1].into_boxed_slice(),
		extern_fns,
		interner,
		module,
	};

	for (i, func) in parsed.closures.iter().enumerate().rev() {
		let id = compile_function(&mut state, func);
		state.closures[i + 1] = Some(id);
	}

	let mut main_fn = parsed.parsed_func;

	if let Some(debug) = main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_function(&mut state, &main_fn);
	state.closures[0] = Some(main_fn);
	
	state.module.exports.add("main", main_fn);

	{
		let func_tab = state.module.tables.main_function_table()?.unwrap_or_else(|| {
			state.module.tables.add_local(false, 0, None, RefType::FUNCREF)
		});

		state.module.elements.add(ElementKind::Active {
			table: func_tab,
			offset: ConstExpr::Value(walrus::ir::Value::I32(2)),
		}, ElementItems::Functions(state.closures.into_iter().map(Option::unwrap).collect()));

		let func_tab = state.module.tables.get_mut(func_tab);
		func_tab.maximum = None;
	}
	// state.module.elements.add(walrus::ElementKind::Declared, walrus::ElementItems::Functions(state.closures.into_iter().map(Option::unwrap).collect()));

	Ok(state.module.emit_wasm())
}

fn compile_function(state: &mut State, func: &parsing::ParsedFunction) -> FunctionId {
	let mut operations = func.operations.iter().copied();
	let mut builder = FunctionBuilder::new(&mut state.module.types, &[ValType::I32], &[ValType::I32]);
	let mut func_hash = std::hash::DefaultHasher::new();
	std::hash::Hash::hash(&func.operations, &mut func_hash);
	let func_hash = std::hash::Hasher::finish(&func_hash);
	builder.name(func.debug.as_ref().and_then(|d| d.func_name().map(|s| format!("{s}_{func_hash:#x}"))).unwrap_or_else(|| format!("<anonymous closure_{func_hash:#x}>")));

	let arg_cnt = state.module.locals.add(ValType::I32);
	state.module.locals.get_mut(arg_cnt).name = Some("__fn_arg_cnt".into());
	// // Without this, the first local seems to not get compiled if it's never used?
	// builder.func_body().local_get(arg_cnt).drop();

	let mut temps = 0;
	for slot in 0..func.frame_size {
		let local = state.module.locals.add(ValType::I64);
		state.locals.insert(slot, local);
		if let Some(debug) = func.debug.as_ref() {
			state.module.locals.get_mut(local).name = Some(match debug.get_local_name(slot) {
				Some(name) => name.to_string(),
				None => { temps += 1; format!("temp_{temps}") },
			});
		}

		// builder.func_body().i64_const(0).local_set(local);
	}

	if func.param_count > 0 {
		builder.func_body()
			// Get the number of arguments passed...
			.local_get(arg_cnt)
			// And the number of arguments expected...
			.i32_const(func.param_count.into())
			// ... And then both again
			.local_get(arg_cnt)
			.i32_const(func.param_count.into())
			// Check if the number of arguments passed is less than the number of arguments expected.
			.binop(BinaryOp::I32LtU)
			// Choose the lower values.
			.select(Some(ValType::I32))
			// And store it for later use.
			.local_set(arg_cnt);

		builder.func_body().block(None, |seq| {
			for i in 0..func.param_count {
				let block_id = seq.id();
				seq
					.local_get(arg_cnt)
					.i32_const(i.into())
					.binop(BinaryOp::I32GtU)
					.br_if(block_id)
					.global_get(state.arg_ptr)
					.load(state.memory, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: u32::from(i) * 8 })
					.local_set(state.locals[&i]);
			}
		});
	}

	while let Some(op) = operations.next() {
		compile_op(state, &mut operations, &mut builder.func_body(), op);
	}

	state.locals.clear();

	builder.finish(vec![arg_cnt], &mut state.module.funcs)
}

fn compile_op(state: &mut State, ops: &mut impl Iterator<Item=Op>, seq: &mut InstrSeqBuilder, op: Op) {
	match op {
		Op::Add(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.add); dst.push_set(state, seq); },
		Op::Sub(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.sub); dst.push_set(state, seq); },
		Op::Mul(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.mul); dst.push_set(state, seq); },
		Op::Div(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.div); dst.push_set(state, seq); },
		Op::Eq(dst, lhs, rhs) => { lhs.push(state, seq); rhs.push(state, seq); seq.call(state.extern_fns.eq); dst.push_set(state, seq); },
		Op::Copy(dst, val) => { val.push(state, seq); dst.push_set(state, seq); },
		Op::LoadClosure(dst, idx) => { seq.i64_const(Value::function(idx).as_i64()); dst.push_set(state, seq); },
		Op::StartIf(cond) => compile_if(state, ops, seq, cond),
		Op::Ret { ret_slot, ret_cnt } => {
			for (i, s) in (ret_slot..ret_slot + ret_cnt).enumerate() {
				seq.global_get(state.arg_ptr);
				Loc::Slot(s).push_get(state, seq);
				seq.store(state.memory, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
			}
			seq.i32_const(ret_cnt.into()).return_();
		},
		Op::Call { func_slot, arg_cnt, ret_kind: RetKind::None } => {
			
		},
		Op::Call { func_slot, arg_cnt, ret_kind: RetKind::Many } => {
			todo!()
		},
		Op::Call { func_slot, arg_cnt, ret_kind: RetKind::Single(loc) } => {
			for i in 0..arg_cnt {
				seq.global_get(state.arg_ptr);
				Loc::Slot(func_slot + i).push_get(state, seq);
				seq.store(state.memory, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
			}
			seq.i32_const(arg_cnt.into());
			Loc::Slot(func_slot).push_get(state, seq);
			seq.call(state.extern_fns.get_fn)
				.i32_const(0)
				.binop(BinaryOp::I32Eq)
				.if_else(ValType::I64, |seq| { seq.i64_const(Value::nil().as_i64()); }, |seq| {
				seq.global_get(state.arg_ptr)
					.load(state.memory, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: 0 });
			});
		},
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
			Expr::VarRet => {
				seq.i32_const(0)
					.binop(BinaryOp::I32Eq)
					.if_else(ValType::I64, |seq| {
						seq.i64_const(0);
					}, |seq| {
						seq.global_get(state.arg_ptr)
							.load(state.memory, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: 0 });
					});
			},
			Expr::VarArgs => todo!(),
		};
	}
}
