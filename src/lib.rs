use anyhow::Result;
use bstr::ByteSlice;
use bytecode::Operation as Op;
use luant_lexer::LexInterner;
use value::Value;
use walrus::{
	ConstExpr, DataId, DataKind, ElementItems, ElementKind, ExportItem, FunctionBuilder, FunctionId, GlobalId, GlobalKind, InstrSeqBuilder, LocalId, MemoryId, Module, RefType, TableId, TypeId, ValType, ir::{BinaryOp, LoadKind, MemArg, StoreKind}
};
use std::collections::BTreeMap;

use crate::{bytecode::{Loc, RetKind}, parsing::expressions::{Const, Expr}};

pub mod parsing;
mod bytecode;
mod debug;

pub struct State<'s> {
	module: Module,
	memory: MemoryId,
	shtack_mem: MemoryId,
	dyn_call_ty: TypeId,
	call_tab: TableId,
	locals: BTreeMap<u8, LocalId>,
	arg_ptr: GlobalId,
	strings: Box<[(u32, u32)]>,
	closures: Box<[Option<FunctionId>]>,
	extern_fns: ExternFns,
	interner: LexInterner<'s>
}

macro_rules! extern_fns {
	(struct $StructName:ident {
			$( $field:ident: $name:ident($($in:expr),*) $(-> $($ret:expr),+)? );+ $(;)?
	}) => {
		struct $StructName {
			$( $field: FunctionId, )+
		}

		impl $StructName {
			fn init(module: &Module) -> Self {
				use ValType::*;
				$(
					let $field = module.funcs.by_name(stringify!($name)).expect(concat!("Failed to find extern fn: ", stringify!($name)));
					let ty = module.types.get(module.funcs.get($field).ty());
					debug_assert_eq!(ty.params(), &[$( $in ),*]);
					debug_assert_eq!(ty.results(), &[$( $( $ret ),* )?]);
				)+

				Self {
					$( $field, )+
				}
			}
		}
	}
}

extern_fns! {
	struct ExternFns {
		add: __luant_add(I64, I64) -> I64;
		sub: __luant_sub(I64, I64) -> I64;
		mul: __luant_mul(I64, I64) -> I64;
		div: __luant_div(I64, I64) -> I64;
		eq: __luant_eq(I64, I64) -> I64;
		get_fn: __luant_get_fn(I64) -> I32;
		get_truthy: __luant_get_truthy(I64) -> I32;
		val_to_i64: __luant_val_to_i64(I64) -> I64;
		val_to_f64: __luant_val_to_f64(I64) -> F64;
		val_to_i32: __luant_val_to_i32(I64) -> I32;
		val_to_f32: __luant_val_to_f32(I64) -> F32;
		i64_to_val: __luant_i64_to_val(I64) -> I64;
		f64_to_val: __luant_f64_to_val(F64) -> I64;
		i32_to_val: __luant_i32_to_val(I32) -> I64;
		f32_to_val: __luant_f32_to_val(F32) -> I64;
		table_load: __luant_init_tab() -> I64;
		table_get: __luant_tab_get(I64, I64) -> I64;
		table_set: __luant_tab_set(I64, I64, I64);
	}
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

	let memory = module.get_memory_id().expect("Input module must have a memory");

	let shtack_mem = module.memories.add_local(false, false, 1, Some(1), None);
	module.memories.get_mut(shtack_mem).name = Some("__luant_shtack".into());

	// $.rodata         Name of the data section used to store static strings.
	// $__stack_pointer Name of the global used the store the current stack pointer.
	//                    This is *decremented* as more stack space is needed and starts *equal* to the rodata pointer.
	// __data_end       (Export name of a global) The end of the rodata portion, presumably equal to $__stack_pointer + len($.rodata).
	// __heap_base      (Export name of a global) The start of the heap, just slightly after __data_end.

	let rodata = module.data.iter().find(|d| d.name.as_ref().is_some_and(|n| n == ".rodata")).expect("Module must have rodata section").id();
	let stack_ptr = module.globals.iter().find(|g| g.name.as_ref().is_some_and(|n| n == "__stack_pointer")).expect("Module must have a stack pointer").id();
	let data_end = module.exports.iter().find_map(|e| (e.name == "__data_end").then(|| {
		let ExportItem::Global(data_end) = e.item else { panic!("data_end isn't a global") };
		data_end
	})).expect("Module must have a data end");
	module.exports.remove("__data_end").unwrap();
	let heap_base = module.exports.iter().find_map(|e| (e.name == "__heap_base").then(|| {
		let ExportItem::Global(heap_base) = e.item else { panic!("heap_base isn't a global") };
		heap_base
	})).expect("Module must have a data end");
	module.exports.remove("__heap_base").unwrap();

	// Append our Lua static strings onto the end of the Rust static strings and shuffle the various pointers accordingly.
	let GlobalKind::Local(ConstExpr::Value(walrus::ir::Value::I32(string_data_base))) = module.globals.get(stack_ptr).kind else {
		panic!("Invalid __stack_pointer");
	};
	
	let mut string_data_size = 0;
	let strings = parsed.strings.iter().map(|s| {
		let (ptr, len) = (string_data_size as u32, s.len() as u32);
		string_data_size += s.len();
		(ptr + string_data_base as u32, len)
	}).collect();

	if cfg!(debug_assertions) {
		let GlobalKind::Local(ConstExpr::Value(walrus::ir::Value::I32(data_end))) = module.globals.get(data_end).kind else {
			panic!("Invalid __data_end");
		};
		let GlobalKind::Local(ConstExpr::Value(walrus::ir::Value::I32(heap_base))) = module.globals.get(heap_base).kind else {
			panic!("Invalid __heap_base");
		};

		// heap_base should be the closest 16-byte aligned address after data_end.
		let expected_heap_base = (data_end + 15) & !15;
		assert_eq!(heap_base, expected_heap_base, "Heap base isn't correctly aligned after end of data");
	}

	// Append our new strings.
	let rodata = &mut module.data.get_mut(rodata).value;
	rodata.extend(parsed.strings.iter().flat_map(|s| s.bytes()));

	let GlobalKind::Local(ConstExpr::Value(walrus::ir::Value::I32(old_data_end))) = module.globals.get(data_end).kind else {
		panic!("Invalid __data_end");
	};
	
	let new_data_end = old_data_end + string_data_size as i32;
	module.globals.get_mut(data_end).kind = GlobalKind::Local(ConstExpr::Value(walrus::ir::Value::I32(new_data_end)));
	module.globals.get_mut(heap_base).kind = GlobalKind::Local(ConstExpr::Value(walrus::ir::Value::I32((new_data_end + 15) & !15)));

	let arg_ptr = module.globals.add_local(ValType::I32, false, false, ConstExpr::Value(walrus::ir::Value::I32(0)));
	module.globals.get_mut(arg_ptr).name = Some("__var_args_ptr".into());

	let dyn_call_ty = module.types.add(&[ValType::I32], &[ValType::I32]);
	let call_tab = module.tables.main_function_table()?.unwrap_or_else(|| {
		module.tables.add_local(false, 0, None, RefType::FUNCREF)
	});

	let mut state = State {
		dyn_call_ty,
		call_tab,
		locals: Default::default(),
		arg_ptr,
		strings,
		memory,
		shtack_mem,
		closures: vec![None; parsed.closures.len() + 1].into_boxed_slice(),
		extern_fns: ExternFns::init(&module),
		interner,
		module,
	};

	for (i, func) in parsed.closures.iter().enumerate().rev() {
		let id = compile_function(&mut state, func);
		state.closures[i + 1] = Some(id);

		if let Some((name, type_sig)) = &func.exported {
			let mut type_sig = type_sig.strip_prefix(b"fn(").ok_or_else(|| anyhow::Error::msg("Invalid export attribute format"))?;
			let mut params = vec![];
			
			loop {
				if let Some(next) = type_sig.strip_prefix(b")") {
					type_sig = next.trim();
					break;
				}

				type_sig = match type_sig {
					[ b'i',b'6',b'4', r@.. ] => { params.push(ValType::I64); r },
					[ b'i',b'3',b'2', r@.. ] => { params.push(ValType::I32); r },
					[ b'f',b'6',b'4', r@.. ] => { params.push(ValType::F64); r },
					[ b'f',b'3',b'2', r@.. ] => { params.push(ValType::F32); r },
					_ => return Err(anyhow::Error::msg("Invalid export attribute format")),
				}.trim();
				
				if let Some(next) = type_sig.strip_prefix(b",") {
					type_sig = next.trim();
				}
			}

			let mut rets = vec![];
			if let Some(next) = type_sig.strip_prefix(b"->") {
				type_sig = next.trim();

				loop {
					if type_sig.is_empty() {
						break;
					}

					type_sig = match type_sig {
						[ b'i',b'6',b'4', r@.. ] => { rets.push(ValType::I64); r },
						[ b'i',b'3',b'2', r@.. ] => { rets.push(ValType::I32); r },
						[ b'f',b'6',b'4', r@.. ] => { rets.push(ValType::F64); r },
						[ b'f',b'3',b'2', r@.. ] => { rets.push(ValType::F32); r },
						_ => return Err(anyhow::Error::msg("Invalid export attribute format")),
					}.trim();

					if let Some(next) = type_sig.strip_prefix(b",") {
						type_sig = next.trim();
					}
				}
			}

			let mut builder = FunctionBuilder::new(&mut state.module.types, &params, &rets);
			builder.name(["__luant_shim_", name.as_str()].concat());

			let mut seq = builder.func_body();
			
			let args = params.iter().enumerate().map(|(i, param)| {
				let arg = state.module.locals.add(*param);
				seq.global_get(state.arg_ptr).local_get(arg);

				match param {
					ValType::I32 => seq.call(state.extern_fns.i32_to_val),
					ValType::I64 => seq.call(state.extern_fns.i64_to_val),
					ValType::F32 => seq.call(state.extern_fns.f32_to_val),
					ValType::F64 => seq.call(state.extern_fns.f64_to_val),
					ValType::V128 => todo!(),
					ValType::Ref(_) => todo!(),
				}.store(state.shtack_mem, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
				arg
			}).collect();

			// Make the dynamic call...
			seq
				.i32_const(params.len() as i32)
				.call(id);

			// Handle the dynamic return values...
			if rets.is_empty() {
				seq.drop();
			} else {
				let arg_cnt = state.module.locals.add(ValType::I32);
				seq.local_set(arg_cnt);

				seq.block(None, |seq| {
					let block_id = seq.id();
					for (i, r) in rets.iter().enumerate() {
						seq.local_get(arg_cnt)
							.i32_const(i as i32)
							.binop(BinaryOp::I32LeU)
							.br_if(block_id)
							.global_get(state.arg_ptr)
							.load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });

						match r {
							ValType::I32 => seq.call(state.extern_fns.val_to_i32),
							ValType::I64 => seq.call(state.extern_fns.val_to_i64),
							ValType::F32 => seq.call(state.extern_fns.val_to_f32),
							ValType::F64 => seq.call(state.extern_fns.val_to_f64),
							ValType::V128 => todo!(),
							ValType::Ref(_) => todo!(),
						};
					}

					seq.return_();
				});

				seq.unreachable(); //TODO: This needs to actually be handled.
			}

			state.module.exports.add(name, builder.finish(args, &mut state.module.funcs));
		}
	}

	let mut main_fn = parsed.parsed_func;

	if let Some(debug) = main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_function(&mut state, &main_fn);
	state.closures[0] = Some(main_fn);
	
	state.module.exports.add("main", main_fn);

	let main_fn_shim = {
		let mut builder = FunctionBuilder::new(&mut state.module.types, &[], &[]);
		builder.name("__luant_shim_<main>".into())
			.func_body()
			.i32_const(0)
			.call(main_fn)
			.drop();
		builder.finish(vec![], &mut state.module.funcs)
	};
	state.module.start = Some(main_fn_shim); //FIXME: Probably a bad idea :P

	{
		let func_tab = state.module.tables.get_mut(state.call_tab);
		func_tab.name = Some("__luant_call_table".into());
		func_tab.initial = state.closures.len() as u64;
		func_tab.maximum = Some(state.closures.len() as u64);
		
		state.module.elements.add(ElementKind::Active {
			table: state.call_tab,
			offset: ConstExpr::Value(walrus::ir::Value::I32(func_tab.elem_segments.len() as i32)),
		}, ElementItems::Functions(state.closures.into_iter().map(Option::unwrap).collect()));

		func_tab.maximum = None;
	}

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
					.binop(BinaryOp::I32LeU)
					.br_if(block_id)
					.global_get(state.arg_ptr)
					.load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: u32::from(i) * 8 })
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
		Op::LoadTab(dst) => { seq.call(state.extern_fns.table_load); dst.push_set(state, seq); },
		Op::Get(dst, tab, key) => { tab.push(state, seq); key.push(state, seq); seq.call(state.extern_fns.table_get); dst.push_set(state, seq); },
		Op::Set(tab, key, val) => { tab.push(state, seq); key.push(state, seq); val.push(state, seq); seq.call(state.extern_fns.table_set); },
		Op::Ret { ret_slot, ret_cnt } => {
			for (i, s) in (ret_slot..ret_slot + ret_cnt).enumerate() {
				seq.global_get(state.arg_ptr);
				Loc::Slot(s).push_get(state, seq);
				seq.store(state.shtack_mem, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
			}
			seq.i32_const(ret_cnt.into()).return_();
		},
		Op::Call { func_slot, arg_cnt, ret_kind } => {
			for i in 0..arg_cnt {
				seq.global_get(state.arg_ptr);
				Loc::Slot(func_slot + i).push_get(state, seq);
				seq.store(state.shtack_mem, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
			}
			seq.i32_const(arg_cnt.into());
			Loc::Slot(func_slot).push_get(state, seq);
			seq.call(state.extern_fns.get_fn);
			seq.call_indirect(state.dyn_call_ty, state.call_tab);

			match ret_kind {
				RetKind::None => { seq.drop(); },
				RetKind::Single(loc) => {
					seq.i32_const(0)
						.binop(BinaryOp::I32Eq)
						.if_else(ValType::I64, |seq| { seq.i64_const(Value::nil().as_i64()); }, |seq| {
							seq.global_get(state.arg_ptr)
								.load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: 0 });
						});
					loc.push_set(state, seq);
				},
				RetKind::Many => todo!(),
			}
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
							.load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: 0 });
					});
			},
			Expr::VarArgs => todo!(),
		};
	}
}
