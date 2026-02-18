use anyhow::Result;
use bstr::ByteSlice;
use bytecode::Operation as Op;
use luant_lexer::LexInterner;
use value::Value;
use walrus::{
	ConstExpr, DataId, DataKind, ElementItems, ElementKind, FunctionBuilder, FunctionId, GlobalId, InstrSeqBuilder, LocalId, MemoryId, Module, RefType, TableId, ValType, ir::{BinaryOp, LoadKind, MemArg, StoreKind}
};
use std::collections::BTreeMap;

use crate::{bytecode::{Loc, RetKind}, parsing::expressions::{Const, Expr}};

pub mod parsing;
mod bytecode;
mod debug;

struct Strings {
	data: DataId,
	table: TableId,
	index: Box<[(i32, i32)]>,
}
// impl Strings {
// 	fn init_str(&self, module: &mut Module, idx: usize) -> i32 {
// 		let (addr, len) = self.index[idx];
		
// 		Value::string(addr, len)
// 	}
// }

pub struct State<'s> {
	module: Module,
	memory: MemoryId,
	locals: BTreeMap<u8, LocalId>,
	arg_ptr: GlobalId,
	strings: Strings,
	closures: Box<[Option<FunctionId>]>,
	extern_fns: ExternFns,
	interner: LexInterner<'s>
}

macro_rules! extern_fns {
	(struct $StructName:ident {
			$( $field:ident: $name:ident($($in:ident),*) $(-> $($ret:ident),+)? );+ $(;)?
	}) => {
		struct $StructName {
			$( $field: FunctionId, )+
		}

		impl $StructName {
			fn init(module: &Module) -> Self {
				$(
					let $field = module.funcs.by_name(stringify!($name)).expect(concat!("Failed to find extern fn: ", stringify!($name)));
					let ty = module.types.get(module.funcs.get($field).ty());
					debug_assert_eq!(ty.params(), &[$( ValType::$in ),*]);
					debug_assert_eq!(ty.results(), &[$( $( ValType::$ret ),* )?]);
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

	// let memory = module.memories.add_local(false, false, 0, None, None);
	let memory = module.get_memory_id()?;

	let strings = {
		let string_table = module.tables.add_local(false, 0, None, RefType::ARRAYREF);
		module.tables.get_mut(string_table).name = Some("__luant_string_table".into());
	
		let mut offset = 0;
		let strings = parsed.strings.iter().map(|s| {
			let (ptr, len) = (offset, s.len() as i32);
			offset += s.len() as i32;
			(ptr, len)
		}).collect();
	
		let string_data = module.data.add(DataKind::Passive, parsed.strings.iter().flat_map(|s| s.bytes()).collect());

		Strings {
			data: string_data,
			table: string_table,
			index: strings,
		}
	};


	let arg_ptr = module.globals.add_local(ValType::I32, false, false, ConstExpr::Value(walrus::ir::Value::I32(0)));
	module.globals.get_mut(arg_ptr).name = Some("__var_args_ptr".into());

	let mut state = State {
		memory,
		locals: Default::default(),
		arg_ptr,
		strings,
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
				}.store(state.memory, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
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
							.load(state.memory, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });

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
		Op::Call { func_slot, arg_cnt, ret_kind } => {
			for i in 0..arg_cnt {
				seq.global_get(state.arg_ptr);
				Loc::Slot(func_slot + i).push_get(state, seq);
				seq.store(state.memory, StoreKind::I64 { atomic: false }, MemArg { align: 8, offset: i as u32 * 8 });
			}
			seq.i32_const(arg_cnt.into());
			Loc::Slot(func_slot).push_get(state, seq);
			seq.call(state.extern_fns.get_fn);

			match ret_kind {
				RetKind::None => { seq.drop(); },
				RetKind::Single(loc) => {
					seq.i32_const(0)
						.binop(BinaryOp::I32Eq)
						.if_else(ValType::I64, |seq| { seq.i64_const(Value::nil().as_i64()); }, |seq| {
							seq.global_get(state.arg_ptr)
								.load(state.memory, LoadKind::I64 { atomic: false }, MemArg { align: 8, offset: 0 });
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
				// let (addr, len) = state.strings[idx];
				// Value::string(addr, len).push(seq);
				todo!()
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
