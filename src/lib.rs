use instructions_ext::InstructionsExt;
use luant_lexer::LexInterner;
use anyhow::Result;
use value::Value;
use std::collections::BTreeMap;
use wasm_encoder::{
    BlockType, Catch, CodeSection, ConstExpr, DataSection, ElementSection, Elements,
    EntityType, ExportSection, Function, FunctionSection, GlobalSection, GlobalType, ImportSection,
    IndirectNameMap, Instruction, InstructionSink, LinkingSection, MemArg, MemorySection,
    MemoryType, Module, NameMap, NameSection, ProducersField, ProducersSection, RefType,
    StartSection, /* SymbolTable, */ TableSection, TableType, TagKind, TagSection, TagType, TypeSection,
    ValType,
};

use crate::reloc_sections::{RelocCodeSection, RelocDataSection, SymbolTab};

pub mod parsing;
mod bytecode;
mod debug;
mod instructions_ext;
mod reloc_sections;

pub struct State<'s> {
	module: Module,
	symbol_table: SymbolTab,
	types_sect: TypeSection,
	import_sect: ImportSection,
	table_sect: TableSection,
	memory_sect: MemorySection,
	tag_sect: TagSection,
	global_sect: GlobalSection,
	export_sect: ExportSection,
	element_sect: ElementSection,
	function_sect: FunctionSection,
	function_names: NameMap,
	local_names: IndirectNameMap,
	reloc_code_sect: RelocCodeSection,
	reloc_data_sect: RelocDataSection,
	function_count: u32,
	code_sect: CodeSection,
	data_sect: DataSection,
	memory: u32,
	shtack_mem: u32,
	dyn_call_ty: u32,
	call_tab: u32,
	locals: BTreeMap<u8, u32>,
	shtack_ptr: u32,
	strings: Box<[StringRef]>,
	closures: Box<[Option<ClosureRef>]>,
	extern_fns: ExternFns,
	interner: LexInterner<'s>,
	global_table: u32,
	/// The distance from the last loop.
	loop_depth: u32,
}

#[derive(Debug, Clone, Copy)]
struct ClosureRef {
	sym: u32,
	id: u32,
}

#[derive(Debug, Clone, Copy)]
struct StringRef {
	addr: u32,
	len: u32,
	sym: u32,
}

macro_rules! extern_fns {
	(struct $StructName:ident {
			$(
				$(#[$attr:meta])*
				$field:ident $(: $name:ident)? ($($in:expr),*) $(-> $($ret:expr),+)?
			);+ $(;)?
	}) => {
		struct $StructName {
			$(
				$(#[$attr])*
				#[allow(unused)] //TODO
				$field: u32,
			)+
		}

		impl $StructName {
			fn init(types_section: &mut TypeSection, import_sect: &mut ImportSection, symbol_table: &mut SymbolTab) -> (u32, Self) {
				use ValType::*;

				let mut count = 0;

				$(
					#[allow(unused)]
					let name = stringify!($field);
					$(let name = stringify!($name);)?
					#[allow(unused)]
					let symbol_name = concat!("__luant_", stringify!($field));
					$(let symbol_name = concat!("__luant_", stringify!($name));)?

					let fn_type = types_section.len();
					types_section.ty().function([ $($in),* ], [ $($($ret),+)? ]);
					import_sect.import("__luant_internal", name, EntityType::Function(fn_type));
					let fn_idx = count;
					let $field = symbol_table.function(SymbolTab::WASM_SYM_UNDEFINED | SymbolTab::WASM_SYM_EXPLICIT_NAME, fn_idx, Some(symbol_name));

					count += 1;
				)+

				(count, Self {
					$( $field, )+
				})
			}
		}
	}
}

extern_fns! {
	struct ExternFns {
		static_str(I32, I32) -> I64;
		static_function(I32) -> I64;

		add(I64, I64) -> I64;
		sub(I64, I64) -> I64;
		mul(I64, I64) -> I64;
		div(I64, I64) -> I64;
		modulo(I64, I64) -> I64;
		pow(I64, I64) -> I64;
		neg(I64) -> I64;

		eq(I64, I64) -> I64;
		neq(I64, I64) -> I64;
		lt(I64, I64) -> I64;
		lte(I64, I64) -> I64;
		gt(I64, I64) -> I64;
		gte(I64, I64) -> I64;
		not(I64) -> I64;

		bit_and(I64, I64) -> I64;
		bit_or(I64, I64) -> I64;
		bit_xor(I64, I64) -> I64;
		bit_sh_l(I64, I64) -> I64;
		bit_sh_r(I64, I64) -> I64;
		bit_not(I64) -> I64;

		concat(I64, I64) -> I64;
		len(I64) -> I64;

		get_fn(I64) -> I32;
		get_truthy(I64) -> I32;

		val_to_i64(I64) -> I64;
		val_to_f64(I64) -> F64;
		val_to_i32(I64) -> I32;
		val_to_f32(I64) -> F32;
		i64_to_val(I64) -> I64;
		f64_to_val(F64) -> I64;
		i32_to_val(I32) -> I64;
		f32_to_val(F32) -> I64;
		
		table_load: init_tab() -> I64;
		table_get: tab_get(I64, I64) -> I64;
		table_set: tab_set(I64, I64, I64);

		/// Doesn't type check input as a table, and assumes a string key. For internal use.
		table_get_name: tab_get_name(I64, I64) -> I64;
		/// Doesn't type check input as a table, and assumes a string key. For internal use.\
		/// This takes `value, table, key` unlike other functions for impl reasons.
		table_set_name: tab_set_name(I64, I64, I64);
	}
}

pub fn lower<'s>(mut parsed: parsing::Parsed<'s>, interner: LexInterner<'s>) -> Result<Vec<u8>> {
	#[derive(Debug, Clone, Copy)]
	struct Strings {
		pub error: usize,
		pub pcall: usize,
	}

	let internal_strings = {
		let error = parsed.constants.get_string("error");
		let pcall = parsed.constants.get_string("pcall");

		Strings { error, pcall }
	};
	
	let mut symbol_table = SymbolTab::new();
	let mut types_sect = TypeSection::new();
	let mut import_sect = ImportSection::new();
	let table_sect = TableSection::new();
	let mut memory_sect = MemorySection::new();
	let mut tag_sect = TagSection::new();
	let mut global_sect = GlobalSection::new();
	let export_sect = ExportSection::new();
	let element_sect = ElementSection::new();
	let function_sect = FunctionSection::new();
	let code_sect = CodeSection::new();
	let mut data_sect = DataSection::new();
	let mut reloc_code_sect = RelocCodeSection::new();
	let mut reloc_data_sect = RelocDataSection::new();

	let strings = {
		let mut offset = 0;
		parsed.constants.strings().iter().enumerate().map(|(i, s)| {
			let (addr, len) = (offset as u32, s.len() as u32);
			offset += s.len();
			data_sect.active(0, &ConstExpr::i32_const(addr.cast_signed()), s.iter().copied());
			let sym = symbol_table.string(i as u32, len);
			StringRef { addr, len, sym }
		}).collect()
	};

	import_sect.import("env", "__linear_memory", MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: None });
	memory_sect.memory(MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: Some(1) });

	global_sect.global(GlobalType { val_type: ValType::I32, mutable: true, shared: false }, &ConstExpr::i32_const(0));
	global_sect.global(GlobalType { val_type: ValType::I64, mutable: true, shared: false }, &ConstExpr::i64_const(0));
	symbol_table.global(SymbolTab::WASM_SYM_BINDING_LOCAL, 0, Some("__luant_shtack_ptr"));
	symbol_table.global(SymbolTab::WASM_SYM_BINDING_LOCAL, 1, Some("__luant_global_table"));

	types_sect.ty().function([ValType::I32], [ValType::I32]);

	import_sect.import("__luant_internal", "__indirect_function_table", TableType {
		element_type: RefType::FUNCREF,
		table64: false,
		minimum: 0,
		maximum: None,
		shared: false,
	});
	symbol_table.table(SymbolTab::WASM_SYM_UNDEFINED, 0, None);

	let (function_count, extern_fns) = ExternFns::init(&mut types_sect, &mut import_sect, &mut symbol_table);

	tag_sect.tag(TagType { kind: TagKind::Exception, func_type_idx: types_sect.len() });
	types_sect.ty().function([ValType::I64], []);

	let mut state = State {
		dyn_call_ty: 0,
		call_tab: 0,
		locals: Default::default(),
		shtack_ptr: 0,
		strings,
		memory: 0,
		shtack_mem: 1,
		closures: vec![None; parsed.constants.closures().len() + 1].into_boxed_slice(),
		function_count,
		extern_fns,
		interner,
		module: Module::new(),
		global_table: 1,
		symbol_table,
		types_sect,
		import_sect,
		table_sect,
		memory_sect,
		tag_sect,
		global_sect,
		export_sect,
		element_sect,
		function_sect,
		code_sect,
		data_sect,
		function_names: NameMap::new(),
		local_names: IndirectNameMap::new(),
		reloc_code_sect,
		reloc_data_sect,
		loop_depth: 0,
	};

	for (i, func) in parsed.constants.closures().iter().enumerate().rev() {
		// 0 is reserved for the main function.
		state.closures[i + 1] = Some(compile_luant_function(&mut state, func));
	}

	let mut main_fn = parsed.parsed_func;

	if let Some(debug) = main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_luant_function(&mut state, &main_fn);
	// let main_fn_sym = state.symbol_table.function(SymbolTab::WASM_SYM_BINDING_LOCAL, main_fn, Some("luant_main"));
	state.closures[0] = Some(main_fn);

	let mut closures: Vec<_> = state.closures.iter().copied().map(Option::unwrap).collect();

	let init_fn = {
		let mut builder = Function::new_with_locals_types([ValType::I64]);
		let mut seq = builder.instructions();

		let global_tab = 0;

		// Initialize the global table.
		seq.call(state.extern_fns.table_load)
			.local_tee(global_tab)
			.global_set(state.global_table);

		// Load the 'error' function into it.
		let error_fn = compile_function(&mut state, "__luant_std_error", 0, |state, seq, _| {
			seq.global_get(state.shtack_ptr)
				// .load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 3, offset: 0 })
				.i64_load(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem }) //TODO: This assumes the first arg is null if not provided...
				.throw(0)
				.i32_const(0);
		});
		
		// let error_fn_sym = state.symbol_table.function(SymbolTab::WASM_SYM_BINDING_LOCAL, error_fn, Some("luant_std_error"));
		closures.push(error_fn);
		seq.push_function(&mut state, error_fn.sym)
			.local_get(global_tab)
			.static_str(&mut state, internal_strings.error)
			.call(state.extern_fns.table_set_name);
		
		// Load the 'pcall' function into it.
		let pcall_fn = compile_function(&mut state, "__luant_std_pcall", 1, |state, seq, _| {
			let arg_cnt = 0;
			let temp_var = 1;

			seq.global_get(state.shtack_ptr)
				// .load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 3, offset: 0 })
				.i64_load(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem }) //TODO: This assumes the first arg is null if not provided...
				.local_set(temp_var);

			// Increase the stack pointer by one to remove the first arg.
			seq.global_get(state.shtack_ptr)
				.i32_const(1)
				.i32_add()
				.global_set(state.shtack_ptr);

			seq.block(BlockType::Result(ValType::I64))
				.try_table(BlockType::Result(ValType::I64), [Catch::One { tag: 0, label: 0 }]);

			// Write the 'try' block...
			seq
				// Pass the current arg count minus one, to a minimum of zero.
				.i32_const(0)
				.i32_const(1)
				.local_get(arg_cnt)
				.i32_sub()
				.i32_const(0)
				.local_get(arg_cnt)
				.i32_eq()
				.typed_select(ValType::I32)
				// Make the call.
				.local_get(temp_var)
				.call(state.extern_fns.get_fn)
				.call_indirect(state.dyn_call_ty, state.call_tab)
				// Increase the count to return, accounting for the error flag.
				.i32_const(1)
				.i32_add()
				// Reset the shtack pointer.
				.global_get(state.shtack_ptr)
				.i32_const(1)
				.i32_sub()
				.global_set(state.shtack_ptr)
				// And prepend the 'success' flag to the return values.
				.global_get(state.shtack_ptr)
				.const_val(true)
				.i64_store(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem })
				// Return the new arg count, still on the stack.
				.return_()
				.end();

			seq.end();

			// Write the 'catch' block...
			seq
				// Store the error object for now.
				.local_set(temp_var)
				// Reset the shtack pointer.
				.global_get(state.shtack_ptr)
				.i32_const(1)
				.i32_sub()
				.global_set(state.shtack_ptr)
				// Prepend the 'error' flag to the return values.
				.global_get(state.shtack_ptr)
				.const_val(false)
				.i64_store(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem })
				// Set the error object as the second return value.
				.global_get(state.shtack_ptr)
				.local_get(temp_var)
				.i64_store(MemArg { align: 3, offset: 8, memory_index: state.shtack_mem })
				// Return the arg count of two.
				.i32_const(2)
				.return_();
		});

		// let pcall_fn_sym = state.symbol_table.function(SymbolTab::WASM_SYM_BINDING_LOCAL, pcall_fn, Some("luant_std_pcall"));
		closures.push(pcall_fn);
		seq.push_function(&mut state, pcall_fn.sym)
			.local_get(global_tab)
			.static_str(&mut state, internal_strings.pcall)
			.call(state.extern_fns.table_set_name);

		// Generate a call to the actual main function.
		seq.i32_const(0)
			.call(main_fn.id) //FIXME: Reloc
			.drop();

		seq.end();

		let id = state.function_count;
		state.function_count += 1;

		state.code_sect.function(&builder);
		state.function_sect.function(state.types_sect.len());
		state.types_sect.ty().function([], []);

		id
	};

	let mut module = state.module;

	state.symbol_table.function(SymbolTab::WASM_SYM_BINDING_LOCAL | SymbolTab::WASM_SYM_NO_STRIP, init_fn, Some("__luant_init_fn"));
	let function_elements = closures.into_iter().map(|c| c.id).collect();
	state.element_sect.active(Some(state.call_tab), &ConstExpr::i32_const(0), Elements::Functions(function_elements));

	let mut names = NameSection::new();
	names.functions(&state.function_names);
	names.locals(&state.local_names);
	names.tag(&{ let mut map = NameMap::new(); map.append(0, "lua_error"); map });

	let mut producers = ProducersSection::new();
	producers
		.field("language", ProducersField::new().value("Lua", "5.5"))
		.field("processed-by", ProducersField::new().value("luant", env!("CARGO_PKG_VERSION")));

	module
		.section(&state.types_sect)
		.section(&state.import_sect)
		.section(&state.function_sect)
		.section(&state.table_sect)
		.section(&state.memory_sect)
		.section(&state.tag_sect)
		.section(&state.global_sect)
		.section(&state.export_sect)
		.section(&StartSection { function_index: init_fn })
		.section(&state.element_sect)
		.section(&state.code_sect)
		.section(&state.data_sect)
		.section(LinkingSection::new().symbol_table(&state.symbol_table))
		.section(&state.reloc_code_sect)
		.section(&state.reloc_data_sect)
		.section(&names)
		.section(&producers);

	Ok(module.finish())
}

fn compile_function(state: &mut State, name: impl AsRef<str>, locals: u32, f: impl FnOnce(&mut State, &mut InstructionSink, u32)) -> ClosureRef {
	let id = state.function_count;
	state.function_count += 1;

	let mut builder = Function::new([(locals, ValType::I64)]);
	f(state, &mut builder.instructions(), id);
	builder.instruction(&Instruction::End);
	state.code_sect.function(&builder);
	state.function_sect.function(state.dyn_call_ty);
	state.function_names.append(id, name.as_ref());

	let sym = state.symbol_table.function(SymbolTab::WASM_SYM_BINDING_LOCAL, id, Some(name.as_ref()));

	ClosureRef { sym, id }
}

fn compile_luant_function(state: &mut State, func: &parsing::ParsedFunction) -> ClosureRef {
	let name = func.debug.as_ref().and_then(|d| d.func_name()).unwrap_or("<anonymous closure>");
	
	compile_function(state, name, func.frame_size.into(), |state, seq, current_fn| {
		let arg_cnt = 0;

		{
			let mut temps = 0;
			let mut name_buf = String::new();
			let mut local_names = NameMap::new();
			for (i, slot) in (0..func.frame_size).enumerate() {
				state.locals.insert(slot, i as u32 + 1); // Account for the argument slot at 0.
				if let Some(debug) = func.debug.as_ref() {
					local_names.append(i as u32 + 1, debug.get_local_name(slot).unwrap_or_else(|| {
						use std::fmt::Write;
						temps += 1;
						name_buf.clear();
						write!(&mut name_buf, "temp_{temps}").unwrap();
						&name_buf
					}));
				}
			}

			state.local_names.append(current_fn, &local_names);
		}

		if func.param_count > 0 {
			seq
				// Get the number of arguments passed...
				.local_get(arg_cnt)
				// And the number of arguments expected...
				.i32_const(func.param_count.into())
				// ... And then both again
				.local_get(arg_cnt)
				.i32_const(func.param_count.into())
				// Check if the number of arguments passed is less than the number of arguments expected.
				.i32_lt_u()
				// Choose the lower value.
				.typed_select(ValType::I32)
				// And store it for later use.
				.local_set(arg_cnt);

			seq.block(BlockType::Empty);
			for i in 0..func.param_count {
				seq
					.local_get(arg_cnt)
					.i32_const(i.into())
					.i32_le_u()
					.br_if(0)
					.global_get(state.shtack_ptr)
					.i64_load(MemArg { align: 3, offset: u64::from(i) * 8, memory_index: state.shtack_mem })
					.local_set(state.locals[&i]);
			}
			seq.end();
		}

		seq.operations(state, func.operations.iter().copied());

		if func.operations.last().is_none_or(|op| !matches!(op, bytecode::Operation::Ret { .. })) {
			seq.i32_const(0)
				.return_();
		}

		state.locals.clear();
	})
}
