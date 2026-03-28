use anyhow::Result;
use std::collections::BTreeMap;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ElementSection, ExportSection, FunctionSection,
	GlobalSection, GlobalType, ImportSection, IndirectNameMap, Instruction, MemArg, MemorySection,
    MemoryType, Module, NameMap, NameSection, ProducersField, ProducersSection, RefType,
    TableSection, TableType, TagKind, TagSection, TagType, TypeSection, ValType,
};

use crate::linking::{LinkingSection, RelocSection, Symbol, SymbolTab};
use crate::instructions::{FunctionBuilder, InstructionSink};
use crate::runtime_impls::external_bindings::ExternFns;

pub mod parsing;
mod bytecode;
mod debug;
mod instructions;
mod linking;
mod runtime_impls;

pub struct State {
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
	reloc_code_sect: RelocSection,
	function_count: u32,
	code_sect: CodeSection,
	data_sect: DataSection,
	linear_memory: u32,
	shtack_mem: u32,
	dyn_call_ty: u32,
	error_tag: Symbol,
	call_tab: Symbol,
	shtack_ptr: Symbol,
	strings: Box<[StringRef]>,
	closures: Box<[Option<ClosureRef>]>,
	extern_fns: ExternFns,
	global_table: Symbol,

	// Per-function data
	locals: BTreeMap<u8, u32>,
	/// The distance from the last loop.
	loop_depth: u32,
}

#[derive(Debug, Clone, Copy)]
struct ClosureRef {
	sym: Symbol,
	id: u32,
}

#[derive(Debug, Clone, Copy)]
struct StringRef {
	len: u32,
	sym: Symbol,
}

pub fn lower<'s>(mut parsed: parsing::Parsed<'s>) -> Result<Vec<u8>> {

	let internal_strings = {
		#[derive(Debug, Clone, Copy)]
		struct Strings {
			pub error: usize,
			pub pcall: usize,
		}

		let error = parsed.constants.get_string("error");
		let pcall = parsed.constants.get_string("pcall");

		Strings { error, pcall }
	};
	
	let mut symbol_table = SymbolTab::new();
	let mut types_sect = TypeSection::new();
	let mut import_sect = ImportSection::new();
	let mut memory_sect = MemorySection::new();
	let mut tag_sect = TagSection::new();
	let mut global_sect = GlobalSection::new();
	let mut data_sect = DataSection::new();

	let strings = {
		let mut offset = 0;
		parsed.constants.strings().iter().enumerate().map(|(i, s)| {
			let (addr, len) = (offset as u32, s.len() as u32);
			offset += s.len();
			data_sect.active(0, &ConstExpr::i32_const(addr.cast_signed()), s.iter().copied());
			let sym = symbol_table.string(i as u32, len);
			StringRef { len, sym }
		}).collect()
	};

	import_sect.import("env", "__linear_memory", MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: None });
	memory_sect.memory(MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: Some(1) });

	global_sect.global(GlobalType { val_type: ValType::I32, mutable: true, shared: false }, &ConstExpr::i32_const(0));
	global_sect.global(GlobalType { val_type: ValType::I64, mutable: true, shared: false }, &ConstExpr::i64_const(0));
	let shtack_ptr = symbol_table.global(SymbolTab::WASM_SYM_BINDING_LOCAL, 0, Some("__luant_shtack_ptr"));
	let global_table = symbol_table.global(SymbolTab::WASM_SYM_BINDING_LOCAL, 1, Some("__luant_global_table"));

	types_sect.ty().function([ValType::I32], [ValType::I32]);

	import_sect.import("env", "__indirect_function_table", TableType {
		element_type: RefType::FUNCREF,
		table64: false,
		minimum: 0,
		maximum: None,
		shared: false,
	});
	let call_tab = symbol_table.table(SymbolTab::WASM_SYM_UNDEFINED, 0, None);

	let (function_count, extern_fns) = ExternFns::init(&mut types_sect, &mut import_sect, &mut symbol_table);

	tag_sect.tag(TagType { kind: TagKind::Exception, func_type_idx: types_sect.len() });
	let error_tag = symbol_table.tag(SymbolTab::WASM_SYM_BINDING_LOCAL, 0, Some("__luant_error_tag"));

	types_sect.ty().function([ValType::I64], []);

	let mut state = State {
		strings,
		
		global_table,
		symbol_table,
		shtack_ptr,
		error_tag,
		call_tab,
		dyn_call_ty: 0,
		linear_memory: 0,
		shtack_mem: 1,
		
		function_count,
		closures: vec![None; parsed.constants.closures().len() + 1].into_boxed_slice(),
		extern_fns,
		
		types_sect,
		import_sect,
		memory_sect,
		tag_sect,
		global_sect,
		data_sect,
		table_sect: TableSection::new(),
		export_sect: ExportSection::new(),
		element_sect: ElementSection::new(),
		function_sect: FunctionSection::new(),
		code_sect: CodeSection::new(),
		reloc_code_sect: RelocSection::new(),
		local_names: IndirectNameMap::new(),
		
		function_names: NameMap::new(),
		locals: Default::default(),
		loop_depth: 0,
	};

	// Compile all the functions in the file...
	for (i, func) in parsed.constants.closures().iter().enumerate().rev() {
		// 0 is reserved for the main function.
		state.closures[i + 1] = Some(compile_luant_function(&mut state, func));
	}

	// As well as the file itself as a function...
	let mut main_fn = parsed.parsed_func;

	if let Some(debug) = main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_luant_function(&mut state, &main_fn);
	state.closures[0] = Some(main_fn);

	runtime_impls::external_bindings::compile_supporting_functions(&mut state);

	let init_fn = {
		let state = &mut state;

		//? Relocations *must* be emitted in order, so we cannot compile these functions as
		//? we assign them to the global table.
		let mut std_fn = |name: &str, locals, f: fn(&mut State, &mut InstructionSink, u32)| {
			compile_function(state, name, SymbolTab::WASM_SYM_BINDING_LOCAL, [(locals, ValType::I64)], state.dyn_call_ty, f)
		};

		let std_error = std_fn("__luant_std_error", 0, runtime_impls::error);
		let std_pcall = std_fn("__luant_std_pcall", 1, runtime_impls::pcall);

		let mut builder = FunctionBuilder::new([(1, ValType::I64)]);

		{
			let mut seq = builder.sink();

			let global_tab = 0;

			// Initialize the global table.
			seq.call(state.extern_fns.table_load)
				.local_tee(global_tab)
				.global_set(state.global_table);

			let mut add_fn = |key, func| {
				seq.push_function(state, func)
					.local_get(global_tab)
					.static_str(state, key)
					.call(state.extern_fns.table_set_name);
			};

			// Load the 'error' function into it.
			add_fn(internal_strings.error, std_error);
			// Load the 'pcall' function into it.
			add_fn(internal_strings.pcall, std_pcall);

			// Generate a call to the actual main function.
			seq.i32_const(0)
				.call(main_fn.sym) //FIXME: Reloc
				.drop();

			seq.end();
		}

		let id = state.function_count;
		state.function_count += 1;

		builder.finish(state); // Encodes the function and the relocations.

		state.function_sect.function(state.types_sect.len());
		state.types_sect.ty().function([], []);

		id
	};

	state.symbol_table.function(SymbolTab::WASM_SYM_EXPORTED, init_fn, Some("start"));

	Ok(build_obj_module(&state)?.finish())
}

fn build_obj_module(state: &State) -> Result<Module> {
	let mut module = Module::new();

	let mut names = NameSection::new();
	names.functions(&state.function_names);
	names.locals(&state.local_names);
	names.tag(&{ let mut map = NameMap::new(); map.append(0, "lua_error"); map });

	let mut producers = ProducersSection::new();
	producers
		.field("language", ProducersField::new().value("Lua", "5.5"))
		.field("processed-by", ProducersField::new().value("luant", env!("CARGO_PKG_VERSION")));

	// Get the number of bytes used by the encoding of the item count in the section...
	// Relocations need this information.
	let code_section_len_len = linking::len_of_encoding_u32(state.code_sect.len());

	module
		.section(&state.types_sect)
		.section(&state.import_sect)
		.section(&state.function_sect)
		.section(&state.table_sect)
		.section(&state.memory_sect)
		.section(&state.tag_sect)
		.section(&state.global_sect)
		.section(&state.export_sect)
		.section(&state.element_sect)
		.section(&state.code_sect)
		.section(&state.data_sect)
		.section(LinkingSection::new().symbol_table(&state.symbol_table))
		.section(&state.reloc_code_sect.finish("reloc.CODE", 9, code_section_len_len))
		.section(&names)
		.section(&producers);

	Ok(module)
}

fn compile_function<L: IntoIterator<Item = (u32, ValType)>>(
	state: &mut State,
	name: impl AsRef<str>,
	flags: u32,
	locals: L,
	signature: u32,
	f: impl FnOnce(&mut State, &mut InstructionSink, u32),
) -> ClosureRef where L::IntoIter: ExactSizeIterator {
	let id = state.function_count;
	state.function_count += 1;

	let mut builder = FunctionBuilder::new(locals);
	f(state, &mut builder.sink(), id);
	builder.function_mut().instruction(&Instruction::End);
	
	builder.finish(state); // Encodes the function and the relocations.

	state.function_sect.function(signature);
	state.function_names.append(id, name.as_ref());

	let sym = state.symbol_table.function(flags, id, Some(name.as_ref()));

	ClosureRef { sym, id }
}

fn compile_luant_function(state: &mut State, func: &parsing::ParsedFunction) -> ClosureRef {
	let name = func.debug.as_ref().and_then(|d| d.func_name()).unwrap_or("<anonymous closure>");
	
	compile_function(state, name, SymbolTab::WASM_SYM_BINDING_LOCAL, [(func.frame_size.into(), ValType::I64)], state.dyn_call_ty, |state, seq, current_fn| {
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
