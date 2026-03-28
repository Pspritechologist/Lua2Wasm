use anyhow::Result;
use std::collections::BTreeMap;
use wasm_encoder::{
    CodeSection, DataSection, FunctionSection, GlobalSection, ImportSection, IndirectNameMap,
    Instruction, Module, NameMap, NameSection, ProducersField, ProducersSection, TagSection,
    TypeSection, ValType,
};

use instructions::{FunctionBuilder, InstructionSink};
use linking::{LinkingSection, RelocSection, Symbol, SymbolTab};
use runtime_impls::external_bindings::ExternFns;

mod lua_modules;
mod instructions;
mod linking;
mod runtime_impls;

//TODO
pub use lua_modules::produce_lua_obj_file;

struct State {
	symbol_table: SymbolTab,
	types_sect: TypeSection,
	import_sect: ImportSection,
	tag_sect: TagSection,
	global_sect: GlobalSection,
	function_sect: FunctionSection,
	code_sect: CodeSection,
	data_sect: DataSection,
	function_names: NameMap,
	local_names: IndirectNameMap,
	reloc_code_sect: RelocSection,
	function_count: u32,
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
		.section(&state.tag_sect)
		.section(&state.global_sect)
		.section(&state.code_sect)
		.section(&state.data_sect)
		.section(LinkingSection::new().symbol_table(&state.symbol_table))
		.section(&state.reloc_code_sect.finish("reloc.CODE", 5, code_section_len_len))
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
