use anyhow::Result;
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, FunctionSection, GlobalSection, GlobalType, ImportSection,
    IndirectNameMap, Instruction, MemoryType, Module, NameMap, NameSection, ProducersField,
    ProducersSection, RefType, TableType, TagKind, TagSection, TagType, TypeSection, ValType,
};

use instructions::{FunctionBuilder, InstructionSink};
use linking::{LinkingSection, RelocSection, Symbol, SymbolTab};
use runtime_impls::external_bindings::ExternFns;

pub use lua_modules::produce_lua_obj_file;
pub use runtime_impls::initialize::generate_runtime_object;

pub mod exports;
mod lua_modules;
mod instructions;
mod linking;
mod runtime_impls;

use crate::object::linking::InitFunctions;

pub const ENTRY_POINT_NAME: &str = "__camento_main_entry";

struct InitPriorities;
impl InitPriorities {
	const REGISTER_MODULES: u32 = 100;
	const INIT_RUNTIME: u32 = 50;
}

pub struct ModuleState {
	symbol_table: SymbolTab,
	types_sect: TypeSection,
	import_sect: ImportSection,
	tag_sect: TagSection,
	global_sect: GlobalSection,
	function_sect: FunctionSection,
	code_sect: CodeSection,
	data_sect: DataSection,
	init_fns: InitFunctions,
	function_names: NameMap,
	local_names: IndirectNameMap,
	reloc_code_sect: RelocSection,
	function_count: u32,
	linear_memory: u32,
	shtack_mem: u32,
	dyn_call_ty: u32,
	error_tag: Symbol,
	call_tab: Symbol,
	extern_fns: ExternFns,
	global_table: Symbol,
	module_table: Symbol,
}

impl Default for ModuleState {
	fn default() -> Self { Self::new_module() }
}

impl ModuleState {
	pub fn new_module() -> Self {
		let mut symbol_table = SymbolTab::new();
		let mut types_sect = TypeSection::new();
		let mut import_sect = ImportSection::new();
		let mut tag_sect = TagSection::new();
		let mut global_sect = GlobalSection::new();

		import_sect.import("env", "__linear_memory", MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: None });

		global_sect.global(GlobalType { val_type: ValType::I64, mutable: true, shared: false }, &ConstExpr::i64_const(0));
		global_sect.global(GlobalType { val_type: ValType::I64, mutable: true, shared: false }, &ConstExpr::i64_const(0));
		let global_table = symbol_table.global(SymbolTab::WASM_SYM_BINDING_WEAK, 0, Some("__camento_global_table"));
		let module_table = symbol_table.global(SymbolTab::WASM_SYM_BINDING_WEAK, 1, Some("__camento_module_table"));

		types_sect.ty().function([ValType::I32, ValType::I32], [ValType::I32]);

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
		let error_tag = symbol_table.tag(SymbolTab::WASM_SYM_BINDING_WEAK, 0, Some("__camento_error_tag"));

		types_sect.ty().function([ValType::I64], []);

		Self {
			global_table,
			module_table,
			error_tag,
			call_tab,
			dyn_call_ty: 0,
			linear_memory: 0,
			shtack_mem: 1,
			
			function_count,
			extern_fns,
			
			types_sect,
			import_sect,
			tag_sect,
			global_sect,
			data_sect: DataSection::new(),
			function_sect: FunctionSection::new(),
			code_sect: CodeSection::new(),
			
			symbol_table,
			init_fns: InitFunctions::new(),
			reloc_code_sect: RelocSection::new(),

			local_names: IndirectNameMap::new(),
			function_names: NameMap::new(),
		}
	}

	pub fn new_global(&mut self, flags: u32, name: Option<&str>, ty: ValType, init_val: &ConstExpr) -> Symbol {
		let idx = self.global_sect.len();
		self.global_sect.global(GlobalType { val_type: ty, mutable: true, shared: false }, init_val);
		self.symbol_table.global(flags, idx, name)
	}

	pub fn add_data<D: IntoIterator<Item = u8>>(&mut self, flags: u32, name: &str, data: D) -> Symbol
	where D::IntoIter: ExactSizeIterator {
		let data = data.into_iter();
		let size = data.len().try_into().unwrap();

		let index = self.data_sect.len();

		let def = wasm_encoder::DataSymbolDefinition { index, size, offset: 0 };
		let sym = self.symbol_table.data(flags, &[".rodata_", name].concat(), Some(def));

		self.data_sect.active(0, &ConstExpr::i32_const(0), data);

		sym
	}

	pub fn build_object(&self) -> Result<Module> {
		let mut module = Module::new();

		let mut names = NameSection::new();
		names.functions(&self.function_names);
		names.locals(&self.local_names);
		names.tag(&{ let mut map = NameMap::new(); map.append(0, "lua_error"); map });

		let mut producers = ProducersSection::new();
		producers
			.field("language", ProducersField::new().value("Lua", "5.5"))
			.field("processed-by", ProducersField::new().value("camento", env!("CARGO_PKG_VERSION")));

		// Get the number of bytes used by the encoding of the item count in the section...
		// Relocations need this information.
		let code_section_len_len = linking::len_of_encoding_u32(self.code_sect.len());

		let mut init_sect = LinkingSection::new();
		init_sect
			.symbol_table(&self.symbol_table)
			.init_functions(&self.init_fns);

		module
			.section(&self.types_sect)
			.section(&self.import_sect)
			.section(&self.function_sect)
			.section(&self.tag_sect)
			.section(&self.global_sect)
			.section(&self.code_sect)
			.section(&self.data_sect)
			.section(&init_sect)
			.section(&self.reloc_code_sect.finish("reloc.CODE", 5, code_section_len_len))
			.section(&names)
			.section(&producers);

		Ok(module)
	}
}

#[derive(Debug, Clone, Copy)]
struct ClosureRef {
	sym: Symbol,
	index: u32,
}

#[derive(Debug, Clone, Copy)]
struct StringRef {
	len: u32,
	sym: Symbol,
}

trait HasModuleState {
	fn module_state(&mut self) -> &mut ModuleState;
}

impl HasModuleState for ModuleState {
	fn module_state(&mut self) -> &mut ModuleState { self }
}

fn try_compile_function<E, S: HasModuleState, L: IntoIterator<Item = (u32, ValType)>>(
	has_state: &mut S,
	name: impl AsRef<str>,
	flags: u32,
	locals: L,
	signature: u32,
	f: impl FnOnce(&mut S, &mut InstructionSink, u32) -> Result<(), E>,
) -> Result<ClosureRef, E> where L::IntoIter: ExactSizeIterator {
	let state = has_state.module_state();

	let id = state.function_count;
	state.function_count += 1;

	let mut builder = FunctionBuilder::new(locals);
	f(has_state, &mut builder.sink(), id)?;

	let state = has_state.module_state();

	builder.function_mut().instruction(&Instruction::End);
	
	builder.finish(state); // Encodes the function and the relocations.

	state.function_sect.function(signature);
	state.function_names.append(id, name.as_ref());

	let sym = state.symbol_table.function(flags, id, Some(name.as_ref()));

	Ok(ClosureRef { sym, index: id })
}

fn compile_function<S: HasModuleState, L: IntoIterator<Item = (u32, ValType)>>(
	has_state: &mut S,
	name: impl AsRef<str>,
	flags: u32,
	locals: L,
	signature: u32,
	f: impl FnOnce(&mut S, &mut InstructionSink, u32),
) -> ClosureRef where L::IntoIter: ExactSizeIterator {
	try_compile_function(has_state, name, flags, locals, signature, |s, seq, id| {
		f(s, seq, id);
		Ok::<_, std::convert::Infallible>(())
	}).unwrap()
}

pub(crate) trait IntoPushedValue {
	type State<'a>;
	fn push(self, state: Self::State<'_>, seq: &mut InstructionSink);
}

impl IntoPushedValue for value::Value {
	type State<'a> = ();
	fn push(self, (): Self::State<'_>, seq: &mut InstructionSink) {
		seq.i64_const(self.as_i64());
	}
}
