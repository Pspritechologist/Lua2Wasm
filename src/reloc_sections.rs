use wasm_encoder::{CustomSection, DataSymbolDefinition, Encode, Section, SectionId, SymbolTable};

pub type RelocCodeSection = RelocSection<RelocCodeSectionImpl>;
pub type RelocDataSection = RelocSection<RelocDataSectionImpl>;

#[derive(Debug, Clone)]
pub struct LinkingSection {
	bytes: Vec<u8>,
}

impl LinkingSection {
	pub fn new() -> Self {
		let mut bytes = Vec::new();
		2u32.encode(&mut bytes); // Version.
		Self { bytes }
	}

	pub fn symbol_table(&mut self, symbol_table: &SymbolTab) -> &mut Self {
		symbol_table.encode(&mut self.bytes);
		self
	}
}

impl Encode for LinkingSection {
	fn encode(&self, sink: &mut Vec<u8>) {
		CustomSection {
			name: "linking".into(),
			data: (&self.bytes).into(),
		}.encode(sink);
	}
}

impl Section for LinkingSection {
	fn id(&self) -> u8 {
		SectionId::Custom.into()
	}
}

#[derive(Debug)]
pub struct SymbolTab {
	bytes: Vec<u8>,
	count: u32,
}

impl Encode for SymbolTab {
    fn encode(&self, sink: &mut Vec<u8>) {
        sink.push(8); // WASM_SYMBOL_TABLE.
		(leb128fmt::encode_u32(self.count).unwrap().1 + self.bytes.len()).encode(sink);
		self.count.encode(sink);
		sink.extend(&self.bytes);
    }
}

const SYMTAB_FUNCTION: u32 = 0;
const SYMTAB_DATA: u32 = 1;
const SYMTAB_GLOBAL: u32 = 2;
const SYMTAB_SECTION: u32 = 3;
const SYMTAB_TAG: u32 = 4;
const SYMTAB_TABLE: u32 = 5;

impl SymbolTab {
	pub fn new() -> Self {
		Self { bytes: Vec::new(), count: 0 }
	}
	
	pub fn function(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.count += 1;

		SYMTAB_FUNCTION.encode(&mut self.bytes);
		flags.encode(&mut self.bytes);
		index.encode(&mut self.bytes);
		if let Some(name) = name {
			name.encode(&mut self.bytes);
		}

		id
	}

	pub fn data(&mut self, flags: u32, name: &str, def: Option<DataSymbolDefinition>) -> u32 {
		let id = self.count;
		self.count += 1;
		
		SYMTAB_DATA.encode(&mut self.bytes);
		flags.encode(&mut self.bytes);
		name.encode(&mut self.bytes);
		if let Some(def) = def {
			def.index.encode(&mut self.bytes);
			def.offset.encode(&mut self.bytes);
			def.size.encode(&mut self.bytes);
		}
		
		id
	}

	pub fn string(&mut self, index: u32, size: u32) -> u32 {
		self.data(SymbolTable::WASM_SYM_BINDING_LOCAL, "__luant_string", Some(DataSymbolDefinition { index, size, offset: 0 }))
	}

	pub fn global(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.count += 1;
		
		SYMTAB_GLOBAL.encode(&mut self.bytes);
		flags.encode(&mut self.bytes);
		index.encode(&mut self.bytes);
		if let Some(name) = name {
			name.encode(&mut self.bytes);
		}
		
		id
	}

	pub fn section(&mut self, section_id: SectionId) -> u32 {
		let id = self.count;
		self.count += 1;
		
		SYMTAB_SECTION.encode(&mut self.bytes);
		SymbolTable::WASM_SYM_BINDING_LOCAL.encode(&mut self.bytes); // Flags.
		section_id.encode(&mut self.bytes);
		
		id
	}

	pub fn tag(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.count += 1;
		
		SYMTAB_TAG.encode(&mut self.bytes);
		flags.encode(&mut self.bytes);
		index.encode(&mut self.bytes);
		if let Some(name) = name {
			name.encode(&mut self.bytes);
		}
		
		id
	}

	pub fn table(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.count += 1;
		
		SYMTAB_TABLE.encode(&mut self.bytes);
		flags.encode(&mut self.bytes);
		index.encode(&mut self.bytes);
		if let Some(name) = name {
			name.encode(&mut self.bytes);
		}
		
		id
	}

	//? Forwarded from [`SymbolTable`] for convenience.

	/// This is a weak symbol.
	///
	/// This flag is mutually exclusive with `WASM_SYM_BINDING_LOCAL`.
	///
	/// When linking multiple modules defining the same symbol, all weak
	/// definitions are discarded if any strong definitions exist; then if
	/// multiple weak definitions exist all but one (unspecified) are discarded;
	/// and finally it is an error if more than one definition remains.
	pub const WASM_SYM_BINDING_WEAK: u32 = SymbolTable::WASM_SYM_BINDING_WEAK;

	/// This is a local symbol.
	///
	/// This flag is mutually exclusive with `WASM_SYM_BINDING_WEAK`.
	///
	/// Local symbols are not to be exported, or linked to other
	/// modules/sections. The names of all non-local symbols must be unique, but
	/// the names of local symbols are not considered for uniqueness. A local
	/// function or global symbol cannot reference an import.
	pub const WASM_SYM_BINDING_LOCAL: u32 = SymbolTable::WASM_SYM_BINDING_LOCAL;

	/// This is a hidden symbol.
	///
	/// Hidden symbols are not to be exported when performing the final link,
	/// but may be linked to other modules.
	pub const WASM_SYM_VISIBILITY_HIDDEN: u32 = SymbolTable::WASM_SYM_VISIBILITY_HIDDEN;

	/// This symbol is not defined.
	///
	/// For non-data symbols, this must match whether the symbol is an import or
	/// is defined; for data symbols, determines whether a segment is specified.
	pub const WASM_SYM_UNDEFINED: u32 = SymbolTable::WASM_SYM_UNDEFINED;

	/// This symbol is intended to be exported from the wasm module to the host
	/// environment.
	///
	/// This differs from the visibility flags in that it effects the static
	/// linker.
	pub const WASM_SYM_EXPORTED: u32 = SymbolTable::WASM_SYM_EXPORTED;

	/// This symbol uses an explicit symbol name, rather than reusing the name
	/// from a wasm import.
	///
	/// This allows it to remap imports from foreign WebAssembly modules into
	/// local symbols with different names.
	pub const WASM_SYM_EXPLICIT_NAME: u32 = SymbolTable::WASM_SYM_EXPLICIT_NAME;

	/// This symbol is intended to be included in the linker output, regardless
	/// of whether it is used by the program.
	pub const WASM_SYM_NO_STRIP: u32 = SymbolTable::WASM_SYM_NO_STRIP;
}

pub trait RelocSectionImpl {
	const SECTION_NAME: &'static str;
	const TARGET_SECTION: SectionId;
}

/// The various functions to create relocation entries assume the offset given is the *current length* of the given
/// section (code or data), and that the item being referenced (be it an i32 const, fn index, etc.) is the last thing
/// added to that section. The calculation for getting the targeted bytes from the end of the section will be done automatically.
/// 
/// [`Self::type_`] refers to the type of relocation entry, which is determined by the WebAssembly spec.\
/// [`Self::over_length`] is how many bytes to go *bacK* from the end of the section's current length to find the start of the byte offset.
/// [`Self::index`] is the index of the item being referenced (e.g. function index, global index, etc.).
#[derive(Debug, Clone, Copy)]
pub struct RelocEntry {
	type_: RelocType,
	over_length: usize,
	index: u32,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RelocType {
	FunctionIndexLeb = 0, // R_WASM_FUNCTION_INDEX_LEB
	TableIndexSleb = 1, // R_WASM_TABLE_INDEX_SLEB
	MemoryAddrSleb = 4, // R_WASM_MEMORY_ADDR_SLEB
	TypeIndexLeb = 6, // R_WASM_TYPE_INDEX_LEB
	GlobalIndexLeb = 7, // R_WASM_GLOBAL_INDEX_LEB
	EventIndexLeb = 10, // R_WASM_EVENT_INDEX_LEB
}

impl RelocEntry {
	/// Emitted immediately after a `call` instruction, `symbol` references the index of a function symbol.
	pub fn function(symbol: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call` opcode.
		Self { type_: RelocType::FunctionIndexLeb, over_length: 5, index: symbol }
	}

	/// Emitted immediately after an `i32.const` instruction representing the index of a function into a table, `func` references the index of a function symbol.
	pub fn i32const_indirect_fn(func: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: RelocType::TableIndexSleb, over_length: 5, index: func }
	}

	/// Emitted immediately after an `i32.const` instruction representing an address, `symbol` references the index of a data symbol.
	pub fn i32const_address(symbol: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: RelocType::MemoryAddrSleb, over_length: 5, index: symbol }
	}

	/// Emitted immediately after a `call_indirect` instruction, `type_index` references the index of a type in the WASM type section, *not* a symbol.
	pub fn call_type(type_index: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call_indirect` opcode.
		Self { type_: RelocType::TypeIndexLeb, over_length: 5, index: type_index }
	}

	/// Emitted immediately after a `global.get` or `global.set` instruction, `global` references the index of a global symbol.
	pub fn global(global: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `global.get` or `global.set` opcode.
		Self { type_: RelocType::GlobalIndexLeb, over_length: 5, index: global }
	}

	/// Emitted immediately after a 
	pub fn tag(tag: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `tag` opcode.
		Self { type_: RelocType::EventIndexLeb, over_length: 5, index: tag }
	}
}

#[derive(Debug, Default)]
pub struct RelocSection<S: RelocSectionImpl + Default> {
	buf: Vec<u8>,
	entries: u32,
	_pd: std::marker::PhantomData<S>,
}

impl<S: RelocSectionImpl + Default> RelocSection<S> {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn append_entry(&mut self, section_len: usize, entry: RelocEntry) {
		let offset = section_len - entry.over_length;

		self.buf.push(entry.type_ as u8);
		offset.encode(&mut self.buf);
		entry.index.encode(&mut self.buf);

		if entry.type_ == RelocType::MemoryAddrSleb {
			// For R_WASM_MEMORY_ADDR_*, R_WASM_FUNCTION_OFFSET_I32, and R_WASM_SECTION_OFFSET_I32 relocations (and their 64-bit counterparts) the following field is additionally present:
			// addend	varint32	addend to add to the address
			0.encode(&mut self.buf)
		}

		self.entries += 1;
	}
}

impl<S: RelocSectionImpl + Default> Encode for RelocSection<S> {
	fn encode(&self, sink: &mut Vec<u8>) {
		let mut buf = vec![];
		(u8::from(S::TARGET_SECTION) as usize).encode(&mut buf);
		self.entries.encode(&mut buf);
		
		buf.extend_from_slice(&self.buf);

		CustomSection {
			name: S::SECTION_NAME.into(),
			data: buf.into(),
		}.encode(sink);
	}
}

impl<S: RelocSectionImpl + Default> Section for RelocSection<S> {
	fn id(&self) -> u8 { SectionId::Custom.into() }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RelocDataSectionImpl;
impl RelocSectionImpl for RelocDataSectionImpl {
	const SECTION_NAME: &'static str = "reloc.DATA";
	const TARGET_SECTION: SectionId = SectionId::Data;
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RelocCodeSectionImpl;
impl RelocSectionImpl for RelocCodeSectionImpl {
	const SECTION_NAME: &'static str = "reloc.CODE";
	const TARGET_SECTION: SectionId = SectionId::Code;
}
