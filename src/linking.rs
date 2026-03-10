use wasm_encoder::{CustomSection, DataSymbolDefinition, Encode, Section, SectionId, SymbolTable};

mod relocations;
mod segment_info;

pub use relocations::*;

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
		self.data(SymbolTab::WASM_SYM_BINDING_LOCAL, "__luant_string", Some(DataSymbolDefinition { index, size, offset: 0 }))
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

	pub fn section(&mut self, idx: u32) -> u32 {
		let id = self.count;
		self.count += 1;
		
		SYMTAB_SECTION.encode(&mut self.bytes);
		SymbolTab::WASM_SYM_BINDING_LOCAL.encode(&mut self.bytes);
		idx.encode(&mut self.bytes);
		
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
