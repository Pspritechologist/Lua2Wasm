use wasm_encoder::{CustomSection, Encode, Section, SectionId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
	index: u32,
}
impl Symbol {
	pub fn new(index: u32) -> Self {
		Self { index }
	}

	pub fn index(&self) -> u32 {
		self.index
	}
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
	symbol: Symbol,
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
	pub fn function(symbol: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call` opcode.
		Self { type_: RelocType::FunctionIndexLeb, over_length: 5, symbol }
	}

	/// Emitted immediately after an `i32.const` instruction representing the index of a function into a table, `func` references the index of a function symbol.
	pub fn i32const_indirect_fn(func: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: RelocType::TableIndexSleb, over_length: 5, symbol: func }
	}

	/// Emitted immediately after an `i32.const` instruction representing an address, `symbol` references the index of a data symbol.
	pub fn i32const_address(symbol: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: RelocType::MemoryAddrSleb, over_length: 5, symbol }
	}

	/// Emitted immediately after a `call_indirect` instruction, `type_index` references the index of a type in the WASM type section, *not* a symbol.
	pub fn call_type(type_index: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call_indirect` opcode.
		// Type indexes aren't actually symbols...
		Self { type_: RelocType::TypeIndexLeb, over_length: 5, symbol: Symbol::new(type_index) }
	}

	/// Emitted immediately after a `global.get` or `global.set` instruction, `global` references the index of a global symbol.
	pub fn global(global: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `global.get` or `global.set` opcode.
		Self { type_: RelocType::GlobalIndexLeb, over_length: 5, symbol: global }
	}

	/// Emitted immediately after a 
	pub fn tag(tag: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `tag` opcode.
		Self { type_: RelocType::EventIndexLeb, over_length: 5, symbol: tag }
	}
}

#[derive(Debug, Default)]
pub struct RelocSection {
	buf: Vec<u8>,
	entries: u32,
}

impl RelocSection {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn append_entry(&mut self, section_len: usize, entry: RelocEntry) {
		let offset = section_len - entry.over_length;

		self.buf.push(entry.type_ as u8);
		offset.encode(&mut self.buf);
		entry.symbol.index.encode(&mut self.buf);

		if entry.type_ == RelocType::MemoryAddrSleb {
			// For R_WASM_MEMORY_ADDR_*, R_WASM_FUNCTION_OFFSET_I32, and R_WASM_SECTION_OFFSET_I32 relocations (and their 64-bit counterparts) the following field is additionally present:
			// addend	varint32	addend to add to the address
			0.encode(&mut self.buf)
		}

		self.entries += 1;
	}

	pub fn finish<'a>(&'a self, name: &'a str, target_section_idx: u32) -> impl Section + 'a {
		RelocSectionEncode { section: self, name, section_idx: target_section_idx }
	}
}

struct RelocSectionEncode<'a, 'b> {
	section: &'a RelocSection,
	name: &'b str,
	section_idx: u32,
}

impl Encode for RelocSectionEncode<'_, '_> {
	fn encode(&self, sink: &mut Vec<u8>) {
		let mut buf = vec![];

		self.section_idx.encode(&mut buf);
		self.section.entries.encode(&mut buf);
		buf.extend_from_slice(&self.section.buf);

		CustomSection {
			name: self.name.into(),
			data: buf.into(),
		}.encode(sink);
	}
}

impl Section for RelocSectionEncode<'_, '_> {
	fn id(&self) -> u8 { SectionId::Custom.into() }
}
