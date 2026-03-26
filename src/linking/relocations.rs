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
	offset: usize,
	symbol: Symbol,
}

impl IntoIterator for RelocEntry {
	type Item = RelocEntry;
	type IntoIter = std::iter::Once<RelocEntry>;

	fn into_iter(self) -> Self::IntoIter {
		std::iter::once(self)
	}
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
	TableNumberLeb = 20, // R_WASM_TABLE_NUMBER_LEB
}

impl RelocEntry {
	fn new(type_: RelocType, symbol: Symbol) -> Self {
		Self { type_, offset: 1, symbol }
	}

	fn new_offset(type_: RelocType, offset: usize, symbol: Symbol) -> Self {
		Self { type_, offset, symbol }
	}

	/// Emitted immediately after a `call` instruction, `symbol` references the index of a function symbol.
	pub fn function(symbol: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call` opcode.
		Self::new(RelocType::FunctionIndexLeb, symbol)
	}

	/// Emitted immediately after an `i32.const` instruction representing the index of a function into a table, `func` references the index of a function symbol.
	pub fn i32const_indirect_fn(func: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self::new(RelocType::TableIndexSleb, func)
	}

	/// Emitted immediately after an `i32.const` instruction representing an address, `symbol` references the index of a data symbol.
	pub fn i32const_address(symbol: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self::new(RelocType::MemoryAddrSleb, symbol)
	}

	/// Emitted immediately after a `call_indirect` instruction, `type_index` references the index of a type in the WASM type section, *not* a symbol.
	pub fn indirect_call(type_index: u32, call_table: Symbol) -> [Self; 2] {
		// `10` is two varuint32 immediate values back, coming immediately after the `call_indirect` opcode, representing the type index.
		// `5` is the length of a varuint32 immediate value, coming immediately after the first one, representing the table index.
		// Type indexes aren't actually symbols...
		let type_ = Self::new_offset(RelocType::TypeIndexLeb, 6, Symbol::new(type_index));
		let table = Self::new(RelocType::TableNumberLeb, call_table);
		[type_, table]
	}

	/// Emitted immediately after a `global.get` or `global.set` instruction, `global` references the index of a global symbol.
	pub fn global(global: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `global.get` or `global.set` opcode.
		Self::new(RelocType::GlobalIndexLeb, global)
	}

	/// Emitted immediately after a 
	pub fn tag(tag: Symbol) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `tag` opcode.
		Self::new(RelocType::EventIndexLeb, tag)
	}
}

#[derive(Debug, Default)]
pub struct RelocSection {
	entries: Vec<RelocData>,
}

#[derive(Debug, Clone, Copy)]
struct RelocData {
	type_: RelocType,
	offset_pre_len: usize,
	symbol_index: u32,
}

impl RelocSection {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn append_entry(&mut self, section_len: usize, entry: RelocEntry) {
		let offset_pre_len = section_len + entry.offset;
		
		self.entries.push(RelocData {
			type_: entry.type_,
			offset_pre_len,
			symbol_index: entry.symbol.index(),
		});
	}

	pub fn finish<'a>(&'a self, name: &'a str, target_section_idx: u32, section_len_bytes_len: usize) -> impl Section + 'a {
		RelocSectionEncode { section: self, name, section_len_bytes_len, section_idx: target_section_idx }
	}
}

struct RelocSectionEncode<'a, 'b> {
	section: &'a RelocSection,
	name: &'b str,
	section_len_bytes_len: usize,
	section_idx: u32,
}

impl Encode for RelocSectionEncode<'_, '_> {
	fn encode(&self, sink: &mut Vec<u8>) {
		let mut buf = vec![];

		self.section_idx.encode(&mut buf);
		self.section.entries.len().encode(&mut buf);

		for reloc in &self.section.entries {
			buf.push(reloc.type_ as u8);
			(reloc.offset_pre_len + self.section_len_bytes_len).encode(&mut buf);
			reloc.symbol_index.encode(&mut buf);

			if reloc.type_ == RelocType::MemoryAddrSleb {
				// For R_WASM_MEMORY_ADDR_*, R_WASM_FUNCTION_OFFSET_I32, and R_WASM_SECTION_OFFSET_I32 relocations (and their 64-bit counterparts) the following field is additionally present:
				// addend	varint32	addend to add to the address
				0.encode(&mut buf)
			}
		}

		CustomSection {
			name: self.name.into(),
			data: buf.into(),
		}.encode(sink);
	}
}

impl Section for RelocSectionEncode<'_, '_> {
	fn id(&self) -> u8 { SectionId::Custom.into() }
}
