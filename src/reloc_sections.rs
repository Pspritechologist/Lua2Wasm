use wasm_encoder::{CustomSection, DataSymbolDefinition, Encode, Section, SectionId, SymbolTable};

pub type RelocCodeSection = RelocSection<RelocCodeSectionImpl>;
pub type RelocDataSection = RelocSection<RelocDataSectionImpl>;

#[derive(Debug, Default)]
pub struct SymbolTab {
	inner: SymbolTable,
	count: u32,
}
impl SymbolTab {
	pub fn new() -> Self {
		Self::default()
	}
	
	pub fn function(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.inner.function(flags, index, name);
		id
	}

	pub fn global(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.inner.global(flags, index, name);
		id
	}

	pub fn table(&mut self, flags: u32, index: u32, name: Option<&str>) -> u32 {
		let id = self.count;
		self.inner.table(flags, index, name);
		id
	}

	pub fn string(&mut self, index: u32, size: u32) -> u32 {
		let id = self.count;
		self.inner.data(SymbolTable::WASM_SYM_BINDING_LOCAL, "__luant_string", Some(DataSymbolDefinition { index, size, offset: 0 }));
		id
	}

	pub fn inner(&self) -> &SymbolTable {
		&self.inner
	}

	//? Copied from [`SymbolTable`] for convenience.

	/// This is a weak symbol.
	///
	/// This flag is mutually exclusive with `WASM_SYM_BINDING_LOCAL`.
	///
	/// When linking multiple modules defining the same symbol, all weak
	/// definitions are discarded if any strong definitions exist; then if
	/// multiple weak definitions exist all but one (unspecified) are discarded;
	/// and finally it is an error if more than one definition remains.
	pub const WASM_SYM_BINDING_WEAK: u32 = 0x1;

	/// This is a local symbol.
	///
	/// This flag is mutually exclusive with `WASM_SYM_BINDING_WEAK`.
	///
	/// Local symbols are not to be exported, or linked to other
	/// modules/sections. The names of all non-local symbols must be unique, but
	/// the names of local symbols are not considered for uniqueness. A local
	/// function or global symbol cannot reference an import.
	pub const WASM_SYM_BINDING_LOCAL: u32 = 0x02;

	/// This is a hidden symbol.
	///
	/// Hidden symbols are not to be exported when performing the final link,
	/// but may be linked to other modules.
	pub const WASM_SYM_VISIBILITY_HIDDEN: u32 = 0x04;

	/// This symbol is not defined.
	///
	/// For non-data symbols, this must match whether the symbol is an import or
	/// is defined; for data symbols, determines whether a segment is specified.
	pub const WASM_SYM_UNDEFINED: u32 = 0x10;

	/// This symbol is intended to be exported from the wasm module to the host
	/// environment.
	///
	/// This differs from the visibility flags in that it effects the static
	/// linker.
	pub const WASM_SYM_EXPORTED: u32 = 0x20;

	/// This symbol uses an explicit symbol name, rather than reusing the name
	/// from a wasm import.
	///
	/// This allows it to remap imports from foreign WebAssembly modules into
	/// local symbols with different names.
	pub const WASM_SYM_EXPLICIT_NAME: u32 = 0x40;

	/// This symbol is intended to be included in the linker output, regardless
	/// of whether it is used by the program.
	pub const WASM_SYM_NO_STRIP: u32 = 0x80;
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
	type_: u8,
	over_length: usize,
	index: u32,
}

impl RelocEntry {
	pub fn function(symbol: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call` opcode.
		Self { type_: 0, over_length: 5, index: symbol }
	}

	pub fn i32const_indirect_fn(func: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: 1, over_length: 5, index: func }
	}

	pub fn i32const_address(symbol: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: 4, over_length: 5, index: symbol }
	}

	pub fn call_type(type_index: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call_indirect` opcode.
		Self { type_: 6, over_length: 5, index: type_index }
	}

	pub fn global(global: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `global.get` or `global.set` opcode.
		Self { type_: 7, over_length: 5, index: global }
	}

	pub fn tag(tag: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `tag` opcode.
		Self { type_: 10, over_length: 5, index: tag }
	}
}


pub fn encode_padded_usize(val: usize, sink: &mut Vec<u8>) {
	assert!(val <= u32::MAX as usize);
	encode_padded_u32(val as u32, sink);
}

pub fn encode_padded_u32(num: u32, sink: &mut Vec<u8>) {
	let (mut value, _pos) = leb128fmt::encode_u32(num).unwrap();
	value.iter_mut().rev().skip(1).rev().for_each(|b| *b |= 0x80);
	debug_assert_eq!(Some((num, value.len())), leb128fmt::decode_u32(value));
	sink.extend_from_slice(&value);
}

pub fn encode_padded_i32(num: i32, sink: &mut Vec<u8>) {
	let (mut value, _pos) = leb128fmt::encode_s32(num).unwrap();
	value.iter_mut().rev().skip(1).rev().for_each(|b| *b |= 0x80);
	debug_assert_eq!(Some((num, value.len())), leb128fmt::decode_s32(value));
	sink.extend_from_slice(&value);
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

		self.buf.push(entry.type_);
		encode_padded_usize(offset, &mut self.buf);
		encode_padded_u32(entry.index, &mut self.buf);

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
