use wasm_encoder::{CustomSection, Encode, Section, SectionId};

pub type RelocCodeSection = RelocSection<RelocCodeSectionImpl>;
pub type RelocDataSection = RelocSection<RelocDataSectionImpl>;

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
impl Encode for RelocEntry {
	fn encode(&self, sink: &mut Vec<u8>) {
		sink.push(self.type_);
		self.over_length.encode(sink);
		self.index.encode(sink);
	}
}

impl RelocEntry {
	pub fn function(func: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call` opcode.
		Self { type_: 0, over_length: 5, index: func }
	}

	pub fn i32const_indirect_fn(func: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: 1, over_length: 5, index: func }
	}

	pub fn i32const_address(addr: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `i32.const` opcode.
		Self { type_: 4, over_length: 5, index: addr }
	}

	pub fn call_type(func_type: u32) -> Self {
		// `5` is the length of a varuint32 immediate value, coming immediately after the `call_indirect` opcode.
		Self { type_: 6, over_length: 5, index: func_type }
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

	pub fn append_entry(&mut self, section_len: usize, mut entry: RelocEntry) {
		entry.over_length = section_len - entry.over_length;
		entry.encode(&mut self.buf);
		dbg!(&self.buf);
		eprintln!();
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
