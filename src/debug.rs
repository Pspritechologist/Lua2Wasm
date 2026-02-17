#[derive(Debug, Clone)]
pub struct DebugInfo {
	func_name: Option<Box<str>>,
	func_span: usize,
	is_top_level: bool,
	src_map: SrcMap,
	locals: Vec<Box<str>>,
}

impl DebugInfo {
	pub fn new(
		src_map: SrcMap,
		locals: Vec<Box<str>>,
		name: Option<impl Into<Box<str>>>,
		span: usize,
		is_top_level: bool,
	) -> Self {
		Self { func_name: name.map(Into::into), func_span: span, is_top_level, src_map, locals }
	}

	pub fn func_name(&self) -> Option<&str> {
		self.func_name.as_deref()
	}
	pub fn set_func_name(&mut self, name: impl Into<Box<str>>) {
		self.func_name = Some(name.into());
	}

	pub fn is_top_level(&self) -> bool {
		self.is_top_level
	}

	pub fn src_map(&self) -> &SrcMap {
		&self.src_map
	}

	pub fn get_local_name(&self, slot: u8) -> Option<&str> {
		self.locals.get(slot as usize).map(|s| s.as_ref())
	}
}

#[derive(Debug, Clone, Default)]
pub struct SrcMap {
	map: Vec<usize>,
}

impl SrcMap {
	pub fn from_vec(map: Vec<usize>) -> Self {
		Self { map }
	}

	pub fn get(&self, addr: usize) -> Option<usize> {
		self.map.get(addr).copied()
	}
}
