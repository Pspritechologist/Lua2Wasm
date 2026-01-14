#[derive(Debug, Clone)]
pub struct DebugInfo {
	func_name: Option<Box<str>>,
	is_top_level: bool,
	src_map: SrcMap,
}

impl DebugInfo {
	pub fn new_file(src_map: SrcMap, path: Option<impl Into<Box<str>>>) -> Self {
		Self {
			func_name: path.map(Into::into),
			is_top_level: true,
			src_map,
		}
	}

	pub fn new_closure(func_name: Option<impl Into<Box<str>>>, src_map: SrcMap) -> Self {
		Self {
			func_name: func_name.map(Into::into),
			is_top_level: false,
			src_map,
		}
	}

	pub fn func_name(&self) -> Option<&str> {
		self.func_name.as_deref()
	}

	pub fn is_top_level(&self) -> bool {
		self.is_top_level
	}

	pub fn src_map(&self) -> &SrcMap {
		&self.src_map
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
