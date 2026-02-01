use crate::vm::debug::SrcMap;

#[derive(Debug, Clone, Default)]
pub struct InfoCollector {
	src_map: Vec<usize>,
}

impl InfoCollector {
	pub fn emit(&mut self, src_index: usize) {
		self.src_map.push(src_index);
	}

	pub fn into_map(self) -> SrcMap {
		SrcMap::from_vec(self.src_map)
	}
}
