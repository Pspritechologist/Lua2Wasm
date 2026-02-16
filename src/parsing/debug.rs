use crate::debug::{DebugInfo, SrcMap};
use luant_lexer::{IdentKey, LexInterner};

#[derive(Debug, Clone, Default)]
pub struct InfoCollector {
	src_map: Vec<usize>,
	locals: Vec<IdentKey>,
}

impl InfoCollector {
	pub fn emit(&mut self, src_index: usize) {
		self.src_map.push(src_index);
	}

	pub fn add_local(&mut self, ident: IdentKey) {
		self.locals.push(ident);
	}

	pub fn into_debug_info(self, interner: &LexInterner, name: Option<impl Into<Box<str>>>, span: usize, is_top_level: bool) -> DebugInfo {
		DebugInfo::new(
			SrcMap::from_vec(self.src_map),
			self.locals.into_iter().map(|i| interner.resolve_ident(i).to_owned().into_boxed_str()).collect(),
			name,
			span,
			is_top_level,
		)
	}
}
