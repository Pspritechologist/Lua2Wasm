use crate::object::{IntoPushedValue, ModuleState, linking::{RelocEntry, Symbol}};
use super::InstructionSink;
use wasm_encoder::BlockType;

impl InstructionSink<'_> {
	pub fn push(&mut self, state: &mut ModuleState, value: impl IntoPushedValue) -> &mut Self {
		value.push(state, self);
		self
	}

	pub fn const_val(&mut self, value: impl Into<value::Value>) -> &mut Self {
		self.i64_const(value.into().as_i64())
	}

	pub fn static_str(&mut self, state: &mut ModuleState, symbol: Symbol, len: u32) -> &mut Self {
		self
			.push_addr(symbol)
			.i32_const(len.cast_signed())
			.call(state.extern_fns.static_str)
	}

	pub fn push_addr(&mut self, symbol: Symbol) -> &mut Self {
		self
			// We set the address to a placeholder value, MAX to ensure padding.
			.reloc(RelocEntry::i32const_address(symbol))
			.i32_const(i32::MAX)
	}

	pub fn block(&mut self, ty: BlockType) -> &mut Self {
		if let BlockType::FunctionType(ty_idx) = ty {
			return self.reloc(RelocEntry::typed_block(ty_idx))
				.block_raw(ty);
		}

		self.block_raw(ty)
	}

	pub fn loop_(&mut self, ty: BlockType) -> &mut Self {
		if let BlockType::FunctionType(ty_idx) = ty {
			return self.reloc(RelocEntry::typed_block(ty_idx))
				.loop_raw(ty);
		}

		self.loop_raw(ty)
	}

	pub fn if_(&mut self, ty: BlockType) -> &mut Self {
		if let BlockType::FunctionType(ty_idx) = ty {
			return self.reloc(RelocEntry::typed_block(ty_idx))
				.if_raw(ty);
		}

		self.if_raw(ty)
	}

	pub fn call(&mut self, symbol: Symbol) -> &mut Self {
		self.reloc(RelocEntry::function(symbol))
			.call_raw(u32::MAX)
	}

	pub fn call_indirect(&mut self, state: &mut ModuleState) -> &mut Self {
		self.reloc(RelocEntry::indirect_call(state.dyn_call_ty, state.call_tab))
			.call_indirect_raw(u32::MAX, u32::MAX)
	}

	pub fn call_as_lua_fn(&mut self, state: &mut ModuleState) -> &mut Self {
		self.call(state.extern_fns.get_fn)
			.call_indirect(state)
	}

	pub fn global_get(&mut self, symbol: Symbol) -> &mut Self {
		self.reloc(RelocEntry::global(symbol))
			.global_get_raw(u32::MAX)
	}

	pub fn global_set(&mut self, symbol: Symbol) -> &mut Self {
		self.reloc(RelocEntry::global(symbol))
			.global_set_raw(u32::MAX)
	}

	pub fn try_table(&mut self, ty: BlockType, tag: Symbol, label: u32) -> &mut Self {
		self.reloc(RelocEntry::catch_table(tag))
			.try_table_raw(ty, [wasm_encoder::Catch::One { tag: u32::MAX, label }])
	}

	pub fn throw(&mut self, tag_symbol: Symbol) -> &mut Self {
		self.reloc(RelocEntry::throw(tag_symbol))
			.throw_raw(u32::MAX)
	}

	pub fn push_function_ptr(&mut self, _state: &mut ModuleState, closure: Symbol) -> &mut Self {
		self.reloc(RelocEntry::i32const_indirect_fn(closure))
			.i32_const(i32::MAX)
	}

	fn reloc(&mut self, entries: impl IntoIterator<Item = RelocEntry>) -> &mut Self {
		let function_len = self.byte_len();
		for entry in entries {
			self.relocations.push(entry.offset_by(function_len));
		}
		self
	}
}
