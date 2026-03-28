use wasm_encoder::{Function, ValType};

use crate::object::{State, instructions::InstructionSink, linking::RelocEntry};

pub struct FunctionBuilder {
	wasm_function: Function,
	relocations: Vec<RelocEntry>,
}

impl FunctionBuilder {
	pub fn new<L: IntoIterator<Item = (u32, ValType)>>(locals: L) -> Self
	where L::IntoIter: ExactSizeIterator {
		Self {
			wasm_function: Function::new(locals),
			relocations: vec![],
		}
	}

	pub fn sink(&mut self) -> InstructionSink<'_> {
		InstructionSink::new(&mut self.wasm_function, &mut self.relocations)
	}
	
	pub fn function(&self) -> &Function {
		&self.wasm_function
	}
	pub fn function_mut(&mut self) -> &mut Function {
		&mut self.wasm_function
	}

	pub fn finish(self, state: &mut State) {
		let function_size = self.wasm_function.byte_len();

		state.code_sect.function(&self.wasm_function);

		let post_encode_size = state.code_sect.byte_len();

		let code_section_len = post_encode_size - function_size;

		state.reloc_code_sect.append_entries(
			self.relocations.into_iter().map(|e| e.offset_by(code_section_len))
		);
	}
}
