use crate::object::{ModuleState, InstructionSink};
use wasm_encoder::{BlockType, MemArg, ValType};

pub mod external_bindings;
pub mod initialize;

trait SeqExt {
	fn arg_or_nil(&mut self, state: &mut ModuleState, arg_idx: u32) -> &mut Self;
}
impl SeqExt for InstructionSink<'_> {
	fn arg_or_nil(&mut self, state: &mut ModuleState, arg_idx: u32) -> &mut Self {
		let arg_cnt = 0;
		let shtack_ptr = 1;

		// Check if the argument index is within the count of arguments passed in.
		self.local_get(arg_cnt)
			.i32_const(arg_idx as i32)
			.i32_le_u()
			.if_(BlockType::Result(ValType::I64))
				// If not, return nil.
				.const_val(())
			.else_()
				// Otherwise, load the argument from the shtack.
				.local_get(shtack_ptr
				)
				.i64_load(MemArg { align: 3, offset: arg_idx.into(), memory_index: state.shtack_mem })
			.end()
	}
}

pub fn pcall(state: &mut ModuleState, seq: &mut InstructionSink, _: u32) {
	let arg_cnt = 0;
	let shtack_ptr = 1;
	let temp_var = 2;

	// Get the function to call from the first argument.
	seq.arg_or_nil(state, 0).local_set(temp_var);

	// In this case, the function passed in as the first argument acts as the function being called on the stack.

	seq.block(BlockType::Result(ValType::I64))
		.try_table(BlockType::Result(ValType::I64), state.error_tag, 0);

	// Write the 'try' block...
	seq
		// Pass the current arg count minus one, to a minimum of zero.
		.local_get(arg_cnt)
		.i32_eqz()
		.if_(BlockType::Result(ValType::I32))
			.i32_const(0)
		.else_()
			.i32_const(-1)
			.local_get(arg_cnt)
			.i32_add()
		.end()
		// Pass the shtack pointer, which is simply one Value increased.
		.local_get(shtack_ptr)
		.i32_const(8)
		.i32_add()
		// Make the call.
		.local_get(temp_var)
		.call_as_lua_fn(state)
		// Increase the count to return, accounting for the error flag.
		.i32_const(1)
		.i32_add()
		// Prepend the 'success' flag to the return values, overwriting where our first argument was.
		.local_get(shtack_ptr)
		.const_val(true)
		.i64_store(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem })
		// Return the new arg count, still on the stack.
		.return_()
		.end();

	seq.end();

	// Write the 'catch' block...
	seq
		// Store the error object for now.
		.local_set(temp_var)
		// Prepend the 'error' flag to the return values, overwriting where our first argument was.
		.local_get(shtack_ptr)
		.const_val(false)
		.i64_store(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem })
		// Set the error object as the second return value.
		.local_get(shtack_ptr)
		.local_get(temp_var)
		.i64_store(MemArg { align: 3, offset: 8, memory_index: state.shtack_mem })
		// Return the arg count of two.
		.i32_const(2)
		.return_();
}

pub fn error(state: &mut ModuleState, seq: &mut InstructionSink, _: u32) {
	seq.arg_or_nil(state, 0)
		.throw(state.error_tag)
		.i32_const(0);
}
