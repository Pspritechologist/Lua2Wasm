use crate::{State, InstructionSink};
use wasm_encoder::{BlockType, Catch, MemArg, ValType};

pub fn pcall(state: &mut State, seq: &mut InstructionSink, _: u32) {
	let arg_cnt = 0;
	let temp_var = 1;

	seq.global_get(state, state.shtack_ptr)
		// .load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 3, offset: 0 })
		.i64_load(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem }) //TODO: This assumes the first arg is null if not provided...
		.local_set(temp_var);

	// Increase the stack pointer by one to remove the first arg.
	seq.global_get(state, state.shtack_ptr)
		.i32_const(1)
		.i32_add()
		.global_set(state, state.shtack_ptr);

	seq.block(BlockType::Result(ValType::I64))
		.try_table(BlockType::Result(ValType::I64), [Catch::One { tag: 0, label: 0 }]);

	// Write the 'try' block...
	seq
		// Pass the current arg count minus one, to a minimum of zero.
		.i32_const(0)
		.i32_const(1)
		.local_get(arg_cnt)
		.i32_sub()
		.i32_const(0)
		.local_get(arg_cnt)
		.i32_eq()
		.typed_select(ValType::I32)
		// Make the call.
		.local_get(temp_var)
		.call(state, state.extern_fns.get_fn)
		.call_indirect(state, state.dyn_call_ty)
		// Increase the count to return, accounting for the error flag.
		.i32_const(1)
		.i32_add()
		// Reset the shtack pointer.
		.global_get(state, state.shtack_ptr)
		.i32_const(1)
		.i32_sub()
		.global_set(state, state.shtack_ptr)
		// And prepend the 'success' flag to the return values.
		.global_get(state, state.shtack_ptr)
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
		// Reset the shtack pointer.
		.global_get(state, state.shtack_ptr)
		.i32_const(1)
		.i32_sub()
		.global_set(state, state.shtack_ptr)
		// Prepend the 'error' flag to the return values.
		.global_get(state, state.shtack_ptr)
		.const_val(false)
		.i64_store(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem })
		// Set the error object as the second return value.
		.global_get(state, state.shtack_ptr)
		.local_get(temp_var)
		.i64_store(MemArg { align: 3, offset: 8, memory_index: state.shtack_mem })
		// Return the arg count of two.
		.i32_const(2)
		.return_();
}

pub fn error(state: &mut State, seq: &mut InstructionSink, _: u32) {
	seq.global_get(state, state.shtack_ptr)
		// .load(state.shtack_mem, LoadKind::I64 { atomic: false }, MemArg { align: 3, offset: 0 })
		.i64_load(MemArg { align: 3, offset: 0, memory_index: state.shtack_mem }) //TODO: This assumes the first arg is null if not provided...
		.throw(0)
		.i32_const(0);
}
