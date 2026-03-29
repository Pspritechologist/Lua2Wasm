use crate::{bytecode::Operation, parsing::{Parsed, ParsedFunction}};
use crate::object::{ClosureRef, ModuleState, StringRef, compile_function, linking::SymbolTab};
use wasm_encoder::{BlockType, ConstExpr, MemArg, NameMap, ValType};

use anyhow::Result;

mod instructions;

struct LuantModuleState {
	module_state: ModuleState,

	strings: Box<[StringRef]>,
	closures: Box<[Option<ClosureRef>]>,

	// Per-function data
	locals: std::collections::BTreeMap<u8, u32>,
	/// The distance from the last loop.
	loop_depth: u32,
}

impl super::HasModuleState for LuantModuleState {
	fn module_state(&mut self) -> &mut ModuleState { &mut self.module_state }
}

pub fn produce_lua_obj_file<'s>(parsed: Parsed<'s>) -> Result<Vec<u8>> {
	let mut state = LuantModuleState {
		module_state: ModuleState::new_module(),
		closures: vec![None; parsed.constants.closures().len() + 1].into_boxed_slice(),
		strings: Default::default(),
		locals: Default::default(),
		loop_depth: 0,
	};

	state.strings = {
		let mut offset = 0;
		parsed.constants.strings().iter().enumerate().map(|(i, s)| {
			let (addr, len) = (offset as u32, s.len() as u32);
			offset += s.len();
			state.module_state.data_sect.active(0, &ConstExpr::i32_const(addr.cast_signed()), s.iter().copied());
			let sym = state.module_state.symbol_table.luant_string(i as u32, len);
			StringRef { len, sym }
		}).collect()
	};

	// Compile all the functions in the file...
	for (i, func) in parsed.constants.closures().iter().enumerate().rev() {
		// 0 is reserved for the main function.
		state.closures[i + 1] = Some(compile_luant_function(&mut state, func));
	}

	// As well as the file itself as a function...
	let mut main_fn = parsed.parsed_func;

	if let Some(debug) = main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_luant_function(&mut state, &main_fn);
	state.closures[0] = Some(main_fn);

	Ok(state.module_state.build_object()?.finish())
}


fn compile_luant_function(state: &mut LuantModuleState, func: &ParsedFunction) -> ClosureRef {
	let name = func.debug.as_ref().and_then(|d| d.func_name()).unwrap_or("<anonymous closure>");
	
	compile_function(state, name, SymbolTab::WASM_SYM_BINDING_LOCAL, [(func.frame_size.into(), ValType::I64)], state.module_state.dyn_call_ty, |state, seq, current_fn| {
		let arg_cnt = 0;

		{
			let mut temps = 0;
			let mut name_buf = String::new();
			let mut local_names = NameMap::new();
			for (i, slot) in (0..func.frame_size).enumerate() {
				state.locals.insert(slot, i as u32 + 1); // Account for the argument slot at 0.
				if let Some(debug) = func.debug.as_ref() {
					local_names.append(i as u32 + 1, debug.get_local_name(slot).unwrap_or_else(|| {
						use std::fmt::Write;
						temps += 1;
						name_buf.clear();
						write!(&mut name_buf, "temp_{temps}").unwrap();
						&name_buf
					}));
				}
			}

			state.module_state.local_names.append(current_fn, &local_names);
		}

		if func.param_count > 0 {
			seq
				// Get the number of arguments passed...
				.local_get(arg_cnt)
				// And the number of arguments expected...
				.i32_const(func.param_count.into())
				// ... And then both again
				.local_get(arg_cnt)
				.i32_const(func.param_count.into())
				// Check if the number of arguments passed is less than the number of arguments expected.
				.i32_lt_u()
				// Choose the lower value.
				.typed_select(ValType::I32)
				// And store it for later use.
				.local_set(arg_cnt);

			seq.block(BlockType::Empty);
			for i in 0..func.param_count {
				seq
					.local_get(arg_cnt)
					.i32_const(i.into())
					.i32_le_u()
					.br_if(0)
					.global_get(state.module_state.shtack_ptr)
					.i64_load(MemArg { align: 3, offset: u64::from(i) * 8, memory_index: state.module_state.shtack_mem })
					.local_set(state.locals[&i]);
			}
			seq.end();
		}

		seq.operations(state, func.operations.iter().copied());

		if func.operations.last().is_none_or(|op| !matches!(op, Operation::Ret { .. })) {
			seq.i32_const(0).return_();
		}

		state.locals.clear();
	})
}
