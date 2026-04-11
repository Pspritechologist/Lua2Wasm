use crate::{bytecode::Operation, object::InitPriorities, parsing::{Parsed, ParsedFunction}};
use crate::object::{ClosureRef, ModuleState, StringRef, InstructionSink, compile_function, linking::SymbolTab};
use wasm_encoder::{BlockType, ConstExpr, MemArg, NameMap, ValType};
use instructions::LuaInstSinkExt;

use anyhow::Result;

mod instructions;

struct LuaModuleState {
	module_state: ModuleState,

	strings: Box<[StringRef]>,
	closures: Box<[Option<ClosureRef>]>,
}

struct LuaFunctionState {
	local_arg_count: u32,
	local_last_shtack_ptr: u32,
	locals: std::collections::BTreeMap<u8, u32>,
	/// The distance from the last loop.
	loop_depth: u32,
}
impl LuaFunctionState {
	pub fn new(local_arg_count: u32, local_last_shtack_ptr: u32) -> Self {
		Self {
			local_arg_count,
			local_last_shtack_ptr,
			locals: Default::default(),
			loop_depth: 0,
		}
	}
}

impl super::HasModuleState for LuaModuleState {
	fn module_state(&mut self) -> &mut ModuleState { &mut self.module_state }
}

pub fn produce_lua_obj_file<'s>(module_name: impl AsRef<[u8]>, as_main: bool, parsed: Parsed<'s>) -> Result<wasm_encoder::Module> {
	let state = &mut LuaModuleState {
		module_state: ModuleState::new_module(),
		closures: vec![None; parsed.constants.closures().len() + 1].into_boxed_slice(),
		strings: Default::default(),
	};

	state.strings = {
		let mut offset = 0;
		parsed.constants.strings().iter().enumerate().map(|(i, s)| {
			let (addr, len) = (offset as u32, s.len() as u32);
			offset += s.len();
			state.module_state.data_sect.active(0, &ConstExpr::i32_const(addr.cast_signed()), s.iter().copied());
			let sym = state.module_state.symbol_table.lua_string(i as u32, len);
			StringRef { len, sym }
		}).collect()
	};

	// Compile all the functions in the file...
	for (i, func) in parsed.constants.closures().iter().enumerate().rev() {
		// 0 is reserved for the main function.
		state.closures[i + 1] = Some(compile_lua_function(state, func));
	}

	// As well as the file itself as a function...
	let mut main_fn = parsed.parsed_func;

	if let Some(debug) = main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_function(
		state,
		crate::object::ENTRY_POINT_NAME,
		if as_main { SymbolTab::WASM_SYM_BINDING_EMPTY } else { SymbolTab::WASM_SYM_BINDING_LOCAL },
		[(main_fn.frame_size.into(), ValType::I64)],
		state.module_state.dyn_call_ty,
		lua_function_compiler(&main_fn)
	);
	state.closures[0] = Some(main_fn);

	// Generate an initialization function that registers the module in the module table,
	// allowing it to be looked up by `require` implementations.
	
	let module_name = module_name.as_ref();
	let module_name_sym = state.module_state.add_data(SymbolTab::WASM_SYM_BINDING_LOCAL, "__camento_module_name", module_name.iter().copied());
	
	let reg_sig = state.module_state.types_sect.len();
	state.module_state.types_sect.ty().function([], []);
	let register_module_fn = compile_function(state, "__camento_register_module", SymbolTab::WASM_SYM_BINDING_LOCAL, [], reg_sig, |state, seq, _| {
		seq.push_function(&mut state.module_state, main_fn.sym)
		.global_get(state.module_state.module_table)
		.static_str(&mut state.module_state, module_name_sym, module_name.len().try_into().unwrap())
		.call(state.module_state.extern_fns.table_set_name);
	});
	
	state.module_state.init_fns.add(register_module_fn.sym, InitPriorities::REGISTER_MODULES);
	
	state.module_state.build_object()
}


fn compile_lua_function(state: &mut LuaModuleState, func: &ParsedFunction) -> ClosureRef {
	let name = func.debug.as_ref().and_then(|d| d.func_name()).unwrap_or("<anonymous closure>");
	
	compile_function(
		state,
		name,
		SymbolTab::WASM_SYM_BINDING_LOCAL,
		[(func.frame_size.into(), ValType::I64)],
		state.module_state.dyn_call_ty,
		lua_function_compiler(func)
	)
}

fn lua_function_compiler(func: &ParsedFunction) -> impl FnOnce(&mut LuaModuleState, &mut InstructionSink<'_>, u32) { |state, seq, current_fn| {
	// Zero for the arg count as the input param, 1 for the local where we store the shtack ptr.
	let mut f_state = LuaFunctionState::new(0, 1);

	{
		let locals_offset = 2; // Account for the argument slot at 0 and the previous shtack pointer at slot 1.

		let mut local_names = NameMap::new();
		local_names.append(0, "__arg_count");
		local_names.append(1, "__prev_shtack_ptr");
		
		for (i, slot) in (0..func.frame_size).enumerate() {
			let slot_index = i as u32 + locals_offset;
			f_state.locals.insert(slot, slot_index);
			if let Some(debug) = func.debug.as_ref() {
				local_names.append(slot_index, debug.get_local_name(slot).unwrap_or("<temp>"));
			}
		}

		state.module_state.local_names.append(current_fn, &local_names);
	}

	if func.param_count > 0 {
		seq
			// Get the number of arguments passed...
			.local_get(f_state.local_arg_count)
			// And the number of arguments expected...
			.i32_const(func.param_count.into())
			// ... And then both again
			.local_get(f_state.local_arg_count)
			.i32_const(func.param_count.into())
			// Check if the number of arguments passed is less than the number of arguments expected.
			.i32_lt_u()
			// Choose the lower value.
			.typed_select(ValType::I32)
			// And store it for later use.
			.local_set(f_state.local_arg_count);

		seq.block(BlockType::Empty);
		for i in 0..func.param_count {
			seq
				.local_get(f_state.local_arg_count)
				.i32_const(i.into())
				.i32_le_u()
				.br_if(0)
				.global_get(state.module_state.shtack_ptr)
				.i64_load(MemArg { align: 3, offset: u64::from(i) * 8, memory_index: state.module_state.shtack_mem })
				.local_set(f_state.locals[&i]);
		}
		seq.end();
	}

	seq.operations(state, &mut f_state, func.operations.iter().copied());

	if func.operations.last().is_none_or(|op| !matches!(op, Operation::Ret { .. })) {
		seq.i32_const(0).return_();
	}
} }
