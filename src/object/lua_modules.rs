use crate::{bytecode::Operation, object::InitPriorities, parsing::{Parsed, ParsedFunction, Upvalue}};
use crate::object::{ClosureRef, ModuleState, StringRef, InstructionSink, compile_function, linking::SymbolTab};
use wasm_encoder::{BlockType, ConstExpr, MemArg, NameMap, ValType};
use instructions::LuaInstSinkExt;

use anyhow::Result;

mod instructions;

struct LuaModuleState {
	module_state: ModuleState,

	strings: Box<[StringRef]>,
	closures: Box<[Option<(ClosureRef, Box<[Upvalue]>)>]>,
}

struct LuaFunctionState {
	/// The number of arguments passed into this function call.
	local_arg_count: u32,
	/// A pointer to the base of the shtack frame for this function.
	local_shtack_base: u32,
	/// A pointer to the current function being executed in the shtack.\
	/// In the typical calling convention this would be `local_shtack_base - 8`.
	local_shtack_fn: u32,
	/// A pointer to the base of the varargs for this function, immediately after the expected arguments.
	local_vaarg_base: u32,
	/// The count of arguments passed into this function exceeding the expected arguments.
	local_vaarg_count: u32,
	/// The total size of the frame for this function, including arguments and captured variables.
	local_frame_size: u32,

	locals: std::collections::BTreeMap<u8, u32>,
	/// The distance from the last loop.
	loop_depth: u32,
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

	let mut constants = parsed.constants;
	// Compile all the functions in the file...
	for (i, func) in constants.take_closures().into_iter().enumerate().rev() {
		// 0 is reserved for the main function.
		let vaa = compile_lua_function(state, &func);
		state.closures[i + 1] = Some((vaa, func.upvalues));
	}

	// As well as the file itself as a function...
	let mut parsed_main_fn = parsed.parsed_func;

	if let Some(debug) = parsed_main_fn.debug.as_mut() && debug.func_name().is_none() {
		debug.set_func_name("<main>");
	}

	let main_fn = compile_lua_function_ext(
		state,
		&parsed_main_fn,
		crate::object::ENTRY_POINT_NAME,
		if as_main { SymbolTab::WASM_SYM_BINDING_EMPTY } else { SymbolTab::WASM_SYM_BINDING_LOCAL },
	);
	state.closures[0] = Some((main_fn, parsed_main_fn.upvalues));

	// Generate an initialization function that registers the module in the module table,
	// allowing it to be looked up by `require` implementations.
	
	let module_name = module_name.as_ref();
	let module_name_sym = state.module_state.add_data(SymbolTab::WASM_SYM_BINDING_LOCAL, "__camento_module_name", module_name.iter().copied());
	
	let reg_sig = state.module_state.types_sect.len();
	state.module_state.types_sect.ty().function([], []);
	let register_module_fn = compile_function(state, "__camento_register_module", SymbolTab::WASM_SYM_BINDING_LOCAL, [], reg_sig, |state, seq, _| {
		//TODO: This hardcodes that every function loaded uses the global table, and that the table will always be at shtack slot 0.
		//TODO: This is fine for `require`, for example, but disallows reimplementing a version of
		//TODO: `dofile` in order to allow setting custom envs for loaded chunks.
		seq.push_function_ptr(&mut state.module_state, main_fn.sym)
			.i32_const(0)
			.call(state.module_state.extern_fns.new_main_closure)
			.global_get(state.module_state.module_table)
			.static_str(&mut state.module_state, module_name_sym, module_name.len().try_into().unwrap())
			.call(state.module_state.extern_fns.table_set_name);
	});
	
	state.module_state.init_fns.add(register_module_fn.sym, InitPriorities::REGISTER_MODULES);
	
	state.module_state.build_object()
}


fn compile_lua_function(state: &mut LuaModuleState, func: &ParsedFunction) -> ClosureRef {
	let name = func.debug.as_ref().and_then(|d| d.func_name()).unwrap_or("<anonymous closure>");
	compile_lua_function_ext(state, func, name, SymbolTab::WASM_SYM_BINDING_LOCAL)
}

fn compile_lua_function_ext(state: &mut LuaModuleState, func: &ParsedFunction, name: &str, flags: u32) -> ClosureRef {
	compile_function(
		state,
		name,
		flags,
		[
			(4, ValType::I32), // Our locals used.
			(func.frame_size.into(), ValType::I64),
		],
		state.module_state.dyn_call_ty,
		lua_function_compiler(func)
	)
}

fn lua_function_compiler(func: &ParsedFunction) -> impl FnOnce(&mut LuaModuleState, &mut InstructionSink<'_>, u32) { |state, seq, current_fn| {
	let mut f_state = LuaFunctionState {
		local_arg_count: 0,
		local_shtack_base: 1,
		local_shtack_fn: 2,
		local_vaarg_base: 3,
		local_vaarg_count: 4,
		local_frame_size: 5,
		locals: Default::default(),
		loop_depth: 0,
	};

	{
		let locals_offset = 6; // Account for the two arguments, and the two locals.

		let mut local_names = NameMap::new();
		local_names.append(0, "__arg_count");
		local_names.append(1, "__shtack_ptr");
		local_names.append(2, "__current_fn"); //TODO: We should cache this, or accept it as a param.
		local_names.append(3, "__vaarg_base");
		local_names.append(4, "__vaarg_count");
		local_names.append(5, "__frame_size");
		
		for (i, slot) in (0..func.frame_size).enumerate() {
			let slot_index = i as u32 + locals_offset;
			f_state.locals.insert(slot, slot_index);
			if let Some(debug) = func.debug.as_ref() {
				local_names.append(slot_index, debug.get_local_name(slot).unwrap_or("<temp>"));
			}
		}

		state.module_state.local_names.append(current_fn, &local_names);
	}

	// Fill in our locals.
	seq.local_get(f_state.local_shtack_base)
		.i32_const(8)
		.i32_sub()
		.local_set(f_state.local_shtack_fn);
	seq.local_get(f_state.local_shtack_base)
		.local_get(f_state.local_arg_count)
		.i32_add()
		.local_set(f_state.local_vaarg_base);
	seq.local_get(f_state.local_arg_count)
		.i32_const(func.param_count.into())
		.i32_lt_u()
		.if_(BlockType::Result(ValType::I32))
			.i32_const(0)
		.else_()
			.local_get(f_state.local_arg_count)
			.i32_const(func.param_count.into())
			.i32_sub()
		.end()
		.local_set(f_state.local_vaarg_count);
	seq.i32_const(func.captures.into())
		.local_get(f_state.local_arg_count)
		.i32_add()
		.local_set(f_state.local_frame_size);

	if func.param_count > 0 {
		seq.block(BlockType::Empty);
		for i in 0..func.param_count {
			seq
				.local_get(f_state.local_arg_count)
				.i32_const(i.into())
				.i32_le_u()
				.br_if(0)
				.local_get(f_state.local_shtack_base)
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
