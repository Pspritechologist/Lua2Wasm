use wasm_encoder::ValType;

use crate::object::{
	ModuleState, StringRef, InitPriorities, runtime_impls, compile_function,
	instructions::InstructionSink,
	linking::SymbolTab,
};

pub fn generate_runtime_object() -> Vec<u8> {
	let state = &mut ModuleState::new_module();

	let is_init = state.new_global(SymbolTab::WASM_SYM_BINDING_LOCAL, Some("__luant_is_init"), ValType::I32, &wasm_encoder::ConstExpr::i32_const(0));

	super::external_bindings::compile_supporting_functions(state);

	let internal_strings = {
		#[derive(Debug, Clone, Copy)]
		struct Strings {
			pub error: StringRef,
			pub pcall: StringRef,
		}

		let error = StringRef {
			sym: state.add_data(SymbolTab::WASM_SYM_BINDING_LOCAL, "error", *b"error"),
			len: 5,
		};
		let pcall = StringRef {
			sym: state.add_data(SymbolTab::WASM_SYM_BINDING_LOCAL, "pcall", *b"pcall"),
			len: 5,
		};

		Strings {
			error,
			pcall,
		}
	};

	//? Relocations *must* be emitted in order, so we cannot compile these functions as
	//? we assign them to the global table.
	let mut std_fn = |name: &str, locals, f: fn(&mut ModuleState, &mut InstructionSink, u32)| {
		crate::object::compile_function(state, name, SymbolTab::WASM_SYM_BINDING_LOCAL, [(locals, ValType::I64)], state.dyn_call_ty, f)
	};

	let std_error = std_fn("__luant_std_error", 0, runtime_impls::error);
	let std_pcall = std_fn("__luant_std_pcall", 1, runtime_impls::pcall);

	let sig = state.types_sect.len();
	state.types_sect.ty().function([], []);
	let init_fn = compile_function(state, "__luant_init_rt", SymbolTab::WASM_SYM_BINDING_LOCAL, [(1, ValType::I64)], sig, |state, seq, _| {
		let global_tab = 0;

		// Check if we're already initialized, and if so return early.
		seq.global_get(is_init)
			.if_(wasm_encoder::BlockType::Empty)
			.return_()
			.end()
			.i32_const(1)
			.global_set(is_init);

		// Initialize the module lookup table.
		seq.call(state.extern_fns.table_load)
			.global_set(state.module_table);

		// Initialize the global table.
		seq.call(state.extern_fns.table_load)
			.local_tee(global_tab)
			.global_set(state.global_table);

		let mut add_fn = |name: StringRef, func| {
			seq.push_function(state, func)
				.local_get(global_tab)
				.static_str(state, name.sym, name.len)
				.call(state.extern_fns.table_set_name);
		};

		// Load the 'error' function into it.
		add_fn(internal_strings.error, std_error.sym);
		// Load the 'pcall' function into it.
		add_fn(internal_strings.pcall, std_pcall.sym);
	});

	state.init_fns.add(init_fn.sym, InitPriorities::INIT_RUNTIME);

	state.build_object().unwrap().finish()
}
