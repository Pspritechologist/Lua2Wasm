
pub fn generate_runtime_object() -> Vec<u8> {
	runtime_impls::external_bindings::compile_supporting_functions(&mut state);


	let internal_strings = {
		#[derive(Debug, Clone, Copy)]
		struct Strings {
			pub error: usize,
			pub pcall: usize,
		}

		let error = parsed.constants.get_string("error");
		let pcall = parsed.constants.get_string("pcall");

		Strings { error, pcall }
	};
	

	let init_fn = {
		let state = &mut state;

		//? Relocations *must* be emitted in order, so we cannot compile these functions as
		//? we assign them to the global table.
		let mut std_fn = |name: &str, locals, f: fn(&mut State, &mut InstructionSink, u32)| {
			compile_function(state, name, SymbolTab::WASM_SYM_BINDING_LOCAL, [(locals, ValType::I64)], state.dyn_call_ty, f)
		};

		let std_error = std_fn("__luant_std_error", 0, runtime_impls::error);
		let std_pcall = std_fn("__luant_std_pcall", 1, runtime_impls::pcall);

		let mut builder = FunctionBuilder::new([(1, ValType::I64)]);

		{
			let mut seq = builder.sink();

			let global_tab = 0;

			// Initialize the global table.
			seq.call(state.extern_fns.table_load)
				.local_tee(global_tab)
				.global_set(state.global_table);

			let mut add_fn = |key, func| {
				seq.push_function(state, func)
					.local_get(global_tab)
					.static_str(state, key)
					.call(state.extern_fns.table_set_name);
			};

			// Load the 'error' function into it.
			add_fn(internal_strings.error, std_error);
			// Load the 'pcall' function into it.
			add_fn(internal_strings.pcall, std_pcall);

			// Generate a call to the actual main function.
			seq.i32_const(0)
				.call(main_fn.sym) //FIXME: Reloc
				.drop();

			seq.end();
		}

		let id = state.function_count;
		state.function_count += 1;

		builder.finish(state); // Encodes the function and the relocations.

		state.function_sect.function(state.types_sect.len());
		state.types_sect.ty().function([], []);

		id
	};
}
