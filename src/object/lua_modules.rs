use crate::{
	bytecode::Operation,
	parsing::{Parsed, ParsedFunction},
};

use crate::object::{
	ClosureRef, State, StringRef, build_obj_module, compile_function,
	linking::{RelocSection, SymbolTab},
	runtime_impls::external_bindings::ExternFns,
};

use wasm_encoder::{
	BlockType, CodeSection, ConstExpr, DataSection, FunctionSection, GlobalSection, GlobalType,
    ImportSection, IndirectNameMap, MemArg, MemoryType, NameMap, RefType, TableType, TagKind,
    TagSection, TagType, TypeSection, ValType,
};

use anyhow::Result;

pub fn produce_lua_obj_file<'s>(parsed: Parsed<'s>) -> Result<Vec<u8>> {
	let mut symbol_table = SymbolTab::new();
	let mut types_sect = TypeSection::new();
	let mut import_sect = ImportSection::new();
	let mut tag_sect = TagSection::new();
	let mut global_sect = GlobalSection::new();
	let mut data_sect = DataSection::new();

	let strings = {
		let mut offset = 0;
		parsed.constants.strings().iter().enumerate().map(|(i, s)| {
			let (addr, len) = (offset as u32, s.len() as u32);
			offset += s.len();
			data_sect.active(0, &ConstExpr::i32_const(addr.cast_signed()), s.iter().copied());
			let sym = symbol_table.string(i as u32, len);
			StringRef { len, sym }
		}).collect()
	};

	import_sect.import("env", "__linear_memory", MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: None });
	// memory_sect.memory(MemoryType { memory64: false, shared: false, minimum: 1, page_size_log2: None, maximum: Some(1) });

	global_sect.global(GlobalType { val_type: ValType::I32, mutable: true, shared: false }, &ConstExpr::i32_const(0));
	global_sect.global(GlobalType { val_type: ValType::I64, mutable: true, shared: false }, &ConstExpr::i64_const(0));
	let shtack_ptr = symbol_table.global(SymbolTab::WASM_SYM_BINDING_LOCAL, 0, Some("__luant_shtack_ptr"));
	let global_table = symbol_table.global(SymbolTab::WASM_SYM_BINDING_LOCAL, 1, Some("__luant_global_table"));

	types_sect.ty().function([ValType::I32], [ValType::I32]);

	import_sect.import("env", "__indirect_function_table", TableType {
		element_type: RefType::FUNCREF,
		table64: false,
		minimum: 0,
		maximum: None,
		shared: false,
	});
	let call_tab = symbol_table.table(SymbolTab::WASM_SYM_UNDEFINED, 0, None);

	let (function_count, extern_fns) = ExternFns::init(&mut types_sect, &mut import_sect, &mut symbol_table);

	tag_sect.tag(TagType { kind: TagKind::Exception, func_type_idx: types_sect.len() });
	let error_tag = symbol_table.tag(SymbolTab::WASM_SYM_BINDING_LOCAL, 0, Some("__luant_error_tag"));

	types_sect.ty().function([ValType::I64], []);

	let mut state = State {
		strings,
		
		global_table,
		symbol_table,
		shtack_ptr,
		error_tag,
		call_tab,
		dyn_call_ty: 0,
		linear_memory: 0,
		shtack_mem: 1,
		
		function_count,
		closures: vec![None; parsed.constants.closures().len() + 1].into_boxed_slice(),
		extern_fns,
		
		types_sect,
		import_sect,
		tag_sect,
		global_sect,
		data_sect,
		function_sect: FunctionSection::new(),
		code_sect: CodeSection::new(),
		reloc_code_sect: RelocSection::new(),
		local_names: IndirectNameMap::new(),
		
		function_names: NameMap::new(),
		locals: Default::default(),
		loop_depth: 0,
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

	Ok(build_obj_module(&state)?.finish())
}


fn compile_luant_function(state: &mut State, func: &ParsedFunction) -> ClosureRef {
	let name = func.debug.as_ref().and_then(|d| d.func_name()).unwrap_or("<anonymous closure>");
	
	compile_function(state, name, SymbolTab::WASM_SYM_BINDING_LOCAL, [(func.frame_size.into(), ValType::I64)], state.dyn_call_ty, |state, seq, current_fn| {
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

			state.local_names.append(current_fn, &local_names);
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
					.global_get(state.shtack_ptr)
					.i64_load(MemArg { align: 3, offset: u64::from(i) * 8, memory_index: state.shtack_mem })
					.local_set(state.locals[&i]);
			}
			seq.end();
		}

		seq.operations(state, func.operations.iter().copied());

		if func.operations.last().is_none_or(|op| !matches!(op, Operation::Ret { .. })) {
			seq.i32_const(0)
				.return_();
		}

		state.locals.clear();
	})
}
