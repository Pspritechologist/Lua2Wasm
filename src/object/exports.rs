use wasm_encoder::{EntityType, FuncType, MemArg, ValType};
use anyhow::{Result, Ok};
use std::borrow::Cow;
use crate::{Config, object::{InstructionSink, ModuleState, linking::{Symbol, SymbolTab}, compile_function, try_compile_function}};

#[derive(Debug, Clone)]
pub struct ExportData<'a> {
	pub export_name: Cow<'a, str>,
	pub global_name: Cow<'a, [u8]>,

	pub func_sig: FuncType,
}

pub fn generate_exports_object(config: &Config) -> Result<Vec<u8>> {
	let state = &mut ModuleState::new_module();

	let mut generated_entry_point = false;

	let main_fn_sym = if config.entry_point.as_export().is_some() {
		generate_main_fn_symbol(state)
	} else {
		Symbol::new(0) // Never gets used.
	};

	for export in &config.exports {
		if config.entry_point.as_export().is_some_and(|e| e == export.export_name) {
			generated_entry_point = true;

			let sig = state.types_sect.len();
			state.types_sect.ty().func_type(&export.func_sig);

			try_compile_function(state, &export.export_name, SymbolTab::WASM_SYM_EXPORTED, [(1, ValType::I32)], sig, |state, seq, _| {
				let tmp_local: u32 = export.func_sig.params().len().try_into().unwrap();

				seq.block(wasm_encoder::BlockType::Result(ValType::I64))
					.try_table(wasm_encoder::BlockType::Empty, state.error_tag, 0);

				cast_args(state, seq, export.func_sig.params())?;

				seq.i32_const(export.func_sig.params().len().try_into().unwrap())
					.call(main_fn_sym);

				seq.local_set(tmp_local);

				cast_results(state, seq, tmp_local, export.func_sig.results())?;

				seq.return_().end().unreachable().end();

				//TODO: This is currently hardcoded to support the error API of our
				//TODO: testing binary. This should allow custom top-level error handling.
				seq.call(state.extern_fns.put_error).unreachable(); // Error case.

				Ok(())
			})?;

			continue;
		}

		generate_export_function(state, export)?;
	}

	// If we can't find a matching export item for the entrypoint, we assume a standard
	// entrypoint with no inputs and no outputs.
	if !generated_entry_point && let Some(entry_point) = config.entry_point.as_export() {
		let sig = state.types_sect.len();
		state.types_sect.ty().func_type(&FuncType::new([], []));

		compile_function(state, entry_point, SymbolTab::WASM_SYM_EXPORTED, [(1, ValType::I32)], sig, |_, seq, _| {
			seq.i32_const(0).call(main_fn_sym);
		});
	}

	Ok(state.build_object()?.finish())
}

fn generate_main_fn_symbol(state: &mut ModuleState) -> Symbol {
	let id = state.function_count;
	state.function_count += 1;
	state.import_sect.import("__luant_internal", crate::object::ENTRY_POINT_NAME, EntityType::Function(state.dyn_call_ty));
	state.symbol_table.function(SymbolTab::WASM_SYM_UNDEFINED, id, None)
}

fn generate_export_function(state: &mut ModuleState, export: &ExportData) -> Result<()> {
	let sig = state.types_sect.len();
	state.types_sect.ty().func_type(&export.func_sig);

	let luant_global_name = state.add_data(SymbolTab::WASM_SYM_BINDING_LOCAL, &export.export_name, export.global_name.iter().copied());

	try_compile_function(state, &export.export_name, SymbolTab::WASM_SYM_EXPORTED, [(1, ValType::I32)], sig, |state, seq, _| {
		let tmp_local: u32 = export.func_sig.params().len().try_into().unwrap();

		seq.block(wasm_encoder::BlockType::Result(ValType::I64))
			.try_table(wasm_encoder::BlockType::Empty, state.error_tag, 0);

		cast_args(state, seq, export.func_sig.params())?;

		seq.i32_const(export.func_sig.params().len().try_into().unwrap())
			.global_get(state.global_table)
			.static_str(state, luant_global_name, export.global_name.len().try_into().unwrap())
			.call(state.extern_fns.table_get_name)
			.call_as_luant_fn(state);

		seq.local_set(tmp_local);

		cast_results(state, seq, tmp_local, export.func_sig.results())?;

		seq.return_().end().unreachable().end();

		//TODO: This is currently hardcoded to support the error API of our
		//TODO: testing binary. This should allow custom top-level error handling.
		seq.call(state.extern_fns.put_error).unreachable(); // Error case.

		Ok(())
	}).map(|_| ())
}

fn cast_args(state: &mut ModuleState, seq: &mut InstructionSink, params: &[ValType]) -> Result<()> {
	for (i, param) in params.iter().enumerate() {
		let i: u32 = i.try_into().unwrap();

		seq.global_get(state.shtack_ptr);

		seq.local_get(i);
		match param {
			ValType::I32 => seq.call(state.extern_fns.i32_to_val),
			ValType::I64 => seq.call(state.extern_fns.i64_to_val),
			ValType::F32 => seq.call(state.extern_fns.f32_to_val),
			ValType::F64 => seq.call(state.extern_fns.f64_to_val),
			ValType::V128 |
			ValType::Ref(_) => return Err(anyhow::anyhow!("Unsupported export parameter type: {param:?}")),
		};

		seq.i64_store(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.shtack_mem });
	}

	Ok(())
}

fn cast_results(state: &mut ModuleState, seq: &mut InstructionSink, ret_count: u32, results: &[ValType]) -> Result<()> {
	for (i, ret) in results.iter().enumerate() {
		let i: u32 = i.try_into().unwrap();

		seq.local_get(ret_count)
			.i32_const(i.cast_signed())
			.i32_gt_u()
			.if_(wasm_encoder::BlockType::Result(*ret));
		{
			// We have a value to convert...
			seq.global_get(state.shtack_ptr)
				.i64_load(MemArg { align: 3, offset: i as u64 * 8, memory_index: state.shtack_mem });

			match ret {
				ValType::I32 => seq.call(state.extern_fns.val_to_i32),
				ValType::I64 => seq.call(state.extern_fns.val_to_i64),
				ValType::F32 => seq.call(state.extern_fns.val_to_f32),
				ValType::F64 => seq.call(state.extern_fns.val_to_f64),
				ValType::V128 |
				ValType::Ref(_) => return Err(anyhow::anyhow!("Unsupported export result type: {ret:?}")),
			};
		}
		seq.else_();
		{
			// We do not have a value to convert...
			match ret {
				ValType::I32 => seq.i32_const(0),
				ValType::I64 => seq.i64_const(0),
				ValType::F32 => seq.f32_const(0.0.into()),
				ValType::F64 => seq.f64_const(0.0.into()),
				ValType::V128 |
				ValType::Ref(_) => return Err(anyhow::anyhow!("Unsupported export result type: {ret:?}")),
			};
		}
		seq.end();
	}

	Ok(())
}
