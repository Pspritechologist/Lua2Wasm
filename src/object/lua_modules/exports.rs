use wasm_encoder::{FuncType, MemArg, ValType};
use anyhow::Result;
use std::borrow::Cow;
use crate::object::{linking::SymbolTab, ModuleState, try_compile_function};

#[derive(Debug, Clone)]
pub struct ExportData<'a> {
	pub export_name: Cow<'a, str>,
	pub global_name: Cow<'a, [u8]>,

	pub func_sig: FuncType,
}

pub fn generate_exports_object<'a>(exports: impl IntoIterator<Item = &'a ExportData<'a>>) -> Result<Vec<u8>> {
	let state = &mut ModuleState::new_module();

	for export in exports {
		let sig = state.types_sect.len();
		state.types_sect.ty().func_type(&export.func_sig);

		let luant_global_name = state.add_data(SymbolTab::WASM_SYM_BINDING_LOCAL, &export.export_name, export.global_name.iter().copied());

		try_compile_function(state, &export.export_name, SymbolTab::WASM_SYM_EXPORTED, [(1, ValType::I32)], sig, |state, seq, _| {
			let tmp_local: u32 = export.func_sig.params().len().try_into().unwrap();

			for (i, param) in export.func_sig.params().iter().enumerate() {
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

			seq.i32_const(export.func_sig.params().len().try_into().unwrap())
				.global_get(state.global_table)
				.static_str(state, luant_global_name, export.global_name.len().try_into().unwrap())
				.call(state.extern_fns.table_get_name)
				.call_as_luant_fn(state);

			seq.local_set(tmp_local);

			for (i, ret) in export.func_sig.results().iter().enumerate() {
				let i: u32 = i.try_into().unwrap();

				seq.local_get(tmp_local)
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
		})?;
	}

	Ok(state.build_object()?.finish())
}
