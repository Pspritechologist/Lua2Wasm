use crate::{State, linking::{Symbol, SymbolTab}};
use wasm_encoder::{
    EntityType,
	ImportSection,
    TypeSection, ValType,
};

/// Compile functions used only by the runtime Crate.
pub fn compile_supporting_functions(state: &mut State) {
	let ty = state.types_sect.len();
	state.types_sect.ty().function([ValType::I64], []);
	
	let tag = state.error_tag;
	crate::compile_function(state, "throw", SymbolTab::WASM_SYM_BINDING_WEAK, [], ty, |_state, seq, _| {
		seq.local_get(0).throw(tag);
	});
}

macro_rules! extern_fns {
	(pub struct $StructName:ident {
			$(
				$(#[$attr:meta])*
				$field:ident $(: $name:ident)? ($($in:expr),*) $(-> $($ret:expr),+)?
			);+ $(;)?
	}) => {
		pub struct $StructName {
			$(
				$(#[$attr])*
				#[allow(unused)] //TODO
				pub $field: Symbol,
			)+
		}

		impl $StructName {
			pub fn init(types_section: &mut TypeSection, import_sect: &mut ImportSection, symbol_table: &mut SymbolTab) -> (u32, Self) {
				use ValType::*;

				let mut count = 0;

				$(
					#[allow(unused)]
					let name = stringify!($field);
					$(let name = stringify!($name);)?
					#[allow(unused)]
					let symbol_name = concat!("__luant_", stringify!($field));
					$(let symbol_name = concat!("__luant_", stringify!($name));)?

					let fn_type = types_section.len();
					types_section.ty().function([ $($in),* ], [ $($($ret),+)? ]);
					import_sect.import("__luant_internal", name, EntityType::Function(fn_type));
					let fn_idx = count;
					let $field = symbol_table.function(SymbolTab::WASM_SYM_UNDEFINED | SymbolTab::WASM_SYM_EXPLICIT_NAME, fn_idx, Some(symbol_name));

					count += 1;
				)+

				(count, Self {
					$( $field, )+
				})
			}
		}
	}
}

extern_fns! {
	pub struct ExternFns {
		static_str(I32, I32) -> I64;
		static_function(I32) -> I64;

		add(I64, I64) -> I64;
		sub(I64, I64) -> I64;
		mul(I64, I64) -> I64;
		div(I64, I64) -> I64;
		modulo(I64, I64) -> I64;
		pow(I64, I64) -> I64;
		neg(I64) -> I64;

		eq(I64, I64) -> I64;
		neq(I64, I64) -> I64;
		lt(I64, I64) -> I64;
		lte(I64, I64) -> I64;
		gt(I64, I64) -> I64;
		gte(I64, I64) -> I64;
		not(I64) -> I64;

		bit_and(I64, I64) -> I64;
		bit_or(I64, I64) -> I64;
		bit_xor(I64, I64) -> I64;
		bit_sh_l(I64, I64) -> I64;
		bit_sh_r(I64, I64) -> I64;
		bit_not(I64) -> I64;

		concat(I64, I64) -> I64;
		len(I64) -> I64;

		get_fn(I64) -> I32;
		get_truthy(I64) -> I32;

		val_to_i64(I64) -> I64;
		val_to_f64(I64) -> F64;
		val_to_i32(I64) -> I32;
		val_to_f32(I64) -> F32;
		i64_to_val(I64) -> I64;
		f64_to_val(F64) -> I64;
		i32_to_val(I32) -> I64;
		f32_to_val(F32) -> I64;
		
		table_load: init_tab() -> I64;
		table_get: tab_get(I64, I64) -> I64;
		table_set: tab_set(I64, I64, I64);

		/// Doesn't type check input as a table, and assumes a string key. For internal use.
		table_get_name: tab_get_name(I64, I64) -> I64;
		/// Doesn't type check input as a table, and assumes a string key. For internal use.\
		/// This takes `value, table, key` unlike other functions for impl reasons.
		table_set_name: tab_set_name(I64, I64, I64);
	}
}
