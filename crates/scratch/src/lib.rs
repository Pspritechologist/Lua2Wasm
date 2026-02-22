#![feature(trim_prefix_suffix, bstr, wasm_numeric_instr)]
#![no_std]


#[global_allocator]
static ALLOCATOR: talc::TalckWasm = unsafe { talc::TalckWasm::new_global() };

use crate::table::{Table, TabValueExt};
use value::{Value, ValueTag};
use core::arch::wasm32;

mod table;

extern crate alloc;

mod binds {
	use super::*;

	unsafe extern "C" {
		fn put_str(ptr: *const u8, len: usize);
	}

	#[link(wasm_import_module = "__luant")]
	unsafe extern "C" {
		fn throw(object: i64) -> !;
	}
	
	// pub fn error(object: i64) -> ! { unsafe { __luant_exception(object); } }

	#[inline]
	pub fn error(object: i64) -> ! {
		unsafe { throw(object) }
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_add(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() + b.to_num()).as_i64(),
		_ => binds::error(Value::from("Attempted to add incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_sub(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() - b.to_num()).as_i64(),
		_ => binds::error(Value::from("Attempted to subtract incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_mul(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() * b.to_num()).as_i64(),
		_ => binds::error(Value::from("Attempted to multiply incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_div(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() / b.to_num()).as_i64(),
		_ => binds::error(Value::from("Attempted to divide incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_eq(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() == b.to_str()).as_i64(),
		_ => Value::bool(a == b).as_i64(),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_get_fn(func: i64) -> extern "C" fn(usize) -> usize {
	let func = Value::from_i64(func);

	match func.get_tag() {
		ValueTag::Function => {
			func.to_function()
		},
		ValueTag::Closure => {
			binds::error(Value::from("Attempted to call a closure, which is not supported yet").as_i64());
		},
		_ => binds::error(Value::from("Attempted to call a non-function value").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_get_truthy(value: i64) -> i32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Nil => 0,
		ValueTag::Bool => if value.to_bool() { 1 } else { 0 },
		_ => 1,
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_i64(value: i64) -> i64 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i64,
		_ => binds::error(Value::from("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_i64_to_val(value: i64) -> i64 {
	Value::float(value as f64).as_i64()
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_f64(value: i64) -> f64 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num(),
		_ => binds::error(Value::from("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_f64_to_val(value: f64) -> i64 {
	Value::float(value).as_i64()
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_i32(value: i64) -> i32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i32,
		_ => binds::error(Value::from("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_i32_to_val(value: i32) -> i64 {
	Value::float(value as f64).as_i64()
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_f32(value: i64) -> f32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as f32,
		_ => binds::error(Value::from("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_f32_to_val(value: f32) -> i64 {
	Value::float(value as f64).as_i64()
}

#[unsafe(no_mangle)]
extern "C" fn __luant_init_tab() -> i64 {
	Value::new_table().as_i64()
}

#[unsafe(no_mangle)]
extern "C" fn __luant_tab_get(table: i64, key: i64) -> i64 {
	let table = Value::from_i64(table);
	let key = Value::from_i64(key);

	let Some(table) = table.as_table() else {
		binds::error(Value::from("Attempted to index into a non-table value").as_i64());
	};

	table.get(&key).unwrap_or(Value::nil()).as_i64()
}

#[unsafe(no_mangle)]
extern "C" fn __luant_tab_set(table: i64, key: i64, value: i64) {
	let table = Value::from_i64(table);
	let key = Value::from_i64(key);
	let value = Value::from_i64(value);

	let Some(mut table) = table.as_table() else {
		binds::error(Value::from("Attempted to index into a non-table value").as_i64());
	};

	if value.get_tag() == ValueTag::Nil {
		table.remove(&key);
	} else {
		table.set(key, value);
	}
}

#[panic_handler]
fn on_panic(_info: &core::panic::PanicInfo) -> ! {
	if cfg!(debug_assertions) {
		wasm32::unreachable();
	} else {
		unsafe { core::hint::unreachable_unchecked() }
	}
}
