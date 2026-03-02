#![no_std]
#![feature(
	linkage,
	wasm_numeric_instr,
	trim_prefix_suffix,
	bstr,
)]

#[global_allocator]
static ALLOCATOR: talc::TalckWasm = unsafe { talc::TalckWasm::new_global() };

use crate::table::TabValueExt;
use macro_rules_attribute::apply;
use value::{Value, ValueTag};
use core::arch::wasm32;

mod table;

extern crate alloc;

mod binds {
	#[link(wasm_import_module = "__luant")]
	unsafe extern "C" {
		fn throw(object: i64) -> !;
	}

	#[inline]
	pub fn error(object: i64) -> ! {
		unsafe { throw(object) }
	}
}

macro_rules! internal {
	(pub fn $name:ident ($($args:tt)*) $(-> $ret:ty)? { $($body:tt)* }) => { pastey::paste! {
		#[unsafe(no_mangle)]
		#[linkage = "internal"]
		pub extern "C" fn [< __luant_ $name >] ($($args)*) $(-> $ret)? {
			$($body)*
		}
	} };
}

trait ValExt {
	fn str(value: &'static (impl AsRef<[u8]> + ?Sized)) -> Value {
		let s = value.as_ref();
		
		let ptr = u32::try_from(s.as_ptr().addr()).expect("infallible");
		let len = u32::try_from(s.len()).expect("infallible");

		Value::string(ptr, len)
	}
}
impl ValExt for Value {}

#[apply(internal)]
pub fn static_str(addr: u32, len: u32) -> i64 {
	Value::string(addr, len).as_i64()
}

#[apply(internal)]
pub fn add(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() + b.to_num()).as_i64(),
		_ => binds::error(Value::str("Attempted to add incompatible values").as_i64()),
	}
}

#[apply(internal)]
pub fn sub(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() - b.to_num()).as_i64(),
		_ => binds::error(Value::str("Attempted to subtract incompatible values").as_i64()),
	}
}

#[apply(internal)]
pub fn mul(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() * b.to_num()).as_i64(),
		_ => binds::error(Value::str("Attempted to multiply incompatible values").as_i64()),
	}
}

#[apply(internal)]
pub fn div(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() / b.to_num()).as_i64(),
		_ => binds::error(Value::str("Attempted to divide incompatible values").as_i64()),
	}
}

#[apply(internal)]
pub fn eq(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() == b.to_str()).as_i64(),
		_ => Value::bool(a == b).as_i64(),
	}
}

#[apply(internal)]
pub fn gt(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() > b.to_str()).as_i64(),
		(ValueTag::Number, ValueTag::Number) => Value::bool(a.to_num() > b.to_num()).as_i64(),
		_ => {
			binds::error(Value::str("Attempted to compare unsupported values").as_i64());
		},
	}
}

#[apply(internal)]
pub fn get_fn(func: i64) -> extern "C" fn(usize) -> usize {
	let func = Value::from_i64(func);

	match func.get_tag() {
		ValueTag::Function => {
			func.to_function()
		},
		ValueTag::Closure => {
			binds::error(Value::str("Attempted to call a closure, which is not supported yet").as_i64());
		},
		_ => binds::error(Value::str("Attempted to call a non-function value").as_i64()),
	}
}

#[apply(internal)]
pub fn get_truthy(value: i64) -> i32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Nil => 0,
		ValueTag::Bool => if value.to_bool() { 1 } else { 0 },
		_ => 1,
	}
}

#[apply(internal)]
pub fn val_to_i64(value: i64) -> i64 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i64,
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[apply(internal)]
pub fn i64_to_val(value: i64) -> i64 {
	Value::float(value as f64).as_i64()
}

#[apply(internal)]
pub fn val_to_f64(value: i64) -> f64 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num(),
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[apply(internal)]
pub fn f64_to_val(value: f64) -> i64 {
	Value::float(value).as_i64()
}

#[apply(internal)]
pub fn val_to_i32(value: i64) -> i32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i32,
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[apply(internal)]
pub fn i32_to_val(value: i32) -> i64 {
	Value::float(value as f64).as_i64()
}

#[apply(internal)]
pub fn val_to_f32(value: i64) -> f32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as f32,
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[apply(internal)]
pub fn f32_to_val(value: f32) -> i64 {
	Value::float(value as f64).as_i64()
}

#[apply(internal)]
pub fn init_tab() -> i64 {
	Value::new_table().as_i64()
}

#[apply(internal)]
pub fn tab_get(table: i64, key: i64) -> i64 {
	let table = Value::from_i64(table);
	let key = Value::from_i64(key);

	let Some(table) = table.as_table() else {
		binds::error(Value::str("Attempted to index into a non-table value").as_i64());
	};

	table.get(&key).unwrap_or(Value::nil()).as_i64()
}

#[apply(internal)]
pub fn tab_set(table: i64, key: i64, value: i64) {
	let table = Value::from_i64(table);
	let key = Value::from_i64(key);
	let value = Value::from_i64(value);

	let Some(mut table) = table.as_table() else {
		binds::error(Value::str("Attempted to index into a non-table value").as_i64());
	};

	if value.get_tag() == ValueTag::Nil {
		table.remove(&key);
	} else {
		table.set(key, value);
	}
}

#[apply(internal)]
pub fn tab_get_name(table: i64, key: i64) -> i64 {
	let table = Value::from_i64(table);
	let key = Value::from_i64(key);
	
	match key.get_tag() {
		ValueTag::String => table.to_table().get(&key).unwrap_or(Value::nil()).as_i64(),
		_ if cfg!(debug_assertions) => panic!("Attempted to get_name with non-string key"),
		_ => unsafe { core::hint::unreachable_unchecked() },
	}
}

//? `value` is the first param here for impl reasons on the WASM side...
#[apply(internal)]
pub fn tab_set_name(value: i64, table: i64, key: i64) {
	let table = Value::from_i64(table);
	let key = Value::from_i64(key);
	let value = Value::from_i64(value);

	match key.get_tag() {
		ValueTag::String => {
			let mut table = table.to_table();
		
			if value.get_tag() == ValueTag::Nil {
				table.remove(&key);
			} else {
				table.set(key, value);
			}
		},
		_ if cfg!(debug_assertions) => panic!("Attempted to set_name with non-string key"),
		_ => unsafe { core::hint::unreachable_unchecked() },
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
