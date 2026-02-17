#![no_std]

use value::{Value, ValueTag};


pub fn string(s: &'static (impl AsRef<[u8]> + ?Sized)) -> Value {
	let s = s.as_ref();
	
	let ptr = u32::try_from(s.as_ptr().addr()).expect("infallible");
	let len = u32::try_from(s.len()).expect("infallible");
	let bytes = ptr as i64 | ((len as i64) << 32);
	let mut v = Value::from_i64(bytes);
	v.set_tag(ValueTag::String);
	v
}

mod binds {
	// unsafe extern "C" {
	// 	fn __luant_exception(object: i64) -> !;
	// }
	
	// pub fn error(object: i64) -> ! { unsafe { __luant_exception(object); } }

	pub fn error(object: i64) -> ! {
		panic!();
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_add(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() + b.to_num()).as_i64(),
		_ => binds::error(string("Attempted to add incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_sub(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() - b.to_num()).as_i64(),
		_ => binds::error(string("Attempted to subtract incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_mul(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() * b.to_num()).as_i64(),
		_ => binds::error(string("Attempted to multiply incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_div(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() / b.to_num()).as_i64(),
		_ => binds::error(string("Attempted to divide incompatible values").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_eq(a: i64, b: i64) -> i64 {
	let (a, b) = (Value::from_i64(a), Value::from_i64(b));

	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() == b.to_str()).as_i64(),
		(lhs, rhs) => Value::bool(lhs == rhs).as_i64(),
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
			binds::error(string("Attempted to call a closure, which is not supported yet").as_i64());
		},
		_ => binds::error(string("Attempted to call a non-function value").as_i64()),
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
		_ => binds::error(string("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_f64(value: i64) -> f64 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num(),
		_ => binds::error(string("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_i32(value: i64) -> i32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i32,
		_ => binds::error(string("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[unsafe(no_mangle)]
extern "C" fn __luant_val_to_f32(value: i64) -> f32 {
	let value = Value::from_i64(value);
	match value.get_tag() {
		ValueTag::Number => value.to_num() as f32,
		_ => binds::error(string("Attempted to convert a non-string, non-number value to a number").as_i64()),
	}
}

#[panic_handler]
fn on_panic(_info: &core::panic::PanicInfo) -> ! {
	unsafe { core::hint::unreachable_unchecked() }
}
