#![no_std]
#![feature(
	linkage,
	ptr_metadata,
	trim_prefix_suffix,
	slice_concat_ext,
	bstr,
)]

#[global_allocator]
static ALLOCATOR: talc::TalckWasm = unsafe { talc::TalckWasm::new_global() };

use crate::{closures::LuaFn, table::TabValueExt};
use alloc::{bstr::ByteString, vec::Vec};
use macro_rules_attribute::apply;
use value::{Value, ValueTag};
use core::bstr::ByteStr;

mod table;
mod closures;

extern crate alloc;

mod binds {
	use super::Value;

	//TODO: Name prefixes.
	unsafe extern "C" {
		fn throw(object: Value) -> !;
		fn read_shtack_val(ptr: *const Value) -> Value;
		fn write_shtack_val(ptr: *mut Value, value: Value);
	}

	#[link(wasm_import_module = "debug")]
	unsafe extern "C" {
		#[link_name = "put_error"]
		fn _put_error(ptr: *const u8, len: usize);
	}

	#[link(wasm_import_module = "env")]
	unsafe extern "C" {
		fn put_str(ptr: *const u8, len: usize);
	}

	pub fn error(object: Value) -> ! {
		unsafe { throw(object) }
	}

	pub fn put_error(string: &[u8]) {
		let ptr = string.as_ptr();
		let len = string.len();
		unsafe { _put_error(ptr, len) }
	}

	pub fn print(string: impl AsRef<[u8]>) {
		let string = string.as_ref();
		let ptr = string.as_ptr();
		let len = string.len();
		unsafe { put_str(ptr, len) }
	}

	#[repr(transparent)]
	pub struct ShtackVal(*mut Value);
	impl ShtackVal {
		pub fn new(ptr: *mut Value) -> Self { Self(ptr) }
		pub fn read(&self) -> Value {
			unsafe { read_shtack_val(self.0) }
		}
		pub fn write(&self, value: Value) {
			unsafe { write_shtack_val(self.0, value) }
		}
	}
}

fn new_string(string: impl Into<Vec<u8>>) -> Value {
	let bytes = string.into().leak();

	let ptr = bytes.as_ptr().addr();
	let len = bytes.len();

	Value::string(ptr as u32, len as u32)
}

fn fmt_value<'a>(value: &'a Value, buf: &'a mut zmij::Buffer) -> &'a ByteStr {
	match value.get_tag() {
		ValueTag::String => value.to_str(),
		ValueTag::Nil => "<nil>".as_ref(),
		ValueTag::Bool => if value.to_bool() { "true" } else { "false" }.as_ref(),
		ValueTag::Number => buf.format(value.to_num()).as_ref(),
		ValueTag::Table => "<table>".as_ref(),
		ValueTag::Function => "<function>".as_ref(),
		ValueTag::Closure => "<closure>".as_ref(),
	}
}

macro_rules! internal {
	( $(#[$attr:meta])* pub fn $name:ident ($($args:tt)*) $(-> $ret:ty)? { $($body:tt)* } ) => { pastey::paste! {
		$(#[$attr])*
		#[unsafe(no_mangle)]
		// #[linkage = "internal"]
		pub extern "C" fn [< __camento_ $name >] ($($args)*) $(-> $ret)? {
			$($body)*
		}
	} };
}
use internal;

trait ValExt {
	fn str(value: &'static (impl AsRef<[u8]> + ?Sized)) -> Value {
		let s = value.as_ref();
		
		let ptr = u32::try_from(s.as_ptr().addr()).expect("infallible");
		let len = u32::try_from(s.len()).expect("infallible");

		Value::string(ptr, len)
	}

	fn to_closure(self) -> closures::Closure;
	fn as_closure(self) -> Option<closures::Closure>;
	fn to_function(self) -> LuaFn;
	fn as_function(self) -> Option<LuaFn>;
	fn to_str(&self) -> &ByteStr;
	fn as_str(&self) -> Option<&ByteStr>;

	fn equals(self, other: Self) -> bool;
}
impl ValExt for Value {

	fn equals(self, other: Self) -> bool {
		match (self.get_tag(), other.get_tag()) {
			(ValueTag::String, ValueTag::String) => self.to_str() == other.to_str(),
			_ => self.as_i64() == other.as_i64(),
		}
	}

	fn to_closure(self) -> closures::Closure {
		closures::Closure::from_idx(self.to_idx())
	}
	fn as_closure(self) -> Option<closures::Closure> {
		(self.get_tag() == ValueTag::Closure).then(|| self.to_closure())
	}

	fn to_function(self) -> LuaFn {
		let bytes = self.meaningful_bits();
		let ptr = u32::from_ne_bytes(bytes[4..8].try_into().unwrap()) as usize;
		let ptr = ptr as *const u8;
		unsafe { core::mem::transmute(ptr) }
	}
	fn as_function(self) -> Option<LuaFn> {
		(self.get_tag() == ValueTag::Function).then(|| self.to_function())
	}

	fn to_str(&self) -> &ByteStr {
		let (addr, len) = self.to_addr_len();
		let ptr = addr as usize as *const u8;
		let data = unsafe { core::slice::from_raw_parts(ptr, len as usize) };
		ByteStr::new(data)
	}
	fn as_str(&self) -> Option<&ByteStr> {
		(self.get_tag() == ValueTag::String).then(|| self.to_str())
	}
}

#[apply(internal)]
pub fn static_str(addr: u32, len: u32) -> Value {
	Value::string(addr, len)
}

#[apply(internal)]
pub fn static_function(addr: usize) -> Value {
	unsafe { Value::idx(addr) }.with_tag(ValueTag::Function)
}

#[apply(internal)]
pub fn add(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() + b.to_num()),
		_ => binds::error(Value::str("Attempted to add incompatible values")),
	}
}

#[apply(internal)]
pub fn sub(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() - b.to_num()),
		_ => binds::error(Value::str("Attempted to subtract incompatible values")),
	}
}

#[apply(internal)]
pub fn mul(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() * b.to_num()),
		_ => binds::error(Value::str("Attempted to multiply incompatible values")),
	}
}

#[apply(internal)]
pub fn div(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() / b.to_num()),
		_ => binds::error(Value::str("Attempted to divide incompatible values")),
	}
}

#[apply(internal)]
pub fn modulo(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(a.to_num() % b.to_num()),
		_ => binds::error(Value::str("Attempted to modulo incompatible values")),
	}
}

#[apply(internal)]
pub fn pow(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::Number, ValueTag::Number) => Value::float(libm::pow(a.to_num(), b.to_num())),
		_ => binds::error(Value::str("Attempted to exponentiate incompatible values")),
	}
}

#[apply(internal)]
pub fn neg(a: Value) -> Value {
	match a.get_tag() {
		ValueTag::Number => Value::float(-a.to_num()),
		_ => binds::error(Value::str("Attempted to negate an incompatible value")),
	}
}


#[apply(internal)]
pub fn eq(a: Value, b: Value) -> Value {
	a.equals(b).into()
}

#[apply(internal)]
pub fn neq(a: Value, b: Value) -> Value {
	(!a.equals(b)).into()
}

#[apply(internal)]
pub fn lt(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() < b.to_str()),
		(ValueTag::Number, ValueTag::Number) => Value::bool(a.to_num() < b.to_num()),
		_ => binds::error(Value::str("Attempted to compare incompatible values")),
	}
}

#[apply(internal)]
pub fn lte(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() <= b.to_str()),
		(ValueTag::Number, ValueTag::Number) => Value::bool(a.to_num() <= b.to_num()),
		_ => binds::error(Value::str("Attempted to compare incompatible values")),
	}
}


#[apply(internal)]
pub fn gt(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() > b.to_str()),
		(ValueTag::Number, ValueTag::Number) => Value::bool(a.to_num() > b.to_num()),
		_ => {
			binds::error(Value::str("Attempted to compare unsupported values"));
		},
	}
}

#[apply(internal)]
pub fn gte(a: Value, b: Value) -> Value {
	match (a.get_tag(), b.get_tag()) {
		(ValueTag::String, ValueTag::String) => Value::bool(a.to_str() >= b.to_str()),
		(ValueTag::Number, ValueTag::Number) => Value::bool(a.to_num() >= b.to_num()),
		_ => binds::error(Value::str("Attempted to compare incompatible values")),
	}
}

#[apply(internal)]
pub fn not(a: Value) -> Value {
	Value::bool(!a.is_truthy())
}


#[apply(internal)]
pub fn bit_and(a: Value, b: Value) -> Value {
	Value::nil()
}

#[apply(internal)]
pub fn bit_or(a: Value, b: Value) -> Value {
	Value::nil()
}

#[apply(internal)]
pub fn bit_xor(a: Value, b: Value) -> Value {
	Value::nil()
}

#[apply(internal)]
pub fn bit_sh_l(a: Value, b: Value) -> Value {
	Value::nil()
}

#[apply(internal)]
pub fn bit_sh_r(a: Value, b: Value) -> Value {
	Value::nil()
}

#[apply(internal)]
pub fn bit_not(a: Value) -> Value {
	Value::nil()
}


#[apply(internal)]
pub fn concat(a: Value, b: Value) -> Value {
	let mut a_buf = zmij::Buffer::new();
	let a = match a.get_tag() {
		ValueTag::String => a.to_str(),
		ValueTag::Number => ByteStr::new(a_buf.format(a.to_num())),
		_ => binds::error(Value::str("Attempted to concatenate incompatible values")),
	};
	
	let mut b_buf = zmij::Buffer::new();
	let b = match b.get_tag() {
		ValueTag::String => b.to_str(),
		ValueTag::Number => ByteStr::new(b_buf.format(b.to_num())),
		_ => binds::error(Value::str("Attempted to concatenate incompatible values")),
	};

	let res = Vec::leak([&**a, &**b].concat());

	Value::str(res)
}

#[apply(internal)]
pub fn len(a: Value) -> Value {
	match a.get_tag() {
		ValueTag::String => Value::float(a.to_str().len() as f64),
		ValueTag::Table => Value::float(a.to_table().len() as f64),
		_ => binds::error(Value::str("Attempted to get the length of an unsupported value")),
	}
}


#[apply(internal)]
pub fn get_fn(func: Value) -> LuaFn {
	match func.get_tag() {
		ValueTag::Function => func.to_function(),
		ValueTag::Closure => func.to_closure().get_fn(),
		_ => binds::error(new_string(["Attempted to call a non-function value: ".as_bytes(), fmt_value(&func, &mut Default::default())].concat())),
	}
}

#[apply(internal)]
pub fn get_truthy(value: Value) -> i32 {
	if value.is_truthy() { 1 } else { 0 }
}

#[apply(internal)]
pub fn val_to_i64(value: Value) -> i64 {
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i64,
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number")),
	}
}

#[apply(internal)]
pub fn i64_to_val(value: i64) -> Value {
	Value::float(value as f64)
}

#[apply(internal)]
pub fn val_to_f64(value: Value) -> f64 {
	match value.get_tag() {
		ValueTag::Number => value.to_num(),
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number")),
	}
}

#[apply(internal)]
pub fn f64_to_val(value: f64) -> Value {
	Value::float(value)
}

#[apply(internal)]
pub fn val_to_i32(value: Value) -> i32 {
	match value.get_tag() {
		ValueTag::Number => value.to_num() as i32,
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number")),
	}
}

#[apply(internal)]
pub fn i32_to_val(value: i32) -> Value {
	Value::float(value as f64)
}

#[apply(internal)]
pub fn val_to_f32(value: Value) -> f32 {
	match value.get_tag() {
		ValueTag::Number => value.to_num() as f32,
		_ => binds::error(Value::str("Attempted to convert a non-string, non-number value to a number")),
	}
}

#[apply(internal)]
pub fn f32_to_val(value: f32) -> Value {
	Value::float(value as f64)
}

#[apply(internal)]
pub fn init_tab() -> Value {
	Value::new_table()
}

#[apply(internal)]
pub fn tab_get(table: Value, key: Value) -> Value {
	let Some(table) = table.as_table() else {
		binds::error(Value::str("Attempted to index into a non-table value"));
	};

	table.get(key).unwrap_or(Value::nil())
}

#[apply(internal)]
pub fn tab_set(table: Value, key: Value, value: Value) {
	let Some(mut table) = table.as_table() else {
		binds::error(Value::str("Attempted to index into a non-table value"));
	};

	if value.get_tag() == ValueTag::Nil {
		table.remove(key);
	} else {
		table.set(key, value);
	}
}

#[apply(internal)]
pub fn tab_get_name(table: Value, key: Value) -> Value {
	match key.get_tag() {
		ValueTag::String => table.to_table().get(key).unwrap_or(Value::nil()),
		_ if cfg!(debug_assertions) => panic!("Attempted to get_name with non-string key"),
		_ => unsafe { core::hint::unreachable_unchecked() },
	}
}

//? `value` is the first param here for impl reasons on the WASM side...
#[apply(internal)]
pub fn tab_set_name(value: Value, table: Value, key: Value) {
	match key.get_tag() {
		ValueTag::String => {
			let mut table = table.to_table();
		
			if value.get_tag() == ValueTag::Nil {
				table.remove(key);
			} else {
				table.set(key, value);
			}
		},
		_ if cfg!(debug_assertions) => panic!("Attempted to set_name with non-string key"),
		_ => unsafe { core::hint::unreachable_unchecked() },
	}
}

#[apply(internal)]
pub fn put_error(value: Value) {
	binds::put_error(fmt_value(&value, &mut Default::default()));
}
#[apply(internal)]
pub fn format_table(value: Value) -> Value {
	let table = match value.get_tag() {
		ValueTag::Table => value.to_table(),
		_ => binds::error(Value::str("Attempted to format a non-table value as a table")),
	};

	let mut buf = ByteString::default();
	buf.extend_from_slice(b"{");
	for (i, (k, v)) in table.iter().enumerate() {
		if i > 0 {
			buf.extend_from_slice(b", ");
		}
		buf.extend_from_slice(fmt_value(&k, &mut Default::default()));
		buf.extend_from_slice(b": ");
		buf.extend_from_slice(fmt_value(&v, &mut Default::default()));
	}
	buf.extend_from_slice(b"}");

	new_string(buf)
}
#[apply(internal)]
pub fn print_str(value: Value) {
	binds::print(fmt_value(&value, &mut Default::default()));
}

#[panic_handler]
fn on_panic(_info: &core::panic::PanicInfo) -> ! {
	unsafe { core::hint::unreachable_unchecked() }
}
