use core::ptr;
use alloc::boxed::Box;
use macro_rules_attribute::apply;
use value::Value;

use crate::{ValExt, binds::ShtackVal};

pub type LuaFn = extern "C" fn(usize) -> usize;

#[derive(Debug, Clone, Copy)]
pub struct Closure {
	inner: *mut slice_dst::SliceWithHeader<LuaFn, UpValue>,
}
impl Closure {
	pub fn from_idx(idx: u32) -> Self {
		let idx = idx as usize;
		let len = unsafe { *(idx as *const usize) };
		let ptr = ptr::from_raw_parts_mut(idx as *mut (), len);
		Self { inner: ptr }
	}

	fn new_empty_closure(function: LuaFn, upvalue_count: usize) -> Self {
		let dst: Box<slice_dst::SliceWithHeader<LuaFn, UpValue>> =
			slice_dst::SliceWithHeader::new(function, core::iter::repeat_n(UpValue::null(), upvalue_count));
		let ptr = Box::into_raw(dst); //TODO: Gc.

		Self { inner: ptr }
	}

	fn from_slice(function: LuaFn, upvalues: &[UpValue]) -> Self {
		let dst: Box<slice_dst::SliceWithHeader<LuaFn, UpValue>> =
			slice_dst::SliceWithHeader::from_slice(function, upvalues);
		let ptr = Box::into_raw(dst); //TODO: Gc.

		Self { inner: ptr }
	}

	pub fn get_fn(&self) -> LuaFn {
		unsafe { &*self.inner }.header
	}

	fn upvalues(&self) -> &[UpValue] {
		&unsafe { &*self.inner }.slice
	}

	fn upvalues_mut(&mut self) -> &mut [UpValue] {
		&mut unsafe { &mut *self.inner }.slice
	}
}

#[repr(C, packed)]
#[derive(Clone, Copy)]
pub struct UpValue {
	closed: bool,
	data: *mut Value,
}

impl UpValue {
	const fn null() -> Self {
		Self { closed: false, data: ptr::null_mut() }
	}

	fn new_open(data: *mut Value) -> Self {
		Self { closed: false, data }
	}

	fn close(&mut self) {
		debug_assert!(!self.closed, "UpValue is already closed");
		let value = self.read();
		let boxed = Box::into_raw(Box::new(value)); //TODO: Gc.
		self.data = boxed;
		self.closed = true;
	}

	fn read(&self) -> Value {
		//TODO: The dispatch when reading from an Upvalue is non-ideal.
		//TODO: In future, it may be worthwhile to the shtack exist within the same
		//TODO: linear memory that Rust uses.
		match self.closed {
			false => unsafe { ptr::read_volatile(self.data) },
			true => ShtackVal::new(self.data).read(),
		}
	}

	fn write(&mut self, value: Value) {
		match self.closed {
			false => unsafe { ptr::write_volatile(self.data, value) },
			true => ShtackVal::new(self.data).write(value),
		}
	}
}

// Extern API
#[apply(crate::internal)]
pub fn new_closure(function: LuaFn, upvalues: usize) -> *mut () {
	Closure::new_empty_closure(function, upvalues).inner.cast()
}
#[apply(crate::internal)]
pub fn new_main_closure(function: LuaFn, env_ptr: ShtackVal) -> Value {
	let closure = Closure::from_slice(function, &[UpValue::new_open(env_ptr.ptr())]);
	unsafe { Value::idx(closure.inner.addr() as u32) }.with_tag(value::ValueTag::Closure)
}
#[apply(crate::internal)]
pub fn set_nth_upvalue(ptr: *mut (), slot: usize, upvalue_slot: ShtackVal) -> *mut () {
	let mut closure = Closure::from_idx(ptr.addr() as u32);
	closure.upvalues_mut()[slot] = UpValue::new_open(upvalue_slot.ptr());
	ptr
}
#[apply(crate::internal)]
pub fn take_nth_upvalue_into(ptr: *mut (), other: ShtackVal, from: usize, into: usize) -> *mut () {
	let mut closure = Closure::from_idx(ptr.addr() as u32);
	let upvalue = unsafe { &*other.read().to_closure().inner }.slice[from];
	closure.upvalues_mut()[into] = upvalue;
	ptr
}
#[apply(crate::internal)]
pub fn finalize_closure(closure: *mut ()) -> Value {
	unsafe { Value::idx(closure as u32) }.with_tag(value::ValueTag::Closure)
}
#[apply(crate::internal)]
pub fn read_upvalue(closure: ShtackVal, idx: usize) -> Value {
	closure.read().to_closure().upvalues()[idx].read()
}
#[apply(crate::internal)]
pub fn write_upvalue(closure: ShtackVal, idx: usize, value: Value) {
	closure.read().to_closure().upvalues_mut()[idx].write(value);
}
