use core::ptr::{self, NonNull};

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

	fn new_empty_closure(function: LuaFn, upvalue_count: usize) -> *mut Self {
		let dst: Box<slice_dst::SliceWithHeader<LuaFn, UpValue>> =
			slice_dst::SliceWithHeader::new(function, core::iter::repeat_n(UpValue::null(), upvalue_count));
		let ptr = Box::into_raw(dst); //TODO: Gc.
		
		// SAFETY: `Self` is transparent over `SliceWithHeader<LuaFn, UpValue>`.
		// `cast` doesn't like unsized types for some reason, thus the `as`...
		ptr as *mut Self
	}

	fn take_nth_upvalue_into(&mut self, other: ShtackVal, from: usize, into: usize) {
		let upvalue = unsafe { &*other.read().to_closure().inner }.slice[from];
		self.upvalues()[into] = upvalue;
	}

	pub fn get_fn(&self) -> LuaFn {
		unsafe { &*self.inner }.header
	}

	fn upvalues(&mut self) -> &mut [UpValue] {
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
pub fn new_closure(start: *mut Value, len: usize) -> *mut UpValue {
	ptr::null_mut()
}
