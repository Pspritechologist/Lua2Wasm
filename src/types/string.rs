use std::ops::Deref;

use crate::prelude::*;
use gc::{Trace, Gc};

#[repr(C)]
#[derive(Debug, Trace)]
pub struct LString {
	hash: u64,
	data: str,
}

impl LString {
	pub fn new(state: &mut State, data: impl AsRef<str>) -> Gc<Self> {
		let data = data.as_ref();

		let hash = state.hasher.hash_bytes(data);

		let buf = &mut state.gc_buf;
		buf.clear();
		buf.extend(hash.to_ne_bytes());
		buf.extend(data.as_bytes());

		let ptr: *const _ = Gc::__private_into_ptr(Gc::<[u8]>::from(buf.as_slice()));
		let gc: Gc<LString> = unsafe { Gc::__private_from_ptr(std::ptr::from_raw_parts(ptr as *const (), data.len())) };

		debug_assert_eq!(gc.hash, hash);
		debug_assert_eq!(&gc.data, data);

		gc
	}

	pub fn as_str(&self) -> &str {
		&self.data
	}

	pub fn get_hash(&self) -> u64 {
		self.hash
	}
}

impl AsRef<str> for LString {
	fn as_ref(&self) -> &str { self }
}

impl Deref for LString {
	type Target = str;
	fn deref(&self) -> &Self::Target {
		&self.data
	}
}
