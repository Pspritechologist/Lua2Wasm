use crate::prelude::*;
use gc::{Trace, Gc};
use std::ops::Deref;

#[repr(C)]
#[derive(Debug, Trace)]
pub struct LString {
	hash: u64,
	data: [u8],
}

impl LString {
	pub fn new(state: &mut State, data: impl AsRef<[u8]>) -> Gc<Self> {
		let data = data.as_ref();

		let hash = state.hasher.hash_bytes(data);

		let buf = &mut state.gc_buf;
		buf.clear();
		buf.extend(hash.to_ne_bytes());
		buf.extend(data);
		
		// SAFETY: We just constructed `buf` to have a valid hash and valid UTF-8 data.
		let gc = unsafe { Self::new_from_buf(buf, data.len()) };

		debug_assert_eq!(gc.hash, hash);
		debug_assert_eq!(&gc.data, data);

		gc
	}

	/// # Safety
	/// Caller must ensure that the first eight bytes of `buf` represent a valid hash
	/// of the remaining bytes, and that the remaining bytes are valid UTF-8 data of length `data_len`.
	pub unsafe fn new_from_buf(buf: &[u8], data_len: usize) -> Gc<Self> {
		let ptr: *const _ = Gc::__private_into_ptr(Gc::<[u8]>::from(buf));
		// SAFETY: Caller must ensure safety.
		unsafe { Gc::__private_from_ptr(std::ptr::from_raw_parts(ptr as *const (), data_len)) }
	}

	pub fn as_str(&self) -> &BStr {
		BStr::new(&self.inner.data)
	}

	pub fn get_hash(&self) -> u64 {
		self.hash
	}
}

impl AsRef<BStr> for LString {
	fn as_ref(&self) -> &BStr { self }
}

impl Deref for LString {
	type Target = BStr;
	fn deref(&self) -> &Self::Target {
		self.as_str()
	}
}
