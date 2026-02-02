use super::Num;
use crate::gc::{Trace, Gc};
use bstr::BStr;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
pub struct LString {
	inner: Gc<LStringInner>,
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq, Trace)]
struct LStringInner {
	hash: u64,
	data: [u8],
}

impl LString {
	pub fn new(state: &super::LuantState, data: impl AsRef<[u8]>) -> Self {
		let mut buf = state.vm_state.gc_buf.take();

		let data = data.as_ref();

		let hash = state.vm_state.hasher.hash_bytes(data);

		buf.clear();
		buf.extend(hash.to_ne_bytes());
		buf.extend(data);
		
		// SAFETY: We just constructed `buf` to have a valid hash and valid UTF-8 data.
		let gc = unsafe { Self::new_from_buf(&buf, data.len()) };

		debug_assert_eq!(gc.inner.hash, hash);
		debug_assert_eq!(&gc.inner.data, data);

		state.vm_state.gc_buf.set(buf);

		gc
	}

	/// # Safety
	/// Caller must ensure that the first eight bytes of `buf` represent a valid hash
	/// of the remaining bytes, and that the remaining bytes are valid UTF-8 data of length `data_len`.
	pub unsafe fn new_from_buf(buf: &[u8], data_len: usize) -> Self {
		let ptr: *const _ = Gc::__private_into_ptr(Gc::<[u8]>::from(buf));
		// SAFETY: Caller must ensure safety.
		let inner = unsafe { Gc::__private_from_ptr(std::ptr::from_raw_parts(ptr as *const (), data_len)) };
		Self { inner }
	}

	pub fn as_str(&self) -> &BStr {
		BStr::new(&self.inner.data)
	}

	pub fn get_hash(&self) -> u64 {
		self.inner.hash
	}

	/// First compares by ref, falling back to hashing, len, and finally content comparison.
	pub fn eq_impl(&self, other: &LString) -> bool {
		let (a, b) = (self, other);
		Gc::ptr_eq(&a.inner, &b.inner) || (a.get_hash() == b.get_hash() && a.len() == b.len() && a.as_str() == b.as_str())
	}

	pub fn to_num(&self) -> Result<Num, Box<dyn std::error::Error>> {
		Ok(luant_lexer::parse_num(self.as_str()).map(Num::new)?)
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
