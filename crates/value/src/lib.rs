#![no_std]
#![feature(bstr)]

use core::bstr::ByteStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueTag {
	Nil = 0,
	Bool = 1,
	Number = 2,
	String = 3,
	Table = 4,
	Closure = 5,
	Function = 6,
}
impl ValueTag {
	pub fn try_from_u8(n: u8) -> Option<Self> {
		match n {
			0 => Some(ValueTag::Nil),
			1 => Some(ValueTag::Bool),
			2 => Some(ValueTag::Number),
			3 => Some(ValueTag::String),
			4 => Some(ValueTag::Table),
			5 => Some(ValueTag::Closure),
			6 => Some(ValueTag::Function),
			_ => None,
		}
	}

	pub fn from_u8(n: u8) -> Self {
		Self::try_from_u8(n).unwrap_or_else(|| {
			debug_assert!(false, "Invalid value tag");
			unsafe { core::hint::unreachable_unchecked(); }
		})
	}

	pub fn as_u8(self) -> u8 {
		self as u8
	}
}

// const _: () = if size_of::<usize>() != size_of::<u32>() {
// 	panic!("pointer is expected to be 32bits");
// };

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Value {
	data: [u8; 8],
}
impl Value {
	pub fn as_i64(self) -> i64 {
		i64::from_le_bytes(self.data)
	}
	pub fn from_i64(n: i64) -> Self {
		Self { data: n.to_le_bytes() }
	}

	pub fn set_tag(&mut self, tag: ValueTag) {
		self.data[0] = (self.data[0] & 0xF0) | (tag as u8 & 0x0F);
	}
	pub fn get_tag(&self) -> ValueTag {
		ValueTag::from_u8(self.data[0] & 0x0F)
	}

	pub fn nil() -> Self {
		Self { data: [0u8; 8] }
	}
	// fn int(n: i64) -> Self {
	// 	let mut v = Self::from_i64(n);
	// 	v.set_tag(ValueTag::Number);
	// 	v
	// }
	pub fn float(n: f64) -> Self {
		let mut v = Self { data: n.to_le_bytes() };
		v.set_tag(ValueTag::Number);
		v
	}
	pub fn bool(b: bool) -> Self {
		let mut v = Self::from_i64(if b { 0x10 } else { 0 });
		v.set_tag(ValueTag::Bool);
		v
	}
	pub fn string(addr: u32, len: u32) -> Self {
		let bytes = ((addr as i64) << 32) | (len as i64);
		let mut v = Self::from_i64(bytes);
		v.set_tag(ValueTag::String);
		v
	}
	pub fn function(idx: usize) -> Self {
		let bytes = (u32::try_from(idx).expect(":(") as i64) << 32;
		let mut v = Self::from_i64(bytes);
		v.set_tag(ValueTag::Function);
		v
	}

	pub fn meaningful_bits(mut self) -> [u8; 8] {
		self.data[0] &= 0xF0; // Clear the tag bits.
		self.data
	}

	pub fn to_num(self) -> f64 {
		f64::from_le_bytes(self.meaningful_bits())
	}

	pub fn to_bool(self) -> bool {
		// Checks if the first non-tag bit is set.
		(self.data[0] & 0x10) != 0
	}
	pub fn to_str(&self) -> &ByteStr {
		let bytes = self.meaningful_bits();
		let ptr = u32::from_le_bytes(bytes[4..8].try_into().unwrap()) as *const u8;
		let len = u32::from_le_bytes(bytes[0..4].try_into().unwrap()) as usize;
		let data = unsafe { core::slice::from_raw_parts(ptr, len) };
		ByteStr::new(data)
	}
	pub fn to_function(self) -> extern "C" fn(usize) -> usize {
		let bytes = self.meaningful_bits();
		let ptr = u32::from_le_bytes(bytes[4..8].try_into().unwrap()) as usize;
		let ptr = ptr as *const u8;
		unsafe { core::mem::transmute(ptr) }
	}
}
