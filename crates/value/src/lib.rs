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

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Value {
	data: i64,
}
impl Value {
	pub fn as_i64(self) -> i64 {
		self.data
	}
	pub fn from_i64(n: i64) -> Self {
		Self { data: n }
	}

	pub fn set_tag(&mut self, tag: ValueTag) {
		let mut data = self.data.to_ne_bytes();
		data[0] = (data[0] & 0xF0) | (tag as u8 & 0x0F);
		self.data = i64::from_ne_bytes(data);
	}
	pub fn with_tag(mut self, tag: ValueTag) -> Self {
		self.set_tag(tag);
		self
	}
	pub fn get_tag(&self) -> ValueTag {
		ValueTag::from_u8(self.data.to_ne_bytes()[0] & 0x0F)
	}

	pub fn is_nil(self) -> bool {
		// Nil is always represented as all zeroes.
		self.data == Self::nil().data
	}

	pub fn nil() -> Self {
		Self { data: 0 }
	}
	// fn int(n: i64) -> Self {
	// 	let mut v = Self::from_i64(n);
	// 	v.set_tag(ValueTag::Number);
	// 	v
	// }
	pub fn float(n: f64) -> Self {
		Self { data: i64::from_ne_bytes(n.to_ne_bytes()) }
			.with_tag(ValueTag::Number)
	}
	pub fn bool(b: bool) -> Self {
		Self::from_i64(if b { 0x10 } else { 0x00 })
			.with_tag(ValueTag::Bool)
	}
	pub fn string(addr: u32, len: u32) -> Self {
		//? Strings store their length in big-endian.
		let bytes = ((addr as i64) << 32) | (len.to_be() as i64);
		Self::from_i64(bytes).with_tag(ValueTag::String)
	}

	/// # SAFETY
	/// The caller must ensure the value is properly tagged after construction.
	pub unsafe fn idx(idx: u32) -> Self {
		let mut bytes = [0; 8];
		let idx = idx.to_ne_bytes();
		bytes[4..8].copy_from_slice(&idx);
		Self::from_i64(i64::from_ne_bytes(bytes))
	}

	pub fn meaningful_bits(self) -> [u8; 8] {
		let mut data = self.data.to_ne_bytes();
		data[0] &= 0xF0; // Clear the tag bits.
		data
	}

	pub fn as_num(self) -> Option<f64> {
		(self.get_tag() == ValueTag::Number).then(|| self.to_num())
	}
	pub fn as_bool(self) -> Option<bool> {
		(self.get_tag() == ValueTag::Bool).then(|| self.to_bool())
	}
	pub fn as_addr_len(&self) -> Option<(u32, u32)> {
		(self.get_tag() == ValueTag::String).then(|| self.to_addr_len())
	}

	pub fn to_num(self) -> f64 {
		f64::from_ne_bytes(self.meaningful_bits())
	}
	pub fn to_bool(self) -> bool {
		// Checks if the first non-tag bit is set.
		(self.data.to_ne_bytes()[0] & 0x10) != 0
	}
	pub fn to_addr_len(&self) -> (u32, u32) {
		let bytes = self.meaningful_bits();
		let addr = u32::from_ne_bytes(bytes[4..8].try_into().unwrap());
		//? Strings store the length in big-endian to avoid getting eaten by the tag.
		let len = u32::from_be_bytes(bytes[0..4].try_into().unwrap());
		(addr, len)
	}
	
	pub fn to_idx(self) -> u32 {
		let bytes = self.meaningful_bits();
		u32::from_ne_bytes(bytes[4..8].try_into().unwrap())
	}

	pub fn is_truthy(self) -> bool {
		!self.is_nil() && self.as_bool().unwrap_or(true)
	}
}

macro_rules! from_num {
	($($ty:ty),*) => { $(
		impl From<$ty> for Value {
			fn from(n: $ty) -> Self { Self::float(n as f64) }
		}
	)* };
}

from_num!(i64, u64, isize, usize, i32, u32, i16, u16, i8, u8, f64, f32);

impl From<bool> for Value {
	fn from(b: bool) -> Self { Self::bool(b) }
}

impl From<()> for Value {
	fn from((): ()) -> Self { Self::nil() }
}

impl<V: Into<Value>> From<Option<V>> for Value {
	fn from(value: Option<V>) -> Self {
		value.map_or(Self::nil(), |v| v.into())
	}
}
