use real_float::Finite;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operation {
	// Loads.
	LoadNil(u8, u8),
	LoadBool(u8, bool),
	LoadNum(u8, Finite<f64>),
	LoadStr(u8, usize),
	LoadTab(u8),
	LoadClosure(u8, usize),
	// Tables.
	Set(u8, u8, u8),
	Get(u8, u8, u8),
	// Arithmetic.
	Add(u8, u8, u8),
	Sub(u8, u8, u8),
	Mul(u8, u8, u8),
	Div(u8, u8, u8),
	Mod(u8, u8, u8),
	Pow(u8, u8, u8),
	Neg(u8, u8),
	// Logic.
	Eq(u8, u8, u8),
	Neq(u8, u8, u8),
	Lt(u8, u8, u8),
	Lte(u8, u8, u8),
	Gt(u8, u8, u8),
	Gte(u8, u8, u8),
	Not(u8, u8),
	// Bitwise.
	BitAnd(u8, u8, u8),
	BitOr(u8, u8, u8),
	BitXor(u8, u8, u8),
	BitShL(u8, u8, u8),
	BitShR(u8, u8, u8),
	BitNot(u8, u8),
	// Misc operators.
	Concat(u8, u8, u8),
	Len(u8, u8),
	// Calling.
	Call(u8, u8, u8),
	Ret(u8, u8),
	// Control.
	GoTo([u8; 3]),
	SkpIf(u8),
	SkpIfNot(u8),
	// Meta.
	Copy(u8, u8),
	GetUpVal(u8, u8),
	SetUpVal(u8, u8),
	GetUpTab(u8, usize),
	SetUpTab(usize, u8),
	Close(u8),
}

impl Operation {
	pub fn tmp_goto() -> Self {
		Self::GoTo([0, 0, 0])
	}

	pub fn goto(position: usize) -> Self {
		let encoded = Self::encode_goto(position);
		Self::GoTo(encoded)
	}

	pub fn encode_goto(position: usize) -> [u8; 3] {
		// Ensure the position can fit in 24 bits.
		debug_assert!(position < (1 << 24), "Goto position out of range");

		let encoded = position as u32;
		[
			((encoded >> 16) & 0xFF) as u8,
			((encoded >> 8) & 0xFF) as u8,
			(encoded & 0xFF) as u8,
		]
	}

	pub fn decode_goto(encoded: [u8; 3]) -> usize {
		let [a, b, c] = encoded;
		let combined = ((a as u32) << 16) | ((b as u32) << 8) | (c as u32);
		combined as usize
	}

	pub fn update_goto_target(&mut self, new_target: usize) {
		if let Self::GoTo(encoded) = self {
			*encoded = Self::encode_goto(new_target);
		} else {
			debug_assert!(false, "Attempted to update target of non-goto operation");
			unsafe { std::hint::unreachable_unchecked() };
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_goto_encoding() {
		let positions = [0, 1, 2, 12345, 54321, 0x7FFFFF, 0xFFFFFF];

		for &pos in &positions {
			let encoded = Operation::encode_goto(pos);
			let decoded_pos = Operation::decode_goto(encoded);
			assert_eq!(pos, decoded_pos, "Position mismatch for pos={pos}");
		}
	}
}
