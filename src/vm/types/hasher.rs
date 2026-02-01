#[derive(Debug, Clone, Copy)]
pub struct Hasher {
	seed: u64,
}

impl Hasher {
	pub fn new(seed: u64) -> Self {
		Self { seed }
	}

	pub fn hash_bytes(&self, bytes: impl AsRef<[u8]>) -> u64 {
		let bytes = bytes.as_ref();
		let len = bytes.len();

		let mut hash = self.seed ^ len as u64;

		let step = (len >> 5).wrapping_add(1);

		bytes.iter()
			.copied()
			.rev()
			.step_by(step)
			.for_each(|b| hash ^= (hash << 5).wrapping_add(hash >> 2).wrapping_add(b.into()));

		hash
	}
}

impl Default for Hasher {
	fn default() -> Self {
		Self::new(fastrand::u64(..))
	}
}

#[cfg(test)]
mod tests {
	use super::Hasher;

	#[test]
	fn test_static() {
		let h = Hasher::new(0xdeadbeef);

		let (a, b) = (
			"Hello, World!",
			"Hello, World!",
		);
		assert_eq!(h.hash_bytes(a), h.hash_bytes(b));

		let b = "Hello, Mars!";
		assert_ne!(h.hash_bytes(a), h.hash_bytes(b));
	}

	#[test]
	fn test_random() {
		let mut a = String::new();
		let mut b = String::new();

		for _ in 0..100 {
			let h = Hasher::default();

			for _ in 0..100 {
				for _ in 0..fastrand::usize(0..100) {
					a.push(fastrand::char(..));
					b.push(fastrand::char(..));
				}

				assert_eq!(a == b, h.hash_bytes(&a) == h.hash_bytes(&b));
				
				b.clear();

				for _ in 0..fastrand::usize(0..100) {
					b.push(fastrand::char(..));
				}

				assert_eq!(a == b, h.hash_bytes(&a) == h.hash_bytes(&b));

				a.clear();
				b.clear();
			}
		}
	}
}
