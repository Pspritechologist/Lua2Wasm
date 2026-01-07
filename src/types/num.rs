type Inner = real_float::Finite<f64>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Num(Inner);

impl Num {
	pub fn new(value: Inner) -> Self { Self(value) }
	pub fn as_int(self) -> Option<i64> {
		let int = self.val() as i64;
		(self.val() == int as f64).then_some(int)
	}
}

impl TryFrom<f64> for Num {
	type Error = real_float::InfiniteError;
	fn try_from(value: f64) -> Result<Self, Self::Error> {
		Inner::try_new(value).map(Self)
	}
}

impl std::ops::Deref for Num {
	type Target = Inner;
	fn deref(&self) -> &Self::Target { &self.0 }
}

impl std::ops::DerefMut for Num {
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

unsafe impl<V: dumpster::Visitor> dumpster::TraceWith<V> for Num {
	fn accept(&self, _visitor: &mut V) -> Result<(), ()> { Ok(()) }
}
