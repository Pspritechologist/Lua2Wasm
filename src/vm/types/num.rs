type Inner = real_float::Finite<f64>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Num(Inner);

impl Num {
	pub fn new(value: Inner) -> Self {
		Self(value)
	}

	pub fn to_str(self, state: &super::LuantState) -> super::LString {
		//TODO: Seems a little silly to use a trim here, but so be it :P
		super::LString::new(state, zmij::Buffer::new().format_finite(self.val()).trim_suffix(".0"))
	}
}

macro_rules! int_impl {
	($($t:ty),*) => {
		impl Num {
			pastey::paste! { $(
				pub fn [<to_ $t>](self) -> $t { self.val() as $t }
				pub fn [<as_ $t>](self) -> Option<$t> {
					let int = self.val() as $t;
					(self.val() == int as f64).then_some(int)
				}
			)* }
		}

		$(
			impl From<$t> for Num {
				fn from(value: $t) -> Self {
					Self(Inner::new(value as f64))
				}
			}
		)*
	};
}

int_impl!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize);

impl TryFrom<f32> for Num {
	type Error = real_float::InfiniteError;
	fn try_from(value: f32) -> Result<Self, Self::Error> {
		Inner::try_new(value.into()).map(Self)
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
