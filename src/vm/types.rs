use crate::Luant;

mod num;
mod table;
mod string;
mod func;
mod hasher;

pub use num::Num;
pub use table::Table;
pub use string::LString;
pub use hasher::Hasher;
pub use func::{Closure, UpvalueRef, UpvalueSlot};

#[derive(Debug, Clone)]
pub enum Value {
	Num(Num),
	Str(LString),
	Bool(bool),
	Table(Table),
	Closure(func::Closure),
	Func(fn(args: &[Option<Value>]) -> Result<Vec<Option<Value>>, Box<dyn std::error::Error>>),
}

impl Value {
	pub fn is_truthy(&self) -> bool {
		!matches!(self, Value::Bool(false))
	}

	pub fn len(&self) -> Option<usize> {
		match self {
			Value::Str(s) => Some(s.len()),
			Value::Table(tab) => Some(tab.len()),
			_ => None,
		}
	}

	pub fn as_num(&self) -> Option<Num> {
		match self {
			Value::Num(n) => Some(*n),
			_ => None,
		}
	}

	pub fn coerce_num(val: Option<&Self>) -> Result<Num, Box<dyn std::error::Error>> {
		Ok(match val {
			Some(Value::Num(n)) => *n,
			Some(Value::Str(s)) => s.to_num()?,
			None => return Err("Expected number found nil".into()),
			_ => return Err("Cannot coerce to number".into()),
		})
	}

	pub fn coerce_str(val: Option<&Self>, state: &Luant) -> Result<LString, Box<dyn std::error::Error>> {
		Ok(match val {
			Some(Value::Str(s)) => s.clone(),
			Some(Value::Num(n)) => n.to_str(state),
			_ => return Err("Cannot coerce to string".into()),
		})
	}
}

unsafe impl<V: dumpster::Visitor> dumpster::TraceWith<V> for Value {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Value::Num(num) => num.accept(visitor),
            Value::Str(s) => s.accept(visitor),
            Value::Bool(b) => b.accept(visitor),
            Value::Table(tab) => tab.accept(visitor),
			Value::Closure(_) => Ok(()),
            Value::Func(_) => Ok(()),
        }
    }
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !f.alternate() {
			return match self {
				Value::Num(num) => write!(f, "{}", num.val()),
				Value::Str(s) => write!(f, "{}", s.as_str()),
				Value::Bool(b) => write!(f, "{b}"),
				Value::Table(tab) => write!(f, "Table({:p})", tab.get_ptr()),
				Value::Closure(_) => write!(f, "Closure(...)"),
				Value::Func(func) => write!(f, "Func({:p})", *func as *const ()),
			};
		}

		match self {
			Value::Num(num) => write!(f, "{:#}", num.val()),
			Value::Str(s) => write!(f, "{:#}", s.as_str()),
			Value::Bool(b) => write!(f, "{b:#}"),
			Value::Table(tab) => {
				write!(f, "Table({:p}) {{", tab.get_ptr())?;

				// let entries = tab.borrow();
				// let mut entries = (&*entries).into_iter();

				// for (k, v) in entries.by_ref().take(7) {
				// 	write!(f, "[{k}] = {v}, ")?;
				// }

				// if entries.next().is_none() {
				// 	write!(f, "}}")
				// } else {
				// 	write!(f, "... }}")
				// }
					write!(f, "}}")
			},
			Value::Closure(_) => write!(f, "Closure(...)"),
			Value::Func(func) => write!(f, "Func({:p})", *func as *const ()),
		}
	}
}

impl Eq for Value {}
impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Value::Num(a), Value::Num(b)) => a == b,
			(Value::Str(a), Value::Str(b)) => a.eq_impl(b),
			(Value::Bool(a), Value::Bool(b)) => a == b,
			(Value::Table(a), Value::Table(b)) => a.eq_ref(b),
			(Value::Func(a), Value::Func(b)) => std::ptr::addr_eq(*a as *const (), *b as *const ()),
			_ => false,
		}
	}
}

impl PartialOrd for Value {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self, other) {
			(Value::Num(a), Value::Num(b)) => Some(a.cmp(b)),
			(Value::Str(a), Value::Str(b)) => Some(a.cmp(b)),
			(Value::Bool(a), Value::Bool(b)) => Some(a.cmp(b)),
			(Value::Table(a), Value::Table(b)) => Some(a.cmp_ref(b)),
			_ => None,
		}
	}
}

impl PartialEq<Option<Self>> for Value {
	fn eq(&self, other: &Option<Self>) -> bool {
		other.as_ref().map(|o| self == o).unwrap_or(false)
	}
}

impl PartialOrd<Option<Self>> for Value {
	fn partial_cmp(&self, other: &Option<Self>) -> Option<std::cmp::Ordering> {
		other.as_ref().map(|o| self.partial_cmp(o)).flatten()
	}
}

pub trait ToValue {
	fn to_value(self, state: &Luant) -> Value;
}

impl ToValue for Value {
	fn to_value(self, _state: &Luant) -> Value { self }
}

impl ToValue for LString {
	fn to_value(self, _state: &Luant) -> Value {
		Value::Str(self)
	}
}

impl ToValue for &str {
	fn to_value(self, state: &Luant) -> Value {
		Value::Str(LString::new(state, self))
	}
}
impl ToValue for &bstr::BStr {
	fn to_value(self, state: &Luant) -> Value {
		Value::Str(LString::new(state, self))
	}
}
impl ToValue for &[u8] {
	fn to_value(self, state: &Luant) -> Value {
		Value::Str(LString::new(state, self))
	}
}

macro_rules! impl_num {
	($($t:ty),*) => {
		$(
			impl ToValue for $t {
				fn to_value(self, _state: &Luant) -> Value {
					Value::Num(Num::try_from(self as f64).unwrap_or_default())
				}
			}

			impl ToValue for &$t {
				fn to_value(self, state: &Luant) -> Value {
					(*self).to_value(state)
				}
			}
		)*
	};
}
impl_num!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize, f32, f64);

impl ToValue for real_float::Finite<f64> {
	fn to_value(self, _state: &Luant) -> Value {
		Value::Num(Num::new(self))
	}
}

impl ToValue for &real_float::Finite<f64> {
	fn to_value(self, state: &Luant) -> Value {
		(*self).to_value(state)
	}
}
