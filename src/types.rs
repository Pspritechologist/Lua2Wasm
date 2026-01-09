use crate::prelude::*;
use gc::Gc;
use std::cell::RefCell;

mod num;
mod table;
mod string;
mod hasher;

pub use num::Num;
pub use table::Table;
pub use string::LString;
pub use hasher::Hasher;

#[derive(Debug, Clone)]
pub enum Value {
	Num(Num),
	Str(Gc<LString>),
	Bool(bool),
	Buffer(Gc<[u8]>),
	Table(Gc<RefCell<table::Table>>),
	Func(fn(args: &[Option<Value>]) -> Vec<Option<Value>>),
}

impl Value {
	pub fn is_truthy(&self) -> bool {
		!matches!(self, Value::Bool(false))
	}

	pub fn len(&self) -> Option<usize> {
		match self {
			Value::Str(s) => Some(s.len()),
			Value::Buffer(buf) => Some(buf.len()),
			Value::Table(tab) => Some(tab.borrow().len()),
			_ => None,
		}
	}
}

unsafe impl<V: dumpster::Visitor> dumpster::TraceWith<V> for Value {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Value::Num(num) => num.accept(visitor),
            Value::Str(s) => s.accept(visitor),
            Value::Bool(b) => b.accept(visitor),
            Value::Buffer(buf) => buf.accept(visitor),
            Value::Table(tab) => tab.accept(visitor),
            Value::Func(_) => Ok(()),
        }
    }
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !f.alternate() {
			return match self {
				Value::Num(num) => write!(f, "{}", num.val()),
				Value::Str(s) => write!(f, "'{}'", s.as_str()),
				Value::Bool(b) => write!(f, "{b}"),
				Value::Buffer(buf) => write!(f, "Buffer({:p}, len={})", Gc::as_ptr(buf), buf.len()),
				Value::Table(tab) => write!(f, "Table({:p})", Gc::as_ptr(tab)),
				Value::Func(func) => write!(f, "Func({:p})", *func as *const ()),
			};
		}

		match self {
			Value::Num(num) => write!(f, "{:#}", num.val()),
			Value::Str(s) => write!(f, "{}", s.as_str()),
			Value::Bool(b) => write!(f, "{b}"),
			Value::Buffer(buf) => f.debug_list().entries(&**buf).finish(),
			Value::Table(tab) => {
				write!(f, "Table({:p}) {{", Gc::as_ptr(tab))?;

				let entries = tab.borrow();
				let mut entries = (&*entries).into_iter();

				for (k, v) in entries.by_ref().take(7) {
					write!(f, "[{k}] = {v}, ")?;
				}

				if entries.next().is_none() {
					write!(f, "}}")
				} else {
					write!(f, "... }}")
				}
			},
			Value::Func(func) => write!(f, "Func({:p})", *func as *const ()),
		}
	}
}

impl Eq for Value {}
impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Value::Num(a), Value::Num(b)) => a == b,
			(Value::Str(a), Value::Str(b)) => Gc::ptr_eq(a, b) || (a.get_hash() == b.get_hash() && a.len() == b.len() && a.as_str() == b.as_str()),
			(Value::Bool(a), Value::Bool(b)) => a == b,
			(Value::Buffer(a), Value::Buffer(b)) => Gc::ptr_eq(a, b),
			(Value::Table(a), Value::Table(b)) => Gc::ptr_eq(a, b),
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
			(Value::Buffer(a), Value::Buffer(b)) => Some(Gc::as_ptr(a).addr().cmp(&Gc::as_ptr(b).addr())),
			(Value::Table(a), Value::Table(b)) => Some(Gc::as_ptr(a).addr().cmp(&Gc::as_ptr(b).addr())),
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
	fn to_value(self, state: &mut State) -> Value;
}

impl ToValue for Value {
	fn to_value(self, _state: &mut State) -> Value { self }
}

impl ToValue for Gc<LString> {
	fn to_value(self, _state: &mut State) -> Value {
		Value::Str(self)
	}
}

impl ToValue for &str {
	fn to_value(self, state: &mut State) -> Value {
		Value::Str(LString::new(state, self))
	}
}

macro_rules! impl_num {
	($($t:ty),*) => {
		$(
			impl ToValue for $t {
				fn to_value(self, _state: &mut State) -> Value {
					Value::Num(Num::try_from(self as f64).unwrap_or_default())
				}
			}

			impl ToValue for &$t {
				fn to_value(self, state: &mut State) -> Value {
					(*self).to_value(state)
				}
			}
		)*
	};
}
impl_num!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize, f32, f64);

impl ToValue for real_float::Finite<f64> {
	fn to_value(self, _state: &mut State) -> Value {
		Value::Num(Num::new(self))
	}
}

impl ToValue for &real_float::Finite<f64> {
	fn to_value(self, state: &mut State) -> Value {
		(*self).to_value(state)
	}
}
