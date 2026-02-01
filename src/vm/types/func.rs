use super::Value;
use crate::gc::{Gc, Trace};
use std::cell::Cell;
use std::rc::Rc;

#[derive(Clone, Trace)]
pub struct Closure {
	pub proto_idx: usize,
	pub upvalues: Gc<[UpvalueSlot]>,
}

impl std::fmt::Debug for Closure {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Closure")
			.field("proto_idx", &self.proto_idx)
			.field("upvalues", &self.upvalues.len())
			// .field("ptr", self.upvalues.as_ptr())
			// .field_with("ptr", |f| f.write_fmt(format_args!("{:p}", self.upvalues.as_ptr())))
			.finish()
	}
}

impl Closure {
	pub fn new(proto_idx: usize, upvalues: impl IntoIterator<Item = UpvalueSlot>) -> Self {
		let upvalues: Vec<_> = upvalues.into_iter().collect();

		Self {
			proto_idx,
			upvalues: upvalues.into(),
		}
	}
}

#[derive(Clone)]
pub struct UpvalueSlot {
	inner: Rc<Cell<UpvalueRef>>,
}
impl UpvalueSlot {
	pub fn new(upvalue: UpvalueRef) -> Self {
		Self { inner: Cell::new(upvalue).into() }
	}

	pub fn get(&self) -> UpvalueRef {
		self.inner.get_cloned().into_inner()
	}

	pub fn set(&self, upvalue: UpvalueRef) {
		self.inner.set(upvalue);
	}

	pub fn get_value(&self, stack: &[Option<Value>]) -> Option<Value> {
		// SAFETY: No references escape from this function.
		let upval = unsafe { self.inner.as_ptr().as_ref().unwrap_unchecked() };
		match upval {
			&UpvalueRef::Open(idx) => stack[idx].clone(),
			UpvalueRef::Closed(inner) => inner.get(),
		}
	}

	pub fn set_value(&self, value: Option<Value>) -> Option<(usize, Option<Value>)> {
		// SAFETY: No references escape from this function.
		let upval = unsafe { self.inner.as_ptr().as_ref().unwrap_unchecked() };
		match upval {
			&UpvalueRef::Open(idx) => return Some((idx, value)),
			UpvalueRef::Closed(inner) => inner.set(value),
		}

		None
	}

	pub fn close_if_above(&self, stack: &[Option<Value>], base: usize) -> bool {
		// SAFETY: No references escape from this function.
		let upval = unsafe { self.inner.as_ptr().as_mut().unwrap_unchecked() };
		if let &mut UpvalueRef::Open(idx) = upval && idx >= base {
			let value = stack[idx].clone();
			let closed = ClosedUpvalueInner::new(value);
			*upval = UpvalueRef::Closed(Gc::new(closed));

			return true;
		}

		false
	}

	pub fn open_idx(&self) -> Option<usize> {
		// SAFETY: No references escape from this function.
		let upval = unsafe { self.inner.as_ptr().as_ref().unwrap_unchecked() };
		if let &UpvalueRef::Open(idx) = upval {
			Some(idx)
		} else {
			None
		}
	}
}
unsafe impl<V: dumpster::Visitor> dumpster::TraceWith<V> for UpvalueSlot {
	fn accept(&self, visitor: &mut V) -> Result<(), ()> {
		// SAFETY: References to the inner value are never handed out.
		// The reference used here is immutable so recursion is sound.
		unsafe { self.inner.as_ptr().as_ref() }.accept(visitor)
	}
}

#[derive(Debug, Clone, Trace)]
pub enum UpvalueRef {
	Open(usize),
	Closed(Gc<ClosedUpvalueInner>),
}

unsafe impl std::cell::CloneFromCell for UpvalueRef { }

pub struct ClosedUpvalueInner {
	inner: Cell<Option<Value>>,
}
unsafe impl std::cell::CloneFromCell for Value { }

impl ClosedUpvalueInner {
	fn new(value: Option<Value>) -> Self {
		Self { inner: Cell::new(value) }
	}
	pub fn get(&self) -> Option<Value> {
		self.inner.get_cloned().into_inner()
	}
	pub fn set(&self, value: Option<Value>) {
		self.inner.set(value);
	}
}

impl Clone for ClosedUpvalueInner {
	fn clone(&self) -> Self {
		Self::new(self.get())
	}
}

impl std::fmt::Debug for ClosedUpvalueInner {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ClosedUpvalueInner")
			.field("inner", &self.get())
			.finish()
	}
}

unsafe impl<V: dumpster::Visitor> dumpster::TraceWith<V> for ClosedUpvalueInner {
	fn accept(&self, visitor: &mut V) -> Result<(), ()> {
		// SAFETY: References to the inner value are never handed out.
		// The reference used here is immutable so recursion is sound.
		unsafe { self.inner.as_ptr().as_ref() }.accept(visitor)
	}
}
