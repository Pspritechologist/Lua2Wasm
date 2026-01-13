use crate::prelude::*;
use gc::Gc;
use types::Value;
use dumpster::{Trace, TraceWith, Visitor};
use hashbrown::HashTable;
use std::iter;

#[derive(Clone, Trace, Default)]
pub struct Table {
	inner: Gc<TableInner>,
}

fn hash_impl(value: &Value) -> u64 {
	match value {
		Value::Num(num) => num.val().to_bits(),
		Value::Str(gc) => gc.get_hash(),
		Value::Bool(b) => if *b { 1 } else { 0 },
		Value::Table(tab) => Gc::as_ptr(&tab.inner).addr() as u64,
		Value::Func(f) => (*f as *const ()).addr() as u64,
	}
}

impl Table {
	fn inner_mut(&mut self) -> <TableInner as TableInnerImpl>::Mut<'_> {
		self.inner.inner_mut()
	}
	fn inner(&self) -> <TableInner as TableInnerImpl>::Ref<'_> {
		self.inner.inner()
	}
}

trait TableInnerImpl {
	type Mut<'a> where Self: 'a;
	type Ref<'a> where Self: 'a;
	fn inner_mut(&self) -> Self::Mut<'_>;
	fn inner(&self) -> Self::Ref<'_>;
}

#[cfg(debug_assertions)]
#[derive(Default)]
struct TableInner(std::cell::RefCell<TableData>);
#[cfg(debug_assertions)]
impl TableInnerImpl for TableInner {
	type Mut<'a> = std::cell::RefMut<'a, TableData>;
	type Ref<'a> = std::cell::Ref<'a, TableData>;
	fn inner_mut(&self) -> Self::Mut<'_> {
		self.0.borrow_mut()
	}
	fn inner(&self) -> Self::Ref<'_> {
		self.0.borrow()
	}
}

#[cfg(not(debug_assertions))]
#[derive(Default)]
struct TableInner(std::cell::UnsafeCell<TableData>);
#[cfg(not(debug_assertions))]
impl TableInnerImpl for TableInner {
	type Mut<'a> = &'a mut TableData;
	type Ref<'a> = &'a TableData;
	fn inner_mut(&self) -> Self::Mut<'_> {
		// SAFETY: Access to this data is only ever provided for the sake of cloning
		// and is enforced throughout this file. Debug assertions ensure this during testing.
		unsafe { &mut *self.0.get() }
	}
	fn inner(&self) -> Self::Ref<'_> {
		// SAFETY: Access to this data is only ever provided for the sake of cloning
		// and is enforced throughout this file. Debug assertions ensure this during testing.
		unsafe { &*self.0.get() }
	}
}

#[derive(Clone, Default)]
struct TableData {
	meta_table: Option<Gc<Table>>,
    hash_contents: HashTable<(Value, Value)>,
	array_contents: Vec<Option<Value>>,
}

fn as_index(value: &Value) -> Option<usize> {
	value.as_num().and_then(|n| n.as_usize()).and_then(|i| i.checked_sub(1))
}

impl Table {
	pub fn new() -> Self {
		Default::default()
	}

	pub fn from_iter<I, K, V>(state: &mut crate::State, iter: I) -> Self
	where
		I: IntoIterator<Item = (K, V)>,
		K: super::ToValue,
		V: super::ToValue,
	{
		let mut table = Table::new();
		for (k, v) in iter {
			table.set(k.to_value(state), v.to_value(state));
		}
		table
	}

	pub fn set(&mut self, key: Value, value: Value) {
		if let Some(index) = as_index(&key) {
			if self.inner().array_contents.len() == index {
				self.inner_mut().array_contents.push(Some(value));
			} else {
				self.inner_mut().array_contents[index] = Some(value);
			}

			return;
		}

		let hash = hash_impl(&key);

		#[allow(unused_mut)]
		let mut inner = self.inner_mut();
		match inner.hash_contents.find_mut(hash, |(k, _)| k == &key) {
			Some(entry) => *entry = (key, value),
			None => { inner.hash_contents.insert_unique(hash, (key, value), |(k, _)| hash_impl(k)); },
		}
	}

	pub fn get(&self, key: &Value) -> Option<Value> {
		if let Some(index) = as_index(key) {
			return self.inner().array_contents.get(index).cloned().flatten();
		}

		let hash = hash_impl(key);
		self.inner().hash_contents.find(hash, |(k, _)| k == key).map(|(_, v)| v.clone())
	}

	pub fn contains(&self, key: &Value) -> bool {
		if let Some(index) = as_index(key) {
			return self.inner().array_contents.get(index).and_then(|v| v.as_ref()).is_some();
		}

		let hash = hash_impl(key);
		self.inner().hash_contents.find(hash, |(k, _)| k == key).is_some()
	}

	pub fn remove(&mut self, key: &Value) -> Option<Value> {
		if let Some(index) = as_index(key) {
			return self.inner_mut().array_contents.get_mut(index).and_then(|v| v.take());
		}

		let hash = hash_impl(key);
		self.inner_mut().hash_contents.find_entry(hash, |(k, _)| k == key)
			.map(|e| e.remove().0.1)
			.ok()
	}

	pub fn len(&self) -> usize {
		self.inner().array_contents.len()
	}
	pub fn is_empty(&self) -> bool {
		self.inner().hash_contents.is_empty() &&
			(self.inner().array_contents.is_empty() || self.inner().array_contents.iter().all(|v| v.is_none()))
	}

	pub fn meta_table(&self) -> Option<Gc<Table>> {
		self.inner().meta_table.clone()
	}
	pub fn set_meta_table(&mut self, table: Gc<Table>) {
		self.inner_mut().meta_table = Some(table);
	}
	pub fn clear_meta_table(&mut self) {
		self.inner_mut().meta_table = None;
	}

	pub fn get_ptr(&self) -> *const u8 {
		Gc::as_ptr(&self.inner) as *const u8
	}

	pub fn eq_ref(&self, other: &Table) -> bool {
		Gc::ptr_eq(&self.inner, &other.inner)
	}
	pub fn cmp_ref(&self, other: &Table) -> std::cmp::Ordering {
		Gc::as_ptr(&self.inner).addr().cmp(&Gc::as_ptr(&other.inner).addr())
	}

	// pub fn iter(&self) -> impl iter::ExactSizeIterator<Item = (Value, Value)> + iter::FusedIterator + Clone + std::fmt::Debug {
	// 	self.iter_raw()
	// }
	// pub fn keys(&self) -> impl iter::ExactSizeIterator<Item = Value> + iter::FusedIterator + Clone + std::fmt::Debug {
	// 	self.iter_raw().map(|(k, _)| k)
	// }
	// pub fn values(&self) -> impl iter::ExactSizeIterator<Item = Value> + iter::FusedIterator + Clone + std::fmt::Debug {
	// 	self.iter_raw().map(|(_, v)| v)
	// }
	// pub fn iter_array(&self) -> impl iter::ExactSizeIterator<Item = (usize, Value)> + iter::FusedIterator + Clone + std::fmt::Debug {
		
	// }

	// fn iter_raw(&self) -> impl iter::ExactSizeIterator<Item = (Value, Value)> + iter::FusedIterator + Clone + std::fmt::Debug {
	// 	use hashbrown::hash_table::Iter;
	// 	let inner = self.inner();
	// 	let iter = inner.hash_contents.iter();
	// 	let iter: Iter<'static, (Value, Value)> = unsafe {
	// 		std::mem::transmute(iter)
	// 	};
	// 	iter.cloned().map(|(k, v)| (k, v))
	// }
}

// struct Iter

// impl IntoIterator for Table {
// 	type Item = (Value, Value);
// 	type IntoIter = hashbrown::hash_table::IntoIter<(Value, Value)>;

// 	fn into_iter(self) -> Self::IntoIter {
// 		self.inner().hash_contents.into_iter()
// 	}
// }

// impl<'a> IntoIterator for &'a Table {
// 	type Item = &'a (Value, Value);
// 	type IntoIter = hashbrown::hash_table::Iter<'a, (Value, Value)>;

// 	fn into_iter(self) -> Self::IntoIter {
// 		self.inner().hash_contents.iter()
// 	}
// }

impl std::fmt::Debug for Table {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut dbg = f.debug_struct("Table");

		dbg.field("contents", &self.inner().hash_contents);

		if let Some(mt) = &self.inner().meta_table {
			dbg.field("meta_table", mt);
		}

		dbg.finish()
	}
}

unsafe impl<Z: Visitor> TraceWith<Z> for TableInner {
	fn accept(&self, visitor: &mut Z) -> Result<(), ()> {
		self.inner().meta_table.accept(visitor)?;

		for (k, v) in &self.inner().hash_contents {
			k.accept(visitor)?;
			v.accept(visitor)?;
		}

		Ok(())
	}
}
