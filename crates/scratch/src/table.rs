use super::{Value, ValueTag};
use alloc::vec::Vec;
use alloc::boxed::Box;
use hashbrown::HashTable;
use core::arch::wasm32;

extern crate alloc;

pub trait TabValueExt {
	fn to_table(self) -> Table;
	fn as_table(self) -> Option<Table>;
	fn table(table: Table) -> Self;
	fn new_table() -> Self;
}
impl TabValueExt for Value {
	fn to_table(self) -> Table {
		unsafe { Table::from_raw(self.to_idx() as _) }
	}

	fn as_table(self) -> Option<Table> {
		(self.get_tag() == ValueTag::Table).then(|| self.to_table())
	}

	fn table(table: Table) -> Self {
		let idx = table.get_ptr() as usize;
		let mut val = unsafe { Value::idx(idx) };
		val.set_tag(ValueTag::Table);
		val
	}

	fn new_table() -> Self {
		Self::table(Table::new())
	}
}

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

#[derive(Clone, Copy)]
pub struct Table {
	inner: *const TableInner,
}

fn hash_impl(value: &Value) -> u64 {
	match value.get_tag() {
		ValueTag::Nil => 0,
		ValueTag::Number => value.to_num().to_bits(),
		ValueTag::String => Hasher::new(0).hash_bytes(value.to_str()),
		ValueTag::Bool => if value.to_bool() { 1 } else { 0 },
		ValueTag::Table => value.to_table().get_ptr() as u64,
		ValueTag::Closure => todo!(),
		#[allow(clippy::fn_to_numeric_cast)]
		ValueTag::Function => value.to_function() as u64,
	}
}

impl Table {
	fn inner_mut(&mut self) -> <TableInner as TableInnerImpl>::Mut<'_> {
		unsafe { &*self.inner }.inner_mut()
	}
	fn inner(&self) -> <TableInner as TableInnerImpl>::Ref<'_> {
		unsafe { &*self.inner }.inner()
	}
}

trait TableInnerImpl {
	type Mut<'a> where Self: 'a;
	type Ref<'a> where Self: 'a;
	fn inner_mut(&self) -> Self::Mut<'_>;
	fn inner(&self) -> Self::Ref<'_>;
}

#[derive(Default)]
struct TableInner(core::cell::UnsafeCell<TableData>);

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
	meta_table: Option<Table>,
    hash_contents: HashTable<(Value, Value)>,
	array_contents: Vec<Value>,
}

fn as_usize(value: f64) -> Option<usize> {
	(value == 0.0 || (value.is_sign_positive() && value == wasm32::f64_trunc(value))).then_some(value as usize)
}

fn as_index(value: &Value) -> Option<usize> {
	value.as_num().and_then(as_usize).and_then(|i| i.checked_sub(1))
}

impl Table {
	pub fn new() -> Self {
		Self { inner: Box::into_raw(Default::default()) }
	}

	// pub fn from_iter<I, K, V>(state: &super::LuantState, iter: I) -> Self
	// where
	// 	I: IntoIterator<Item = (K, V)>,
	// 	K: super::ToValue,
	// 	V: super::ToValue,
	// {
	// 	let mut table = Table::new();
	// 	for (k, v) in iter {
	// 		table.set(k.to_value(state), v.to_value(state));
	// 	}
	// 	table
	// }

	pub fn set(&mut self, key: Value, value: Value) {
		if let Some(index) = as_index(&key) {
			if self.inner().array_contents.len() == index {
				self.inner_mut().array_contents.push(value);
			} else {
				self.inner_mut().array_contents[index] = value;
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
			return self.inner().array_contents.get(index).copied();
		}

		let hash = hash_impl(key);
		self.inner().hash_contents.find(hash, |(k, _)| k == key).map(|(_, v)| *v)
	}

	pub fn contains(&self, key: &Value) -> bool {
		if let Some(index) = as_index(key) {
			return self.inner().array_contents.get(index).is_some();
		}

		let hash = hash_impl(key);
		self.inner().hash_contents.find(hash, |(k, _)| k == key).is_some()
	}

	pub fn remove(&mut self, key: &Value) -> Option<Value> {
		if let Some(index) = as_index(key) {
			return self.inner_mut().array_contents.get_mut(index).map(|v| {
				let val = *v;
				*v = Value::nil();
				val
			});
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
			self.inner().array_contents.first().is_none_or(|v| v.is_nil())
	}

	pub fn meta_table(&self) -> Option<Table> {
		self.inner().meta_table.clone()
	}
	pub fn set_meta_table(&mut self, table: Table) {
		self.inner_mut().meta_table = Some(table);
	}
	pub fn clear_meta_table(&mut self) {
		self.inner_mut().meta_table = None;
	}

	pub fn get_ptr(&self) -> *const u8 {
		self.inner as *const u8
	}

	pub fn eq_ref(&self, other: &Table) -> bool {
		core::ptr::addr_eq(&self.inner, &other.inner)
	}
	pub fn cmp_ref(&self, other: &Table) -> core::cmp::Ordering {
		self.inner.cmp(&other.inner)
	}

	pub unsafe fn from_raw(ptr: *const u8) -> Self {
		Self { inner: ptr.cast() }
	}

	// pub fn iter(&self) -> impl iter::ExactSizeIterator<Item = (Value, Value)> + iter::FusedIterator + Clone + core::fmt::Debug {
	// 	self.iter_raw()
	// }
	// pub fn keys(&self) -> impl iter::ExactSizeIterator<Item = Value> + iter::FusedIterator + Clone + core::fmt::Debug {
	// 	self.iter_raw().map(|(k, _)| k)
	// }
	// pub fn values(&self) -> impl iter::ExactSizeIterator<Item = Value> + iter::FusedIterator + Clone + core::fmt::Debug {
	// 	self.iter_raw().map(|(_, v)| v)
	// }
	// pub fn iter_array(&self) -> impl iter::ExactSizeIterator<Item = (usize, Value)> + iter::FusedIterator + Clone + core::fmt::Debug {
		
	// }

	// fn iter_raw(&self) -> impl iter::ExactSizeIterator<Item = (Value, Value)> + iter::FusedIterator + Clone + core::fmt::Debug {
	// 	use hashbrown::hash_table::Iter;
	// 	let inner = self.inner();
	// 	let iter = inner.hash_contents.iter();
	// 	let iter: Iter<'static, (Value, Value)> = unsafe {
	// 		core::mem::transmute(iter)
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

// impl core::fmt::Debug for Table {
// 	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
// 		let mut dbg = f.debug_struct("Table");

// 		dbg.field("contents", &self.inner().hash_contents);

// 		if let Some(mt) = &self.inner().meta_table {
// 			dbg.field("meta_table", mt);
// 		}

// 		dbg.finish()
// 	}
// }
