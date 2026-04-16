use super::{Value, ValueTag, ValExt};
use alloc::vec::Vec;
use alloc::boxed::Box;
use hashbrown::HashTable;
use core::iter;

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

fn hash_impl(value: Value) -> u64 {
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
	(value == 0.0 || (value.is_sign_positive() && value == libm::trunc(value))).then_some(value as usize)
}

fn as_index(value: Value) -> Option<usize> {
	value.as_num().and_then(as_usize).and_then(|i| i.checked_sub(1))
}

impl Table {
	pub fn new() -> Self {
		Self { inner: Box::into_raw(Default::default()) }
	}

	pub fn from_iter<I, K, V>(iter: I) -> Self
	where
		I: IntoIterator<Item = (K, V)>,
		K: Into<value::Value>,
		V: Into<value::Value>,
	{
		let mut table = Table::new();
		for (k, v) in iter {
			table.set(k.into(), v.into());
		}
		table
	}

	#[inline(never)]
	pub fn set(&mut self, key: Value, value: Value) {
		if let Some(index) = as_index(key) {
			if self.inner().array_contents.len() == index {
				self.inner_mut().array_contents.push(value);
			} else {
				self.inner_mut().array_contents[index] = value;
			}

			return;
		}

		let hash = hash_impl(key);

		#[allow(unused_mut)]
		let mut inner = self.inner_mut();
		match inner.hash_contents.find_mut(hash, |(k, _)| k.equals(key)) {
			Some(entry) => *entry = (key, value),
			None => { inner.hash_contents.insert_unique(hash, (key, value), |(k, _)| hash_impl(*k)); },
		}
	}

	#[inline(never)]
	pub fn get(&self, key: Value) -> Option<Value> {
		if let Some(index) = as_index(key) {
			return self.inner().array_contents.get(index).copied();
		}

		let hash = hash_impl(key);
		self.inner().hash_contents.find(hash, |(k, _)| k.equals(key)).map(|(_, v)| *v)
	}

	pub fn contains(&self, key: Value) -> bool {
		if let Some(index) = as_index(key) {
			return self.inner().array_contents.get(index).is_some();
		}

		let hash = hash_impl(key);
		self.inner().hash_contents.find(hash, |(k, _)| k.equals(key)).is_some()
	}

	#[inline(never)]
	pub fn remove(&mut self, key: Value) -> Option<Value> {
		if let Some(index) = as_index(key) {
			return self.inner_mut().array_contents.get_mut(index).map(|v| {
				let val = *v;
				*v = Value::nil();
				val
			});
		}

		let hash = hash_impl(key);
		self.inner_mut().hash_contents.find_entry(hash, |(k, _)| k.equals(key))
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
		self.inner().meta_table
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

	pub fn iter(&self) -> Iter<'_> {
		Iter::new(self)
	}
	pub fn keys(&self) -> impl iter::ExactSizeIterator<Item = Value> + Clone {
		Iter::new(self).map(|(k, _)| k)
	}
	pub fn values(&self) -> impl iter::ExactSizeIterator<Item = Value> + Clone {
		Iter::new(self).map(|(_, v)| v)
	}
	pub fn iter_array(&self) -> ArrayIter<'_> {
		ArrayIter::new(self)
	}
}

pub struct Iter<'a> {
	table: &'a Table,
	hash_iter: hashbrown::hash_table::Iter<'a, (Value, Value)>,
	array_index: usize,
}
impl<'a> Iter<'a> {
	fn new(table: &'a Table) -> Self {
		Self {
			hash_iter: table.inner().hash_contents.iter(),
			table,
			array_index: 1,
		}
	}
}
impl<'a> Clone for Iter<'a> {
	fn clone(&self) -> Self {
		Self {
			hash_iter: self.hash_iter.clone(),
			table: self.table,
			array_index: self.array_index,
		}
	}
}
impl<'a> ExactSizeIterator for Iter<'a> {
	fn len(&self) -> usize {
		self.table.inner().hash_contents.len() + self.table.inner().array_contents.len().saturating_sub(self.array_index)
	}
}
impl<'a> Iterator for Iter<'a> {
	type Item = (Value, Value);

	fn next(&mut self) -> Option<Self::Item> {
		if let Some(v) = self.table.inner().array_contents.get(self.array_index).copied() {
			let idx = self.array_index;
			self.array_index += 1;
			return Some((Value::float((idx + 1) as f64), v));
		}

		self.hash_iter.next().copied()
	}
}

impl<'a> IntoIterator for &'a Table {
	type Item = (Value, Value);
	type IntoIter = Iter<'a>;

	fn into_iter(self) -> Self::IntoIter {
		Iter::new(self)
	}
}

pub struct ArrayIter<'a> {
	table: &'a Table,
	index: usize,
}
impl<'a> ArrayIter<'a> {
	fn new(table: &'a Table) -> Self {
		Self { table, index: 1 }
	}
}
impl<'a> Clone for ArrayIter<'a> {
	fn clone(&self) -> Self {
		Self { table: self.table, index: self.index }
	}
}
impl<'a> ExactSizeIterator for ArrayIter<'a> {
	fn len(&self) -> usize {
		self.table.inner().array_contents.len().saturating_sub(self.index)
	}
}
impl<'a> Iterator for ArrayIter<'a> {
	type Item = (usize, Value);

	fn next(&mut self) -> Option<Self::Item> {
		self.table.inner().array_contents.get(self.index).copied().map(|v| {
			let idx = self.index;
			self.index += 1;
			(idx, v)
		})
	}
}
