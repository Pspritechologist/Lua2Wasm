use crate::prelude::*;
use gc::Gc;
use types::Value;
use dumpster::{TraceWith, Visitor};
use hashbrown::HashTable;
use std::iter;

#[derive(Debug, Default)]
pub struct Table {
	meta_table: Option<Gc<Table>>,
    contents: HashTable<(Value, Value)>,
}

fn hash_impl(value: &Value) -> u64 {
	match value {
		Value::Num(num) => num.val().to_bits(),
		Value::Str(gc) => gc.get_hash(),
		Value::Bool(b) => if *b { 1 } else { 0 },
		Value::Buffer(gc) => Gc::as_ptr(gc).addr() as u64,
		Value::Table(gc) => Gc::as_ptr(gc).addr() as u64,
		Value::Func(f) => (*f as *const ()).addr() as u64,
	}
}

impl Table {
	pub fn set(&mut self, key: Value, value: Value) {
		let hash = hash_impl(&key);

		match self.contents.find_mut(hash, |(k, _)| k == &key) {
			Some((_, v)) => *v = value,
			None => { self.contents.insert_unique(hash, (key, value), |(k, _)| hash_impl(k)); },
		}
	}

	pub fn get(&self, key: &Value) -> Option<Value> {
		let hash = hash_impl(key);
		self.contents.find(hash, |(k, _)| k == key).map(|(_, v)| v.clone())
	}

	pub fn contains(&self, key: &Value) -> bool {
		let hash = hash_impl(key);
		self.contents.find(hash, |(k, _)| k == key).is_some()
	}

	pub fn remove(&mut self, key: &Value) -> Option<Value> {
		let hash = hash_impl(key);
		self.contents.find_entry(hash, |(k, _)| k == key)
			.map(|e| e.remove().0.1)
			.ok()
	}

	pub fn len(&self) -> usize {
		self.contents.len()
	}
	pub fn is_empty(&self) -> bool {
		self.contents.is_empty()
	}

	pub fn meta_table(&self) -> Option<&Gc<Table>> {
		self.meta_table.as_ref()
	}
	pub fn set_meta_table(&mut self, table: Gc<Table>) {
		self.meta_table = Some(table);
	}
	pub fn clear_meta_table(&mut self) {
		self.meta_table = None;
	}

	pub fn iter(&self) -> impl iter::ExactSizeIterator<Item = (&Value, &Value)> + iter::FusedIterator + Clone + std::fmt::Debug {
		self.contents.iter().map(|(k, v)| (k, v))
	}
	pub fn keys(&self) -> impl iter::ExactSizeIterator<Item = &Value> + iter::FusedIterator + Clone + std::fmt::Debug {
		self.contents.iter().map(|(k, _)| k)
	}
	pub fn values(&self) -> impl iter::ExactSizeIterator<Item = &Value> + iter::FusedIterator + Clone + std::fmt::Debug {
		self.contents.iter().map(|(_, v)| v)
	}
}

impl IntoIterator for Table {
	type Item = (Value, Value);
	type IntoIter = hashbrown::hash_table::IntoIter<(Value, Value)>;

	fn into_iter(self) -> Self::IntoIter {
		self.contents.into_iter()
	}
}

impl<'a> IntoIterator for &'a Table {
	type Item = &'a (Value, Value);
	type IntoIter = hashbrown::hash_table::Iter<'a, (Value, Value)>;

	fn into_iter(self) -> Self::IntoIter {
		self.contents.iter()
	}
}

unsafe impl<Z: Visitor> TraceWith<Z> for Table {
	fn accept(&self, visitor: &mut Z) -> Result<(), ()> {
		self.meta_table.accept(visitor)?;

		for (k, v) in &self.contents {
			k.accept(visitor)?;
			v.accept(visitor)?;
		}

		Ok(())
	}
}
