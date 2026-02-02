use super::{Operation, Upvalue, debug};
use super::types::{Hasher, Table};
use crate::BStr;

pub(crate) struct ClosureProto {
	pub operations: Box<[Operation]>,
	pub param_count: u8,
	pub slots_needed: u8,
	pub number_offset: usize,
	pub string_offset: usize,
	pub closure_offset: usize,
	pub upvalues: Box<[Upvalue]>,
	pub debug: Option<debug::DebugInfo>,
}

pub(crate) struct Frame {
	pub pc: usize,
	pub stack_base: usize,
	pub closure_proto: usize,
	pub expected_to_return: u8,
}

impl Frame {
	pub fn initial() -> Self {
		Self {
			pc: 0,
			// The first slot is reserved for the main closure.
			//TODO: Handle passing arguments to main functions.
			stack_base: 2,
			closure_proto: 0,
			expected_to_return: 0,
		}
	}

	pub fn new(call_proto: usize, stack_base: usize, expected_returns: u8) -> Self {
		Self {
			pc: 0,
			stack_base,
			closure_proto: call_proto,
			expected_to_return: expected_returns,
		}
	}
}

#[derive(Default)]
pub(crate) struct ConstStrings {
	data: Vec<u8>,
	map: Vec<(usize, usize)>,
}
impl ConstStrings {
	pub fn get(&self, index: usize) -> &BStr {
		let (start, end) = self.map[index];
		BStr::new(&self.data[start..end])
	}

	pub fn push(&mut self, s: &BStr) -> usize {
		let start = self.data.len();
		self.data.extend_from_slice(s);
		let end = self.data.len();
		self.map.push((start, end));
		self.map.len() - 1
	}

	pub fn new<'a>(strings: impl IntoIterator<Item = &'a BStr>) -> Self {
		let mut data = Vec::new();
		let mut map = Vec::new();

		for s in strings {
			let start = data.len();
			data.extend_from_slice(s);
			let end = data.len();
			map.push((start, end));
		}

		Self { data, map }
	}

	pub fn len(&self) -> usize {
		self.data.len()
	}
}

#[derive(Default)]
pub(crate) struct Constants {
	pub numbers: Vec<f64>,
	pub strings: ConstStrings,
	pub closures: Vec<ClosureProto>,
}

#[derive(Default)]
pub(crate) struct VmState {
	pub hasher: Hasher,
	pub gc_buf: std::cell::Cell<Vec<u8>>,
}

pub struct LuantState {
	pub(crate) vm_state: VmState,
	pub(crate) constants: Constants,
	pub(crate) global_table: Table,
}

impl LuantState {
	pub(crate) fn new() -> Self {
		let mut inst = Self {
			vm_state: Default::default(),
			constants: Default::default(),
			global_table: Default::default(),
		};
		super::globals::fill_globals(&mut inst);
		inst
	}
}
