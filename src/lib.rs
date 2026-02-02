#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
	trim_prefix_suffix,
	cell_get_cloned,
	try_blocks,
)]

pub mod parsing; //TODO: Likely should not be public.
mod vm;

mod gc {
	pub use dumpster::unsync::*;
	pub use dumpster::Trace;
}

pub use vm::LuantState;
pub use bstr::BStr;

pub use vm::*;

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub struct Luant {
	inner: Rc<RefCell<LuantState>>,
}

impl Default for Luant {
	fn default() -> Self {
		Self { inner: Rc::new(RefCell::new(LuantState::new())) }
	}
}

impl Luant {
	pub(crate) fn borrow(&self) -> std::cell::Ref<'_, LuantState> {
		self.inner.borrow()
	}
	pub(crate) fn borrow_mut(&self) -> std::cell::RefMut<'_, LuantState> {
		self.inner.borrow_mut()
	}

	pub fn new() -> Self {
		Self::default()
	}

	pub fn load<'s, S: parsing::ParseSrc + ?Sized>(&self, src: &'s S) -> Result<Function, parsing::Error<'s>> {
		let luant_clone = self.clone();

		let luant = &mut *self.inner.borrow_mut();

		let parsed = parsing::parse(src)?;
		let parsing::Parsed { parsed_func, numbers, strings, closures } = parsed;

		let const_strings = &mut luant.constants.strings;
		let string_offset = const_strings.len();
		strings.iter().for_each(|s| { const_strings.push(s); });

		let const_nums = &mut luant.constants.numbers;
		let number_offset = const_nums.len();
		const_nums.extend_from_slice(&numbers);

		let const_closures = &mut luant.constants.closures;
		let closure_offset = const_closures.len();
		const_closures.extend(std::iter::once(parsed_func).chain(closures).map(|c| {
			vm::datatypes::ClosureProto {
				operations: c.operations,
				param_count: c.param_count,
				number_offset,
				string_offset,
				closure_offset,
				slots_needed: c.frame_size,
				upvalues: c.upvalues,
				debug: c.debug,
			}
		}));

		let closure = types::Closure::new(closure_offset, [types::UpvalueSlot::new(types::UpvalueRef::Open(0))]);

		Ok(Function::new(luant_clone, closure))
	}
}
