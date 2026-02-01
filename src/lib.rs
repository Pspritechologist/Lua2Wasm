#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
	trim_prefix_suffix,
	cell_get_cloned,
)]

pub mod parsing; //TODO: Likely should not be public.
mod vm;

mod gc {
	pub use dumpster::unsync::*;
	pub use dumpster::Trace;
}

pub use vm::Luant;
pub use bstr::BStr;

pub use vm::call_func;
pub use vm::*;

use std::rc::Rc;
use std::cell::RefCell;

pub fn new_luant() -> Rc<RefCell<Luant>> {
	Rc::new(RefCell::new(Luant::new()))
}

pub fn load_closure<'s, S: parsing::ParseSrc + ?Sized>(luant: &Rc<RefCell<Luant>>, src: &'s S) -> Result<Closure, parsing::Error<'s>> {
	let parsed = parsing::parse(src)?;
	let parsing::Parsed { parsed_func, numbers, strings, closures } = parsed;

	let const_strings = &mut luant.borrow_mut().constants.strings;
	let string_offset = const_strings.len();
	strings.iter().for_each(|s| { const_strings.push(s); });

	let const_nums = &mut luant.borrow_mut().constants.numbers;
	let number_offset = const_nums.len();
	const_nums.extend_from_slice(&numbers);

	let const_closures = &mut luant.borrow_mut().constants.closures;
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

	Ok(Closure::new(0, [UpvalueSlot::new(UpvalueRef::Open(0))]))
}
