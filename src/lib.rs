#![feature(
	iter_array_chunks,
	ptr_metadata,
	macro_metavar_expr,
	trim_prefix_suffix,
	cell_get_cloned,
	try_blocks,
)]

use bstr::BStr;

mod vm;

pub mod parsing; //TODO: Likely should not be public.
