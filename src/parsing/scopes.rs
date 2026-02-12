use crate::bytecode::Loc;

use super::{Error, Op, functions::FuncState};
use luant_lexer::{IdentKey, Lexer};
use slotmap::SparseSecondaryMap;
use std::num::NonZero;

/// Encapsulates the state during parsing.\
/// This trait may be implemented separately to represent
/// scopes, functions, etc.
#[auto_impl::auto_impl(&mut)]
pub trait ParseScope<'s> {
	fn new_label(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, label: IdentKey, pos: usize) -> Result<(), Error<'s>> { self.parent().new_label(lexer, state, label, pos) }
	fn emit_goto(&mut self, state: &mut FuncState<'_, 's>, label: IdentKey, span: usize, closed_base: Option<u8>) -> Result<(), Error<'s>> { self.parent().emit_goto(state, label, span, closed_base) }
	fn label_exists(&mut self, label: IdentKey) -> bool { self.parent().label_exists(label) }
	fn merge_missing_labels(&mut self, other: Vec<(IdentKey, usize, Option<u8>)>) { self.parent().merge_missing_labels(other) }
	fn emit_break(&mut self, state: &mut FuncState<'_, 's>, span: usize) -> Result<(), Error<'s>> { self.parent().emit_break(state, span) }
	fn emit_continue(&mut self, state: &mut FuncState<'_, 's>, span: usize) -> Result<(), Error<'s>> { self.parent().emit_continue(state, span) }

	fn new_local(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<Loc, Error<'s>> { self.parent().new_local(lexer, state, name) }
	fn resolve_name(&mut self, state: &mut FuncState<'_, 's>, name: IdentKey, is_capturing: bool) -> Result<Named, Error<'s>> { self.parent().resolve_name(state, name, is_capturing) }
	fn total_locals(&mut self) -> u8 { self.parent().total_locals() }
	fn needs_closing(&mut self) -> bool { self.parent().needs_closing() }

	fn parent(&'_ mut self) -> &'_ mut dyn ParseScope<'s>;
}

#[derive(Debug, Clone, Copy)]
pub enum Named {
	Local(u8),
	UpValue(u8),
	Global(IdentKey),
}

/// The root parsing scope.\
/// All methods on [`ParseState`] must be implemented here as
/// attempting to call `parent()` will panic.
#[derive(Debug, Clone)]
pub struct RootScope {
	env_ident: IdentKey,
}
impl RootScope {
	pub fn new_root<'s>(lexer: &mut Lexer<'s>) -> VariableScope<'s, Self> {
		VariableScope::new(Self { env_ident: lexer.get_ident("_ENV") })
	}
}

impl<'s> ParseScope<'s> for RootScope {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { unreachable!() }

	fn new_label(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _label: IdentKey, _label_pos: usize) -> Result<(), Error<'s>> {
		unreachable!()
	}
	fn emit_goto(&mut self, _state: &mut FuncState<'_, 's>, _label: IdentKey, _span: usize, _closed_base: Option<u8>) -> Result<(), Error<'s>> { unreachable!() }
	fn label_exists(&mut self, _label: IdentKey) -> bool { false }

	fn merge_missing_labels(&mut self, _other: Vec<(IdentKey, usize, Option<u8>)>) {
		unreachable!()
	}

	fn emit_break(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Break statement not within a loop".into())
	}
	fn emit_continue(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Continue statement not within a loop".into())
	}

	fn new_local(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _name: IdentKey) -> Result<Loc, Error<'s>> {
		unreachable!()
	}

	fn resolve_name(&mut self, _state: &mut FuncState<'_, 's>, name: IdentKey, _is_capturing: bool) -> Result<Named, Error<'s>> {
		if name == self.env_ident {
			// The 'main' function of a script always has exactly one upvalue, _ENV, the global environment.
			Ok(Named::UpValue(0))
		} else {
			Ok(Named::Global(name))
		}
	}

	fn total_locals(&mut self) -> u8 { 0 }
}

impl<'s> VariableScope<'s, RootScope> {
	pub fn finalize(self, state: &mut FuncState<'_, 's>, lexer: &Lexer<'s>) -> Result<(), Error<'s>> {
		self.into_inner(&mut *state, lexer).map(|_| ())
	}
}

/// A basic state-extension that layers locals and labels on top
/// of an existing state. Used for blocks such as `do` or `if`.
#[derive(Debug, Default)]
pub struct VariableScope<'s, P: ParseScope<'s>> {
	parent: P,
	locals: SparseSecondaryMap<IdentKey, u8>,
	local_count: u8,
	labels: SparseSecondaryMap<IdentKey, LabelData>,
	missing_labels: Vec<(IdentKey, usize, Option<u8>)>,
	has_captured_locals: bool,
	pd: std::marker::PhantomData<&'s ()>,
}

#[derive(Debug, Clone, Copy)]
struct LabelData {
	/// The index of the label within the bytecode.
	position: usize,
	/// The number of locals declared within this scope when the label is found.
	local_count_at_label: u8,
}
impl LabelData {
	fn new(position: usize, local_count: u8) -> Self {
		Self { position, local_count_at_label: local_count }
	}
}

impl<'s, P: ParseScope<'s>> VariableScope<'s, P> {
	pub fn new(parent: P) -> Self {
		Self {
			parent,
			labels: Default::default(),
			locals: Default::default(),
			local_count: 0,
			missing_labels: Default::default(),
			has_captured_locals: false,
			pd: std::marker::PhantomData,
		}
	}

	pub fn finalize_scope(self) -> P {
		let Self { mut parent, missing_labels, .. } = self;
		parent.merge_missing_labels(missing_labels);
		parent
	}

	pub fn into_inner(self, _state: &mut FuncState<'_, 's>, lexer: &Lexer<'s>) -> Result<P, Error<'s>> {
		let Self { parent, missing_labels, .. } = self;

		// Ensure all missing labels were resolved.
		if let Some((label, _, _)) = missing_labels.first() {
			return Err(format!("Undefined label '{}'", lexer.resolve_ident(*label)).into());
		}

		Ok(parent)
	}

	pub fn local_base(&mut self) -> u8 {
		self.parent.total_locals()
	}
}

impl<'s, P: ParseScope<'s>> ParseScope<'s> for VariableScope<'s, P> {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { &mut self.parent }
	fn new_local(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<Loc, Error<'s>> {
		if self.locals.contains_key(name) {
			return Err(format!("Local variable '{}' already defined", lexer.resolve_ident(name)).into());
		}
		//TODO: Feels like this should be cached.
		let reg = self.total_locals();
		self.locals.insert(name, reg);
		self.local_count += 1;

		if state.slots_used() <= reg {
			state.reserve_slot();
		}

		Ok(Loc::Local(reg))
	}

	fn resolve_name(&mut self, state: &mut FuncState<'_, 's>, name: IdentKey, is_capturing: bool) -> Result<Named, Error<'s>> {
		if let Some(&slot) = self.locals.get(name) {
			self.has_captured_locals |= is_capturing;
			return Ok(Named::Local(slot));
		}

		self.parent.resolve_name(state, name, is_capturing)
	}

	fn total_locals(&mut self) -> u8 {
		self.local_count + self.parent.total_locals()
	}

	fn needs_closing(&mut self) -> bool {
		self.has_captured_locals
	}

	fn new_label(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, label: IdentKey, label_pos: usize) -> Result<(), Error<'s>> {
		//TODO!

		// if self.label_exists(label) {
		// 	return Err(format!("Label '{}' already defined", lexer.resolve_ident(label)).into());
		// }
		// self.labels.insert(label, LabelData::new(label_pos, self.local_count));

		// let mut missing_labels = std::mem::take(&mut self.missing_labels);
		// missing_labels.retain(|&(missing, op_pos, base)| {
		// 	if missing == label {
		// 		match base {
		// 			// The base is Some, so we need to swap the placeholder GoTo and Close.
		// 			//TODO: The logic here is kinda roundabout.
		// 			Some(base) => {
		// 				state.ops_mut().swap(op_pos, op_pos + 1);
		// 				let close_pos = op_pos;
		// 				let goto_pos = op_pos + 1;

		// 				state.ops_mut()[close_pos] = Op::Close(base);
		// 				state.ops_mut()[goto_pos].update_goto_target(label_pos);
		// 			},
		// 			// Since the base is None, we know this is actually a naive goto.
		// 			None => state.ops_mut()[op_pos].update_goto_target(label_pos),
		// 		}
		// 		false
		// 	} else { true }
		// });
		// self.missing_labels = missing_labels;

		Ok(())
	}

	fn emit_goto(&mut self, state: &mut FuncState<'_, 's>, label: IdentKey, src_index: usize, closed_base: Option<u8>) -> Result<(), Error<'s>> {
		//TODO!

		// if let Some(&LabelData { position, local_count_at_label }) = self.labels.get(label) {
		// 	// Label is handled within this scope.
		// 	// If captures have occurred, check if any locals have been declared within the implicit scope of the label.
		// 	if self.has_captured_locals && let Some(label_locals) = self.local_count.checked_sub(local_count_at_label).and_then(NonZero::new) {
		// 		// Since captures have occurred and this implicit scope owns locals, we need to conservatively close them.
		// 		let base = self.local_base() + label_locals.get();
		// 		state.emit_closing_goto(self, base, position, src_index);
		// 	} else {
		// 		// Otherwise, if we weren't told by the parent to close any locals, we know this is a naive goto.
		// 		match closed_base {
		// 			None => state.emit_flat_goto(self, position, src_index),
		// 			Some(base) => state.emit_closing_goto(self, base, position, src_index),
		// 		}
		// 	}
		// 	Ok(())
		// } else if self.parent.label_exists(label) { //TODO: Extremely inefficient to recheck this at every level.
		// 	// Pass it upwards. If this scope requires closing when exited, pass its base. Otherwise, just pass up what we were given from the previous scope.
		// 	let closed_base = self.has_captured_locals.then(|| self.local_base()).or(closed_base);
		// 	self.parent.emit_goto(state, label, src_index, closed_base)
		// } else {
		// 	// We don't yet know if this goto requires closing variables since we don't know where it goes.
		// 	// We instead compile the goto followed by an unreachable closing statement. If we later determine that
		// 	// we do need to close locals, we switch the order of them.
		// 	let base = self.has_captured_locals.then(|| self.local_base());
		// 	let goto_op_pos = state.ops().len();
		// 	self.missing_labels.push((label, goto_op_pos, base));
		// 	state.emit(self, Op::goto(0), src_index); // Placeholder.
		// 	state.emit(self, Op::Close(u8::MAX), src_index); // Meaningless for now.
		// 	Ok(())
		// }

		Ok(())
	}

	fn label_exists(&mut self, label: IdentKey) -> bool {
		self.labels.contains_key(label) || self.parent.label_exists(label)
	}

	fn merge_missing_labels(&mut self, other: Vec<(IdentKey, usize, Option<u8>)>) {
		let base = self.has_captured_locals.then(|| self.local_base());
		self.missing_labels.extend(other.into_iter().map(|(l, p, b)| (l, p, base.or(b))));
	}
}
