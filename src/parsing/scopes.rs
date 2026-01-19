use super::{Error, Op, functions::FuncState};
use luant_lexer::{IdentKey, Lexer};
use slotmap::SparseSecondaryMap;

/// Encapsulates the state during parsing.\
/// This trait may be implemented separately to represent
/// scopes, functions, etc.
#[auto_impl::auto_impl(&mut)]
pub trait ParseScope<'s> {
	fn new_label(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, label: IdentKey, pos: usize) -> Result<(), Error<'s>> { self.parent().new_label(lexer, state, label, pos) }
	fn find_label(&mut self, label: IdentKey, pos: usize) -> usize { self.parent().find_label(label, pos) }
	fn label_exists(&mut self, label: IdentKey) -> bool { self.parent().label_exists(label) }
	fn merge_missing_labels(&mut self, other: &mut Vec<(IdentKey, usize)>) { self.parent().merge_missing_labels(other) }
	fn emit_break(&mut self, state: &mut FuncState<'_, 's>, span: usize) -> Result<(), Error<'s>> { self.parent().emit_break(state, span) }
	fn emit_continue(&mut self, state: &mut FuncState<'_, 's>, span: usize) -> Result<(), Error<'s>> { self.parent().emit_continue(state, span) }

	fn new_local(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<u8, Error<'s>> { self.parent().new_local(lexer, state, name) }
	fn get_local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> { self.parent().get_local(lexer, name) }
	fn resolve_name(&mut self, state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<Named, Error<'s>> { self.parent().resolve_name(state, name) }
	fn local_count(&mut self) -> u8 { self.parent().local_count() }

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
pub struct RootScope(());
impl RootScope {
	pub fn new_root<'s>() -> VariableScope<'s, Self> {
		VariableScope::new(Self(()))
	}
}

impl<'s> ParseScope<'s> for RootScope {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { unreachable!() }

	fn new_label(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _label: IdentKey, _label_pos: usize) -> Result<(), Error<'s>> {
		unreachable!()
	}
	fn find_label(&mut self, _label: IdentKey, _pos: usize) -> usize { unreachable!() }
	fn label_exists(&mut self, _label: IdentKey) -> bool { false }

	fn merge_missing_labels(&mut self, _other: &mut Vec<(IdentKey, usize)>) {
		unreachable!()
	}

	fn emit_break(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Break statement not within a loop".into())
	}
	fn emit_continue(&mut self, _state: &mut FuncState<'_, 's>, _span: usize) -> Result<(), Error<'s>> {
		Err("Continue statement not within a loop".into())
	}

	fn new_local(&mut self, _lexer: &Lexer<'s>, _state: &mut FuncState<'_, 's>, _name: IdentKey) -> Result<u8, Error<'s>> {
		unreachable!()
	}
	fn get_local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s> > {
		Err(format!("Local variable `{}` not found", lexer.resolve_ident(name)).into())
	}

	fn resolve_name(&mut self, _state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<Named, Error<'s>> {
		Ok(Named::Global(name))
	}

	fn local_count(&mut self) -> u8 { 0 }
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
	labels: SparseSecondaryMap<IdentKey, usize>,
	missing_labels: Vec<(IdentKey, usize)>,
	pd: std::marker::PhantomData<&'s ()>,
}
impl<'s, P: ParseScope<'s>> VariableScope<'s, P> {
	pub fn new(parent: P) -> Self {
		Self {
			parent,
			labels: Default::default(),
			locals: Default::default(),
			local_count: 0,
			missing_labels: Default::default(),
			pd: std::marker::PhantomData,
		}
	}

	pub fn finalize_root(self) -> P {
		let Self { mut parent, mut missing_labels, .. } = self;
		
		// We need to manually call merge_missing_labels since Drop won't run
		parent.merge_missing_labels(&mut missing_labels);
		parent
	}

	pub fn into_inner(self, _state: &mut FuncState<'_, 's>, lexer: &Lexer<'s>) -> Result<P, Error<'s>> {
		let Self { parent, missing_labels, .. } = self;

		// Attempt to resolve all missing labels.
		if let Some((label, _)) = missing_labels.first() {
			return Err(format!("Undefined label '{}'", lexer.resolve_ident(*label)).into());
		}

		Ok(parent)
	}
}

impl<'s, P: ParseScope<'s>> ParseScope<'s> for VariableScope<'s, P> {
	fn parent(&mut self) -> &mut dyn ParseScope<'s> { &mut self.parent }
	fn new_local(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<u8, Error<'s>> {
		if self.locals.contains_key(name) {
			return Err(format!("Local variable '{}' already defined", lexer.resolve_ident(name)).into());
		}
		let reg = self.local_count();
		self.locals.insert(name, reg);
		self.local_count += 1;

		if state.slots_used() <= reg {
			state.reserve_slot();
		}

		Ok(reg)
	}

	fn get_local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> {
		self.locals.get(name).copied().map_or_else(|| self.parent.get_local(lexer, name), Ok)
	}

	fn resolve_name(&mut self, state: &mut FuncState<'_, 's>, name: IdentKey) -> Result<Named, Error<'s>> {
		self.locals.get(name).copied().map(Named::Local)
			.map_or_else(|| self.parent.resolve_name(state, name), Ok)
	}

	fn local_count(&mut self) -> u8 {
		self.local_count + self.parent.local_count()
	}

	fn new_label(&mut self, lexer: &Lexer<'s>, state: &mut FuncState<'_, 's>, label: IdentKey, label_pos: usize) -> Result<(), Error<'s>> {
		if self.label_exists(label) {
			return Err(format!("Label '{}' already defined", lexer.resolve_ident(label)).into());
		}
		self.labels.insert(label, label_pos);

		let mut missing_labels = std::mem::take(&mut self.missing_labels);
		missing_labels.retain(|(missing, op_pos)| {
			if *missing == label {
				state.ops_mut()[*op_pos] = Op::goto(label_pos);
				false
			} else { true }
		});
		self.missing_labels = missing_labels;

		Ok(())
	}

	fn find_label(&mut self, label: IdentKey, pos: usize) -> usize {
		self.labels.get(label)
			.copied()
			.or_else(|| self.parent.label_exists(label).then(|| self.parent.find_label(label, pos)))
			.unwrap_or_else(|| {
				// Add to the list to fill in at a later date.
				self.missing_labels.push((label, pos));
				0
			})
	}

	fn label_exists(&mut self, label: IdentKey) -> bool {
		self.labels.contains_key(label) || self.parent.label_exists(label)
	}

	fn merge_missing_labels(&mut self,other: &mut Vec<(IdentKey,usize)>) {
		self.missing_labels.append(other);
	}
}
