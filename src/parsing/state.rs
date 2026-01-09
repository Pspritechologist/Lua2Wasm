use super::{Error, Op};
use luant_lexer::{IdentKey, Lexer};
use slotmap::SparseSecondaryMap;
use hashbrown::HashMap;

/// Encapsulates the state during parsing.\
/// This trait may be implemented separately to represent
/// scopes, functions, etc.
#[auto_impl::auto_impl(&mut)]
pub trait ParseState<'s> {
	fn emit(&mut self, op: Op) { self.parent().emit(op) }
	fn get_ops(&self) -> &[Op];
	fn get_ops_mut(&mut self) -> &mut [Op];
	fn number_idx(&mut self, n: f64) -> u16 { self.parent().number_idx(n) }
	fn string_idx(&mut self, s: &'s str) -> u16 { self.parent().string_idx(s) }
	fn label(&mut self, lexer: &Lexer<'s>, label: IdentKey, pos: usize) -> Result<(), Error<'s>> { self.parent().label(lexer, label, pos) }
	fn find_label(&mut self, label: IdentKey, pos: usize) -> usize { self.parent().find_label(label, pos) }
	fn label_exists(&mut self, label: IdentKey) -> bool { self.parent().label_exists(label) }
	fn merge_missing_labels(&mut self, other: &mut Vec<(IdentKey, usize)>) { self.parent().merge_missing_labels(other) }
	fn emit_break(&mut self) -> Result<(), Error<'s>> { self.parent().emit_break() }
	fn emit_continue(&mut self) -> Result<(), Error<'s>> { self.parent().emit_continue() }

	fn new_register(&mut self) -> u8 { self.parent().new_register() }
	fn new_local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> { self.parent().new_local(lexer, name) }
	fn make_local(&mut self, lexer: &Lexer<'s>, name: IdentKey, slot: u8) -> Result<(), Error<'s>> { self.parent().make_local(lexer, name, slot) }
	fn local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> { self.parent().local(lexer, name) }

	fn parent(&'_ mut self) -> &'_ mut dyn ParseState<'s>;
}

/// The root parsing state.\
/// All methods on [`ParseState`] must be implemented here as
/// attempting to call `parent()` will panic.
#[derive(Default, Clone)]
pub struct RootState<'s> {
	operations: Vec<Op>,

	locals: SparseSecondaryMap<IdentKey, u8>,
	registers: u8,
	numbers: Vec<f64>,
	string_indexes: HashMap<&'s str, usize>,
	strings: Vec<&'s str>,
	labels: SparseSecondaryMap<IdentKey, usize>,
	missing_labels: Vec<(IdentKey, usize)>,
}

#[derive(Debug)]
pub struct Parsed<'s> {
	pub operations: Vec<Op>,
	pub numbers: Vec<f64>,
	pub strings: Vec<&'s str>,

	pub locals: u8,
}

impl<'s> RootState<'s> {
	/// Checks that no labels were left unresolved and returns the completed bytecode.
	pub fn finalize(mut self, lexer: &Lexer<'s>) -> Result<Parsed<'s>, Error<'s>> {
		let missing_labels = std::mem::take(&mut self.missing_labels);
		// Attempt to resolve all missing labels.
		if let Some((label, _)) = missing_labels.first() {
			return Err(format!("Undefined label '{}'", lexer.resolve_ident(*label)).into());
		}

		Ok(Parsed {
			operations: self.operations,
			numbers: self.numbers,
			strings: self.strings,

			locals: self.registers,
		})
	}
}

impl<'s> ParseState<'s> for RootState<'s> {
	fn parent(&mut self) -> &mut dyn ParseState<'s> { unreachable!() }

	fn emit(&mut self, op: Op) {
		self.operations.push(op);
	}
	fn get_ops(&self) -> &[Op] {
		&self.operations
	}
	fn get_ops_mut(&mut self) -> &mut [Op] {
		&mut self.operations
	}

	fn number_idx(&mut self, n: f64) -> u16 {
		if let Some(idx) = self.numbers.iter().position(|&num| num == n) {
			idx
		} else {
			let idx = self.numbers.len();
			self.numbers.push(n);
			idx
		}.try_into().expect("Too many numbers consts :(")
	}
	fn string_idx(&mut self, s: &'s str) -> u16 {
		if let Some(&idx) = self.string_indexes.get(s) {
			idx
		} else {
			let idx = self.strings.len();
			self.strings.push(s);
			self.string_indexes.insert(s, idx);
			idx
		}.try_into().expect("Too many string consts :(")
	}
	fn label(&mut self, lexer: &Lexer<'s>, label: IdentKey, label_pos: usize) -> Result<(), Error<'s>> {
		if self.label_exists(label) {
			return Err(format!("Label '{}' already defined", lexer.resolve_ident(label)).into());
		}
		self.labels.insert(label, label_pos);

		let mut missing_labels = std::mem::take(&mut self.missing_labels);
		missing_labels.retain(|(missing, op_pos)| {
			if *missing == label {
				self.get_ops_mut()[label_pos] = Op::goto(*op_pos);
				false
			} else { true }
		});
		self.missing_labels = missing_labels;

		Ok(())
	}
	fn find_label(&mut self, label: IdentKey, pos: usize) -> usize {
		if let Some(&pos) = self.labels.get(label) {
			return pos;
		}

		// Add to the list to fill in at a later date.
		self.missing_labels.push((label, pos));
		0
	}
	fn label_exists(&mut self, label: IdentKey) -> bool {
		self.labels.contains_key(label)
	}
	fn merge_missing_labels(&mut self,other: &mut Vec<(IdentKey,usize)>) {
		self.missing_labels.append(other);
	}

	fn emit_break(&mut self) -> Result<(), Error<'s>> {
		Err("Break statement not within a loop".into())
	}
	fn emit_continue(&mut self) -> Result<(), Error<'s>> {
		Err("Continue statement not within a loop".into())
	}

	fn new_register(&mut self) -> u8 {
		let reg = self.registers;
		self.registers += 1;
		reg
	}
	fn new_local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> {
		if self.locals.contains_key(name) {
			return Err(format!("Local variable '{}' already defined", lexer.resolve_ident(name)).into());
		}
		let reg = self.new_register();
		self.locals.insert(name, reg);
		Ok(reg)
	}
	fn make_local(&mut self, lexer: &Lexer<'s>, name: IdentKey, slot: u8) -> Result<(), Error<'s>> {
		if self.locals.contains_key(name) {
			return Err(format!("Local variable '{}' already defined", lexer.resolve_ident(name)).into());
		}
		self.locals.insert(name, slot);
		Ok(())
	}
	fn local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s> > {
		self.locals.get(name).copied().ok_or_else(|| format!("Local variable `{}` not found", lexer.resolve_ident(name)).into())
	}
}

/// A collection of non-dyn-compatible helper methods for ParseState.
pub trait ParseStateExt<'s>: ParseState<'s> {
	fn new_registers<const COUNT: usize>(&mut self) -> [u8; COUNT] {
		[(); COUNT].map(|_| self.new_register())
	}
}
impl<'s, T: ParseState<'s> + ?Sized> ParseStateExt<'s> for T { }

/// A basic state-extension that layers locals and labels on top
/// of an existing state. Used for blocks such as `do` or `if`.
#[derive(Debug, Default)]
pub struct VariableScope<'s, P: ParseState<'s>> {
	parent: P,
	locals: SparseSecondaryMap<IdentKey, u8>,
	labels: SparseSecondaryMap<IdentKey, usize>,
	missing_labels: Vec<(IdentKey, usize)>,
	pd: std::marker::PhantomData<&'s ()>,
}
impl<'s, P: ParseState<'s>> VariableScope<'s, P> {
	pub fn new(parent: P) -> Self {
		Self {
			parent,
			labels: Default::default(),
			locals: Default::default(),
			missing_labels: Default::default(),
			pd: std::marker::PhantomData,
		}
	}

	pub fn into_inner(mut self) -> P {
		let mut missing_labels = std::mem::take(&mut self.missing_labels);
		let mut parent = unsafe { std::ptr::read(&self.parent) };
		// Prevent Drop from running since we've moved out the important fields
		std::mem::forget(self);
		// We need to manually call merge_missing_labels since Drop won't run
		parent.merge_missing_labels(&mut missing_labels);
		parent
	}
}
impl<'s, P: ParseState<'s>> Drop for VariableScope<'s, P> {
	fn drop(&mut self) {
		self.parent.merge_missing_labels(&mut self.missing_labels);
	}
}
impl<'s, P: ParseState<'s>> ParseState<'s> for VariableScope<'s, P> {
	fn parent(&mut self) -> &mut dyn ParseState<'s> { &mut self.parent }
	fn get_ops(&self) -> &[Op] { self.parent.get_ops() }
	fn get_ops_mut(&mut self) -> &mut [Op] { self.parent.get_ops_mut() }

	fn new_local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> {
		if self.locals.contains_key(name) {
			return Err(format!("Local variable '{}' already defined", lexer.resolve_ident(name)).into());
		}
		let reg = self.parent().new_register();
		self.locals.insert(name, reg);
		Ok(reg)
	}

	fn local(&mut self, lexer: &Lexer<'s>, name: IdentKey) -> Result<u8, Error<'s>> {
		self.locals.get(name).copied().map_or_else(|| self.parent().local(lexer, name), Ok)
	}

	fn label(&mut self, lexer: &Lexer<'s>, label: IdentKey, label_pos: usize) -> Result<(), Error<'s>> {
		if self.label_exists(label) {
			return Err(format!("Label '{}' already defined", lexer.resolve_ident(label)).into());
		}
		self.labels.insert(label, label_pos);

		let mut missing_labels = std::mem::take(&mut self.missing_labels);
		missing_labels.retain(|(missing, op_pos)| {
			if *missing == label {
				self.get_ops_mut()[*op_pos] = Op::goto(label_pos);
				false
			} else { true }
		});
		self.missing_labels = missing_labels;

		Ok(())
	}

	fn find_label(&mut self, label: IdentKey, pos: usize) -> usize {
		self.labels.get(label)
			.copied()
			.or_else(|| self.parent().label_exists(label).then(|| self.parent.find_label(label, pos)))
			.unwrap_or_else(|| {
				// Add to the list to fill in at a later date.
				self.missing_labels.push((label, pos));
				0
			})
	}

	fn label_exists(&mut self, label: IdentKey) -> bool {
		self.labels.contains_key(label) || self.parent().label_exists(label)
	}

	fn merge_missing_labels(&mut self,other: &mut Vec<(IdentKey,usize)>) {
		self.missing_labels.append(other);
	}
}
