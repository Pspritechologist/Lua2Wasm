use crate::parsing::expressions::Expr;
use luant_lexer::IdentKey;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Loc {
	Slot(u8),
	UpValue(u8),
	Global(IdentKey),
}
impl Loc {
	pub fn as_slot(self) -> Option<u8> {
		match self {
			Loc::Slot(slot) => Some(slot),
			Loc::UpValue(_) | Loc::Global(_) => None,
		}
	}

	pub fn is_same_slot(self, other: Loc) -> bool {
		match (self, other) {
			(Loc::Slot(slot1), Loc::Slot(slot2)) => slot1 == slot2,
			(Loc::UpValue(up1), Loc::UpValue(up2)) => up1 == up2,
			_ => false,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operation {
	// Loads.
	LoadTab(Loc),
	LoadClosure(Loc, usize),
	// Tables.
	Set(Expr, Expr, Expr),
	Get(Loc, Expr, Expr),
	// Arithmetic.
	Add(Loc, Expr, Expr),
	Sub(Loc, Expr, Expr),
	Mul(Loc, Expr, Expr),
	Div(Loc, Expr, Expr),
	Mod(Loc, Expr, Expr),
	Pow(Loc, Expr, Expr),
	Neg(Loc, Expr),
	// Logic.
	Eq(Loc, Expr, Expr),
	Neq(Loc, Expr, Expr),
	Lt(Loc, Expr, Expr),
	Lte(Loc, Expr, Expr),
	Gt(Loc, Expr, Expr),
	Gte(Loc, Expr, Expr),
	Not(Loc, Expr),
	// Bitwise.
	BitAnd(Loc, Expr, Expr),
	BitOr(Loc, Expr, Expr),
	BitXor(Loc, Expr, Expr),
	BitShL(Loc, Expr, Expr),
	BitShR(Loc, Expr, Expr),
	BitNot(Loc, Expr),
	// Misc operators.
	Concat(Loc, Expr, Expr),
	Len(Loc, Expr),
	// Calling.
	Call(Loc, u8, u8),
	Ret(u8, u8),
	// Control.
	StartIf(Expr),
	ElseIf(Expr),
	Else,
	EndIf,
	// Meta.
	Copy(Loc, Expr),
	Close(u8),
	MaybeClose(u8, bool),
}
