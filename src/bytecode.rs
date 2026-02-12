use crate::parsing::expressions::Expr;
use luant_lexer::IdentKey;
use real_float::Finite;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Loc {
	Local(u8),
	UpValue(u8),
	Global(IdentKey),
	Temp(u8),
}
impl Loc {
	pub fn as_slot(self) -> Option<u8> {
		match self {
			Loc::Local(slot) => Some(slot),
			Loc::Temp(temp) => Some(temp),
			Loc::UpValue(_) | Loc::Global(_) => None,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operation {
	// Loads.
	LoadNil(Loc, u8),
	LoadBool(Loc, bool),
	LoadNum(Loc, Finite<f64>),
	LoadStr(Loc, usize),
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
	StartIf(Loc),
	Else,
	EndIf,
	// Meta.
	Copy(Loc, Expr),
	Close(u8),
}
