use crate::Operation;

pub fn parse_asm(src: &str) -> super::Parsed<'_> {
	let mut ops = Vec::new();
	let mut max_reg = 0;
	let mut numbers = Vec::new();

	let mut lines = src.lines();

	for op in &mut lines.by_ref() {
		let mut parts = op.split_whitespace();

		let Some(op) = parts.next() else {
			continue;
		};

		if op.starts_with('#') {
			continue;
		}

		if op == "-DATA-" {
			break;
		}

		macro_rules! args {
			($a:ty$(, $b:ty$(, $c:ty)?)?) => {
				(
					parts.next().map(|p| p.parse::<$a>().unwrap()).unwrap_or_default()
					$(,
						parts.next().map(|p| p.parse::<$b>().unwrap()).unwrap_or_default()
						$(, parts.next().map(|p| p.parse::<$c>().unwrap()).unwrap_or_default() )?
					)?
				)
			};
		}

		macro_rules! simple_op {
			($name:ident) => {
				simple_op!($name u8, u8, u8)
			};
			($name:ident $a:ty $(, $b:ty $(, $c:ty)?)?) => {
				{
					#[allow(unused_parens)]
					let (a $(${ignore($b)}, b $(${ignore($c)}, c)?)?) = args!($a$(, $b$(, $c)?)?);
					max_reg = max_reg.max(a);
					Operation::$name(a $(${ignore($b)}, b $(${ignore($c)}, c)?)?)
				}
			};
		}

		ops.push(match op {
			"loadnull" => simple_op!(LoadNull u8),
			"loadbool" => simple_op!(LoadBool u8, bool),
			"loadnum" => {
				let (reg, num) = args!(u8, f64);

				max_reg = max_reg.max(reg);

				let index = numbers.iter().position(|n| *n == num).unwrap_or_else(|| {
					let idx = numbers.len();
					numbers.push(num);
					idx
				});

				Operation::LoadNum(reg, index as u16)
			},
			"loadstr" => simple_op!(LoadStr u8, u16),
			"loadtab" => simple_op!(LoadTab u8),
			"set" => simple_op!(Set u8, u8, u8),
			"get" => simple_op!(Get u8, u8, u8),
			"copy" => simple_op!(Copy u8, u8),
			"add" => simple_op!(Add),
			"sub" => simple_op!(Sub),
			"mul" => simple_op!(Mul),
			"div" => simple_op!(Div),
			"mod" => simple_op!(Mod),
			"pow" => simple_op!(Pow),
			"neg" => simple_op!(Neg u8, u8),
			"eq" => simple_op!(Eq),
			"neq" => simple_op!(Neq),
			"lt" => simple_op!(Lt),
			"lte" => simple_op!(Lte),
			"gt" => simple_op!(Gt),
			"gte" => simple_op!(Gte),
			"not" => simple_op!(Not u8, u8),
			"band" => simple_op!(BitAnd),
			"bor" => simple_op!(BitOr),
			"bxor" => simple_op!(BitXor),
			"shl" => simple_op!(BitShL),
			"shr" => simple_op!(BitShR),
			"bnot" => simple_op!(BitNot u8, u8),
			"concat" => simple_op!(Concat),
			"len" => simple_op!(Len u8, u8),
			"skpif" => {
				let cond = args!(u8);
				max_reg = max_reg.max(cond);
				Operation::SkpIf(cond)
			},
			"skpifnot" => {
				let cond = args!(u8);
				max_reg = max_reg.max(cond);
				Operation::SkpIfNot(cond)
			},
			"goto" => {
				let offset = args!(isize);
				let position = ops.len().strict_add_signed(offset);
				Operation::goto(position)
			},
			"put" => simple_op!(Put u8),
			op => panic!("Unknown operation: {op}"),
		});
	}

	let mut strings = Vec::new();
	for line in lines {
		strings.push(bstr::BStr::new(line));
	}

	super::Parsed {
		operations: ops,
		numbers,
		strings,
		locals: max_reg + 1,
	}
}

pub fn fmt_asm(mut buf: impl std::fmt::Write, bytecode: &super::Parsed) -> Result<(), std::fmt::Error> {
	let super::Parsed {
		operations,
		numbers,
		strings,
		locals: _,
	} = bytecode;

	// Print all operations
	for (idx, op) in operations.iter().enumerate() {
		match op {
			Operation::LoadNull(dst) => writeln!(buf, "loadnull {dst}")?,
			Operation::LoadBool(dst, val) => writeln!(buf, "loadbool {dst} {val}")?,
			Operation::LoadNum(dst, num_idx) => {
				let num = numbers.get(*num_idx as usize).unwrap_or(&0.0);
				writeln!(buf, "loadnum {dst} {num}")?
			},
			Operation::LoadStr(dst, str_idx) => writeln!(buf, "loadstr {dst} {str_idx}")?,
			Operation::LoadTab(dst) => writeln!(buf, "loadtab {dst}")?,
			Operation::Set(tab, key, val) => writeln!(buf, "set {tab} {key} {val}")?,
			Operation::Get(dst, tab, key) => writeln!(buf, "get {dst} {tab} {key}")?,
			Operation::Copy(dst, src) => writeln!(buf, "copy {dst} {src}")?,
			Operation::Add(dst, a, b) => writeln!(buf, "add {dst} {a} {b}")?,
			Operation::Sub(dst, a, b) => writeln!(buf, "sub {dst} {a} {b}")?,
			Operation::Mul(dst, a, b) => writeln!(buf, "mul {dst} {a} {b}")?,
			Operation::Div(dst, a, b) => writeln!(buf, "div {dst} {a} {b}")?,
			Operation::Mod(dst, a, b) => writeln!(buf, "mod {dst} {a} {b}")?,
			Operation::Pow(dst, a, b) => writeln!(buf, "pow {dst} {a} {b}")?,
			Operation::Neg(dst, src) => writeln!(buf, "neg {dst} {src}")?,
			Operation::Eq(dst, a, b) => writeln!(buf, "eq {dst} {a} {b}")?,
			Operation::Neq(dst, a, b) => writeln!(buf, "neq {dst} {a} {b}")?,
			Operation::Lt(dst, a, b) => writeln!(buf, "lt {dst} {a} {b}")?,
			Operation::Lte(dst, a, b) => writeln!(buf, "lte {dst} {a} {b}")?,
			Operation::Gt(dst, a, b) => writeln!(buf, "gt {dst} {a} {b}")?,
			Operation::Gte(dst, a, b) => writeln!(buf, "gte {dst} {a} {b}")?,
			Operation::Not(dst, src) => writeln!(buf, "not {dst} {src}")?,
			Operation::BitAnd(dst, a, b) => writeln!(buf, "band {dst} {a} {b}")?,
			Operation::BitOr(dst, a, b) => writeln!(buf, "bor {dst} {a} {b}")?,
			Operation::BitXor(dst, a, b) => writeln!(buf, "bxor {dst} {a} {b}")?,
			Operation::BitShL(dst, a, b) => writeln!(buf, "shl {dst} {a} {b}")?,
			Operation::BitShR(dst, a, b) => writeln!(buf, "shr {dst} {a} {b}")?,
			Operation::BitNot(dst, src) => writeln!(buf, "bnot {dst} {src}")?,
			Operation::Concat(dst, a, b) => writeln!(buf, "concat {dst} {a} {b}")?,
			Operation::Len(dst, src) => writeln!(buf, "len {dst} {src}")?,
			Operation::SkpIf(cond) => writeln!(buf, "skpif {cond}")?,
			Operation::SkpIfNot(cond) => writeln!(buf, "skpifnot {cond}")?,
			Operation::GoTo(p32, p8) => {
				// Convert absolute position back to relative offset
				let position = Operation::decode_goto(*p32, *p8);
				let offset = position as isize - idx as isize;
				writeln!(buf, "goto {offset}")?
			},
			Operation::Put(dst) => writeln!(buf, "put {dst}")?,
		}
	}

	// Print the data separator
	writeln!(buf, "-DATA-")?;

	// Print all strings (skip the first empty one since it's added automatically)
	for s in strings.iter() {
		writeln!(buf, "{s}")?;
	}

	Ok(())
}
