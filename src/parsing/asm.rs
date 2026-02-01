use super::Op;

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
					Op::$name(a $(${ignore($b)}, b $(${ignore($c)}, c)?)?)
				}
			};
		}

		ops.push(match op {
			"loadnil" => simple_op!(LoadNil u8, u8),
			"loadbool" => simple_op!(LoadBool u8, bool),
			"loadnum" => {
				let (reg, num) = args!(u8, f64);

				max_reg = max_reg.max(reg);

				let index = numbers.iter().position(|n| *n == num).unwrap_or_else(|| {
					let idx = numbers.len();
					numbers.push(num);
					idx
				});

				Op::LoadNum(reg, index as u16)
			},
			"loadstr" => simple_op!(LoadStr u8, u16),
			"loadtab" => simple_op!(LoadTab u8),
			"loadclosure" => simple_op!(LoadClosure u8, u16),
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
			"call" => simple_op!(Call u8, u8, u8),
			"ret" => simple_op!(Ret u8, u8),
			"skpif" => {
				let cond = args!(u8);
				max_reg = max_reg.max(cond);
				Op::SkpIf(cond)
			},
			"skpifnot" => {
				let cond = args!(u8);
				max_reg = max_reg.max(cond);
				Op::SkpIfNot(cond)
			},
			"goto" => {
				let offset = args!(isize);
				let position = ops.len().strict_add_signed(offset);
				Op::goto(position)
			},
			"getupval" => simple_op!(GetUpVal u8, u8),
			"setupval" => simple_op!(SetUpVal u8, u8),
			"getuptab" => simple_op!(GetUpTab u8, u16),
			"setuptab" => {
				let (idx, src) = args!(u16, u8);
				Op::SetUpTab(idx, src)
			},
			"close" => simple_op!(Close u8),
			op => panic!("Unknown operation: {op}"),
		});
	}

	let mut strings = Vec::new();
	for line in lines {
		strings.push(bstr::BStr::new(line));
	}

	let parsed_func = super::functions::ParsedFunction {
		operations: ops.into_boxed_slice(),
		param_count: 0,
		frame_size: max_reg + 1,
		debug: None,
		upvalues: Box::new([]),
	};

	super::Parsed {
		numbers: numbers.into_boxed_slice(),
		strings: strings.into_boxed_slice(),
		closures: Default::default(),
		parsed_func,
	}
}

pub fn fmt_asm(mut buf: impl std::fmt::Write, parsed: &super::Parsed) -> Result<(), std::fmt::Error> {
	let super::Parsed {
		parsed_func,
		closures,
		numbers,
		strings,
	} = parsed;

	for closure in std::iter::once(parsed_func).chain(closures) {
		let name = closure.debug.as_ref()
			.and_then(|d| d.func_name())
			.unwrap_or("<unnamed>");
		writeln!(buf, "{name}:")?;
		
		writeln!(buf, "\t# Frame size: {}, Params: {}, Upvalues: {}",
			closure.frame_size,
			closure.param_count,
			closure.upvalues.len())?;

		// Print all operations
		for (idx, op) in closure.operations.iter().enumerate() {
			match op {
				Op::LoadNil(dst, cnt) => writeln!(buf, "\tloadnull {dst} {cnt}")?,
				Op::LoadBool(dst, val) => writeln!(buf, "\tloadbool {dst} {val}")?,
				Op::LoadNum(dst, num_idx) => {
					let num = numbers.get(*num_idx as usize).unwrap_or(&0.0);
					writeln!(buf, "\tloadnum {dst} {num}")?
				},
				Op::LoadStr(dst, str_idx) => writeln!(buf, "\tloadstr {dst} {str_idx}")?,
				Op::LoadTab(dst) => writeln!(buf, "\tloadtab {dst}")?,
				Op::LoadClosure(dst, idx) => writeln!(buf, "\tloadclosure {dst} {idx}")?,
				Op::Set(tab, key, val) => writeln!(buf, "\tset {tab} {key} {val}")?,
				Op::Get(dst, tab, key) => writeln!(buf, "\tget {dst} {tab} {key}")?,
				Op::Copy(dst, src) => writeln!(buf, "\tcopy {dst} {src}")?,
				Op::Add(dst, a, b) => writeln!(buf, "\tadd {dst} {a} {b}")?,
				Op::Sub(dst, a, b) => writeln!(buf, "\tsub {dst} {a} {b}")?,
				Op::Mul(dst, a, b) => writeln!(buf, "\tmul {dst} {a} {b}")?,
				Op::Div(dst, a, b) => writeln!(buf, "\tdiv {dst} {a} {b}")?,
				Op::Mod(dst, a, b) => writeln!(buf, "\tmod {dst} {a} {b}")?,
				Op::Pow(dst, a, b) => writeln!(buf, "\tpow {dst} {a} {b}")?,
				Op::Neg(dst, src) => writeln!(buf, "\tneg {dst} {src}")?,
				Op::Eq(dst, a, b) => writeln!(buf, "\teq {dst} {a} {b}")?,
				Op::Neq(dst, a, b) => writeln!(buf, "\tneq {dst} {a} {b}")?,
				Op::Lt(dst, a, b) => writeln!(buf, "\tlt {dst} {a} {b}")?,
				Op::Lte(dst, a, b) => writeln!(buf, "\tlte {dst} {a} {b}")?,
				Op::Gt(dst, a, b) => writeln!(buf, "\tgt {dst} {a} {b}")?,
				Op::Gte(dst, a, b) => writeln!(buf, "\tgte {dst} {a} {b}")?,
				Op::Not(dst, src) => writeln!(buf, "\tnot {dst} {src}")?,
				Op::BitAnd(dst, a, b) => writeln!(buf, "\tband {dst} {a} {b}")?,
				Op::BitOr(dst, a, b) => writeln!(buf, "\tbor {dst} {a} {b}")?,
				Op::BitXor(dst, a, b) => writeln!(buf, "\tbxor {dst} {a} {b}")?,
				Op::BitShL(dst, a, b) => writeln!(buf, "\tshl {dst} {a} {b}")?,
				Op::BitShR(dst, a, b) => writeln!(buf, "\tshr {dst} {a} {b}")?,
				Op::BitNot(dst, src) => writeln!(buf, "\tbnot {dst} {src}")?,
				Op::Concat(dst, a, b) => writeln!(buf, "\tconcat {dst} {a} {b}")?,
				Op::Len(dst, src) => writeln!(buf, "\tlen {dst} {src}")?,
				Op::Call(dst, func, arg_cnt) => writeln!(buf, "\tcall {dst} {func} {arg_cnt}")?,
				Op::Ret(dst, ret_cnt) => writeln!(buf, "\tret {dst} {ret_cnt}")?,
				Op::SkpIf(cond) => writeln!(buf, "\tskpif {cond}")?,
				Op::SkpIfNot(cond) => writeln!(buf, "\tskpifnot {cond}")?,
				Op::GoTo(encoded) => {
					// Convert absolute position back to relative offset
					let position = Op::decode_goto(*encoded);
					let offset = position as isize - idx as isize;
					writeln!(buf, "\tgoto {offset}")?
				},
				Op::GetUpVal(dst, idx) => writeln!(buf, "\tgetupval {dst} {idx}")?,
				Op::SetUpVal(idx, src) => writeln!(buf, "\tsetupval {idx} {src}")?,
				Op::GetUpTab(dst, key) => writeln!(buf, "\tgetuptab {dst} {key}")?,
				Op::SetUpTab(tab, key) => writeln!(buf, "\tsetuptab {tab} {key}")?,
				Op::Close(base) => writeln!(buf, "\tclose {base}")?,
			}
		}

		writeln!(buf)?;
	}

	// Print the data separator
	writeln!(buf, "-DATA-")?;

	// Print all strings (skip the first empty one since it's added automatically)
	for s in strings.iter() {
		writeln!(buf, "{s}")?;
	}

	Ok(())
}
