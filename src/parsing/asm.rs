use crate::Operation;

pub fn parse_asm(src: &str) -> (Vec<Operation>, u8, Vec<&str>, Vec<f64>) {
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
			"loadbuf" => simple_op!(LoadBuf u8, u16),
			"loadtab" => simple_op!(LoadTab u8),
			"set" => simple_op!(Set u8, u8, u8),
			"get" => simple_op!(Get u8, u8),
			"copy" => simple_op!(Copy u8, u8),
			"add" => simple_op!(Add),
			"sub" => simple_op!(Sub),
			"mul" => simple_op!(Mul),
			"div" => simple_op!(Div),
			"mod" => simple_op!(Mod),
			"pow" => simple_op!(Pow),
			"eq" => simple_op!(Eq),
			"neq" => simple_op!(Neq),
			"lt" => simple_op!(Lt),
			"lte" => simple_op!(Lte),
			"gt" => simple_op!(Gt),
			"gte" => simple_op!(Gte),
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
				let (p32, p8) = ((position & 0xFFFF_FFFF) as u32, (position >> 32) as u8);
				Operation::GoTo(p32, p8)
			},
			"put" => simple_op!(Put u8),
			op => panic!("Unknown operation: {op}"),
		});
	}

	let mut strings = Vec::new();
	strings.push("");
	for line in lines {
		strings.push(line);
	}

	(ops, max_reg + 1, strings, numbers)
}
