use crate::types::*;

impl crate::Luant {
	pub fn fill_globals(&mut self) {
		let print_fn = Value::Func(print);
		let assert_fn = Value::Func(assert);
		let assert_eq_fn = Value::Func(assert_eq);

		self.global_table = Table::from_iter(self, [
			("print", print_fn),
			("assert", assert_fn),
			("assert_eq", assert_eq_fn),
		]);
	}
}

fn print(args: &[Option<Value>]) -> Result<Vec<Option<Value>>, Box<dyn std::error::Error>> {
	let mut values = args.iter();

	if let Some(v) = values.next() {
		match v {
			Some(v) => print!("{v}"),
			None => print!("<nil>"),
		}
	}

	for val in values {
		match val {
			Some(v) => print!("\t{v}"),
			None => print!("\t<nil>"),
		}
	}

	println!();

	Ok(vec![])
}

fn assert(args: &[Option<Value>]) -> Result<Vec<Option<Value>>, Box<dyn std::error::Error>> {
	let condition = &args[0];
	let msg = args.get(1);

	if !condition.as_ref().is_some_and(|v| v.is_truthy()) {
		if let Some(Some(Value::Str(s))) = msg {
			return Err(format!("Assertion failed: {}", s.as_str()).into());
		} else {
			return Err("Assertion failed".into());
		}
	}

	Ok(vec![])
}

fn assert_eq(args: &[Option<Value>]) -> Result<Vec<Option<Value>>, Box<dyn std::error::Error>> {
	let a = &args[0];
	let b = &args[1];
	let msg = args.get(2);

	if a != b {
		if let Some(Some(Value::Str(s))) = msg {
			return Err(format!("Assertion failed: {a:?} != {b:?}: {}", s.as_str()).into());
		} else {
			return Err(format!("Assertion failed: {a:#?} != {b:#?}").into());
		}
	}

	Ok(vec![])
}
