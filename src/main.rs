// use luant::Luant;

fn main() -> std::process::ExitCode {
	// if let Err(e) = try_main() {
	// 	eprintln!("{e}");
	// 	return std::process::ExitCode::FAILURE;
	// }

	std::process::ExitCode::SUCCESS
}

// fn try_main() -> Result<(), Box<dyn std::error::Error>> {
// 	let src = include_str!("../test.lua");

// 	let luant = Luant::new();
	
// 	let main_func = luant.load(src)?;

// 	let stack = match main_func.run()? {
// 		Ok(stack) => stack,
// 		Err((op_idx, e)) => {
// 			let span = luant.constants.closures[0].debug.as_ref()
// 				.and_then(|d| d.src_map().get(op_idx))
// 				.unwrap_or(0);

// 			let line = src[..span].lines().count();
// 			let col = src[..span].lines().last().map_or(0, |l| l.len()) + 1;
			
// 			Err(format!("Runtime error at {line}:{col}: {e}"))?
// 		}
// 	};

// 	for (reg, val) in stack.iter().enumerate() {
// 		print!("{reg} = ");
// 		match val {
// 			Some(val) => println!("{val:#}"),
// 			None => println!("null"),
// 		}
// 	}

// 	Ok(())
// }
