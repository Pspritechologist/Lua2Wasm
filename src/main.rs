fn main() -> std::process::ExitCode {
	if let Err(e) = try_main() {
		eprintln!("{e}");
		return std::process::ExitCode::FAILURE;
	}

	std::process::ExitCode::SUCCESS
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
	let src = include_str!("../test.lua");
	
	let ast = luant::parsing::parse(src)?;
	println!("{ast:#?}");

	Ok(())
}
