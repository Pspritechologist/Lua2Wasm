use std::{borrow::Cow, ffi::{OsStr, OsString}, path::{Path, PathBuf}};
use anyhow::{Context, Result};
use luant::{Config, LuaFile};

fn main() -> std::process::ExitCode {
	if let Err(e) = try_main() {
		eprintln!("{e}");
		return std::process::ExitCode::FAILURE;
	}

	std::process::ExitCode::SUCCESS
}

pub struct LuaPath(OsString);

impl LuaFile for LuaPath {
	fn name(&self) -> Result<Cow<'_, OsStr>> {
		Ok(self.0.as_os_str().into())
	}
	fn contents(&self) -> Result<Cow<'_, [u8]>> {
		let on_err = || format!("Failed to read lua file '{}'", self.0.display());
		Ok(std::fs::read(&self.0).with_context(on_err)?.into())
	}
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
	// Takes the first param as a file path;
	let in_path = std::env::args_os().nth(1).unwrap_or_else(|| "test.lua".into());
	let out_path = std::env::args_os().nth(2).unwrap_or_else(|| "out.wasm".into());
	let opt = std::env::args_os().nth(3).is_some_and(|arg| arg == "--opt" || arg == "-o");

	luant::process_files(&Config {
		target_path: Path::new("output").into(),
		cleanup_objects: true,
		wasm_ld_path: OsStr::new("wasm-ld").into(),
		output_module_path: PathBuf::from(out_path).into(),
		wasm_opt_path: opt.then_some(OsStr::new("wasm-opt").into()),
	}, [LuaPath(in_path)])?;

	Ok(())
}
