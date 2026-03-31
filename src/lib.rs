use std::{borrow::Cow, ffi::OsStr, hash::BuildHasher, path::{Path, PathBuf}, process::Command};
use anyhow::{Context, Result};

pub mod parsing;
mod bytecode;
mod debug;
mod object;

static RUST_RUNTIME_LIB: &[u8] = include_bytes!("../target/wasm32-unknown-unknown/release/libwasm_scratch.a");

#[derive(Debug)]
pub struct Config<'c> {
	pub obj_path: Cow<'c, Path>,
	pub cleanup_objects: bool,
	pub wasm_ld_path: Cow<'c, OsStr>,
	pub output_module_path: Cow<'c, Path>,
}

/// A trait for representing a Lua file from which both a name and contents can be obtained.\
/// This could be a path from which the contents of a file are read, or a buffer already in memory.
pub trait LuaFile {
	/// Returns the name of the Lua file, which should be the equivalents to its 'full path'.
	fn name(&self) -> Result<Cow<'_, OsStr>>;
	/// Returns the contents of the Lua file as a byte slice.
	fn contents(&self) -> Result<Cow<'_, [u8]>>;
}

pub fn process_files<I: IntoIterator>(config: &Config, files: I) -> anyhow::Result<()> where I::Item: LuaFile {
	let mut cmd = Command::new(&config.wasm_ld_path);
	cmd.arg("--no-entry");

	std::fs::create_dir_all(&config.obj_path).with_context(|| format!("Failed to create object directory '{}'", config.obj_path.display()))?;

	// Generate the runtime object.
	let runtime_obj_path = config.obj_path.join("runtime.o");
	let runtime_obj = object::generate_runtime_object();
	std::fs::write(&runtime_obj_path, runtime_obj).with_context(|| format!("Failed to write runtime object file '{}'", runtime_obj_path.display()))?;
	cmd.arg(runtime_obj_path);

	// Generate the rust library.
	let lib_path = config.obj_path.join("libwasm_scratch.a");
	std::fs::write(&lib_path, RUST_RUNTIME_LIB).with_context(|| format!("Failed to write runtime library '{}'", lib_path.display()))?;
	cmd.arg(lib_path);

	for file in files {
		let obj = parse_lua_file(&file)?.finish();
		
		let mut obj_path = PathBuf::from(file.name()?.into_owned());
		obj_path.set_extension("o");
		let obj_path = config.obj_path.join(obj_path);

		std::fs::write(&obj_path, obj).with_context(|| format!("Failed to write object file '{}'", obj_path.display()))?;
		cmd.arg(obj_path);
	}

	cmd.arg("-o").arg(config.output_module_path.as_ref());

	let status = cmd.status().with_context(|| format!("Failed to execute wasm-ld at '{}'", config.wasm_ld_path.display()))?;
	if !status.success() {
		return Err(anyhow::anyhow!("wasm-ld failed with status code: {status}"));
	}

	Ok(())
}

fn parse_lua_file(lua_file: &impl LuaFile) -> Result<wasm_encoder::Module> {
	let contents = lua_file.contents()?;
	let name = lua_file.name()?;

	//TODO: Unwrap.
	let parsed = parsing::parse(contents.as_ref()).unwrap().0;
	object::produce_lua_obj_file(name.as_encoded_bytes(), parsed)
}
