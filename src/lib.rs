use std::{borrow::Cow, ffi::OsStr, io::Write, path::{Path, PathBuf}, process::Command};
use anyhow::{Context, Result};

pub use object::exports::ExportData;

pub mod parsing;
mod bytecode;
mod debug;
mod object;

static RUST_RUNTIME_LIB: &[u8] = include_bytes!("../target/wasm32-unknown-unknown/release/libwasm_scratch.a");

#[derive(Debug)]
pub struct Config<'c> {
	pub target_path: Cow<'c, Path>,
	pub cleanup_objects: bool,
	pub wasm_ld_path: Cow<'c, OsStr>,
	pub output_module_path: Cow<'c, Path>,

	pub entry_point: EntryPoint<'c>,

	pub wasm_opt_path: Option<Cow<'c, OsStr>>,

	pub exports: Vec<ExportData<'static>>,
}

#[derive(Debug, Clone)]
pub enum EntryPoint<'a> {
	None,
	Start,
	Export(Cow<'a, str>),
}
impl<'c> EntryPoint<'c> {
	pub fn as_export(&self) -> Option<&str> {
		match self {
			Self::Export(name) => Some(name),
			_ => None,
		}
	}

	pub fn is_start(&self) -> bool {
		matches!(self, Self::Start)
	}

	pub fn is_none(&self) -> bool {
		matches!(self, Self::None)
	}
}
impl<'c> From<&'c str> for EntryPoint<'c> {
	fn from(value: &'c str) -> Self {
		Self::Export(value.into())
	}
}
impl<'c> From<Option<&'c str>> for EntryPoint<'c> {
	fn from(value: Option<&'c str>) -> Self {
		match value {
			Some(s) => Self::Export(s.into()),
			None => Self::None,
		}
	}
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

	let obj_path = config.target_path.join("obj");

	std::fs::create_dir_all(&obj_path).with_context(|| format!("Failed to create object directory '{}'", config.target_path.display()))?;

	// Generate the runtime object.
	let runtime_obj_path = config.target_path.join("runtime.o");
	let runtime_obj = object::generate_runtime_object();
	std::fs::write(&runtime_obj_path, runtime_obj).with_context(|| format!("Failed to write runtime object file '{}'", runtime_obj_path.display()))?;
	cmd.arg(runtime_obj_path);

	// Generate the rust library.
	let lib_path = config.target_path.join("libwasm_scratch.a");
	std::fs::write(&lib_path, RUST_RUNTIME_LIB).with_context(|| format!("Failed to write runtime library '{}'", lib_path.display()))?;
	cmd.arg(lib_path);

	// Generate the export object, which contains the exports of the module.
	let export_obj_path = config.target_path.join("exports.o");
	let export_obj = object::exports::generate_exports_object(config)?;
	std::fs::write(&export_obj_path, export_obj).with_context(|| format!("Failed to write export object file '{}'", export_obj_path.display()))?;
	cmd.arg(export_obj_path);

	let mut files = files.into_iter();

	if !config.entry_point.is_none() {
		let main_file = files.next().context("At least one Lua file must be provided")?;

		let main_obj = parse_lua_file(&main_file, true)?.finish();

		let mut main_obj_path = PathBuf::from(main_file.name()?.into_owned());
		main_obj_path.set_extension("o");
		let main_obj_path = obj_path.join(main_obj_path);

		std::fs::write(&main_obj_path, main_obj).with_context(|| format!("Failed to write object file '{}'", main_obj_path.display()))?;
		cmd.arg(main_obj_path);
	}

	for file in files {
		let obj = parse_lua_file(&file, false)?.finish();
		
		let mut file_obj_path = PathBuf::from(file.name()?.into_owned());
		file_obj_path.set_extension("o");
		let obj_path = obj_path.join(file_obj_path);

		std::fs::write(&obj_path, obj).with_context(|| format!("Failed to write object file '{}'", obj_path.display()))?;
		cmd.arg(obj_path);
	}

	let temp_module_path = config.target_path.join("linked_module.wasm");

	cmd.arg("-o").arg(&temp_module_path);

	let status = cmd.status().with_context(|| format!("Failed to execute wasm-ld at '{}'", config.wasm_ld_path.display()))?;
	if !status.success() {
		return Err(anyhow::anyhow!("wasm-ld failed with status code: {status}"));
	}

	let old_module = std::fs::read(&temp_module_path).with_context(|| format!("Failed to read output module file '{}'", config.output_module_path.display()))?;

	let new_module = add_second_memory(&old_module)?;

	let Some(wasm_opt_path) = &config.wasm_opt_path else {
		std::fs::write(&config.output_module_path, new_module).with_context(|| format!("Failed to write output module file '{}'", config.output_module_path.display()))?;
		return Ok(())
	};

	let mut wasm_opt_proc = Command::new(wasm_opt_path)
		.arg("-O3").arg("-c")
		.arg("--enable-multimemory")
		.arg("--enable-exception-handling")
		.arg("-o").arg(config.output_module_path.as_ref())
		.stdin(std::process::Stdio::piped())
		.spawn()
		.with_context(|| format!("Failed to execute wasm-opt at '{}'", wasm_opt_path.display()))?;

	wasm_opt_proc.stdin.take().context("Failed to open stdin for wasm-opt")?.write_all(&new_module).context("Failed to write module to wasm-opt stdin")?;
	let opt_status = wasm_opt_proc.wait().context("Failed to wait for wasm-opt process")?;

	if !opt_status.success() {
		return Err(anyhow::anyhow!("wasm-opt failed with status code: {opt_status}"));
	}

	Ok(())
}

/// Finds the tail of the memory section in a wasm module, where new memories can be inserted.
fn add_second_memory(wasm: &[u8]) -> Result<Vec<u8>> {
	const MEMORY_SECTION_ID: u8 = 5;

	if wasm.len() < 8 {
		return Err(anyhow::anyhow!("Wasm module is too small to be valid"));
	}

	// Find the tail end of the memory section.
	let mut cursor = 8usize;
	let mem_offset = loop {
		if cursor >= wasm.len() {
			return Err(anyhow::anyhow!("Failed to find memory section in wasm module"));
		}

		let section_id = wasm[cursor];
		cursor += 1;

		if section_id == MEMORY_SECTION_ID {
			break cursor;
		}

		let content_len = leb128fmt::decode_uint_slice::<u32, 32>(wasm, &mut cursor).context("Failed to decode value while looking for memory")? as usize;

		cursor += content_len;
	};
	
	let len_offset = mem_offset;
	let mem_len = leb128fmt::decode_uint_slice::<u32, 32>(wasm, &mut cursor).context("Failed to decode memory section length")? as usize;

	let mem_count = wasm[cursor] as usize;
	assert_eq!(mem_count, 1, "Expected exactly one memory in the output module");

	let mem_end_offset = cursor + mem_len;

	cursor += 1;

	let content_offset = cursor;

	let mut new_module = Vec::with_capacity(wasm.len() + 3);
	new_module.extend_from_slice(&wasm[..len_offset]);

	// Encode the new length of the memory section, which is the old length + 3 (the size of one memory entry).
	wasm_encoder::Encode::encode(&(mem_len + 3), &mut new_module);

	new_module.push(2); // The new memory count, which is the old count + 1.

	new_module.extend_from_slice(&wasm[content_offset..mem_end_offset]);

	wasm_encoder::Encode::encode(&wasm_encoder::MemoryType { minimum: 1, maximum: Some(1), memory64: false, shared: false, page_size_log2: None }, &mut new_module);

	new_module.extend_from_slice(&wasm[mem_end_offset..]);

	Ok(new_module)
}

fn parse_lua_file(lua_file: &impl LuaFile, as_main: bool) -> Result<wasm_encoder::Module> {
	let contents = lua_file.contents()?;
	let name = lua_file.name()?;

	//TODO: Unwrap.
	let parsed = parsing::parse(contents.as_ref()).unwrap().0;
	object::produce_lua_obj_file(name.as_encoded_bytes(), as_main, parsed)
}
