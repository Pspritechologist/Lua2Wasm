use std::{borrow::Cow, ffi::OsStr, io::Write, path::{Path, PathBuf}, process::Command};
use anyhow::{Context, Result};

pub use object::exports::ExportData;

pub mod parsing;
mod bytecode;
mod debug;
mod object;

#[cfg(not(debug_assertions))]
static RUST_RUNTIME_LIB: &[u8] = include_bytes!("../target/wasm32-unknown-unknown/release/libcamento_runtime.a");
#[cfg(debug_assertions)]
static RUST_RUNTIME_LIB: &[u8] = include_bytes!("../target/wasm32-unknown-unknown/testing/libcamento_runtime.a");

#[derive(Debug)]
pub struct Config<'c> {
	pub target_path: Cow<'c, Path>,
	pub cleanup_objects: bool,
	pub wasm_ld_path: Cow<'c, OsStr>,
	pub output_module_path: Cow<'c, Path>,

	pub entry_point: EntryPoint<'c>,

	pub wasm_opt_path: Option<Cow<'c, OsStr>>,

	pub exports: Vec<ExportData<'static>>,

	pub export_shadow_stack: Option<Cow<'c, str>>,
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
	let lib_path = config.target_path.join("libcamento_runtime.a");
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

	let new_module = post_process(config, &old_module)?;

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

fn post_process(config: &Config, wasm: &[u8]) -> Result<Vec<u8>> {
	const MEMORY_SECTION_ID: u8 = 5;
	const EXPORT_SECTION_ID: u8 = 7;

	if wasm.len() < 8 {
		return Err(anyhow::anyhow!("Wasm module is too small to be valid"));
	}

	let mut new_module = Vec::with_capacity(wasm.len() + 10);
	new_module.extend_from_slice(&wasm[..8]);

	// Find the tail end of the memory section or export section.
	let mut cursor = 8usize;
	loop {
		let section_id = wasm[cursor];
		new_module.push(section_id);

		cursor += 1;

		let content_len = leb128fmt::decode_uint_slice::<u32, 32>(wasm, &mut cursor).context("Failed to decode value while looking for memory")? as usize;

		match section_id {
			MEMORY_SECTION_ID => handle_memory_section(config, &mut new_module, &wasm[cursor..cursor + content_len])?,
			EXPORT_SECTION_ID => handle_export_section(config, &mut new_module, &wasm[cursor..cursor + content_len])?,
			_ => {
				wasm_encoder::Encode::encode(&(content_len as u32), &mut new_module);
				new_module.extend_from_slice(&wasm[cursor..cursor + content_len]);
			},
		}

		cursor += content_len;

		if cursor >= wasm.len() {
			break;
		}
	};

	Ok(new_module)
}

fn handle_memory_section(_config: &Config, new_module: &mut Vec<u8>, section: &[u8]) -> Result<()> {
	let mem_count = section[0] as usize;
	assert_eq!(mem_count, 1, "Expected exactly one memory in the output module");

	// Encode the new length of the memory section, which is the old length + 3 (the size of one memory entry).
	wasm_encoder::Encode::encode(&(section.len() + 3), new_module);
	new_module.push(2); // The new memory count, which is the old count + 1.
	new_module.extend_from_slice(&section[1..]); // The rest of the memory section, which is unchanged.
	wasm_encoder::Encode::encode(&wasm_encoder::MemoryType { minimum: 1, maximum: Some(1), memory64: false, shared: false, page_size_log2: None }, new_module); // The new memory entry.

	Ok(())
}

fn handle_export_section(config: &Config, new_module: &mut Vec<u8>, section: &[u8]) -> Result<()> {
	let Some(name) = config.export_shadow_stack.as_ref() else {
		// If no shadow stack export is specified, we can just copy the export section as is.
		wasm_encoder::Encode::encode(&(section.len() as u32), new_module);
		new_module.extend_from_slice(section);
		return Ok(());
	};

	let mut cursor = 0;

	let export_count = leb128fmt::decode_uint_slice::<u32, 32>(section, &mut cursor).context("Failed to decode export count")? as usize;

	let mut new_section = Vec::with_capacity(section.len() + 10);

	wasm_encoder::Encode::encode(&((export_count as u32) + 1), &mut new_section); // The new export count, which is the old count + 1.
	new_section.extend_from_slice(&section[cursor..]); // The rest of the export section, which is unchanged.

	// Encode the new export, which exports the shadow stack memory.
	wasm_encoder::Encode::encode(&(name.len() as u32), &mut new_section);
	new_section.extend_from_slice(name.as_bytes());
	wasm_encoder::Encode::encode(&wasm_encoder::ExportKind::Memory, &mut new_section);
	wasm_encoder::Encode::encode(&1u32, &mut new_section); // The export index.

	wasm_encoder::Encode::encode(new_section.as_slice(), new_module);

	Ok(())
}

fn parse_lua_file(lua_file: &impl LuaFile, as_main: bool) -> Result<wasm_encoder::Module> {
	let contents = lua_file.contents()?;
	let name = lua_file.name()?;

	//TODO: Unwrap.
	let parsed = parsing::parse(contents.as_ref()).unwrap().0;
	object::produce_lua_obj_file(name.as_encoded_bytes(), as_main, parsed)
}
