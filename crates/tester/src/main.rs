#![feature(try_blocks, bstr, iter_array_chunks)]

use anyhow::{Context, Result};
use wasmer::{Function, FunctionEnv, FunctionEnvMut, Instance, Memory, Module, RuntimeError, Store, WasmPtr, imports, sys::{Cranelift, EngineBuilder, Features}};
use std::{bstr::ByteStr, process::Command};

mod errors;

fn main() -> Result<()> {
	if !std::env::args_os().any(|arg| arg == "skip") && !Command::new("./output_wasm.sh")
		.status()
		.context("Running output_wasm.sh")?
		.success() {
		return Err(anyhow::anyhow!("output_wasm.sh failed"));
	}

	let bytes = std::fs::read("output/camento.wasm").context("Reading WASM file")?;
	// let bytes = std::fs::read("camento-opt.wasm").context("Reading optimized WASM file")?;

	let mut store = Store::new(EngineBuilder::new(Cranelift::default()).set_features(Some(Features::all())));

	let mut err_slot = errors::ErrorSlot::new(&mut store);

	let env = FunctionEnv::new(&mut store, None);

	let import_obj = {
		let mut obj = imports! {
			"env" => {
				"put_str" => Function::new_typed_with_env(&mut store, &env, |mut env: FunctionEnvMut<Option<Memory>>, ptr: WasmPtr<u8>, len: u32| {
					let (Some(memory), store) = env.data_and_store_mut() else { unreachable!() };
					let view = memory.view(&store);
					let msg = ptr.slice(&view, len).unwrap();
					let msg = msg.access().unwrap();
					println!("WASM: {}", ByteStr::new(&msg));
				}),
			},
		};
		err_slot.register_debug(&mut store, &mut obj);
		obj
	};

	let module = Module::new(&store, bytes).context("loading module")?;
	eprint!("Loading WASM module...");
	let instance = Instance::new(&mut store, &module, &import_obj).context("creating instance")?;
	eprintln!(" done.");

	let memory = instance.exports.get_memory("memory").unwrap();
	err_slot.set_memory(&mut store, memory.clone());
	env.as_mut(&mut store).replace(memory.clone());

	let res: Result<()> = try {
		eprint!("Initializing module...");
		eprint!(" Getting init function...");
		let init = instance.exports.get_typed_function::<(), ()>(&store, "init").context("getting init")?;
		eprint!(" Calling init...");
		init.call(&mut store).context("calling init")?;
		
		eprint!(" Getting add_one function...");
		let add_one = instance.exports.get_typed_function::<i32, i32>(&store, "add_one").context("getting add_one")?;
		eprintln!(" done.");

		eprint!("Running module...");
		for i in 0..10 {
			let res = add_one.call(&mut store, i).context("calling add_one")?;
			eprint!(" {res}");
		}
		eprintln!(" done.");
	};

	let Err(e) = res else { return Ok(()) };

	// let memory = instance.exports.get_memory("shtack").unwrap();
	// let view = memory.view(&store);
	// let mem = WasmPtr::<i64>::new(0).slice(&view, (view.data_size() / 8) as u32).unwrap();
	// let end = mem.iter().rposition(|b| b.access().unwrap().read() != 0).unwrap_or(0);

	// let mut shtack_state = String::new();
	// for value in mem.iter().take_while(|b| b.offset() <= end as u64 * 8).map(|v| value::Value::from_i64(v.access().unwrap().read())) {
	// 	use std::fmt::Write;
	// 	write!(&mut shtack_state, "{}({:#x}) ", value.get_tag(), u64::from_le_bytes(value.meaningful_bits())).unwrap();
	// }

	// let e = e.context(format!("Shtack at time of error > {shtack_state}"));
	
	Err(match err_slot.get_error(&mut store) {
		Some(msg) => e.context(format!("Error within WASM: {msg}")),
		None => e.context("Unspecified error"),
	})
}
