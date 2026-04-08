#![feature(try_blocks, bstr)]

use anyhow::{Context, Result};
use wasmer::{Function, FunctionEnv, FunctionEnvMut, Instance, Memory, Module, RuntimeError, Store, WasmPtr, imports, sys::{Cranelift, EngineBuilder, Features}};
use std::{bstr::ByteStr, process::Command};

mod errors;

fn main() -> Result<()> {
	Command::new("./output_wasm.sh")
		.status()
		.context("Running output_wasm.sh")?;

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

	let res: Result<()> = try {
		let module = Module::new(&store, bytes).context("loading module")?;
		eprint!("Loading WASM module...");
		let instance = Instance::new(&mut store, &module, &import_obj).context("creating instance")?;
		eprintln!(" done.");

		let memory = instance.exports.get_memory("memory").unwrap();
			err_slot.set_memory(&mut store, memory.clone());
		env.as_mut(&mut store).replace(memory.clone());

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
			std::hint::black_box(res);
		}
		eprintln!(" done.");
	};

	let Err(e) = res else { return Ok(()) };

	Err(match err_slot.get_error(&mut store) {
		Some(msg) => e.context(format!("Error within WASM: {msg}")),
		None => e.context("Unspecified error"),
	})
}
