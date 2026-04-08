#!/usr/bin/sh

cargo build --package wasm_scratch --target wasm32-unknown-unknown --release &&
	cargo run test.lua luant.wasm &&
	(wasm-tools print luant.wasm -o luant.wat || (echo 'failed to print module'; wasm-tools validate luant.wasm -g; false)) &&
	(wasm-tools validate luant.wat -g || (echo 'failed to validate module' && false)) &&
	wasm-objdump luant.wasm -x -d > luant.txt &&
	
	cargo run test.lua luant-opt.wasm -o &&
	(wasm-tools print luant-opt.wasm -o luant-opt.wat || (echo 'failed to print optimized module' && false)) &&
	(wasm-tools validate luant-opt.wat -g || (echo 'failed to print optimized module'; wasm-tools validate luant.wasm -g; false)) &&
	wasm-objdump luant-opt.wasm -x -d > luant-opt.txt
