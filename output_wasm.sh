#!/usr/bin/sh

cargo build --package wasm_scratch --target wasm32-unknown-unknown --release &&
	cargo run test.lua output/luant.wasm &&
	(wasm-tools print output/luant.wasm -o output/luant.wat || (echo 'failed to print module'; wasm-tools validate output/luant.wasm -g; false)) &&
	(wasm-tools validate output/luant.wat -g || (echo 'failed to validate module' && false)) &&
	wasm-objdump output/luant.wasm -x -d > output/luant.txt &&
	
	cargo run test.lua output/luant-opt.wasm -o &&
	(wasm-tools print output/luant-opt.wasm -o output/luant-opt.wat || (echo 'failed to print optimized module' && false)) &&
	(wasm-tools validate output/luant-opt.wat -g || (echo 'failed to print optimized module'; wasm-tools validate output/luant.wasm -g; false)) &&
	wasm-objdump output/luant-opt.wasm -x -d > output/luant-opt.txt
