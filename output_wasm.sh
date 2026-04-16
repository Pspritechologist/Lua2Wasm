#!/usr/bin/sh

cargo build --package camento_runtime --target wasm32-unknown-unknown --profile=testing &&
	cargo build --package camento_runtime --target wasm32-unknown-unknown --release &&
	clear &&
	cargo run test.lua output/camento.wasm &&
	(wasm-tools print output/camento.wasm -o output/camento.wat || (echo 'failed to print module; Attempting to validate then objdump...'; wasm-tools validate output/camento.wasm -g; wasm-objdump output/camento.wasm -x -d > output/camento.txt; false)) &&
	(wasm-tools validate output/camento.wat -g || (echo 'failed to validate module' && false)) &&
	wasm-objdump output/camento.wasm -x -d > output/camento.txt &&
	
	cargo run test.lua output/camento-opt.wasm -o &&
	(wasm-tools print output/camento-opt.wasm -o output/camento-opt.wat || (echo 'failed to print optimized module; Attempting to validate then objdump...'; wasm-tools validate output/camento-opt.wasm -g; wasm-objdump output/camento-opt.wasm -x -d > output/camento-opt.txt; false)) &&
	(wasm-tools validate output/camento-opt.wat -g || (echo 'failed to print optimized module'; wasm-tools validate output/camento.wasm -g; false)) &&
	wasm-objdump output/camento-opt.wasm -x -d > output/camento-opt.txt
