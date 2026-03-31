#!/usr/bin/sh

cargo build --package wasm_scratch --target wasm32-unknown-unknown --release &&
	cargo run > out.wasm &&
	wasm-tools print out.wasm -o out.wat &&
	# wasm-tools validate out.wat -g &&
	wasm-ld --no-entry target/wasm32-unknown-unknown/release/libwasm_scratch.a out.wasm -o main.wasm &&
	wasm-tools print main.wasm -o main.wat &&
	wasm-tools validate main.wat -g &&
	# -O4 is causing issues with non-legacy exceptions...
	wasm-opt main.wasm -O3 -o luant.wasm --enable-exception-handling --enable-multimemory -g -c &&
	wasm-tools print luant.wasm -o luant.wat &&
	wasm-tools validate -f legacy-exceptions luant.wat -g
