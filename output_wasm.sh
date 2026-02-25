#!/usr/bin/sh

cargo build --package wasm_scratch --target wasm32-unknown-unknown --release &&
	cargo run > out.wasm &&
	wasm-tools print out.wasm -o out.wat &&
	wasm-tools validate -f legacy-exceptions out.wat -g &&
	# -O4 is causing issues with non-legacy exceptions...
	wasm-opt out.wasm -O3 -o luant.wasm --enable-exception-handling --enable-multimemory -g -c &&
	wasm-tools print luant.wasm -o luant.wat &&
	wasm-tools validate -f legacy-exceptions luant.wat -g
