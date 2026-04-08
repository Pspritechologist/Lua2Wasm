use wasmer::{Exports, Function, FunctionEnv, FunctionEnvMut, Imports, Memory, Store, WasmPtr};

pub struct ErrorSlot {
	fn_env: FunctionEnv<(Option<Memory>, ErrData)>,
}

#[derive(Default)]
struct ErrData {
	buf: Vec<u8>,
	init: bool,
}

impl ErrorSlot {
	pub fn new(store: &mut Store) -> Self {
		let fn_env = FunctionEnv::new(
			store,
			(None, ErrData::default()),
		);

		Self { fn_env }
	}

	pub fn set_memory(&mut self, store: &mut Store, memory: Memory) {
		self.fn_env.as_mut(store).0 = Some(memory);
	}

	pub fn get_error<'s>(&mut self, store: &'s mut Store) -> Option<ErrorGuard<'_, 's>> {
		let (_, data) = self.fn_env.as_ref(store);
		if data.init { Some(ErrorGuard { slot: self, store }) } else { None }
	}

	pub fn register_debug(&mut self, st: &mut Store, import_obj: &mut Imports) {
		let mut ns = Exports::new();

		ns.insert("put_error", Function::new_typed_with_env(st, &self.fn_env, Self::put_error));
		ns.insert("has_error", Function::new_typed_with_env(st, &self.fn_env, Self::has_error));

		import_obj.register_namespace("debug", ns);
	}

	fn put_error(mut env: FunctionEnvMut<(Option<Memory>, ErrData)>, ptr: WasmPtr<u8>, len: u32) {
		let ((memory, data), store) = env.data_and_store_mut();

		if data.init {
			let old_msg = unsafe { std::str::from_utf8_unchecked(&data.buf) };
			println!("Received new error before last was read! old: {old_msg}");
		}

		let memory = memory.as_ref().expect("Memory not initalized");
		let view = memory.view(&store);
		let msg = ptr.slice(&view, len).unwrap();
		let msg = msg.access().unwrap();

		debug_assert!(std::str::from_utf8(msg.as_ref()).is_ok(), "WASM passed non UTF-8 error message");

		data.buf.clear();
		data.buf.extend_from_slice(msg.as_ref());
		data.init = true;
	}

	fn has_error(env: FunctionEnvMut<(Option<Memory>, ErrData)>) -> i32 {
		env.data().1.init as i32
	}
}

pub struct ErrorGuard<'a, 's> {
	store: &'s mut Store,
	slot: &'a mut ErrorSlot,
}

impl ErrorGuard<'_, '_> {
	fn as_str(&self) -> &str {
		let (_, data) = self.slot.fn_env.as_ref(self.store);
		debug_assert!(data.init, "ErrorGuard created when no error present");
		unsafe { std::str::from_utf8_unchecked(&data.buf) }
	}
}

impl std::fmt::Display for ErrorGuard<'_, '_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.as_str().fmt(f)
	}
}

impl<'a> Drop for ErrorGuard<'a, '_> {
	fn drop(&mut self) {
		let (_, data) = self.slot.fn_env.as_mut(self.store);
		data.init = false;
	}
}

impl std::ops::Deref for ErrorGuard<'_, '_> {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		self.as_str()
	}
}
