use wasm_encoder::{
	BlockType, Catch, Encode, Handle, HeapType, Ieee32, Ieee64, Lane, MemArg, Ordering, RefType,
	ValType,
};

use crate::object::linking::RelocEntry;

mod instructions_ext;
mod functions;

pub use functions::*;

/// An encoder for Wasm instructions.
#[derive(Debug)]
pub struct InstructionSink<'a> {
	func: &'a mut wasm_encoder::Function,
	sink: Vec<u8>,
	relocations: &'a mut Vec<RelocEntry>,
}

impl Drop for InstructionSink<'_> {
	// Ensures the encoded bytes get appended to the function.
	fn drop(&mut self) {
		self.func.raw(self.sink.iter().copied());
	}
}

impl<'a> InstructionSink<'a> {
	/// Create an instruction encoder pointing to the given byte sink.
	pub(crate) fn new(func: &'a mut wasm_encoder::Function, relocations: &'a mut Vec<RelocEntry>) -> Self {
		// This is silly but was just the easiest way to implement it at the time.
		Self {
			func,
			sink: Vec::new(),
			relocations,
		}
	}

	/// Gets the length of the generated function so far.
	pub(crate) fn byte_len(&self) -> usize {
		self.func.byte_len() + self.sink.len()
	}

	// Control instructions.

	/// Encode [`Instruction::Unreachable`].
	pub(crate) fn unreachable(&mut self) -> &mut Self {
		self.sink.push(0x00);
		self
	}

	/// Encode [`Instruction::Nop`].
	pub(crate) fn nop(&mut self) -> &mut Self {
		self.sink.push(0x01);
		self
	}

	/// Encode [`Instruction::Block`].
	fn block_raw(&mut self, bt: BlockType) -> &mut Self {
		self.sink.push(0x02);
		bt.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Loop`].
	fn loop_raw(&mut self, bt: BlockType) -> &mut Self {
		self.sink.push(0x03);
		bt.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::If`].
	fn if_raw(&mut self, bt: BlockType) -> &mut Self {
		self.sink.push(0x04);
		bt.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Else`].
	pub(crate) fn else_(&mut self) -> &mut Self {
		self.sink.push(0x05);
		self
	}

	/// Encode [`Instruction::End`].
	pub(crate) fn end(&mut self) -> &mut Self {
		self.sink.push(0x0B);
		self
	}

	/// Encode [`Instruction::Br`].
	pub(crate) fn br(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x0C);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrIf`].
	pub(crate) fn br_if(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x0D);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrTable`].
	pub(crate) fn br_table<V: IntoIterator<Item = u32>>(&mut self, ls: V, l: u32) -> &mut Self
	where
		V::IntoIter: ExactSizeIterator,
	{
		self.sink.push(0x0E);
		encode_vec(ls, &mut self.sink);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrOnNull`].
	pub(crate) fn br_on_null(&mut self, l: u32) -> &mut Self {
		self.sink.push(0xD5);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrOnNonNull`].
	pub(crate) fn br_on_non_null(&mut self, l: u32) -> &mut Self {
		self.sink.push(0xD6);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Return`].
	pub(crate) fn return_(&mut self) -> &mut Self {
		self.sink.push(0x0F);
		self
	}

	//? Calls made private for relocations.
	/// Encode [`Instruction::Call`].
	fn call_raw(&mut self, f: u32) -> &mut Self {
		self.sink.push(0x10);
		f.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::CallRef`].
	fn call_ref_raw(&mut self, ty: u32) -> &mut Self {
		self.sink.push(0x14);
		ty.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::CallIndirect`].
	fn call_indirect_raw(&mut self, table_index: u32, type_index: u32) -> &mut Self {
		self.sink.push(0x11);
		type_index.encode(&mut self.sink);
		table_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ReturnCallRef`].
	fn return_call_ref_raw(&mut self, ty: u32) -> &mut Self {
		self.sink.push(0x15);
		ty.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ReturnCall`].
	fn return_call_raw(&mut self, f: u32) -> &mut Self {
		self.sink.push(0x12);
		f.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ReturnCallIndirect`].
	fn return_call_indirect_raw(&mut self, table_index: u32, type_index: u32) -> &mut Self {
		self.sink.push(0x13);
		type_index.encode(&mut self.sink);
		table_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TryTable`].
	fn try_table_raw<V: IntoIterator<Item = Catch>>(
		&mut self,
		ty: BlockType,
		catches: V,
	) -> &mut Self
	where
		V::IntoIter: ExactSizeIterator,
	{
		self.sink.push(0x1f);
		ty.encode(&mut self.sink);
		encode_vec(catches, &mut self.sink);
		self
	}

	/// Encode [`Instruction::Throw`].
	fn throw_raw(&mut self, t: u32) -> &mut Self {
		self.sink.push(0x08);
		t.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ThrowRef`].
	fn throw_ref_raw(&mut self) -> &mut Self {
		self.sink.push(0x0A);
		self
	}

	// Deprecated exception-handling instructions

	/// Encode [`Instruction::Try`].
	fn try_raw(&mut self, bt: BlockType) -> &mut Self {
		self.sink.push(0x06);
		bt.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Delegate`].
	fn delegate_raw(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x18);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Catch`].
	fn catch_raw(&mut self, t: u32) -> &mut Self {
		self.sink.push(0x07);
		t.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::CatchAll`].
	fn catch_all_raw(&mut self) -> &mut Self {
		self.sink.push(0x19);
		self
	}

	/// Encode [`Instruction::Rethrow`].
	fn rethrow_raw(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x09);
		l.encode(&mut self.sink);
		self
	}

	// Parametric instructions.

	/// Encode [`Instruction::Drop`].
	pub(crate) fn drop(&mut self) -> &mut Self {
		self.sink.push(0x1A);
		self
	}

	/// Encode [`Instruction::Select`].
	pub(crate) fn select(&mut self) -> &mut Self {
		self.sink.push(0x1B);
		self
	}

	// Variable instructions.

	/// Encode [`Instruction::LocalGet`].
	pub(crate) fn local_get(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x20);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::LocalSet`].
	pub(crate) fn local_set(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x21);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::LocalTee`].
	pub(crate) fn local_tee(&mut self, l: u32) -> &mut Self {
		self.sink.push(0x22);
		l.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalGet`].
	fn global_get_raw(&mut self, g: u32) -> &mut Self {
		self.sink.push(0x23);
		g.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalSet`].
	fn global_set_raw(&mut self, g: u32) -> &mut Self {
		self.sink.push(0x24);
		g.encode(&mut self.sink);
		self
	}

	// Memory instructions.

	/// Encode [`Instruction::I32Load`].
	pub(crate) fn i32_load(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x28);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load`].
	pub(crate) fn i64_load(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x29);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32Load`].
	pub(crate) fn f32_load(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x2A);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64Load`].
	pub(crate) fn f64_load(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x2B);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Load8S`].
	pub(crate) fn i32_load8_s(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x2C);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Load8U`].
	pub(crate) fn i32_load8_u(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x2D);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Load16S`].
	pub(crate) fn i32_load16_s(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x2E);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Load16U`].
	pub(crate) fn i32_load16_u(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x2F);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load8S`].
	pub(crate) fn i64_load8_s(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x30);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load8U`].
	pub(crate) fn i64_load8_u(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x31);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load16S`].
	pub(crate) fn i64_load16_s(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x32);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load16U`].
	pub(crate) fn i64_load16_u(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x33);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load32S`].
	pub(crate) fn i64_load32_s(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x34);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Load32U`].
	pub(crate) fn i64_load32_u(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x35);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Store`].
	pub(crate) fn i32_store(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x36);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Store`].
	pub(crate) fn i64_store(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x37);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32Store`].
	pub(crate) fn f32_store(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x38);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64Store`].
	pub(crate) fn f64_store(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x39);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Store8`].
	pub(crate) fn i32_store8(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x3A);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32Store16`].
	pub(crate) fn i32_store16(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x3B);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Store8`].
	pub(crate) fn i64_store8(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x3C);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Store16`].
	pub(crate) fn i64_store16(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x3D);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Store32`].
	pub(crate) fn i64_store32(&mut self, m: MemArg) -> &mut Self {
		self.sink.push(0x3E);
		m.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemorySize`].
	pub(crate) fn memory_size(&mut self, i: u32) -> &mut Self {
		self.sink.push(0x3F);
		i.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryGrow`].
	pub(crate) fn memory_grow(&mut self, i: u32) -> &mut Self {
		self.sink.push(0x40);
		i.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryInit`].
	pub(crate) fn memory_init(&mut self, mem: u32, data_index: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x08);
		data_index.encode(&mut self.sink);
		mem.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::DataDrop`].
	pub(crate) fn data_drop(&mut self, data: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x09);
		data.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryCopy`].
	pub(crate) fn memory_copy(&mut self, dst_mem: u32, src_mem: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x0a);
		dst_mem.encode(&mut self.sink);
		src_mem.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryFill`].
	pub(crate) fn memory_fill(&mut self, mem: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x0b);
		mem.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryDiscard`].
	pub(crate) fn memory_discard(&mut self, mem: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x12);
		mem.encode(&mut self.sink);
		self
	}

	// Numeric instructions.

	/// Encode [`Instruction::I32Const`].
	pub(crate) fn i32_const(&mut self, x: i32) -> &mut Self {
		self.sink.push(0x41);
		x.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Const`].
	pub(crate) fn i64_const(&mut self, x: i64) -> &mut Self {
		self.sink.push(0x42);
		x.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32Const`].
	pub(crate) fn f32_const(&mut self, x: Ieee32) -> &mut Self {
		self.sink.push(0x43);
		let x = x.bits();
		self.sink.extend(x.to_le_bytes().iter().copied());
		self
	}

	/// Encode [`Instruction::F64Const`].
	pub(crate) fn f64_const(&mut self, x: Ieee64) -> &mut Self {
		self.sink.push(0x44);
		let x = x.bits();
		self.sink.extend(x.to_le_bytes().iter().copied());
		self
	}

	/// Encode [`Instruction::I32Eqz`].
	pub(crate) fn i32_eqz(&mut self) -> &mut Self {
		self.sink.push(0x45);
		self
	}

	/// Encode [`Instruction::I32Eq`].
	pub(crate) fn i32_eq(&mut self) -> &mut Self {
		self.sink.push(0x46);
		self
	}

	/// Encode [`Instruction::I32Ne`].
	pub(crate) fn i32_ne(&mut self) -> &mut Self {
		self.sink.push(0x47);
		self
	}

	/// Encode [`Instruction::I32LtS`].
	pub(crate) fn i32_lt_s(&mut self) -> &mut Self {
		self.sink.push(0x48);
		self
	}

	/// Encode [`Instruction::I32LtU`].
	pub(crate) fn i32_lt_u(&mut self) -> &mut Self {
		self.sink.push(0x49);
		self
	}

	/// Encode [`Instruction::I32GtS`].
	pub(crate) fn i32_gt_s(&mut self) -> &mut Self {
		self.sink.push(0x4A);
		self
	}

	/// Encode [`Instruction::I32GtU`].
	pub(crate) fn i32_gt_u(&mut self) -> &mut Self {
		self.sink.push(0x4B);
		self
	}

	/// Encode [`Instruction::I32LeS`].
	pub(crate) fn i32_le_s(&mut self) -> &mut Self {
		self.sink.push(0x4C);
		self
	}

	/// Encode [`Instruction::I32LeU`].
	pub(crate) fn i32_le_u(&mut self) -> &mut Self {
		self.sink.push(0x4D);
		self
	}

	/// Encode [`Instruction::I32GeS`].
	pub(crate) fn i32_ge_s(&mut self) -> &mut Self {
		self.sink.push(0x4E);
		self
	}

	/// Encode [`Instruction::I32GeU`].
	pub(crate) fn i32_ge_u(&mut self) -> &mut Self {
		self.sink.push(0x4F);
		self
	}

	/// Encode [`Instruction::I64Eqz`].
	pub(crate) fn i64_eqz(&mut self) -> &mut Self {
		self.sink.push(0x50);
		self
	}

	/// Encode [`Instruction::I64Eq`].
	pub(crate) fn i64_eq(&mut self) -> &mut Self {
		self.sink.push(0x51);
		self
	}

	/// Encode [`Instruction::I64Ne`].
	pub(crate) fn i64_ne(&mut self) -> &mut Self {
		self.sink.push(0x52);
		self
	}

	/// Encode [`Instruction::I64LtS`].
	pub(crate) fn i64_lt_s(&mut self) -> &mut Self {
		self.sink.push(0x53);
		self
	}

	/// Encode [`Instruction::I64LtU`].
	pub(crate) fn i64_lt_u(&mut self) -> &mut Self {
		self.sink.push(0x54);
		self
	}

	/// Encode [`Instruction::I64GtS`].
	pub(crate) fn i64_gt_s(&mut self) -> &mut Self {
		self.sink.push(0x55);
		self
	}

	/// Encode [`Instruction::I64GtU`].
	pub(crate) fn i64_gt_u(&mut self) -> &mut Self {
		self.sink.push(0x56);
		self
	}

	/// Encode [`Instruction::I64LeS`].
	pub(crate) fn i64_le_s(&mut self) -> &mut Self {
		self.sink.push(0x57);
		self
	}

	/// Encode [`Instruction::I64LeU`].
	pub(crate) fn i64_le_u(&mut self) -> &mut Self {
		self.sink.push(0x58);
		self
	}

	/// Encode [`Instruction::I64GeS`].
	pub(crate) fn i64_ge_s(&mut self) -> &mut Self {
		self.sink.push(0x59);
		self
	}

	/// Encode [`Instruction::I64GeU`].
	pub(crate) fn i64_ge_u(&mut self) -> &mut Self {
		self.sink.push(0x5A);
		self
	}

	/// Encode [`Instruction::F32Eq`].
	pub(crate) fn f32_eq(&mut self) -> &mut Self {
		self.sink.push(0x5B);
		self
	}

	/// Encode [`Instruction::F32Ne`].
	pub(crate) fn f32_ne(&mut self) -> &mut Self {
		self.sink.push(0x5C);
		self
	}

	/// Encode [`Instruction::F32Lt`].
	pub(crate) fn f32_lt(&mut self) -> &mut Self {
		self.sink.push(0x5D);
		self
	}

	/// Encode [`Instruction::F32Gt`].
	pub(crate) fn f32_gt(&mut self) -> &mut Self {
		self.sink.push(0x5E);
		self
	}

	/// Encode [`Instruction::F32Le`].
	pub(crate) fn f32_le(&mut self) -> &mut Self {
		self.sink.push(0x5F);
		self
	}

	/// Encode [`Instruction::F32Ge`].
	pub(crate) fn f32_ge(&mut self) -> &mut Self {
		self.sink.push(0x60);
		self
	}

	/// Encode [`Instruction::F64Eq`].
	pub(crate) fn f64_eq(&mut self) -> &mut Self {
		self.sink.push(0x61);
		self
	}

	/// Encode [`Instruction::F64Ne`].
	pub(crate) fn f64_ne(&mut self) -> &mut Self {
		self.sink.push(0x62);
		self
	}

	/// Encode [`Instruction::F64Lt`].
	pub(crate) fn f64_lt(&mut self) -> &mut Self {
		self.sink.push(0x63);
		self
	}

	/// Encode [`Instruction::F64Gt`].
	pub(crate) fn f64_gt(&mut self) -> &mut Self {
		self.sink.push(0x64);
		self
	}

	/// Encode [`Instruction::F64Le`].
	pub(crate) fn f64_le(&mut self) -> &mut Self {
		self.sink.push(0x65);
		self
	}

	/// Encode [`Instruction::F64Ge`].
	pub(crate) fn f64_ge(&mut self) -> &mut Self {
		self.sink.push(0x66);
		self
	}

	/// Encode [`Instruction::I32Clz`].
	pub(crate) fn i32_clz(&mut self) -> &mut Self {
		self.sink.push(0x67);
		self
	}

	/// Encode [`Instruction::I32Ctz`].
	pub(crate) fn i32_ctz(&mut self) -> &mut Self {
		self.sink.push(0x68);
		self
	}

	/// Encode [`Instruction::I32Popcnt`].
	pub(crate) fn i32_popcnt(&mut self) -> &mut Self {
		self.sink.push(0x69);
		self
	}

	/// Encode [`Instruction::I32Add`].
	pub(crate) fn i32_add(&mut self) -> &mut Self {
		self.sink.push(0x6A);
		self
	}

	/// Encode [`Instruction::I32Sub`].
	pub(crate) fn i32_sub(&mut self) -> &mut Self {
		self.sink.push(0x6B);
		self
	}

	/// Encode [`Instruction::I32Mul`].
	pub(crate) fn i32_mul(&mut self) -> &mut Self {
		self.sink.push(0x6C);
		self
	}

	/// Encode [`Instruction::I32DivS`].
	pub(crate) fn i32_div_s(&mut self) -> &mut Self {
		self.sink.push(0x6D);
		self
	}

	/// Encode [`Instruction::I32DivU`].
	pub(crate) fn i32_div_u(&mut self) -> &mut Self {
		self.sink.push(0x6E);
		self
	}

	/// Encode [`Instruction::I32RemS`].
	pub(crate) fn i32_rem_s(&mut self) -> &mut Self {
		self.sink.push(0x6F);
		self
	}

	/// Encode [`Instruction::I32RemU`].
	pub(crate) fn i32_rem_u(&mut self) -> &mut Self {
		self.sink.push(0x70);
		self
	}

	/// Encode [`Instruction::I32And`].
	pub(crate) fn i32_and(&mut self) -> &mut Self {
		self.sink.push(0x71);
		self
	}

	/// Encode [`Instruction::I32Or`].
	pub(crate) fn i32_or(&mut self) -> &mut Self {
		self.sink.push(0x72);
		self
	}

	/// Encode [`Instruction::I32Xor`].
	pub(crate) fn i32_xor(&mut self) -> &mut Self {
		self.sink.push(0x73);
		self
	}

	/// Encode [`Instruction::I32Shl`].
	pub(crate) fn i32_shl(&mut self) -> &mut Self {
		self.sink.push(0x74);
		self
	}

	/// Encode [`Instruction::I32ShrS`].
	pub(crate) fn i32_shr_s(&mut self) -> &mut Self {
		self.sink.push(0x75);
		self
	}

	/// Encode [`Instruction::I32ShrU`].
	pub(crate) fn i32_shr_u(&mut self) -> &mut Self {
		self.sink.push(0x76);
		self
	}

	/// Encode [`Instruction::I32Rotl`].
	pub(crate) fn i32_rotl(&mut self) -> &mut Self {
		self.sink.push(0x77);
		self
	}

	/// Encode [`Instruction::I32Rotr`].
	pub(crate) fn i32_rotr(&mut self) -> &mut Self {
		self.sink.push(0x78);
		self
	}

	/// Encode [`Instruction::I64Clz`].
	pub(crate) fn i64_clz(&mut self) -> &mut Self {
		self.sink.push(0x79);
		self
	}

	/// Encode [`Instruction::I64Ctz`].
	pub(crate) fn i64_ctz(&mut self) -> &mut Self {
		self.sink.push(0x7A);
		self
	}

	/// Encode [`Instruction::I64Popcnt`].
	pub(crate) fn i64_popcnt(&mut self) -> &mut Self {
		self.sink.push(0x7B);
		self
	}

	/// Encode [`Instruction::I64Add`].
	pub(crate) fn i64_add(&mut self) -> &mut Self {
		self.sink.push(0x7C);
		self
	}

	/// Encode [`Instruction::I64Sub`].
	pub(crate) fn i64_sub(&mut self) -> &mut Self {
		self.sink.push(0x7D);
		self
	}

	/// Encode [`Instruction::I64Mul`].
	pub(crate) fn i64_mul(&mut self) -> &mut Self {
		self.sink.push(0x7E);
		self
	}

	/// Encode [`Instruction::I64DivS`].
	pub(crate) fn i64_div_s(&mut self) -> &mut Self {
		self.sink.push(0x7F);
		self
	}

	/// Encode [`Instruction::I64DivU`].
	pub(crate) fn i64_div_u(&mut self) -> &mut Self {
		self.sink.push(0x80);
		self
	}

	/// Encode [`Instruction::I64RemS`].
	pub(crate) fn i64_rem_s(&mut self) -> &mut Self {
		self.sink.push(0x81);
		self
	}

	/// Encode [`Instruction::I64RemU`].
	pub(crate) fn i64_rem_u(&mut self) -> &mut Self {
		self.sink.push(0x82);
		self
	}

	/// Encode [`Instruction::I64And`].
	pub(crate) fn i64_and(&mut self) -> &mut Self {
		self.sink.push(0x83);
		self
	}

	/// Encode [`Instruction::I64Or`].
	pub(crate) fn i64_or(&mut self) -> &mut Self {
		self.sink.push(0x84);
		self
	}

	/// Encode [`Instruction::I64Xor`].
	pub(crate) fn i64_xor(&mut self) -> &mut Self {
		self.sink.push(0x85);
		self
	}

	/// Encode [`Instruction::I64Shl`].
	pub(crate) fn i64_shl(&mut self) -> &mut Self {
		self.sink.push(0x86);
		self
	}

	/// Encode [`Instruction::I64ShrS`].
	pub(crate) fn i64_shr_s(&mut self) -> &mut Self {
		self.sink.push(0x87);
		self
	}

	/// Encode [`Instruction::I64ShrU`].
	pub(crate) fn i64_shr_u(&mut self) -> &mut Self {
		self.sink.push(0x88);
		self
	}

	/// Encode [`Instruction::I64Rotl`].
	pub(crate) fn i64_rotl(&mut self) -> &mut Self {
		self.sink.push(0x89);
		self
	}

	/// Encode [`Instruction::I64Rotr`].
	pub(crate) fn i64_rotr(&mut self) -> &mut Self {
		self.sink.push(0x8A);
		self
	}

	/// Encode [`Instruction::F32Abs`].
	pub(crate) fn f32_abs(&mut self) -> &mut Self {
		self.sink.push(0x8B);
		self
	}

	/// Encode [`Instruction::F32Neg`].
	pub(crate) fn f32_neg(&mut self) -> &mut Self {
		self.sink.push(0x8C);
		self
	}

	/// Encode [`Instruction::F32Ceil`].
	pub(crate) fn f32_ceil(&mut self) -> &mut Self {
		self.sink.push(0x8D);
		self
	}

	/// Encode [`Instruction::F32Floor`].
	pub(crate) fn f32_floor(&mut self) -> &mut Self {
		self.sink.push(0x8E);
		self
	}

	/// Encode [`Instruction::F32Trunc`].
	pub(crate) fn f32_trunc(&mut self) -> &mut Self {
		self.sink.push(0x8F);
		self
	}

	/// Encode [`Instruction::F32Nearest`].
	pub(crate) fn f32_nearest(&mut self) -> &mut Self {
		self.sink.push(0x90);
		self
	}

	/// Encode [`Instruction::F32Sqrt`].
	pub(crate) fn f32_sqrt(&mut self) -> &mut Self {
		self.sink.push(0x91);
		self
	}

	/// Encode [`Instruction::F32Add`].
	pub(crate) fn f32_add(&mut self) -> &mut Self {
		self.sink.push(0x92);
		self
	}

	/// Encode [`Instruction::F32Sub`].
	pub(crate) fn f32_sub(&mut self) -> &mut Self {
		self.sink.push(0x93);
		self
	}

	/// Encode [`Instruction::F32Mul`].
	pub(crate) fn f32_mul(&mut self) -> &mut Self {
		self.sink.push(0x94);
		self
	}

	/// Encode [`Instruction::F32Div`].
	pub(crate) fn f32_div(&mut self) -> &mut Self {
		self.sink.push(0x95);
		self
	}

	/// Encode [`Instruction::F32Min`].
	pub(crate) fn f32_min(&mut self) -> &mut Self {
		self.sink.push(0x96);
		self
	}

	/// Encode [`Instruction::F32Max`].
	pub(crate) fn f32_max(&mut self) -> &mut Self {
		self.sink.push(0x97);
		self
	}

	/// Encode [`Instruction::F32Copysign`].
	pub(crate) fn f32_copysign(&mut self) -> &mut Self {
		self.sink.push(0x98);
		self
	}

	/// Encode [`Instruction::F64Abs`].
	pub(crate) fn f64_abs(&mut self) -> &mut Self {
		self.sink.push(0x99);
		self
	}

	/// Encode [`Instruction::F64Neg`].
	pub(crate) fn f64_neg(&mut self) -> &mut Self {
		self.sink.push(0x9A);
		self
	}

	/// Encode [`Instruction::F64Ceil`].
	pub(crate) fn f64_ceil(&mut self) -> &mut Self {
		self.sink.push(0x9B);
		self
	}

	/// Encode [`Instruction::F64Floor`].
	pub(crate) fn f64_floor(&mut self) -> &mut Self {
		self.sink.push(0x9C);
		self
	}

	/// Encode [`Instruction::F64Trunc`].
	pub(crate) fn f64_trunc(&mut self) -> &mut Self {
		self.sink.push(0x9D);
		self
	}

	/// Encode [`Instruction::F64Nearest`].
	pub(crate) fn f64_nearest(&mut self) -> &mut Self {
		self.sink.push(0x9E);
		self
	}

	/// Encode [`Instruction::F64Sqrt`].
	pub(crate) fn f64_sqrt(&mut self) -> &mut Self {
		self.sink.push(0x9F);
		self
	}

	/// Encode [`Instruction::F64Add`].
	pub(crate) fn f64_add(&mut self) -> &mut Self {
		self.sink.push(0xA0);
		self
	}

	/// Encode [`Instruction::F64Sub`].
	pub(crate) fn f64_sub(&mut self) -> &mut Self {
		self.sink.push(0xA1);
		self
	}

	/// Encode [`Instruction::F64Mul`].
	pub(crate) fn f64_mul(&mut self) -> &mut Self {
		self.sink.push(0xA2);
		self
	}

	/// Encode [`Instruction::F64Div`].
	pub(crate) fn f64_div(&mut self) -> &mut Self {
		self.sink.push(0xA3);
		self
	}

	/// Encode [`Instruction::F64Min`].
	pub(crate) fn f64_min(&mut self) -> &mut Self {
		self.sink.push(0xA4);
		self
	}

	/// Encode [`Instruction::F64Max`].
	pub(crate) fn f64_max(&mut self) -> &mut Self {
		self.sink.push(0xA5);
		self
	}

	/// Encode [`Instruction::F64Copysign`].
	pub(crate) fn f64_copysign(&mut self) -> &mut Self {
		self.sink.push(0xA6);
		self
	}

	/// Encode [`Instruction::I32WrapI64`].
	pub(crate) fn i32_wrap_i64(&mut self) -> &mut Self {
		self.sink.push(0xA7);
		self
	}

	/// Encode [`Instruction::I32TruncF32S`].
	pub(crate) fn i32_trunc_f32_s(&mut self) -> &mut Self {
		self.sink.push(0xA8);
		self
	}

	/// Encode [`Instruction::I32TruncF32U`].
	pub(crate) fn i32_trunc_f32_u(&mut self) -> &mut Self {
		self.sink.push(0xA9);
		self
	}

	/// Encode [`Instruction::I32TruncF64S`].
	pub(crate) fn i32_trunc_f64_s(&mut self) -> &mut Self {
		self.sink.push(0xAA);
		self
	}

	/// Encode [`Instruction::I32TruncF64U`].
	pub(crate) fn i32_trunc_f64_u(&mut self) -> &mut Self {
		self.sink.push(0xAB);
		self
	}

	/// Encode [`Instruction::I64ExtendI32S`].
	pub(crate) fn i64_extend_i32_s(&mut self) -> &mut Self {
		self.sink.push(0xAC);
		self
	}

	/// Encode [`Instruction::I64ExtendI32U`].
	pub(crate) fn i64_extend_i32_u(&mut self) -> &mut Self {
		self.sink.push(0xAD);
		self
	}

	/// Encode [`Instruction::I64TruncF32S`].
	pub(crate) fn i64_trunc_f32_s(&mut self) -> &mut Self {
		self.sink.push(0xAE);
		self
	}

	/// Encode [`Instruction::I64TruncF32U`].
	pub(crate) fn i64_trunc_f32_u(&mut self) -> &mut Self {
		self.sink.push(0xAF);
		self
	}

	/// Encode [`Instruction::I64TruncF64S`].
	pub(crate) fn i64_trunc_f64_s(&mut self) -> &mut Self {
		self.sink.push(0xB0);
		self
	}

	/// Encode [`Instruction::I64TruncF64U`].
	pub(crate) fn i64_trunc_f64_u(&mut self) -> &mut Self {
		self.sink.push(0xB1);
		self
	}

	/// Encode [`Instruction::F32ConvertI32S`].
	pub(crate) fn f32_convert_i32_s(&mut self) -> &mut Self {
		self.sink.push(0xB2);
		self
	}

	/// Encode [`Instruction::F32ConvertI32U`].
	pub(crate) fn f32_convert_i32_u(&mut self) -> &mut Self {
		self.sink.push(0xB3);
		self
	}

	/// Encode [`Instruction::F32ConvertI64S`].
	pub(crate) fn f32_convert_i64_s(&mut self) -> &mut Self {
		self.sink.push(0xB4);
		self
	}

	/// Encode [`Instruction::F32ConvertI64U`].
	pub(crate) fn f32_convert_i64_u(&mut self) -> &mut Self {
		self.sink.push(0xB5);
		self
	}

	/// Encode [`Instruction::F32DemoteF64`].
	pub(crate) fn f32_demote_f64(&mut self) -> &mut Self {
		self.sink.push(0xB6);
		self
	}

	/// Encode [`Instruction::F64ConvertI32S`].
	pub(crate) fn f64_convert_i32_s(&mut self) -> &mut Self {
		self.sink.push(0xB7);
		self
	}

	/// Encode [`Instruction::F64ConvertI32U`].
	pub(crate) fn f64_convert_i32_u(&mut self) -> &mut Self {
		self.sink.push(0xB8);
		self
	}

	/// Encode [`Instruction::F64ConvertI64S`].
	pub(crate) fn f64_convert_i64_s(&mut self) -> &mut Self {
		self.sink.push(0xB9);
		self
	}

	/// Encode [`Instruction::F64ConvertI64U`].
	pub(crate) fn f64_convert_i64_u(&mut self) -> &mut Self {
		self.sink.push(0xBA);
		self
	}

	/// Encode [`Instruction::F64PromoteF32`].
	pub(crate) fn f64_promote_f32(&mut self) -> &mut Self {
		self.sink.push(0xBB);
		self
	}

	/// Encode [`Instruction::I32ReinterpretF32`].
	pub(crate) fn i32_reinterpret_f32(&mut self) -> &mut Self {
		self.sink.push(0xBC);
		self
	}

	/// Encode [`Instruction::I64ReinterpretF64`].
	pub(crate) fn i64_reinterpret_f64(&mut self) -> &mut Self {
		self.sink.push(0xBD);
		self
	}

	/// Encode [`Instruction::F32ReinterpretI32`].
	pub(crate) fn f32_reinterpret_i32(&mut self) -> &mut Self {
		self.sink.push(0xBE);
		self
	}

	/// Encode [`Instruction::F64ReinterpretI64`].
	pub(crate) fn f64_reinterpret_i64(&mut self) -> &mut Self {
		self.sink.push(0xBF);
		self
	}

	/// Encode [`Instruction::I32Extend8S`].
	pub(crate) fn i32_extend8_s(&mut self) -> &mut Self {
		self.sink.push(0xC0);
		self
	}

	/// Encode [`Instruction::I32Extend16S`].
	pub(crate) fn i32_extend16_s(&mut self) -> &mut Self {
		self.sink.push(0xC1);
		self
	}

	/// Encode [`Instruction::I64Extend8S`].
	pub(crate) fn i64_extend8_s(&mut self) -> &mut Self {
		self.sink.push(0xC2);
		self
	}

	/// Encode [`Instruction::I64Extend16S`].
	pub(crate) fn i64_extend16_s(&mut self) -> &mut Self {
		self.sink.push(0xC3);
		self
	}

	/// Encode [`Instruction::I64Extend32S`].
	pub(crate) fn i64_extend32_s(&mut self) -> &mut Self {
		self.sink.push(0xC4);
		self
	}

	/// Encode [`Instruction::I32TruncSatF32S`].
	pub(crate) fn i32_trunc_sat_f32_s(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x00);
		self
	}

	/// Encode [`Instruction::I32TruncSatF32U`].
	pub(crate) fn i32_trunc_sat_f32_u(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x01);
		self
	}

	/// Encode [`Instruction::I32TruncSatF64S`].
	pub(crate) fn i32_trunc_sat_f64_s(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x02);
		self
	}

	/// Encode [`Instruction::I32TruncSatF64U`].
	pub(crate) fn i32_trunc_sat_f64_u(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x03);
		self
	}

	/// Encode [`Instruction::I64TruncSatF32S`].
	pub(crate) fn i64_trunc_sat_f32_s(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x04);
		self
	}

	/// Encode [`Instruction::I64TruncSatF32U`].
	pub(crate) fn i64_trunc_sat_f32_u(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x05);
		self
	}

	/// Encode [`Instruction::I64TruncSatF64S`].
	pub(crate) fn i64_trunc_sat_f64_s(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x06);
		self
	}

	/// Encode [`Instruction::I64TruncSatF64U`].
	pub(crate) fn i64_trunc_sat_f64_u(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		self.sink.push(0x07);
		self
	}

	// Reference types instructions.

	/// Encode [`Instruction::TypedSelect`].
	pub(crate) fn typed_select(&mut self, ty: ValType) -> &mut Self {
		self.sink.push(0x1c);
		[ty].encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TypedSelect`] with multiple results (currently invalid).
	pub(crate) fn typed_select_multi(&mut self, tys: &[ValType]) -> &mut Self {
		self.sink.push(0x1c);
		tys.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefNull`].
	pub(crate) fn ref_null(&mut self, ty: HeapType) -> &mut Self {
		self.sink.push(0xd0);
		ty.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefIsNull`].
	pub(crate) fn ref_is_null(&mut self) -> &mut Self {
		self.sink.push(0xd1);
		self
	}

	/// Encode [`Instruction::RefFunc`].
	pub(crate) fn ref_func(&mut self, f: u32) -> &mut Self {
		self.sink.push(0xd2);
		f.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefEq`].
	pub(crate) fn ref_eq(&mut self) -> &mut Self {
		self.sink.push(0xd3);
		self
	}

	/// Encode [`Instruction::RefAsNonNull`].
	pub(crate) fn ref_as_non_null(&mut self) -> &mut Self {
		self.sink.push(0xd4);
		self
	}

	// GC types instructions.

	/// Encode [`Instruction::StructNew`].
	pub(crate) fn struct_new(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x00);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructNewDefault`].
	pub(crate) fn struct_new_default(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x01);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructGet`].
	pub(crate) fn struct_get(&mut self, struct_type_index: u32, field_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x02);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructGetS`].
	pub(crate) fn struct_get_s(&mut self, struct_type_index: u32, field_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x03);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructGetU`].
	pub(crate) fn struct_get_u(&mut self, struct_type_index: u32, field_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x04);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructSet`].
	pub(crate) fn struct_set(&mut self, struct_type_index: u32, field_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x05);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructNewDesc`].
	pub(crate) fn struct_new_desc(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x20);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructNewDefaultDesc`].
	pub(crate) fn struct_new_default_desc(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x21);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayNew`].
	pub(crate) fn array_new(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x06);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayNewDefault`].
	pub(crate) fn array_new_default(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x07);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayNewFixed`].
	pub(crate) fn array_new_fixed(&mut self, array_type_index: u32, array_size: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x08);
		array_type_index.encode(&mut self.sink);
		array_size.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayNewData`].
	pub(crate) fn array_new_data(&mut self, array_type_index: u32, array_data_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x09);
		array_type_index.encode(&mut self.sink);
		array_data_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayNewElem`].
	pub(crate) fn array_new_elem(&mut self, array_type_index: u32, array_elem_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x0a);
		array_type_index.encode(&mut self.sink);
		array_elem_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayGet`].
	pub(crate) fn array_get(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x0b);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayGetS`].
	pub(crate) fn array_get_s(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x0c);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayGetU`].
	pub(crate) fn array_get_u(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x0d);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArraySet`].
	pub(crate) fn array_set(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x0e);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayLen`].
	pub(crate) fn array_len(&mut self) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x0f);
		self
	}

	/// Encode [`Instruction::ArrayFill`].
	pub(crate) fn array_fill(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x10);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayCopy`].
	pub(crate) fn array_copy(
		&mut self,
		array_type_index_dst: u32,
		array_type_index_src: u32,
	) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x11);
		array_type_index_dst.encode(&mut self.sink);
		array_type_index_src.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayInitData`].
	pub(crate) fn array_init_data(&mut self, array_type_index: u32, array_data_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x12);
		array_type_index.encode(&mut self.sink);
		array_data_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayInitElem`].
	pub(crate) fn array_init_elem(&mut self, array_type_index: u32, array_elem_index: u32) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x13);
		array_type_index.encode(&mut self.sink);
		array_elem_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefTestNonNull`].
	pub(crate) fn ref_test_non_null(&mut self, heap_type: HeapType) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x14);
		heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefTestNullable`].
	pub(crate) fn ref_test_nullable(&mut self, heap_type: HeapType) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x15);
		heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefCastNonNull`].
	pub(crate) fn ref_cast_non_null(&mut self, heap_type: HeapType) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x16);
		heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefCastNullable`].
	pub(crate) fn ref_cast_nullable(&mut self, heap_type: HeapType) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x17);
		heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrOnCast`].
	pub(crate) fn br_on_cast(
		&mut self,
		relative_depth: u32,
		from_ref_type: RefType,
		to_ref_type: RefType,
	) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x18);
		let cast_flags = (from_ref_type.nullable as u8) | ((to_ref_type.nullable as u8) << 1);
		self.sink.push(cast_flags);
		relative_depth.encode(&mut self.sink);
		from_ref_type.heap_type.encode(&mut self.sink);
		to_ref_type.heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrOnCastFail`].
	pub(crate) fn br_on_cast_fail(
		&mut self,
		relative_depth: u32,
		from_ref_type: RefType,
		to_ref_type: RefType,
	) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x19);
		let cast_flags = (from_ref_type.nullable as u8) | ((to_ref_type.nullable as u8) << 1);
		self.sink.push(cast_flags);
		relative_depth.encode(&mut self.sink);
		from_ref_type.heap_type.encode(&mut self.sink);
		to_ref_type.heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::AnyConvertExtern`].
	pub(crate) fn any_convert_extern(&mut self) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x1a);
		self
	}

	/// Encode [`Instruction::ExternConvertAny`].
	pub(crate) fn extern_convert_any(&mut self) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x1b);
		self
	}

	/// Encode [`Instruction::RefI31`].
	pub(crate) fn ref_i31(&mut self) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x1c);
		self
	}

	/// Encode [`Instruction::I31GetS`].
	pub(crate) fn i31_get_s(&mut self) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x1d);
		self
	}

	/// Encode [`Instruction::I31GetU`].
	pub(crate) fn i31_get_u(&mut self) -> &mut Self {
		self.sink.push(0xfb);
		self.sink.push(0x1e);
		self
	}

	// Bulk memory instructions.

	/// Encode [`Instruction::TableInit`].
	pub(crate) fn table_init(&mut self, table: u32, elem_index: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x0c);
		elem_index.encode(&mut self.sink);
		table.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ElemDrop`].
	pub(crate) fn elem_drop(&mut self, segment: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x0d);
		segment.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableFill`].
	fn table_fill(&mut self, table: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x11);
		table.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableSet`].
	fn table_set(&mut self, table: u32) -> &mut Self {
		self.sink.push(0x26);
		table.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableGet`].
	fn table_get(&mut self, table: u32) -> &mut Self {
		self.sink.push(0x25);
		table.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableGrow`].
	fn table_grow(&mut self, table: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x0f);
		table.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableSize`].
	fn table_size(&mut self, table: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x10);
		table.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableCopy`].
	fn table_copy(&mut self, dst_table: u32, src_table: u32) -> &mut Self {
		self.sink.push(0xfc);
		self.sink.push(0x0e);
		dst_table.encode(&mut self.sink);
		src_table.encode(&mut self.sink);
		self
	}

	// SIMD instructions.

	/// Encode [`Instruction::V128Load`].
	pub(crate) fn v128_load(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x00u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load8x8S`].
	pub(crate) fn v128_load8x8_s(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x01u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load8x8U`].
	pub(crate) fn v128_load8x8_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x02u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load16x4S`].
	pub(crate) fn v128_load16x4_s(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x03u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load16x4U`].
	pub(crate) fn v128_load16x4_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x04u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load32x2S`].
	pub(crate) fn v128_load32x2_s(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x05u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load32x2U`].
	pub(crate) fn v128_load32x2_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x06u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load8Splat`].
	pub(crate) fn v128_load8_splat(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x07u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load16Splat`].
	pub(crate) fn v128_load16_splat(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x08u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load32Splat`].
	pub(crate) fn v128_load32_splat(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x09u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load64Splat`].
	pub(crate) fn v128_load64_splat(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x0Au32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load32Zero`].
	pub(crate) fn v128_load32_zero(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x5Cu32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load64Zero`].
	pub(crate) fn v128_load64_zero(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x5Du32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Store`].
	pub(crate) fn v128_store(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFD);
		0x0Bu32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Load8Lane`].
	pub(crate) fn v128_load8_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x54u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Load16Lane`].
	pub(crate) fn v128_load16_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x55u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Load32Lane`].
	pub(crate) fn v128_load32_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x56u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Load64Lane`].
	pub(crate) fn v128_load64_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x57u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Store8Lane`].
	pub(crate) fn v128_store8_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x58u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Store16Lane`].
	pub(crate) fn v128_store16_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x59u32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Store32Lane`].
	pub(crate) fn v128_store32_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x5Au32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Store64Lane`].
	pub(crate) fn v128_store64_lane(&mut self, memarg: MemArg, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x5Bu32.encode(&mut self.sink);
		memarg.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::V128Const`].
	pub(crate) fn v128_const(&mut self, x: i128) -> &mut Self {
		self.sink.push(0xFD);
		0x0Cu32.encode(&mut self.sink);
		self.sink.extend(x.to_le_bytes().iter().copied());
		self
	}

	/// Encode [`Instruction::I8x16Shuffle`].
	pub(crate) fn i8x16_shuffle(&mut self, lanes: [Lane; 16]) -> &mut Self {
		self.sink.push(0xFD);
		0x0Du32.encode(&mut self.sink);
		self.sink.extend(lanes.iter().copied());
		self
	}

	/// Encode [`Instruction::I8x16ExtractLaneS`].
	pub(crate) fn i8x16_extract_lane_s(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x15u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I8x16ExtractLaneU`].
	pub(crate) fn i8x16_extract_lane_u(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x16u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I8x16ReplaceLane`].
	pub(crate) fn i8x16_replace_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x17u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I16x8ExtractLaneS`].
	pub(crate) fn i16x8_extract_lane_s(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x18u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I16x8ExtractLaneU`].
	pub(crate) fn i16x8_extract_lane_u(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x19u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I16x8ReplaceLane`].
	pub(crate) fn i16x8_replace_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x1Au32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I32x4ExtractLane`].
	pub(crate) fn i32x4_extract_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x1Bu32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I32x4ReplaceLane`].
	pub(crate) fn i32x4_replace_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x1Cu32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I64x2ExtractLane`].
	pub(crate) fn i64x2_extract_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x1Du32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I64x2ReplaceLane`].
	pub(crate) fn i64x2_replace_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x1Eu32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::F32x4ExtractLane`].
	pub(crate) fn f32x4_extract_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x1Fu32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::F32x4ReplaceLane`].
	pub(crate) fn f32x4_replace_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x20u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::F64x2ExtractLane`].
	pub(crate) fn f64x2_extract_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x21u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::F64x2ReplaceLane`].
	pub(crate) fn f64x2_replace_lane(&mut self, lane: Lane) -> &mut Self {
		self.sink.push(0xFD);
		0x22u32.encode(&mut self.sink);
		self.sink.push(lane);
		self
	}

	/// Encode [`Instruction::I8x16Swizzle`].
	pub(crate) fn i8x16_swizzle(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x0Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Splat`].
	pub(crate) fn i8x16_splat(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x0Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Splat`].
	pub(crate) fn i16x8_splat(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Splat`].
	pub(crate) fn i32x4_splat(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x11u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Splat`].
	pub(crate) fn i64x2_splat(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x12u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Splat`].
	pub(crate) fn f32x4_splat(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x13u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Splat`].
	pub(crate) fn f64x2_splat(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x14u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Eq`].
	pub(crate) fn i8x16_eq(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x23u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Ne`].
	pub(crate) fn i8x16_ne(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x24u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16LtS`].
	pub(crate) fn i8x16_lt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x25u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16LtU`].
	pub(crate) fn i8x16_lt_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x26u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16GtS`].
	pub(crate) fn i8x16_gt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x27u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16GtU`].
	pub(crate) fn i8x16_gt_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x28u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16LeS`].
	pub(crate) fn i8x16_le_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x29u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16LeU`].
	pub(crate) fn i8x16_le_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x2Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16GeS`].
	pub(crate) fn i8x16_ge_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x2Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16GeU`].
	pub(crate) fn i8x16_ge_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x2Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Eq`].
	pub(crate) fn i16x8_eq(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x2Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Ne`].
	pub(crate) fn i16x8_ne(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x2Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8LtS`].
	pub(crate) fn i16x8_lt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x2Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8LtU`].
	pub(crate) fn i16x8_lt_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x30u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8GtS`].
	pub(crate) fn i16x8_gt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x31u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8GtU`].
	pub(crate) fn i16x8_gt_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x32u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8LeS`].
	pub(crate) fn i16x8_le_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x33u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8LeU`].
	pub(crate) fn i16x8_le_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x34u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8GeS`].
	pub(crate) fn i16x8_ge_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x35u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8GeU`].
	pub(crate) fn i16x8_ge_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x36u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Eq`].
	pub(crate) fn i32x4_eq(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x37u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Ne`].
	pub(crate) fn i32x4_ne(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x38u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4LtS`].
	pub(crate) fn i32x4_lt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x39u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4LtU`].
	pub(crate) fn i32x4_lt_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x3Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4GtS`].
	pub(crate) fn i32x4_gt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x3Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4GtU`].
	pub(crate) fn i32x4_gt_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x3Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4LeS`].
	pub(crate) fn i32x4_le_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x3Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4LeU`].
	pub(crate) fn i32x4_le_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x3Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4GeS`].
	pub(crate) fn i32x4_ge_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x3Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4GeU`].
	pub(crate) fn i32x4_ge_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x40u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Eq`].
	pub(crate) fn i64x2_eq(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xD6u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Ne`].
	pub(crate) fn i64x2_ne(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xD7u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2LtS`].
	pub(crate) fn i64x2_lt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xD8u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2GtS`].
	pub(crate) fn i64x2_gt_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xD9u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2LeS`].
	pub(crate) fn i64x2_le_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xDAu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2GeS`].
	pub(crate) fn i64x2_ge_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xDBu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Eq`].
	pub(crate) fn f32x4_eq(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x41u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Ne`].
	pub(crate) fn f32x4_ne(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x42u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Lt`].
	pub(crate) fn f32x4_lt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x43u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Gt`].
	pub(crate) fn f32x4_gt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x44u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Le`].
	pub(crate) fn f32x4_le(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x45u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Ge`].
	pub(crate) fn f32x4_ge(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x46u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Eq`].
	pub(crate) fn f64x2_eq(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x47u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Ne`].
	pub(crate) fn f64x2_ne(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x48u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Lt`].
	pub(crate) fn f64x2_lt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x49u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Gt`].
	pub(crate) fn f64x2_gt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x4Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Le`].
	pub(crate) fn f64x2_le(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x4Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Ge`].
	pub(crate) fn f64x2_ge(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x4Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Not`].
	pub(crate) fn v128_not(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x4Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128And`].
	pub(crate) fn v128_and(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x4Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128AndNot`].
	pub(crate) fn v128_andnot(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x4Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Or`].
	pub(crate) fn v128_or(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x50u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Xor`].
	pub(crate) fn v128_xor(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x51u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128Bitselect`].
	pub(crate) fn v128_bitselect(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x52u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::V128AnyTrue`].
	pub(crate) fn v128_any_true(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x53u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Abs`].
	pub(crate) fn i8x16_abs(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x60u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Neg`].
	pub(crate) fn i8x16_neg(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x61u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Popcnt`].
	pub(crate) fn i8x16_popcnt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x62u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16AllTrue`].
	pub(crate) fn i8x16_all_true(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x63u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Bitmask`].
	pub(crate) fn i8x16_bitmask(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x64u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16NarrowI16x8S`].
	pub(crate) fn i8x16_narrow_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x65u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16NarrowI16x8U`].
	pub(crate) fn i8x16_narrow_i16x8_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x66u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Shl`].
	pub(crate) fn i8x16_shl(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x6bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16ShrS`].
	pub(crate) fn i8x16_shr_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x6cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16ShrU`].
	pub(crate) fn i8x16_shr_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x6du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Add`].
	pub(crate) fn i8x16_add(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x6eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16AddSatS`].
	pub(crate) fn i8x16_add_sat_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x6fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16AddSatU`].
	pub(crate) fn i8x16_add_sat_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x70u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16Sub`].
	pub(crate) fn i8x16_sub(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x71u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16SubSatS`].
	pub(crate) fn i8x16_sub_sat_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x72u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16SubSatU`].
	pub(crate) fn i8x16_sub_sat_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x73u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16MinS`].
	pub(crate) fn i8x16_min_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x76u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16MinU`].
	pub(crate) fn i8x16_min_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x77u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16MaxS`].
	pub(crate) fn i8x16_max_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x78u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16MaxU`].
	pub(crate) fn i8x16_max_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x79u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16AvgrU`].
	pub(crate) fn i8x16_avgr_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x7Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtAddPairwiseI8x16S`].
	pub(crate) fn i16x8_extadd_pairwise_i8x16_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x7Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtAddPairwiseI8x16U`].
	pub(crate) fn i16x8_extadd_pairwise_i8x16_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x7Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Abs`].
	pub(crate) fn i16x8_abs(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x80u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Neg`].
	pub(crate) fn i16x8_neg(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x81u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Q15MulrSatS`].
	pub(crate) fn i16x8_q15mulr_sat_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x82u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8AllTrue`].
	pub(crate) fn i16x8_all_true(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x83u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Bitmask`].
	pub(crate) fn i16x8_bitmask(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x84u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8NarrowI32x4S`].
	pub(crate) fn i16x8_narrow_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x85u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8NarrowI32x4U`].
	pub(crate) fn i16x8_narrow_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x86u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtendLowI8x16S`].
	pub(crate) fn i16x8_extend_low_i8x16_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x87u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtendHighI8x16S`].
	pub(crate) fn i16x8_extend_high_i8x16_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x88u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtendLowI8x16U`].
	pub(crate) fn i16x8_extend_low_i8x16_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x89u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtendHighI8x16U`].
	pub(crate) fn i16x8_extend_high_i8x16_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x8Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Shl`].
	pub(crate) fn i16x8_shl(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x8Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ShrS`].
	pub(crate) fn i16x8_shr_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x8Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ShrU`].
	pub(crate) fn i16x8_shr_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x8Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Add`].
	pub(crate) fn i16x8_add(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x8Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8AddSatS`].
	pub(crate) fn i16x8_add_sat_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x8Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8AddSatU`].
	pub(crate) fn i16x8_add_sat_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x90u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Sub`].
	pub(crate) fn i16x8_sub(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x91u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8SubSatS`].
	pub(crate) fn i16x8_sub_sat_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x92u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8SubSatU`].
	pub(crate) fn i16x8_sub_sat_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x93u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8Mul`].
	pub(crate) fn i16x8_mul(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x95u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8MinS`].
	pub(crate) fn i16x8_min_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x96u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8MinU`].
	pub(crate) fn i16x8_min_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x97u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8MaxS`].
	pub(crate) fn i16x8_max_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x98u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8MaxU`].
	pub(crate) fn i16x8_max_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x99u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8AvgrU`].
	pub(crate) fn i16x8_avgr_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x9Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtMulLowI8x16S`].
	pub(crate) fn i16x8_extmul_low_i8x16_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x9Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtMulHighI8x16S`].
	pub(crate) fn i16x8_extmul_high_i8x16_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x9Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtMulLowI8x16U`].
	pub(crate) fn i16x8_extmul_low_i8x16_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x9Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8ExtMulHighI8x16U`].
	pub(crate) fn i16x8_extmul_high_i8x16_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x9Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtAddPairwiseI16x8S`].
	pub(crate) fn i32x4_extadd_pairwise_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x7Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtAddPairwiseI16x8U`].
	pub(crate) fn i32x4_extadd_pairwise_i16x8_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x7Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Abs`].
	pub(crate) fn i32x4_abs(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA0u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Neg`].
	pub(crate) fn i32x4_neg(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA1u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4AllTrue`].
	pub(crate) fn i32x4_all_true(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA3u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Bitmask`].
	pub(crate) fn i32x4_bitmask(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA4u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtendLowI16x8S`].
	pub(crate) fn i32x4_extend_low_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA7u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtendHighI16x8S`].
	pub(crate) fn i32x4_extend_high_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA8u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtendLowI16x8U`].
	pub(crate) fn i32x4_extend_low_i16x8_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xA9u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtendHighI16x8U`].
	pub(crate) fn i32x4_extend_high_i16x8_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xAAu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Shl`].
	pub(crate) fn i32x4_shl(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xABu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ShrS`].
	pub(crate) fn i32x4_shr_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xACu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ShrU`].
	pub(crate) fn i32x4_shr_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xADu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Add`].
	pub(crate) fn i32x4_add(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xAEu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Sub`].
	pub(crate) fn i32x4_sub(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xB1u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4Mul`].
	pub(crate) fn i32x4_mul(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xB5u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4MinS`].
	pub(crate) fn i32x4_min_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xB6u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4MinU`].
	pub(crate) fn i32x4_min_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xB7u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4MaxS`].
	pub(crate) fn i32x4_max_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xB8u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4MaxU`].
	pub(crate) fn i32x4_max_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xB9u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4DotI16x8S`].
	pub(crate) fn i32x4_dot_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xBAu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtMulLowI16x8S`].
	pub(crate) fn i32x4_extmul_low_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xBCu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtMulHighI16x8S`].
	pub(crate) fn i32x4_extmul_high_i16x8_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xBDu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtMulLowI16x8U`].
	pub(crate) fn i32x4_extmul_low_i16x8_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xBEu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4ExtMulHighI16x8U`].
	pub(crate) fn i32x4_extmul_high_i16x8_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xBFu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Abs`].
	pub(crate) fn i64x2_abs(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC0u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Neg`].
	pub(crate) fn i64x2_neg(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC1u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2AllTrue`].
	pub(crate) fn i64x2_all_true(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC3u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Bitmask`].
	pub(crate) fn i64x2_bitmask(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC4u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtendLowI32x4S`].
	pub(crate) fn i64x2_extend_low_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC7u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtendHighI32x4S`].
	pub(crate) fn i64x2_extend_high_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC8u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtendLowI32x4U`].
	pub(crate) fn i64x2_extend_low_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xC9u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtendHighI32x4U`].
	pub(crate) fn i64x2_extend_high_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xCAu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Shl`].
	pub(crate) fn i64x2_shl(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xCBu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ShrS`].
	pub(crate) fn i64x2_shr_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xCCu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ShrU`].
	pub(crate) fn i64x2_shr_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xCDu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Add`].
	pub(crate) fn i64x2_add(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xCEu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Sub`].
	pub(crate) fn i64x2_sub(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xD1u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2Mul`].
	pub(crate) fn i64x2_mul(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xD5u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtMulLowI32x4S`].
	pub(crate) fn i64x2_extmul_low_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xDCu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtMulHighI32x4S`].
	pub(crate) fn i64x2_extmul_high_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xDDu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtMulLowI32x4U`].
	pub(crate) fn i64x2_extmul_low_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xDEu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2ExtMulHighI32x4U`].
	pub(crate) fn i64x2_extmul_high_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xDFu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Ceil`].
	pub(crate) fn f32x4_ceil(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x67u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Floor`].
	pub(crate) fn f32x4_floor(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x68u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Trunc`].
	pub(crate) fn f32x4_trunc(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x69u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Nearest`].
	pub(crate) fn f32x4_nearest(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x6Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Abs`].
	pub(crate) fn f32x4_abs(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE0u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Neg`].
	pub(crate) fn f32x4_neg(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE1u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Sqrt`].
	pub(crate) fn f32x4_sqrt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE3u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Add`].
	pub(crate) fn f32x4_add(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE4u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Sub`].
	pub(crate) fn f32x4_sub(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE5u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Mul`].
	pub(crate) fn f32x4_mul(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE6u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Div`].
	pub(crate) fn f32x4_div(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE7u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Min`].
	pub(crate) fn f32x4_min(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE8u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4Max`].
	pub(crate) fn f32x4_max(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xE9u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4PMin`].
	pub(crate) fn f32x4_pmin(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xEAu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4PMax`].
	pub(crate) fn f32x4_pmax(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xEBu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Ceil`].
	pub(crate) fn f64x2_ceil(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x74u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Floor`].
	pub(crate) fn f64x2_floor(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x75u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Trunc`].
	pub(crate) fn f64x2_trunc(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x7Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Nearest`].
	pub(crate) fn f64x2_nearest(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x94u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Abs`].
	pub(crate) fn f64x2_abs(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xECu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Neg`].
	pub(crate) fn f64x2_neg(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xEDu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Sqrt`].
	pub(crate) fn f64x2_sqrt(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xEFu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Add`].
	pub(crate) fn f64x2_add(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF0u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Sub`].
	pub(crate) fn f64x2_sub(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF1u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Mul`].
	pub(crate) fn f64x2_mul(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF2u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Div`].
	pub(crate) fn f64x2_div(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF3u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Min`].
	pub(crate) fn f64x2_min(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF4u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2Max`].
	pub(crate) fn f64x2_max(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF5u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2PMin`].
	pub(crate) fn f64x2_pmin(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF6u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2PMax`].
	pub(crate) fn f64x2_pmax(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF7u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4TruncSatF32x4S`].
	pub(crate) fn i32x4_trunc_sat_f32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF8u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4TruncSatF32x4U`].
	pub(crate) fn i32x4_trunc_sat_f32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xF9u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4ConvertI32x4S`].
	pub(crate) fn f32x4_convert_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xFAu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4ConvertI32x4U`].
	pub(crate) fn f32x4_convert_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xFBu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4TruncSatF64x2SZero`].
	pub(crate) fn i32x4_trunc_sat_f64x2_s_zero(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xFCu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4TruncSatF64x2UZero`].
	pub(crate) fn i32x4_trunc_sat_f64x2_u_zero(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xFDu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2ConvertLowI32x4S`].
	pub(crate) fn f64x2_convert_low_i32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xFEu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2ConvertLowI32x4U`].
	pub(crate) fn f64x2_convert_low_i32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0xFFu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4DemoteF64x2Zero`].
	pub(crate) fn f32x4_demote_f64x2_zero(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x5Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2PromoteLowF32x4`].
	pub(crate) fn f64x2_promote_low_f32x4(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x5Fu32.encode(&mut self.sink);
		self
	}

	// Relaxed simd proposal

	/// Encode [`Instruction::I8x16RelaxedSwizzle`].
	pub(crate) fn i8x16_relaxed_swizzle(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x100u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4RelaxedTruncF32x4S`].
	pub(crate) fn i32x4_relaxed_trunc_f32x4_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x101u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4RelaxedTruncF32x4U`].
	pub(crate) fn i32x4_relaxed_trunc_f32x4_u(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x102u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4RelaxedTruncF64x2SZero`].
	pub(crate) fn i32x4_relaxed_trunc_f64x2_s_zero(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x103u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4RelaxedTruncF64x2UZero`].
	pub(crate) fn i32x4_relaxed_trunc_f64x2_u_zero(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x104u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4RelaxedMadd`].
	pub(crate) fn f32x4_relaxed_madd(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x105u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4RelaxedNmadd`].
	pub(crate) fn f32x4_relaxed_nmadd(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x106u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2RelaxedMadd`].
	pub(crate) fn f64x2_relaxed_madd(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x107u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2RelaxedNmadd`].
	pub(crate) fn f64x2_relaxed_nmadd(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x108u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I8x16RelaxedLaneselect`].
	pub(crate) fn i8x16_relaxed_laneselect(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x109u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8RelaxedLaneselect`].
	pub(crate) fn i16x8_relaxed_laneselect(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10Au32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4RelaxedLaneselect`].
	pub(crate) fn i32x4_relaxed_laneselect(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10Bu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64x2RelaxedLaneselect`].
	pub(crate) fn i64x2_relaxed_laneselect(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10Cu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4RelaxedMin`].
	pub(crate) fn f32x4_relaxed_min(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10Du32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F32x4RelaxedMax`].
	pub(crate) fn f32x4_relaxed_max(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10Eu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2RelaxedMin`].
	pub(crate) fn f64x2_relaxed_min(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x10Fu32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::F64x2RelaxedMax`].
	pub(crate) fn f64x2_relaxed_max(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x110u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8RelaxedQ15mulrS`].
	pub(crate) fn i16x8_relaxed_q15mulr_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x111u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I16x8RelaxedDotI8x16I7x16S`].
	pub(crate) fn i16x8_relaxed_dot_i8x16_i7x16_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x112u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32x4RelaxedDotI8x16I7x16AddS`].
	pub(crate) fn i32x4_relaxed_dot_i8x16_i7x16_add_s(&mut self) -> &mut Self {
		self.sink.push(0xFD);
		0x113u32.encode(&mut self.sink);
		self
	}

	// Atomic instructions (the threads proposal)

	/// Encode [`Instruction::MemoryAtomicNotify`].
	pub(crate) fn memory_atomic_notify(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x00);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryAtomicWait32`].
	pub(crate) fn memory_atomic_wait32(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x01);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::MemoryAtomicWait64`].
	pub(crate) fn memory_atomic_wait64(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x02);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::AtomicFence`].
	pub(crate) fn atomic_fence(&mut self) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x03);
		self.sink.push(0x00);
		self
	}

	/// Encode [`Instruction::I32AtomicLoad`].
	pub(crate) fn i32_atomic_load(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x10);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicLoad`].
	pub(crate) fn i64_atomic_load(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x11);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicLoad8U`].
	pub(crate) fn i32_atomic_load8_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x12);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicLoad16U`].
	pub(crate) fn i32_atomic_load16_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x13);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicLoad8U`].
	pub(crate) fn i64_atomic_load8_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x14);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicLoad16U`].
	pub(crate) fn i64_atomic_load16_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x15);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicLoad32U`].
	pub(crate) fn i64_atomic_load32_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x16);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicStore`].
	pub(crate) fn i32_atomic_store(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x17);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicStore`].
	pub(crate) fn i64_atomic_store(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x18);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicStore8`].
	pub(crate) fn i32_atomic_store8(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x19);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicStore16`].
	pub(crate) fn i32_atomic_store16(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x1A);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicStore8`].
	pub(crate) fn i64_atomic_store8(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x1B);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicStore16`].
	pub(crate) fn i64_atomic_store16(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x1C);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicStore32`].
	pub(crate) fn i64_atomic_store32(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x1D);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwAdd`].
	pub(crate) fn i32_atomic_rmw_add(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x1E);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwAdd`].
	pub(crate) fn i64_atomic_rmw_add(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x1F);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8AddU`].
	pub(crate) fn i32_atomic_rmw8_add_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x20);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16AddU`].
	pub(crate) fn i32_atomic_rmw16_add_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x21);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8AddU`].
	pub(crate) fn i64_atomic_rmw8_add_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x22);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16AddU`].
	pub(crate) fn i64_atomic_rmw16_add_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x23);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32AddU`].
	pub(crate) fn i64_atomic_rmw32_add_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x24);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwSub`].
	pub(crate) fn i32_atomic_rmw_sub(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x25);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwSub`].
	pub(crate) fn i64_atomic_rmw_sub(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x26);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8SubU`].
	pub(crate) fn i32_atomic_rmw8_sub_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x27);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16SubU`].
	pub(crate) fn i32_atomic_rmw16_sub_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x28);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8SubU`].
	pub(crate) fn i64_atomic_rmw8_sub_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x29);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16SubU`].
	pub(crate) fn i64_atomic_rmw16_sub_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x2A);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32SubU`].
	pub(crate) fn i64_atomic_rmw32_sub_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x2B);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwAnd`].
	pub(crate) fn i32_atomic_rmw_and(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x2C);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwAnd`].
	pub(crate) fn i64_atomic_rmw_and(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x2D);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8AndU`].
	pub(crate) fn i32_atomic_rmw8_and_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x2E);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16AndU`].
	pub(crate) fn i32_atomic_rmw16_and_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x2F);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8AndU`].
	pub(crate) fn i64_atomic_rmw8_and_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x30);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16AndU`].
	pub(crate) fn i64_atomic_rmw16_and_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x31);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32AndU`].
	pub(crate) fn i64_atomic_rmw32_and_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x32);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwOr`].
	pub(crate) fn i32_atomic_rmw_or(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x33);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwOr`].
	pub(crate) fn i64_atomic_rmw_or(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x34);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8OrU`].
	pub(crate) fn i32_atomic_rmw8_or_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x35);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16OrU`].
	pub(crate) fn i32_atomic_rmw16_or_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x36);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8OrU`].
	pub(crate) fn i64_atomic_rmw8_or_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x37);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16OrU`].
	pub(crate) fn i64_atomic_rmw16_or_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x38);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32OrU`].
	pub(crate) fn i64_atomic_rmw32_or_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x39);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwXor`].
	pub(crate) fn i32_atomic_rmw_xor(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x3A);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwXor`].
	pub(crate) fn i64_atomic_rmw_xor(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x3B);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8XorU`].
	pub(crate) fn i32_atomic_rmw8_xor_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x3C);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16XorU`].
	pub(crate) fn i32_atomic_rmw16_xor_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x3D);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8XorU`].
	pub(crate) fn i64_atomic_rmw8_xor_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x3E);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16XorU`].
	pub(crate) fn i64_atomic_rmw16_xor_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x3F);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32XorU`].
	pub(crate) fn i64_atomic_rmw32_xor_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x40);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwXchg`].
	pub(crate) fn i32_atomic_rmw_xchg(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x41);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwXchg`].
	pub(crate) fn i64_atomic_rmw_xchg(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x42);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8XchgU`].
	pub(crate) fn i32_atomic_rmw8_xchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x43);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16XchgU`].
	pub(crate) fn i32_atomic_rmw16_xchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x44);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8XchgU`].
	pub(crate) fn i64_atomic_rmw8_xchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x45);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16XchgU`].
	pub(crate) fn i64_atomic_rmw16_xchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x46);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32XchgU`].
	pub(crate) fn i64_atomic_rmw32_xchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x47);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmwCmpxchg`].
	pub(crate) fn i32_atomic_rmw_cmpxchg(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x48);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmwCmpxchg`].
	pub(crate) fn i64_atomic_rmw_cmpxchg(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x49);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw8CmpxchgU`].
	pub(crate) fn i32_atomic_rmw8_cmpxchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x4A);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I32AtomicRmw16CmpxchgU`].
	pub(crate) fn i32_atomic_rmw16_cmpxchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x4B);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw8CmpxchgU`].
	pub(crate) fn i64_atomic_rmw8_cmpxchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x4C);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw16CmpxchgU`].
	pub(crate) fn i64_atomic_rmw16_cmpxchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x4D);
		memarg.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64AtomicRmw32CmpxchgU`].
	pub(crate) fn i64_atomic_rmw32_cmpxchg_u(&mut self, memarg: MemArg) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x4E);
		memarg.encode(&mut self.sink);
		self
	}

	// More atomic instructions (the shared-everything-threads proposal)

	/// Encode [`Instruction::GlobalAtomicGet`].
	pub(crate) fn global_atomic_get(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x4F);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicSet`].
	pub(crate) fn global_atomic_set(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x50);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwAdd`].
	pub(crate) fn global_atomic_rmw_add(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x51);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwSub`].
	pub(crate) fn global_atomic_rmw_sub(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x52);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwAnd`].
	pub(crate) fn global_atomic_rmw_and(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x53);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwOr`].
	pub(crate) fn global_atomic_rmw_or(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x54);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwXor`].
	pub(crate) fn global_atomic_rmw_xor(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x55);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwXchg`].
	pub(crate) fn global_atomic_rmw_xchg(&mut self, ordering: Ordering, global_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x56);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::GlobalAtomicRmwCmpxchg`].
	pub(crate) fn global_atomic_rmw_cmpxchg(
		&mut self,
		ordering: Ordering,
		global_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x57);
		ordering.encode(&mut self.sink);
		global_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableAtomicGet`].
	pub(crate) fn table_atomic_get(&mut self, ordering: Ordering, table_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x58);
		ordering.encode(&mut self.sink);
		table_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableAtomicSet`].
	pub(crate) fn table_atomic_set(&mut self, ordering: Ordering, table_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x59);
		ordering.encode(&mut self.sink);
		table_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableAtomicRmwXchg`].
	pub(crate) fn table_atomic_rmw_xchg(&mut self, ordering: Ordering, table_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x5A);
		ordering.encode(&mut self.sink);
		table_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::TableAtomicRmwCmpxchg`].
	pub(crate) fn table_atomic_rmw_cmpxchg(&mut self, ordering: Ordering, table_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x5B);
		ordering.encode(&mut self.sink);
		table_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicGet`].
	pub(crate) fn struct_atomic_get(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x5C);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicGetS`].
	pub(crate) fn struct_atomic_get_s(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x5D);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicGetU`].
	pub(crate) fn struct_atomic_get_u(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x5E);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicSet`].
	pub(crate) fn struct_atomic_set(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x5F);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwAdd`].
	pub(crate) fn struct_atomic_rmw_add(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x60);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwSub`].
	pub(crate) fn struct_atomic_rmw_sub(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x61);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwAnd`].
	pub(crate) fn struct_atomic_rmw_and(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x62);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwOr`].
	pub(crate) fn struct_atomic_rmw_or(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x63);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwXor`].
	pub(crate) fn struct_atomic_rmw_xor(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x64);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwXchg`].
	pub(crate) fn struct_atomic_rmw_xchg(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x65);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::StructAtomicRmwCmpxchg`].
	pub(crate) fn struct_atomic_rmw_cmpxchg(
		&mut self,
		ordering: Ordering,
		struct_type_index: u32,
		field_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x66);
		ordering.encode(&mut self.sink);
		struct_type_index.encode(&mut self.sink);
		field_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicGet`].
	pub(crate) fn array_atomic_get(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x67);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicGetS`].
	pub(crate) fn array_atomic_get_s(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x68);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicGetU`].
	pub(crate) fn array_atomic_get_u(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x69);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicSet`].
	pub(crate) fn array_atomic_set(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x6A);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwAdd`].
	pub(crate) fn array_atomic_rmw_add(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x6B);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwSub`].
	pub(crate) fn array_atomic_rmw_sub(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x6C);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwAnd`].
	pub(crate) fn array_atomic_rmw_and(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x6D);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwOr`].
	pub(crate) fn array_atomic_rmw_or(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x6E);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwXor`].
	pub(crate) fn array_atomic_rmw_xor(&mut self, ordering: Ordering, array_type_index: u32) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x6F);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwXchg`].
	pub(crate) fn array_atomic_rmw_xchg(
		&mut self,
		ordering: Ordering,
		array_type_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x70);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ArrayAtomicRmwCmpxchg`].
	pub(crate) fn array_atomic_rmw_cmpxchg(
		&mut self,
		ordering: Ordering,
		array_type_index: u32,
	) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x71);
		ordering.encode(&mut self.sink);
		array_type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefI31Shared`].
	pub(crate) fn ref_i31_shared(&mut self) -> &mut Self {
		self.sink.push(0xFE);
		self.sink.push(0x72);
		self
	}

	// Stack switching

	/// Encode [`Instruction::ContNew`].
	pub(crate) fn cont_new(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xE0);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::ContBind`].
	pub(crate) fn cont_bind(&mut self, argument_index: u32, result_index: u32) -> &mut Self {
		self.sink.push(0xE1);
		argument_index.encode(&mut self.sink);
		result_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Suspend`].
	pub(crate) fn suspend(&mut self, tag_index: u32) -> &mut Self {
		self.sink.push(0xE2);
		tag_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::Resume`].
	pub(crate) fn resume<V: IntoIterator<Item = Handle>>(
		&mut self,
		cont_type_index: u32,
		resume_table: V,
	) -> &mut Self
	where
		V::IntoIter: ExactSizeIterator,
	{
		self.sink.push(0xE3);
		cont_type_index.encode(&mut self.sink);
		encode_vec(resume_table, &mut self.sink);
		self
	}

	/// Encode [`Instruction::ResumeThrow`].
	pub(crate) fn resume_throw<V: IntoIterator<Item = Handle>>(
		&mut self,
		cont_type_index: u32,
		tag_index: u32,
		resume_table: V,
	) -> &mut Self
	where
		V::IntoIter: ExactSizeIterator,
	{
		self.sink.push(0xE4);
		cont_type_index.encode(&mut self.sink);
		tag_index.encode(&mut self.sink);
		encode_vec(resume_table, &mut self.sink);
		self
	}

	/// Encode [`Instruction::ResumeThrowRef`].
	pub(crate) fn resume_throw_ref<V: IntoIterator<Item = Handle>>(
		&mut self,
		cont_type_index: u32,
		resume_table: V,
	) -> &mut Self
	where
		V::IntoIter: ExactSizeIterator,
	{
		self.sink.push(0xE5);
		cont_type_index.encode(&mut self.sink);
		encode_vec(resume_table, &mut self.sink);
		self
	}

	/// Encode [`Instruction::Switch`].
	pub(crate) fn switch(&mut self, cont_type_index: u32, tag_index: u32) -> &mut Self {
		self.sink.push(0xE6);
		cont_type_index.encode(&mut self.sink);
		tag_index.encode(&mut self.sink);
		self
	}

	// Wide Arithmetic

	/// Encode [`Instruction::I64Add128`].
	pub(crate) fn i64_add128(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		19u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64Sub128`].
	pub(crate) fn i64_sub128(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		20u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64MulWideS`].
	pub(crate) fn i64_mul_wide_s(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		21u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::I64MulWideU`].
	pub(crate) fn i64_mul_wide_u(&mut self) -> &mut Self {
		self.sink.push(0xFC);
		22u32.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefGetDesc`].
	pub(crate) fn ref_get_desc(&mut self, type_index: u32) -> &mut Self {
		self.sink.push(0xFB);
		34u32.encode(&mut self.sink);
		type_index.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefCastDescEqNonNull`].
	pub(crate) fn ref_cast_desc_eq_non_null(&mut self, ht: HeapType) -> &mut Self {
		self.sink.push(0xFB);
		35u32.encode(&mut self.sink);
		ht.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::RefCastDescEqNullable`].
	pub(crate) fn ref_cast_desc_eq_nullable(&mut self, ht: HeapType) -> &mut Self {
		self.sink.push(0xFB);
		36u32.encode(&mut self.sink);
		ht.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrOnCastDescEq`].
	pub(crate) fn br_on_cast_desc_eq(
		&mut self,
		relative_depth: u32,
		from_ref_type: RefType,
		to_ref_type: RefType,
	) -> &mut Self {
		self.sink.push(0xFB);
		37u32.encode(&mut self.sink);
		let cast_flags = (from_ref_type.nullable as u8) | ((to_ref_type.nullable as u8) << 1);
		self.sink.push(cast_flags);
		relative_depth.encode(&mut self.sink);
		from_ref_type.heap_type.encode(&mut self.sink);
		to_ref_type.heap_type.encode(&mut self.sink);
		self
	}

	/// Encode [`Instruction::BrOnCastDescEqFail`].
	pub(crate) fn br_on_cast_desc_eq_fail(
		&mut self,
		relative_depth: u32,
		from_ref_type: RefType,
		to_ref_type: RefType,
	) -> &mut Self {
		self.sink.push(0xFB);
		38u32.encode(&mut self.sink);
		let cast_flags = (from_ref_type.nullable as u8) | ((to_ref_type.nullable as u8) << 1);
		self.sink.push(cast_flags);
		relative_depth.encode(&mut self.sink);
		from_ref_type.heap_type.encode(&mut self.sink);
		to_ref_type.heap_type.encode(&mut self.sink);
		self
	}
}

fn encode_vec<T, V>(elements: V, sink: &mut Vec<u8>)
where
	T: Encode,
	V: IntoIterator<Item = T>,
	V::IntoIter: ExactSizeIterator,
{
	let elements = elements.into_iter();
	u32::try_from(elements.len()).unwrap().encode(sink);
	for x in elements {
		x.encode(sink);
	}
}
