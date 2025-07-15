//! Value representation and ABI for native code
//!
//! This module defines how FLC values are represented in native code.
//! We use a tagged pointer scheme similar to the JIT compiler for efficiency.

use cranelift::prelude::*;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_module::{Module, Linkage};

/// Tagged value representation matching the JIT ABI
///
/// We use a 64-bit tagged pointer scheme:
/// - Bits 0-2: Type tag (3 bits)
/// - Bits 3-63: Payload (61 bits)
///
/// Tags:
/// - 0b000 (0): Integer (61-bit signed integer)
/// - 0b001 (1): Float (pointer to heap-allocated f64)
/// - 0b010 (2): Boolean (0 = false, 1 = true in payload)
/// - 0b011 (3): Nil (payload ignored)
/// - 0b100 (4): String (pointer to string object)
/// - 0b101 (5): List (pointer to list object)
/// - 0b110 (6): Map (pointer to map object)
/// - 0b111 (7): Object (pointer to general object)
pub struct ValueAbi;

impl ValueAbi {
    /// Tag values
    pub const TAG_INT: u64 = 0;
    pub const TAG_FLOAT: u64 = 1;
    pub const TAG_BOOL: u64 = 2;
    pub const TAG_NIL: u64 = 3;
    pub const TAG_STRING: u64 = 4;
    pub const TAG_LIST: u64 = 5;
    pub const TAG_MAP: u64 = 6;
    pub const TAG_OBJECT: u64 = 7;
    
    /// Tag mask and shift
    pub const TAG_MASK: u64 = 0b111;
    pub const TAG_SHIFT: u64 = 3;
    
    /// Create a tagged integer value
    pub fn make_int(ctx: &mut FunctionBuilder, value: Value) -> Value {
        // Shift left by 3 bits, tag is already 0
        let shifted = ctx.ins().ishl_imm(value, Self::TAG_SHIFT as i64);
        shifted
    }
    
    /// Create a tagged boolean value
    pub fn make_bool(ctx: &mut FunctionBuilder, value: Value) -> Value {
        // Convert bool to int, shift left, and add tag
        // In Cranelift, booleans are already represented as i8
        let extended = ctx.ins().uextend(types::I64, value);
        let shifted = ctx.ins().ishl_imm(extended, Self::TAG_SHIFT as i64);
        ctx.ins().iadd_imm(shifted, Self::TAG_BOOL as i64)
    }
    
    /// Create a tagged nil value
    pub fn make_nil(ctx: &mut FunctionBuilder) -> Value {
        ctx.ins().iconst(types::I64, Self::TAG_NIL as i64)
    }
    
    /// Create a tagged pointer value
    pub fn make_pointer(ctx: &mut FunctionBuilder, ptr: Value, tag: u64) -> Value {
        // Pointers are already aligned, so we can just OR the tag
        ctx.ins().bor_imm(ptr, tag as i64)
    }
    
    /// Extract the tag from a value
    pub fn get_tag(ctx: &mut FunctionBuilder, value: Value) -> Value {
        ctx.ins().band_imm(value, Self::TAG_MASK as i64)
    }
    
    /// Extract the payload from a value
    pub fn get_payload(ctx: &mut FunctionBuilder, value: Value) -> Value {
        ctx.ins().ushr_imm(value, Self::TAG_SHIFT as i64)
    }
    
    /// Extract a pointer from a tagged value (removes tag)
    pub fn get_pointer(ctx: &mut FunctionBuilder, value: Value) -> Value {
        ctx.ins().band_imm(value, !(Self::TAG_MASK as i64))
    }
    
    /// Generate type checking code
    pub fn check_tag(
        ctx: &mut FunctionBuilder,
        value: Value,
        expected_tag: u64,
    ) -> Value {
        let tag = Self::get_tag(ctx, value);
        let expected = ctx.ins().iconst(types::I64, expected_tag as i64);
        ctx.ins().icmp(IntCC::Equal, tag, expected)
    }
}

/// Runtime support functions that need to be linked
pub struct RuntimeFunctions;

impl RuntimeFunctions {
    /// Get the list of runtime functions that need to be available
    pub fn required_functions() -> Vec<(&'static str, &'static str)> {
        vec![
            // Memory management
            ("flc_alloc", "fn(size: i64) -> i64"),
            ("flc_free", "fn(ptr: i64)"),
            ("flc_gc_collect", "fn()"),
            
            // String operations
            ("flc_string_new", "fn(data: i64, len: i64) -> i64"),
            ("flc_string_concat", "fn(a: i64, b: i64) -> i64"),
            ("flc_string_len", "fn(s: i64) -> i64"),
            
            // List operations
            ("flc_list_new", "fn(capacity: i64) -> i64"),
            ("flc_list_push", "fn(list: i64, value: i64)"),
            ("flc_list_get", "fn(list: i64, index: i64) -> i64"),
            ("flc_list_len", "fn(list: i64) -> i64"),
            
            // Map operations
            ("flc_map_new", "fn() -> i64"),
            ("flc_map_insert", "fn(map: i64, key: i64, value: i64)"),
            ("flc_map_get", "fn(map: i64, key: i64) -> i64"),
            
            // Error handling
            ("flc_panic", "fn(msg: i64) -> !"),
            ("flc_type_error", "fn(expected: i64, actual: i64) -> !"),
        ]
    }
    
    /// Declare runtime functions in a Cranelift module
    pub fn declare_in_module<M: Module>(module: &mut M) -> anyhow::Result<()> {
        let pointer_type = module.target_config().pointer_type();
        
        // Declare each runtime function
        for (name, _sig) in Self::required_functions() {
            let mut sig = module.make_signature();
            
            // For now, all functions use pointer-sized integers
            // Real implementation would parse the signature string
            match name {
                "flc_alloc" => {
                    sig.params.push(AbiParam::new(pointer_type));
                    sig.returns.push(AbiParam::new(pointer_type));
                }
                "flc_free" | "flc_gc_collect" => {
                    sig.params.push(AbiParam::new(pointer_type));
                }
                "flc_string_new" | "flc_string_concat" => {
                    sig.params.push(AbiParam::new(pointer_type));
                    sig.params.push(AbiParam::new(pointer_type));
                    sig.returns.push(AbiParam::new(pointer_type));
                }
                _ => {
                    // Default: one parameter, one return
                    sig.params.push(AbiParam::new(pointer_type));
                    sig.returns.push(AbiParam::new(pointer_type));
                }
            }
            
            module.declare_function(name, cranelift_module::Linkage::Import, &sig)?;
        }
        
        Ok(())
    }
}