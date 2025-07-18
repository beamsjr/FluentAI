//! Function registry for JIT compilation
//!
//! This module manages the mapping between VM functions and JIT-compiled functions,
//! allowing efficient function calls within JIT code.

use cranelift::prelude::*;
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use fluentai_core::value::Value;
use std::collections::HashMap;
use anyhow::Result;

/// Information about a JIT-compiled function
#[derive(Clone)]
pub struct JitFunctionInfo {
    /// Cranelift function ID
    pub func_id: FuncId,
    /// Function signature
    pub signature: Signature,
    /// Number of parameters
    pub arity: usize,
    /// VM chunk ID (if this is a VM function)
    pub chunk_id: Option<usize>,
}

/// Registry of functions available to JIT code
pub struct FunctionRegistry {
    /// Map from VM chunk ID to JIT function info
    chunk_to_jit: HashMap<usize, JitFunctionInfo>,
    /// Map from native function name to JIT function info
    native_functions: HashMap<String, JitFunctionInfo>,
    /// Runtime function table for indirect calls
    function_table: Vec<*const u8>,
    /// Map from chunk ID to function table index
    chunk_to_index: HashMap<usize, usize>,
    /// Map from native function name to function table index
    native_to_index: HashMap<String, usize>,
}

impl FunctionRegistry {
    /// Create a new function registry
    pub fn new() -> Self {
        Self {
            chunk_to_jit: HashMap::new(),
            native_functions: HashMap::new(),
            function_table: Vec::new(),
            chunk_to_index: HashMap::new(),
            native_to_index: HashMap::new(),
        }
    }
    
    /// Register a JIT-compiled VM function
    pub fn register_vm_function(&mut self, chunk_id: usize, info: JitFunctionInfo, code_ptr: *const u8) {
        let index = self.function_table.len();
        self.chunk_to_jit.insert(chunk_id, info);
        self.chunk_to_index.insert(chunk_id, index);
        self.function_table.push(code_ptr);
    }
    
    /// Register a native function wrapper
    pub fn register_native_function(&mut self, name: String, info: JitFunctionInfo, code_ptr: *const u8) {
        let index = self.function_table.len();
        self.native_functions.insert(name.clone(), info);
        self.native_to_index.insert(name, index);
        self.function_table.push(code_ptr);
    }
    
    /// Get JIT function info for a VM chunk
    pub fn get_vm_function(&self, chunk_id: usize) -> Option<&JitFunctionInfo> {
        self.chunk_to_jit.get(&chunk_id)
    }
    
    /// Get JIT function info for a native function
    pub fn get_native_function(&self, name: &str) -> Option<&JitFunctionInfo> {
        self.native_functions.get(name)
    }
    
    /// Get function table index for a value (returns None if not JIT-compiled)
    pub fn get_function_index(&self, value: &Value) -> Option<usize> {
        match value {
            Value::Function { chunk_id, .. } => {
                self.chunk_to_index.get(chunk_id).copied()
            }
            Value::NativeFunction { name, .. } => {
                self.native_to_index.get(name).copied()
            }
            _ => None,
        }
    }
    
    /// Get function pointer from table by index
    pub fn get_function_pointer(&self, index: usize) -> Option<*const u8> {
        self.function_table.get(index).copied()
    }
    
    /// Get the function table for runtime use
    pub fn function_table(&self) -> &[*const u8] {
        &self.function_table
    }
}

/// Helper to create a function signature for a given arity
pub fn make_function_signature(isa: &dyn TargetIsa, arity: usize) -> Signature {
    let mut sig = Signature::new(isa.default_call_conv());
    
    // All parameters are tagged i64 values
    for _ in 0..arity {
        sig.params.push(AbiParam::new(types::I64));
    }
    
    // Return value is also a tagged i64
    sig.returns.push(AbiParam::new(types::I64));
    
    sig
}

/// Create a trampoline function for calling VM functions from JIT code
pub fn create_vm_call_trampoline(
    module: &mut dyn Module,
    isa: &dyn TargetIsa,
) -> Result<FuncId> {
    let mut sig = Signature::new(isa.default_call_conv());
    
    // Parameters:
    // - Function pointer (i64)
    // - Argument count (i64)
    // - Arguments array pointer (i64)
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    
    // Return value
    sig.returns.push(AbiParam::new(types::I64));
    
    let func_id = module.declare_function("vm_call_trampoline", cranelift_module::Linkage::Local, &sig)?;
    
    Ok(func_id)
}