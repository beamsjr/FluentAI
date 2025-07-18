//! Cranelift-based JIT compiler for FluentAi
//!
//! This module provides Just-In-Time compilation of FluentAi bytecode
//! to native machine code using Cranelift, achieving near-native performance.
//! It uses a tagged-pointer ABI to handle different value types.

#![warn(missing_docs)]

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_native;

use anyhow::{anyhow, Result};
use fluentai_bytecode::Bytecode;
use fluentai_core::value::Value;
use std::collections::HashMap;
use std::mem;

pub mod codegen;
pub mod value;
pub mod function_registry;
pub mod runtime;
pub mod runtime_extended;
pub mod symbol_table;

use value::TaggedValue;
use function_registry::FunctionRegistry;

// The ABI is now defined in the value module using 3-bit tags

/// JIT compilation statistics
#[derive(Debug, Default)]
pub struct JitStats {
    /// Number of functions that have been compiled
    pub functions_compiled: usize,
    /// Total number of bytecode instructions compiled
    pub total_instructions: usize,
    /// Time spent in optimization passes (milliseconds)
    pub optimization_time_ms: f64,
    /// Time spent in code generation (milliseconds)
    pub codegen_time_ms: f64,
}

/// A handle to a compiled function, containing its signature and memory location.
pub struct CompiledFunction {
    /// The Cranelift function signature
    pub signature: Signature,
    /// Raw pointer to the compiled machine code
    pub code_ptr: *const u8,
}

/// Main JIT compiler interface
pub struct JitCompiler {
    /// The JIT module which manages code generation and linking.
    module: JITModule,

    /// Caches compiled functions to avoid redundant work.
    function_cache: HashMap<usize, CompiledFunction>,
    
    /// Function registry for managing function calls
    function_registry: FunctionRegistry,

    /// Compilation statistics.
    stats: JitStats,
}

impl JitCompiler {
    /// Creates a new JIT compiler.
    pub fn new() -> Result<Self> {
        // Create builder with architecture-specific settings
        let mut builder = if cfg!(target_arch = "aarch64") || cfg!(target_arch = "arm") {
            // For ARM architectures, disable PIC to work around PLT limitations
            let mut flag_builder = settings::builder();
            flag_builder.set("is_pic", "false")
                .map_err(|e| anyhow!("Failed to set is_pic flag: {}", e))?;
            
            let isa_builder = cranelift_native::builder()
                .map_err(|e| anyhow!("Failed to create ISA builder: {}", e))?
                .finish(settings::Flags::new(flag_builder))?;
            
            JITBuilder::with_isa(isa_builder, cranelift_module::default_libcall_names())
        } else {
            // For other architectures (x86_64), use default settings
            JITBuilder::new(cranelift_module::default_libcall_names())?
        };
        
        // Register runtime functions
        Self::register_runtime_functions(&mut builder);
        
        let module = JITModule::new(builder);

        Ok(Self {
            module,
            function_cache: HashMap::new(),
            function_registry: FunctionRegistry::new(),
            stats: JitStats::default(),
        })
    }

    /// Compiles a bytecode chunk into a native function.
    ///
    /// Returns a `CompiledFunction` handle, which includes the function's signature
    /// and a raw pointer to the executable code.
    pub fn compile(&mut self, bytecode: &Bytecode, chunk_id: usize) -> Result<&CompiledFunction> {
        // Return the cached function if it's already compiled.
        if self.function_cache.contains_key(&chunk_id) {
            return Ok(&self.function_cache[&chunk_id]);
        }

        let start_time = std::time::Instant::now();

        let chunk = bytecode
            .chunks
            .get(chunk_id)
            .ok_or_else(|| anyhow!("Invalid chunk ID: {}", chunk_id))?;

        // Create a new context for this function.
        let mut ctx = self.module.make_context();

        // Define the function signature. For now, all functions are `() -> i64`.
        // The returned i64 is a tagged value according to our ABI.
        ctx.func.signature.returns.push(AbiParam::new(types::I64));

        // Build the function IR using the codegen module.
        let mut func_ctx = FunctionBuilderContext::new();
        codegen::build_function(&mut ctx.func, &mut func_ctx, chunk, &mut self.module)?;
        self.stats.codegen_time_ms += start_time.elapsed().as_secs_f64() * 1000.0;

        // Define the function within the JIT module.
        let func_name = format!("fluentai_func_{}", chunk_id);
        let func_id = self
            .module
            .declare_function(&func_name, Linkage::Local, &ctx.func.signature)?;

        // Define the function - cranelift-jit handles redefinition internally
        self.module.define_function(func_id, &mut ctx)?;

        // Finalize definitions, which triggers optimization and code emission.
        let opt_start = std::time::Instant::now();
        self.module.finalize_definitions()?;
        self.stats.optimization_time_ms += opt_start.elapsed().as_secs_f64() * 1000.0;

        // Get the raw pointer to the compiled machine code.
        let code_ptr = self.module.get_finalized_function(func_id);

        // Cache the compiled function's signature and pointer.
        let compiled_func = CompiledFunction {
            signature: ctx.func.signature.clone(),
            code_ptr,
        };
        self.function_cache.insert(chunk_id, compiled_func);
        self.stats.functions_compiled += 1;
        self.stats.total_instructions += chunk.instructions.len();

        Ok(&self.function_cache[&chunk_id])
    }

    /// Executes a compiled function and decodes the result.
    ///
    /// # Safety
    /// The caller must ensure that the signature used to transmute the function pointer
    /// matches the `compiled_func.signature`.
    pub fn execute(&self, compiled_func: &CompiledFunction) -> Result<Value> {
        // Transmute the raw pointer to a safe, callable function pointer.
        // This is the primary unsafe block, relying on the correctness of the signature.
        let func_ptr: fn() -> i64 = unsafe { mem::transmute(compiled_func.code_ptr) };

        // Execute the native code.
        let result = func_ptr();

        // Decode the tagged result according to our ABI.
        self.decode_result(result)
    }

    /// Decodes an i64 result from the JIT into a VM Value based on the ABI.
    fn decode_result(&self, result: i64) -> Result<Value> {
        let tagged = TaggedValue(result as u64);
        Ok(tagged.to_value())
    }

    /// Compiles and executes the main bytecode chunk in one step.
    pub fn compile_and_run(&mut self, bytecode: &Bytecode) -> Result<Value> {
        // First compile the function
        self.compile(bytecode, bytecode.main_chunk)?;
        
        // Then get it from the cache and execute it
        let func = &self.function_cache[&bytecode.main_chunk];
        let func_ptr: fn() -> i64 = unsafe { mem::transmute(func.code_ptr) };
        
        // Execute the JIT'd function
        let result = func_ptr();
        
        // Decode the tagged result
        self.decode_result(result)
    }

    /// Returns statistics about the JIT compilation process.
    pub fn stats(&self) -> &JitStats {
        &self.stats
    }

    /// Clears the function cache, forcing recompilation on the next call.
    pub fn clear_cache(&mut self) {
        self.function_cache.clear();
    }
    
    /// Register all runtime functions with the JIT builder
    fn register_runtime_functions(builder: &mut JITBuilder) {
        // Core runtime functions
        builder.symbol("jit_runtime_call", runtime::jit_runtime_call as *const u8);
        builder.symbol("jit_runtime_add_checked", runtime::jit_runtime_add_checked as *const u8);
        
        // Type-checked arithmetic operations
        builder.symbol("jit_runtime_add_typed", runtime::jit_runtime_add_typed as *const u8);
        builder.symbol("jit_runtime_sub_typed", runtime::jit_runtime_sub_typed as *const u8);
        builder.symbol("jit_runtime_mul_typed", runtime::jit_runtime_mul_typed as *const u8);
        builder.symbol("jit_runtime_div_typed", runtime::jit_runtime_div_typed as *const u8);
        builder.symbol("jit_runtime_mod_typed", runtime::jit_runtime_mod_typed as *const u8);
        builder.symbol("jit_runtime_neg_typed", runtime::jit_runtime_neg_typed as *const u8);
        
        builder.symbol("jit_runtime_string_concat", runtime::jit_runtime_string_concat as *const u8);
        builder.symbol("jit_runtime_string_len", runtime::jit_runtime_string_len as *const u8);
        builder.symbol("jit_runtime_string_upper", runtime::jit_runtime_string_upper as *const u8);
        builder.symbol("jit_runtime_string_lower", runtime::jit_runtime_string_lower as *const u8);
        builder.symbol("jit_runtime_make_list", runtime::jit_runtime_make_list as *const u8);
        builder.symbol("jit_runtime_list_len", runtime::jit_runtime_list_len as *const u8);
        builder.symbol("jit_runtime_list_empty", runtime::jit_runtime_list_empty as *const u8);
        builder.symbol("jit_runtime_list_head", runtime::jit_runtime_list_head as *const u8);
        builder.symbol("jit_runtime_list_tail", runtime::jit_runtime_list_tail as *const u8);
        builder.symbol("jit_runtime_list_cons", runtime::jit_runtime_list_cons as *const u8);
        
        // Extended runtime functions
        use runtime_extended::*;
        
        // Map operations
        builder.symbol("jit_runtime_make_map", jit_runtime_make_map as *const u8);
        builder.symbol("jit_runtime_map_get", jit_runtime_map_get as *const u8);
        builder.symbol("jit_runtime_map_set", jit_runtime_map_set as *const u8);
        builder.symbol("jit_runtime_map_contains", jit_runtime_map_contains as *const u8);
        builder.symbol("jit_runtime_map_size", jit_runtime_map_size as *const u8);
        
        // Vector operations
        builder.symbol("jit_runtime_make_vector", jit_runtime_make_vector as *const u8);
        builder.symbol("jit_runtime_vector_push", jit_runtime_vector_push as *const u8);
        builder.symbol("jit_runtime_vector_get", jit_runtime_vector_get as *const u8);
        builder.symbol("jit_runtime_vector_len", jit_runtime_vector_len as *const u8);
        
        // Type checking
        builder.symbol("jit_runtime_is_type", jit_runtime_is_type as *const u8);
        builder.symbol("jit_runtime_type_name", jit_runtime_type_name as *const u8);
        
        // Error handling
        builder.symbol("jit_runtime_make_error", jit_runtime_make_error as *const u8);
        builder.symbol("jit_runtime_is_error", jit_runtime_is_error as *const u8);
        
        // Closure support
        builder.symbol("jit_runtime_make_closure", jit_runtime_make_closure as *const u8);
        builder.symbol("jit_runtime_closure_get_env", jit_runtime_closure_get_env as *const u8);
        
        // Channel operations
        builder.symbol("jit_runtime_make_channel", jit_runtime_make_channel as *const u8);
        builder.symbol("jit_runtime_channel_send", jit_runtime_channel_send as *const u8);
        builder.symbol("jit_runtime_channel_recv", jit_runtime_channel_recv as *const u8);
        
        // Promise operations
        builder.symbol("jit_runtime_make_promise", jit_runtime_make_promise as *const u8);
        builder.symbol("jit_runtime_promise_await", jit_runtime_promise_await as *const u8);
        
        // Float operations
        builder.symbol("jit_runtime_float_add", jit_runtime_float_add as *const u8);
        builder.symbol("jit_runtime_float_sub", jit_runtime_float_sub as *const u8);
        builder.symbol("jit_runtime_float_mul", jit_runtime_float_mul as *const u8);
        builder.symbol("jit_runtime_float_div", jit_runtime_float_div as *const u8);
    }
}