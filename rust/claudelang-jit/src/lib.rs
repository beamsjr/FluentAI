//! Cranelift-based JIT compiler for ClaudeLang
//! 
//! This module provides Just-In-Time compilation of ClaudeLang bytecode
//! to native machine code using Cranelift, achieving near-native performance.

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, Linkage};

use claudelang_vm::bytecode::{Bytecode, Value};
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::mem;

pub mod codegen;

/// JIT compilation statistics
#[derive(Debug, Default)]
pub struct JitStats {
    pub functions_compiled: usize,
    pub total_instructions: usize,
    pub optimization_time_ms: f64,
    pub codegen_time_ms: f64,
}

/// Main JIT compiler interface
pub struct JitCompiler {
    /// The JIT module
    module: JITModule,
    
    /// Cached compiled functions
    function_cache: HashMap<usize, *const u8>,
    
    /// Compilation statistics
    stats: JitStats,
}

impl JitCompiler {
    /// Create a new JIT compiler
    pub fn new() -> Result<Self> {
        // Create JIT builder
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
        
        // Disable PLT on non-x86_64 platforms
        #[cfg(not(target_arch = "x86_64"))]
        builder.symbol_lookup_fn(Box::new(|_name| None));
        
        // Create the JIT module
        let module = JITModule::new(builder);
        
        Ok(Self {
            module,
            function_cache: HashMap::new(),
            stats: JitStats::default(),
        })
    }
    
    /// Compile bytecode to native function
    pub fn compile(&mut self, bytecode: &Bytecode, chunk_id: usize) -> Result<fn() -> i64> {
        // Check cache first
        if let Some(&cached) = self.function_cache.get(&chunk_id) {
            return Ok(unsafe { mem::transmute::<*const u8, fn() -> i64>(cached) });
        }
        
        let start = std::time::Instant::now();
        
        // Get the chunk to compile
        let chunk = bytecode.chunks.get(chunk_id)
            .ok_or_else(|| anyhow!("Invalid chunk ID: {}", chunk_id))?;
        
        // Create a new context for this function
        let mut ctx = self.module.make_context();
        
        // Create function signature
        ctx.func.signature.returns.push(AbiParam::new(types::I64));
        
        // Create the function builder context
        let mut func_ctx = FunctionBuilderContext::new();
        
        // Build the function
        codegen::build_function(&mut ctx.func, &mut func_ctx, chunk)?;
        
        self.stats.codegen_time_ms += start.elapsed().as_secs_f64() * 1000.0;
        
        // Compile and link the function
        let opt_start = std::time::Instant::now();
        
        // Define the function in the JIT module
        let func_id = self.module.declare_function(
            &format!("claudelang_func_{}", chunk_id),
            Linkage::Local,
            &ctx.func.signature,
        )?;
        
        self.module.define_function(func_id, &mut ctx)?;
        self.module.finalize_definitions()?;
        
        self.stats.optimization_time_ms += opt_start.elapsed().as_secs_f64() * 1000.0;
        
        // Get the compiled function pointer
        let func_ptr = self.module.get_finalized_function(func_id);
        
        // Cache the compiled function
        self.function_cache.insert(chunk_id, func_ptr);
        self.stats.functions_compiled += 1;
        self.stats.total_instructions += chunk.instructions.len();
        
        // Convert to safe function pointer
        Ok(unsafe { mem::transmute::<*const u8, fn() -> i64>(func_ptr) })
    }
    
    /// Execute a compiled function
    pub fn execute(&self, func: fn() -> i64) -> Result<Value> {
        let result = func();
        Ok(Value::Int(result))
    }
    
    /// Compile and execute bytecode in one step
    pub fn compile_and_run(&mut self, bytecode: &Bytecode) -> Result<Value> {
        let func = self.compile(bytecode, bytecode.main_chunk)?;
        self.execute(func)
    }
    
    /// Get compilation statistics
    pub fn stats(&self) -> &JitStats {
        &self.stats
    }
    
    /// Clear the function cache
    pub fn clear_cache(&mut self) {
        self.function_cache.clear();
    }
}