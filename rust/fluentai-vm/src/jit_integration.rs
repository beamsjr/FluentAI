//! JIT compilation integration for hot path optimization
//!
//! This module provides integration between the VM and the JIT compiler,
//! automatically compiling frequently executed functions to native code.

use fluentai_bytecode::Bytecode;
use crate::error::{VMError, VMResult};
use fluentai_core::ast::{NodeId, UsageStatistics};
use fluentai_core::value::Value;
#[cfg(feature = "jit")]
use fluentai_jit::JitCompiler;
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
use web_time::Instant;

/// Threshold for JIT compilation
pub struct JitConfig {
    /// Number of calls before a function is considered for JIT compilation
    pub call_threshold: u64,
    /// Total execution time (in nanoseconds) before considering JIT compilation
    pub time_threshold: u64,
    /// Maximum number of JIT-compiled functions to keep in memory
    pub max_compiled_functions: usize,
    /// Enable JIT compilation
    pub enabled: bool,
}

impl Default for JitConfig {
    fn default() -> Self {
        Self {
            call_threshold: 50,  // Compile after 50 calls
            time_threshold: 1_000_000,  // Or after 1ms total execution time
            max_compiled_functions: 1000,
            enabled: cfg!(target_arch = "x86_64"),  // Only enable on x86_64
        }
    }
}

/// Manages JIT compilation for the VM
pub struct JitManager {
    /// JIT compiler instance
    compiler: Option<JitCompiler>,
    /// Configuration
    config: JitConfig,
    /// Map from chunk_id to whether it's been JIT compiled
    compiled_chunks: FxHashMap<usize, bool>,
    /// Statistics about JIT compilation
    stats: JitStats,
}

#[derive(Default)]
pub struct JitStats {
    pub functions_compiled: usize,
    pub compilation_failures: usize,
    pub total_compilation_time_ms: f64,
    pub jit_execution_count: u64,
}

impl JitManager {
    pub fn new(config: JitConfig) -> Self {
        let compiler = if config.enabled {
            JitCompiler::new().ok()
        } else {
            None
        };
        
        Self {
            compiler,
            config,
            compiled_chunks: FxHashMap::default(),
            stats: JitStats::default(),
        }
    }
    
    /// Check if a function should be JIT compiled based on usage statistics
    pub fn should_compile(&self, stats: &UsageStatistics) -> bool {
        if !self.config.enabled || self.compiler.is_none() {
            return false;
        }
        
        // Don't compile if we've already reached the limit
        if self.compiled_chunks.len() >= self.config.max_compiled_functions {
            return false;
        }
        
        // Check if it meets the thresholds
        stats.call_count >= self.config.call_threshold ||
        stats.total_time >= self.config.time_threshold
    }
    
    /// Attempt to JIT compile a chunk
    pub fn compile_chunk(
        &mut self,
        chunk_id: usize,
        bytecode: &Arc<Bytecode>,
    ) -> VMResult<()> {
        if self.compiled_chunks.contains_key(&chunk_id) {
            return Ok(()); // Already compiled or attempted
        }
        
        let start_time = web_time::Instant::now();
        
        match self.compiler.as_mut() {
            Some(compiler) => {
                match compiler.compile(bytecode, chunk_id) {
                    Ok(_) => {
                        self.compiled_chunks.insert(chunk_id, true);
                        self.stats.functions_compiled += 1;
                        self.stats.total_compilation_time_ms += 
                            start_time.elapsed().as_secs_f64() * 1000.0;
                        Ok(())
                    }
                    Err(e) => {
                        // Mark as attempted but failed
                        self.compiled_chunks.insert(chunk_id, false);
                        self.stats.compilation_failures += 1;
                        // Log error but don't fail execution
                        eprintln!("JIT compilation failed for chunk {}: {}", chunk_id, e);
                        Ok(())
                    }
                }
            }
            None => Ok(()),
        }
    }
    
    /// Execute a JIT-compiled function if available
    pub fn execute_if_compiled(
        &mut self,
        chunk_id: usize,
        bytecode: &Arc<Bytecode>,
    ) -> Option<VMResult<Value>> {
        if !self.config.enabled {
            return None;
        }
        
        // Check if successfully compiled
        if self.compiled_chunks.get(&chunk_id) != Some(&true) {
            return None;
        }
        
        match self.compiler.as_mut() {
            Some(compiler) => {
                // Get the compiled function from cache
                match compiler.compile(bytecode, chunk_id) {
                    Ok(compiled_func) => {
                        // Execute the JIT-compiled function
                        match compiler.execute(compiled_func) {
                            Ok(result) => {
                                self.stats.jit_execution_count += 1;
                                Some(Ok(result))
                            }
                            Err(e) => Some(Err(VMError::Runtime {
                                message: format!("JIT execution error: {}", e),
                                stack_trace: None,
                            })),
                        }
                    }
                    Err(_) => None, // Fall back to interpreter
                }
            }
            None => None,
        }
    }
    
    /// Get JIT compilation statistics
    pub fn stats(&self) -> &JitStats {
        &self.stats
    }
    
    /// Clear the JIT cache
    pub fn clear_cache(&mut self) {
        if let Some(compiler) = &mut self.compiler {
            compiler.clear_cache();
        }
        self.compiled_chunks.clear();
    }
    
    /// Check if JIT is enabled
    pub fn is_enabled(&self) -> bool {
        self.config.enabled && self.compiler.is_some()
    }
}