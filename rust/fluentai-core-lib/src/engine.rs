//! Runtime engine - the main entry point for executing FluentAI programs

use fluentai_core::value::Value;
use std::sync::Arc;
use std::time::Instant;
use tracing::{debug, info, warn};

use crate::config::{ExecutionMode, RuntimeConfig};
use crate::context::{RuntimeContext, RuntimeState};
use crate::error::{Result, RuntimeError};
use crate::host::HostFunction;
use crate::module::CompiledModule;

/// Runtime engine
pub struct RuntimeEngine {
    /// Runtime context
    context: RuntimeContext,
}

impl RuntimeEngine {
    /// Create a new runtime engine
    pub fn new(config: RuntimeConfig) -> Self {
        Self {
            context: RuntimeContext::new(config),
        }
    }

    /// Create with default configuration
    pub fn default() -> Self {
        Self::new(RuntimeConfig::default())
    }

    /// Create with development configuration
    pub fn development() -> Self {
        Self::new(RuntimeConfig::development())
    }

    /// Create with production configuration
    pub fn production() -> Self {
        Self::new(RuntimeConfig::production())
    }

    /// Create with sandboxed configuration
    pub fn sandboxed() -> Self {
        Self::new(RuntimeConfig::sandboxed())
    }

    /// Get the runtime context
    pub fn context(&self) -> &RuntimeContext {
        &self.context
    }

    /// Register a host function
    pub fn register_function(&self, func: HostFunction) -> Result<()> {
        self.context.host_registry().register(func)
    }

    /// Register multiple host functions
    pub fn register_functions(&self, funcs: impl IntoIterator<Item = HostFunction>) -> Result<()> {
        self.context.host_registry().register_all(funcs)
    }

    /// Set a global value
    pub fn set_global(&self, name: impl Into<String>, value: Value) {
        self.context.set_global(name, value);
    }

    /// Get a global value
    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.context.get_global(name)
    }

    /// Load a module
    pub fn load_module(&self, name: &str) -> Result<Arc<CompiledModule>> {
        info!("Loading module: {}", name);
        self.context.loader().load(name)
    }

    /// Load a module from source
    pub fn load_module_from_source(&self, name: &str, source: &str) -> Result<Arc<CompiledModule>> {
        info!("Loading module from source: {}", name);
        self.context.loader().load_from_source(name, source)
    }

    /// Execute a module
    pub fn execute_module(&mut self, module_name: &str) -> Result<Value> {
        info!("Executing module: {}", module_name);

        // Load module
        let module = self.context.prepare_module(module_name)?;

        // Execute
        self.execute_compiled_module(&module)
    }

    /// Execute source code
    pub fn execute(&mut self, source: &str) -> Result<Value> {
        info!("Executing source code");

        // Load as anonymous module
        let module = self.context.loader().load_from_source("__main__", source)?;

        // Execute
        self.execute_compiled_module(&module)
    }

    /// Execute a compiled module
    fn execute_compiled_module(&mut self, module: &CompiledModule) -> Result<Value> {
        // Check state
        if self.context.state() == RuntimeState::Running {
            return Err(RuntimeError::other("Runtime is already executing"));
        }

        // Mark start
        self.context.mark_start();
        let start_time = Instant::now();

        // Create VM with bytecode
        let mut vm = self.context.create_vm(module.bytecode.as_ref().clone())?;

        // Register host functions with VM
        for _func in self.context.host_registry().all() {
            // TODO: Register host functions with VM
            // This would require extending the VM to support host functions
        }

        // Set globals in VM
        for (_name, _value) in self.context.globals() {
            // TODO: Set globals in VM
        }

        // Execute based on execution mode
        let result = match self.context.config().execution_mode {
            ExecutionMode::Interpreted => {
                debug!("Executing in interpreted mode");
                self.execute_interpreted(&mut vm, &module.bytecode)
            }
            #[cfg(feature = "jit")]
            ExecutionMode::JIT { threshold } => {
                debug!("Executing in JIT mode (threshold: {})", threshold);
                self.execute_jit(&mut vm, &module.bytecode, threshold)
            }
            #[cfg(feature = "aot")]
            ExecutionMode::AOT => {
                debug!("Executing in AOT mode");
                Err(RuntimeError::not_implemented(
                    "AOT execution not yet implemented",
                ))
            }
        };

        // Update stats
        let elapsed = start_time.elapsed();
        self.context.update_stats(|stats| {
            stats.total_time = elapsed;
            // TODO: Get more stats from VM
        });

        // Mark end
        self.context.mark_end();

        result
    }

    /// Execute in interpreted mode
    fn execute_interpreted(
        &self,
        vm: &mut fluentai_vm::VM,
        _bytecode: &fluentai_bytecode::Bytecode,
    ) -> Result<Value> {
        // Check timeout before running
        self.context.check_timeout()?;

        // Run the VM - it executes to completion
        vm.run().map_err(|e| RuntimeError::VmError(e))
    }

    /// Execute with JIT compilation
    #[cfg(feature = "jit")]
    fn execute_jit(
        &self,
        vm: &mut fluentai_vm::VM,
        bytecode: &fluentai_bytecode::Bytecode,
        _threshold: u32,
    ) -> Result<Value> {
        // For now, just use interpreted mode
        // TODO: Implement JIT execution
        warn!("JIT execution not fully implemented, falling back to interpreted mode");
        self.execute_interpreted(vm, bytecode)
    }

    /// Evaluate an expression
    pub fn eval(&mut self, expr: &str) -> Result<Value> {
        let source = format!("(begin {})", expr);
        self.execute(&source)
    }

    /// Call a function
    pub fn call_function(&mut self, name: &str, args: &[Value]) -> Result<Value> {
        // Check if it's a host function
        if let Some(host_func) = self.context.host_registry().get(name) {
            return host_func.call(args);
        }

        // Otherwise, look for it in loaded modules
        // TODO: Implement module function lookup and calling
        Err(RuntimeError::other(format!(
            "Function '{}' not found",
            name
        )))
    }

    /// Reset the runtime
    pub fn reset(&mut self) {
        self.context.set_state(RuntimeState::Idle);
        self.context.clear_globals();
        self.context.reset_stats();
        self.context.loader().clear_modules();
    }

    /// Get execution statistics
    pub fn stats(&self) -> crate::context::ExecutionStats {
        self.context.stats()
    }
}

/// Engine builder
pub struct RuntimeEngineBuilder {
    config: RuntimeConfig,
}

impl RuntimeEngineBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: RuntimeConfig::default(),
        }
    }

    /// Set configuration
    pub fn config(mut self, config: RuntimeConfig) -> Self {
        self.config = config;
        self
    }

    /// Set execution mode
    pub fn execution_mode(mut self, mode: ExecutionMode) -> Self {
        self.config.execution_mode = mode;
        self
    }

    /// Enable debug mode
    pub fn debug(mut self, enabled: bool) -> Self {
        self.config.debug.enabled = enabled;
        self
    }

    /// Enable sandboxing
    pub fn sandboxed(mut self, enabled: bool) -> Self {
        self.config.security.enable_sandbox = enabled;
        self
    }

    /// Set memory limits
    pub fn memory_limit(mut self, limit: usize) -> Self {
        self.config.memory.max_heap_size = limit;
        self
    }

    /// Set execution timeout
    pub fn timeout(mut self, ms: u64) -> Self {
        self.config.security.max_execution_time = ms;
        self
    }

    /// Add module search path
    pub fn add_search_path(mut self, path: impl Into<std::path::PathBuf>) -> Self {
        self.config.modules.search_paths.push(path.into());
        self
    }

    /// Build the engine
    pub fn build(self) -> RuntimeEngine {
        RuntimeEngine::new(self.config)
    }
}

impl Default for RuntimeEngineBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::host::HostFunction;

    #[test]
    fn test_runtime_engine() {
        let mut engine = RuntimeEngine::default();

        // Register a simple host function
        let add = HostFunction::new("add", 2, |args| match (&args[0], &args[1]) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            _ => Err(RuntimeError::host("Expected two numbers")),
        });

        engine.register_function(add).unwrap();

        // Test execution
        let result = engine.execute("1 + 2").unwrap();
        assert_eq!(result, Value::Integer(3));
    }

    #[test]
    fn test_engine_builder() {
        let engine = RuntimeEngineBuilder::new()
            .debug(true)
            .sandboxed(true)
            .timeout(5000)
            .build();

        assert!(engine.context.config().debug.enabled);
        assert!(engine.context.config().security.enable_sandbox);
        assert_eq!(engine.context.config().security.max_execution_time, 5000);
    }
}
