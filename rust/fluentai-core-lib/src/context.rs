//! Runtime context and state management

use fluentai_core::value::Value;
use fluentai_vm::VM;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

use crate::config::RuntimeConfig;
use crate::error::{Result, RuntimeError};
use crate::host::HostRegistry;
use crate::loader::ModuleLoader;
use crate::module::CompiledModule;

/// Runtime state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeState {
    /// Runtime is idle
    Idle,
    /// Runtime is executing
    Running,
    /// Runtime is paused
    Paused,
    /// Runtime has finished
    Finished,
    /// Runtime encountered an error
    Error,
}

/// Execution statistics
#[derive(Debug, Default, Clone)]
pub struct ExecutionStats {
    /// Total execution time
    pub total_time: Duration,
    /// Number of bytecode instructions executed
    pub instructions_executed: u64,
    /// Number of function calls
    pub function_calls: u64,
    /// Number of GC runs
    pub gc_runs: u32,
    /// Total GC time
    pub gc_time: Duration,
    /// Peak memory usage
    pub peak_memory: usize,
}

/// Runtime context
pub struct RuntimeContext {
    /// Configuration
    config: Arc<RuntimeConfig>,
    /// Current state
    state: RwLock<RuntimeState>,
    /// Module loader
    loader: Arc<ModuleLoader>,
    /// Host function registry
    host_registry: Arc<HostRegistry>,
    /// Global values
    globals: RwLock<HashMap<String, Value>>,
    /// Execution statistics
    stats: RwLock<ExecutionStats>,
    /// Start time
    start_time: Option<Instant>,
}

impl RuntimeContext {
    /// Create a new runtime context
    pub fn new(config: RuntimeConfig) -> Self {
        let config = Arc::new(config);
        let loader = Arc::new(ModuleLoader::new(config.clone()));
        let host_registry = Arc::new(HostRegistry::new());

        Self {
            config,
            state: RwLock::new(RuntimeState::Idle),
            loader,
            host_registry,
            globals: RwLock::new(HashMap::new()),
            stats: RwLock::new(ExecutionStats::default()),
            start_time: None,
        }
    }

    /// Get the configuration
    pub fn config(&self) -> &RuntimeConfig {
        &self.config
    }

    /// Get the current state
    pub fn state(&self) -> RuntimeState {
        *self.state.read()
    }

    /// Set the state
    pub fn set_state(&self, state: RuntimeState) {
        *self.state.write() = state;
    }

    /// Get the module loader
    pub fn loader(&self) -> &ModuleLoader {
        &self.loader
    }

    /// Get the host registry
    pub fn host_registry(&self) -> &HostRegistry {
        &self.host_registry
    }

    /// Get a global value
    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.globals.read().get(name).cloned()
    }

    /// Set a global value
    pub fn set_global(&self, name: impl Into<String>, value: Value) {
        self.globals.write().insert(name.into(), value);
    }

    /// Remove a global value
    pub fn remove_global(&self, name: &str) -> Option<Value> {
        self.globals.write().remove(name)
    }

    /// Get all globals
    pub fn globals(&self) -> HashMap<String, Value> {
        self.globals.read().clone()
    }

    /// Clear all globals
    pub fn clear_globals(&self) {
        self.globals.write().clear();
    }

    /// Get execution statistics
    pub fn stats(&self) -> ExecutionStats {
        self.stats.read().clone()
    }

    /// Update execution statistics
    pub fn update_stats<F>(&self, f: F)
    where
        F: FnOnce(&mut ExecutionStats),
    {
        f(&mut self.stats.write());
    }

    /// Reset execution statistics
    pub fn reset_stats(&self) {
        *self.stats.write() = ExecutionStats::default();
    }

    /// Create a VM instance for this context
    pub fn create_vm(&self, bytecode: fluentai_vm::bytecode::Bytecode) -> Result<VM> {
        // Create VM with bytecode
        let vm = VM::new(bytecode);

        // TODO: Add configuration support when VM builder is available
        // For now, just return the basic VM

        Ok(vm)
    }

    /// Load and prepare a module for execution
    pub fn prepare_module(&self, module_name: &str) -> Result<Arc<CompiledModule>> {
        self.loader.load(module_name)
    }

    /// Check security constraints
    pub fn check_security(&self, operation: &str) -> Result<()> {
        if !self.config.security.enable_sandbox {
            return Ok(());
        }

        match operation {
            "fs_read" | "fs_write" if !self.config.security.allow_fs => {
                Err(RuntimeError::security("File system access denied"))
            }
            "network" if !self.config.security.allow_network => {
                Err(RuntimeError::security("Network access denied"))
            }
            "process" if !self.config.security.allow_process => {
                Err(RuntimeError::security("Process spawning denied"))
            }
            _ => Ok(()),
        }
    }

    /// Check execution timeout
    pub fn check_timeout(&self) -> Result<()> {
        if self.config.security.max_execution_time == 0 {
            return Ok(());
        }

        if let Some(start_time) = self.start_time {
            let elapsed = start_time.elapsed();
            let max_duration = Duration::from_millis(self.config.security.max_execution_time);

            if elapsed > max_duration {
                return Err(RuntimeError::Timeout);
            }
        }

        Ok(())
    }

    /// Mark execution start
    pub fn mark_start(&mut self) {
        self.start_time = Some(Instant::now());
        self.set_state(RuntimeState::Running);
    }

    /// Mark execution end
    pub fn mark_end(&mut self) {
        if let Some(start_time) = self.start_time {
            let duration = start_time.elapsed();
            self.update_stats(|stats| {
                stats.total_time = duration;
            });
        }
        self.set_state(RuntimeState::Finished);
    }
}

/// Context builder
pub struct RuntimeContextBuilder {
    config: RuntimeConfig,
}

impl RuntimeContextBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: RuntimeConfig::default(),
        }
    }

    /// Set the configuration
    pub fn config(mut self, config: RuntimeConfig) -> Self {
        self.config = config;
        self
    }

    /// Use development configuration
    pub fn development(mut self) -> Self {
        self.config = RuntimeConfig::development();
        self
    }

    /// Use production configuration
    pub fn production(mut self) -> Self {
        self.config = RuntimeConfig::production();
        self
    }

    /// Use sandboxed configuration
    pub fn sandboxed(mut self) -> Self {
        self.config = RuntimeConfig::sandboxed();
        self
    }

    /// Build the context
    pub fn build(self) -> RuntimeContext {
        RuntimeContext::new(self.config)
    }
}

impl Default for RuntimeContextBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_context() {
        let context = RuntimeContext::new(RuntimeConfig::default());

        assert_eq!(context.state(), RuntimeState::Idle);

        // Test globals
        context.set_global("x", Value::Float(42.0));
        assert_eq!(context.get_global("x"), Some(Value::Float(42.0)));

        // Test state changes
        context.set_state(RuntimeState::Running);
        assert_eq!(context.state(), RuntimeState::Running);
    }

    #[test]
    fn test_security_checks() {
        let config = RuntimeConfig::sandboxed();
        let context = RuntimeContext::new(config);

        assert!(context.check_security("fs_read").is_err());
        assert!(context.check_security("network").is_err());
        assert!(context.check_security("process").is_err());
        assert!(context.check_security("other").is_ok());
    }

    #[test]
    fn test_context_builder() {
        let context = RuntimeContextBuilder::new().development().build();

        assert!(context.config().debug.enabled);
        assert!(context.config().debug.enable_tracing);
    }
}
