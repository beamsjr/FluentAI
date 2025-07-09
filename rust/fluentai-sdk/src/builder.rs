//! FluentAI SDK builder

use crate::session::{Session, SessionOptions};
use crate::error::Result;
use fluentai_core_lib::{RuntimeConfig, HostFunction};
use std::path::PathBuf;

/// FluentAI SDK builder
pub struct FluentAIBuilder {
    options: SessionOptions,
}

impl FluentAIBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            options: SessionOptions::default(),
        }
    }
    
    /// Create with development configuration
    pub fn development() -> Self {
        Self {
            options: SessionOptions::development(),
        }
    }
    
    /// Create with production configuration  
    pub fn production() -> Self {
        Self {
            options: SessionOptions::production(),
        }
    }
    
    /// Create with sandboxed configuration
    pub fn sandboxed() -> Self {
        Self {
            options: SessionOptions::sandboxed(),
        }
    }
    
    /// Set custom configuration
    pub fn config(mut self, config: RuntimeConfig) -> Self {
        self.options.config = config;
        self
    }
    
    /// Set execution timeout
    pub fn timeout(mut self, ms: u64) -> Self {
        self.options.config.security.max_execution_time = ms;
        self
    }
    
    /// Set memory limit
    pub fn memory_limit(mut self, bytes: usize) -> Self {
        self.options.config.memory.max_heap_size = bytes;
        self
    }
    
    /// Enable or disable debug mode
    pub fn debug(mut self, enabled: bool) -> Self {
        self.options.config.debug.enabled = enabled;
        self
    }
    
    /// Enable or disable standard library
    pub fn stdlib(mut self, enabled: bool) -> Self {
        self.options.enable_stdlib = enabled;
        self
    }
    
    /// Add module search path
    pub fn add_module_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.options.module_paths.push(path.into());
        self
    }
    
    /// Add multiple module paths
    pub fn module_paths(mut self, paths: impl IntoIterator<Item = impl Into<PathBuf>>) -> Self {
        self.options.module_paths.extend(paths.into_iter().map(|p| p.into()));
        self
    }
    
    /// Register a host function
    pub fn add_function(mut self, func: HostFunction) -> Self {
        self.options.host_functions.push(func);
        self
    }
    
    /// Register multiple host functions
    pub fn add_functions(mut self, funcs: impl IntoIterator<Item = HostFunction>) -> Self {
        self.options.host_functions.extend(funcs);
        self
    }
    
    /// Set worker threads
    pub fn worker_threads(mut self, count: usize) -> Self {
        self.options.config.worker_threads = count;
        self
    }
    
    /// Enable sandboxing
    pub fn enable_sandbox(mut self) -> Self {
        self.options.config.security.enable_sandbox = true;
        self
    }
    
    /// Allow file system access
    pub fn allow_fs(mut self, allowed: bool) -> Self {
        self.options.config.security.allow_fs = allowed;
        self
    }
    
    /// Allow network access
    pub fn allow_network(mut self, allowed: bool) -> Self {
        self.options.config.security.allow_network = allowed;
        self
    }
    
    /// Allow process spawning
    pub fn allow_process(mut self, allowed: bool) -> Self {
        self.options.config.security.allow_process = allowed;
        self
    }
    
    /// Build a session
    pub fn build(self) -> Result<Session> {
        Session::new(self.options)
    }
}

impl Default for FluentAIBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::value::Value;
    
    #[test]
    fn test_builder_basic() {
        let session = FluentAIBuilder::new()
            .debug(true)
            .timeout(5000)
            .build()
            .unwrap();
        
        assert!(!session.id().is_empty());
    }
    
    #[test]
    fn test_builder_with_function() {
        let double = HostFunction::new("double", 1, |args| {
            match &args[0] {
                Value::Number(n) => Ok(Value::Number(n * 2.0)),
                _ => Err(fluentai_core_lib::RuntimeError::host("double expects a number")),
            }
        });
        
        let mut session = FluentAIBuilder::new()
            .add_function(double)
            .build()
            .unwrap();
        
        let result = session.eval("(double 21)").unwrap();
        assert_eq!(result, Value::Number(42.0));
    }
    
    #[test]
    fn test_builder_sandboxed() {
        let session = FluentAIBuilder::sandboxed()
            .timeout(1000)
            .build()
            .unwrap();
        
        assert!(!session.id().is_empty());
    }
}