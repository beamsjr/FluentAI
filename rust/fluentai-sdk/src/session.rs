//! FluentAI session management

use fluentai_core_lib::{RuntimeEngine, RuntimeConfig, HostFunction};
use fluentai_core::value::Value;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::path::Path;
use crate::error::Result;
use crate::script::Script;
use crate::stdlib;

/// Session options
#[derive(Debug, Clone)]
pub struct SessionOptions {
    /// Runtime configuration
    pub config: RuntimeConfig,
    /// Enable standard library
    pub enable_stdlib: bool,
    /// Custom module search paths
    pub module_paths: Vec<std::path::PathBuf>,
    /// Custom host functions
    pub host_functions: Vec<HostFunction>,
}

impl Default for SessionOptions {
    fn default() -> Self {
        let mut config = RuntimeConfig::default();
        // Add current directory to module search paths
        if let Ok(cwd) = std::env::current_dir() {
            config.modules.search_paths.push(cwd);
        }
        
        Self {
            config,
            enable_stdlib: true,
            module_paths: Vec::new(),
            host_functions: Vec::new(),
        }
    }
}

impl SessionOptions {
    /// Create new options with default settings
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Use development settings
    pub fn development() -> Self {
        Self {
            config: RuntimeConfig::development(),
            ..Default::default()
        }
    }
    
    /// Use production settings
    pub fn production() -> Self {
        Self {
            config: RuntimeConfig::production(),
            ..Default::default()
        }
    }
    
    /// Use sandboxed settings
    pub fn sandboxed() -> Self {
        Self {
            config: RuntimeConfig::sandboxed(),
            enable_stdlib: false,  // Restricted stdlib in sandbox
            ..Default::default()
        }
    }
    
    /// Set execution timeout
    pub fn timeout(mut self, ms: u64) -> Self {
        self.config.security.max_execution_time = ms;
        self
    }
    
    /// Enable debug mode
    pub fn debug(mut self, enabled: bool) -> Self {
        self.config.debug.enabled = enabled;
        self
    }
    
    /// Add module search path
    pub fn add_module_path(mut self, path: impl Into<std::path::PathBuf>) -> Self {
        self.module_paths.push(path.into());
        self
    }
    
    /// Add host function
    pub fn add_host_function(mut self, func: HostFunction) -> Self {
        self.host_functions.push(func);
        self
    }
    
    /// Disable standard library
    pub fn no_stdlib(mut self) -> Self {
        self.enable_stdlib = false;
        self
    }
}

/// FluentAI session
pub struct Session {
    /// Runtime engine
    engine: Mutex<RuntimeEngine>,
    /// Session ID
    id: String,
    /// Session metadata
    metadata: HashMap<String, String>,
}

impl Session {
    /// Create a new session
    pub fn new(mut options: SessionOptions) -> Result<Self> {
        // Add module paths to config
        for path in options.module_paths {
            options.config.modules.search_paths.push(path);
        }
        
        let engine = RuntimeEngine::new(options.config);
        
        // Register standard library if enabled
        if options.enable_stdlib {
            stdlib::register_all(&engine)?;
        }
        
        // Register custom host functions
        for func in options.host_functions {
            engine.register_function(func)?;
        }
        
        Ok(Self {
            engine: Mutex::new(engine),
            id: generate_session_id(),
            metadata: HashMap::new(),
        })
    }
    
    /// Get session ID
    pub fn id(&self) -> &str {
        &self.id
    }
    
    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }
    
    /// Set metadata value
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
    
    /// Evaluate code
    pub fn eval(&mut self, code: &str) -> Result<Value> {
        self.engine.lock().eval(code).map_err(Into::into)
    }
    
    /// Execute code
    pub fn execute(&mut self, code: &str) -> Result<Value> {
        self.engine.lock().execute(code).map_err(Into::into)
    }
    
    /// Run a script file
    pub fn run_file(&mut self, path: impl AsRef<Path>) -> Result<Value> {
        let script = Script::from_file(path)?;
        self.run_script(&script)
    }
    
    /// Run a script
    pub fn run_script(&mut self, script: &Script) -> Result<Value> {
        self.execute(script.source())
    }
    
    /// Load a module
    pub fn load_module(&mut self, name: &str) -> Result<()> {
        self.engine.lock().load_module(name)?;
        Ok(())
    }
    
    /// Set a global value
    pub fn set_global(&mut self, name: impl Into<String>, value: Value) {
        self.engine.lock().set_global(name, value);
    }
    
    /// Get a global value
    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.engine.lock().get_global(name)
    }
    
    /// Call a function
    pub fn call_function(&mut self, name: &str, args: &[Value]) -> Result<Value> {
        self.engine.lock().call_function(name, args).map_err(Into::into)
    }
    
    /// Register a host function
    pub fn register_function(&mut self, func: HostFunction) -> Result<()> {
        self.engine.lock().register_function(func).map_err(Into::into)
    }
    
    /// Reset the session
    pub fn reset(&mut self) {
        self.engine.lock().reset();
    }
    
    /// Get execution statistics
    pub fn stats(&self) -> fluentai_core_lib::context::ExecutionStats {
        self.engine.lock().stats()
    }
}

/// Generate a unique session ID
fn generate_session_id() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    
    format!("session_{}", timestamp)
}

/// Session builder
pub struct SessionBuilder {
    options: SessionOptions,
}

impl SessionBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            options: SessionOptions::default(),
        }
    }
    
    /// Set options
    pub fn options(mut self, options: SessionOptions) -> Self {
        self.options = options;
        self
    }
    
    /// Set timeout
    pub fn timeout(mut self, ms: u64) -> Self {
        self.options.config.security.max_execution_time = ms;
        self
    }
    
    /// Enable debug mode
    pub fn debug(mut self, enabled: bool) -> Self {
        self.options.config.debug.enabled = enabled;
        self
    }
    
    /// Add module path
    pub fn add_module_path(mut self, path: impl Into<std::path::PathBuf>) -> Self {
        self.options.module_paths.push(path.into());
        self
    }
    
    /// Add host function
    pub fn add_host_function(mut self, func: HostFunction) -> Self {
        self.options.host_functions.push(func);
        self
    }
    
    /// Disable standard library
    pub fn no_stdlib(mut self) -> Self {
        self.options.enable_stdlib = false;
        self
    }
    
    /// Build the session
    pub fn build(self) -> Result<Session> {
        Session::new(self.options)
    }
}

impl Default for SessionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_session_creation() {
        let session = Session::new(SessionOptions::default()).unwrap();
        assert!(!session.id().is_empty());
    }
    
    #[test]
    fn test_session_eval() {
        let mut session = Session::new(SessionOptions::default()).unwrap();
        let result = session.eval("(+ 1 2)").unwrap();
        assert_eq!(result, Value::Float(3.0));
    }
    
    #[test]
    fn test_session_globals() {
        let mut session = Session::new(SessionOptions::default()).unwrap();
        session.set_global("x", Value::Float(42.0));
        
        let result = session.eval("x").unwrap();
        assert_eq!(result, Value::Float(42.0));
    }
    
    #[test]
    fn test_session_builder() {
        let session = SessionBuilder::new()
            .debug(true)
            .timeout(5000)
            .build()
            .unwrap();
        
        assert!(!session.id().is_empty());
    }
}