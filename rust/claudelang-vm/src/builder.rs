//! VM Builder for dependency injection

use std::sync::Arc;
use std::collections::HashMap;
use anyhow::Result;

use crate::vm::VM;
use crate::bytecode::Bytecode;
use crate::bytecode::Value;
use claudelang_effects::{EffectContext, EffectRuntime};
use claudelang_stdlib::StdlibRegistry;
use claudelang_modules::{ModuleLoader, ModuleConfig};

/// Trait for VM configuration
pub trait VMConfig {
    /// Configure the VM builder
    fn configure(&self, builder: &mut VMBuilder);
}

/// Builder for constructing a VM with injected dependencies
pub struct VMBuilder {
    /// The bytecode to execute
    bytecode: Option<Bytecode>,
    /// Optional custom effect context
    effect_context: Option<Arc<EffectContext>>,
    /// Optional custom effect runtime
    effect_runtime: Option<Arc<EffectRuntime>>,
    /// Optional custom stdlib registry
    stdlib_registry: Option<StdlibRegistry>,
    /// Optional custom module loader
    module_loader: Option<ModuleLoader>,
    /// Module configuration
    module_config: Option<ModuleConfig>,
    /// Initial global values
    initial_globals: HashMap<String, Value>,
    /// Stack size
    stack_size: Option<usize>,
    /// Enable trace mode
    trace_mode: bool,
}

impl VMBuilder {
    /// Create a new VM builder
    pub fn new() -> Self {
        Self {
            bytecode: None,
            effect_context: None,
            effect_runtime: None,
            stdlib_registry: None,
            module_loader: None,
            module_config: None,
            initial_globals: HashMap::new(),
            stack_size: None,
            trace_mode: false,
        }
    }
    
    /// Set the bytecode to execute
    pub fn with_bytecode(mut self, bytecode: Bytecode) -> Self {
        self.bytecode = Some(bytecode);
        self
    }
    
    /// Set a custom effect context
    pub fn with_effect_context(mut self, context: Arc<EffectContext>) -> Self {
        self.effect_context = Some(context);
        self
    }
    
    /// Set a custom effect runtime
    pub fn with_effect_runtime(mut self, runtime: Arc<EffectRuntime>) -> Self {
        self.effect_runtime = Some(runtime);
        self
    }
    
    /// Set a custom stdlib registry
    pub fn with_stdlib_registry(mut self, registry: StdlibRegistry) -> Self {
        self.stdlib_registry = Some(registry);
        self
    }
    
    /// Set a custom module loader
    pub fn with_module_loader(mut self, loader: ModuleLoader) -> Self {
        self.module_loader = Some(loader);
        self
    }
    
    /// Set module configuration
    pub fn with_module_config(mut self, config: ModuleConfig) -> Self {
        self.module_config = Some(config);
        self
    }
    
    /// Add an initial global value
    pub fn with_global(mut self, name: &str, value: Value) -> Self {
        self.initial_globals.insert(name.to_string(), value);
        self
    }
    
    /// Set stack size
    pub fn with_stack_size(mut self, size: usize) -> Self {
        self.stack_size = Some(size);
        self
    }
    
    /// Enable trace mode
    pub fn with_trace_mode(mut self, trace: bool) -> Self {
        self.trace_mode = trace;
        self
    }
    
    /// Apply a configuration
    pub fn with_config<C: VMConfig>(mut self, config: C) -> Self {
        config.configure(&mut self);
        self
    }
    
    /// Build the VM
    pub fn build(self) -> Result<VM> {
        // Bytecode is required
        let bytecode = self.bytecode
            .ok_or_else(|| anyhow::anyhow!("Bytecode is required"))?;
        
        // Create VM with default dependencies
        let mut vm = VM::new(bytecode);
        
        // Apply custom dependencies if provided
        if let Some(context) = self.effect_context {
            vm.set_effect_context(context);
        }
        
        if let Some(runtime) = self.effect_runtime {
            vm.set_effect_runtime(runtime);
        }
        
        // Note: The current VM doesn't have setters for stdlib and module_loader
        // These would need to be added to the VM implementation
        
        // Configure VM
        if self.trace_mode {
            vm.enable_trace();
        }
        
        // Set initial globals
        for (name, value) in self.initial_globals {
            vm.set_global(name, value);
        }
        
        Ok(vm)
    }
}

impl Default for VMBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Example VM configurations

/// Development configuration with trace mode
pub struct DevelopmentConfig;

impl VMConfig for DevelopmentConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        builder.trace_mode = true;
        builder.stack_size = Some(1024 * 1024); // 1MB stack
    }
}

/// Production configuration with optimizations
pub struct ProductionConfig {
    pub stack_size: usize,
}

impl VMConfig for ProductionConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        builder.trace_mode = false;
        builder.stack_size = Some(self.stack_size);
    }
}

/// Testing configuration with isolated environment
pub struct TestingConfig {
    pub enable_trace: bool,
}

impl VMConfig for TestingConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        builder.trace_mode = self.enable_trace;
        // Could add test-specific effect handlers here
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_vm_builder() {
        let bytecode = Bytecode::new();
        let vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_trace_mode(true)
            .with_stack_size(2048)
            .with_global("version", Value::String("1.0.0".to_string()))
            .build()
            .unwrap();
        
        // VM should be created with the specified configuration
        assert_eq!(vm.globals.get("version"), Some(&Value::String("1.0.0".to_string())));
    }
    
    #[test]
    fn test_vm_builder_with_config() {
        let bytecode = Bytecode::new();
        let vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_config(DevelopmentConfig)
            .build()
            .unwrap();
        
        // Development config enables trace mode
        // (would need getter on VM to test this)
    }
    
    #[test]
    fn test_vm_builder_requires_bytecode() {
        let result = VMBuilder::new().build();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Bytecode is required"));
    }
}