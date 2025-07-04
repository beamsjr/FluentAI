//! VM Builder for dependency injection

use std::sync::Arc;
use rustc_hash::FxHashMap;
use anyhow::Result;

use crate::vm::VM;
use crate::bytecode::Bytecode;
use crate::bytecode::Value;
use crate::security::{SecurityManager, SecurityPolicy};
use crate::gc::{GarbageCollector, GcConfig};
use fluentai_effects::{EffectContext, EffectRuntime};
use fluentai_stdlib::StdlibRegistry;
use fluentai_modules::{ModuleLoader, ModuleConfig};

/// Trait for VM configuration
pub trait VMConfig {
    /// Configure the VM builder
    fn configure(&self, builder: &mut VMBuilder);
}

/// Builder for constructing a VM with injected dependencies
#[derive(Clone)]
pub struct VMBuilder {
    /// The bytecode to execute
    bytecode: Option<Bytecode>,
    /// Optional custom effect context
    effect_context: Option<Arc<EffectContext>>,
    /// Optional custom effect runtime
    effect_runtime: Option<Arc<EffectRuntime>>,
    /// Optional custom stdlib registry
    stdlib_registry: Option<StdlibRegistry>,
    /// Module configuration
    module_config: Option<ModuleConfig>,
    /// Initial global values
    initial_globals: FxHashMap<String, Value>,
    /// Stack size
    stack_size: Option<usize>,
    /// Enable trace mode
    trace_mode: bool,
    /// Security manager
    security_manager: Option<Arc<SecurityManager>>,
    /// Security policy (used if security_manager is not provided)
    security_policy: Option<SecurityPolicy>,
    /// Garbage collector configuration
    gc_config: Option<GcConfig>,
    /// Whether to enable GC
    enable_gc: bool,
}

impl VMBuilder {
    /// Create a new VM builder
    pub fn new() -> Self {
        Self {
            bytecode: None,
            effect_context: None,
            effect_runtime: None,
            stdlib_registry: None,
            module_config: None,
            initial_globals: FxHashMap::default(),
            stack_size: None,
            trace_mode: false,
            security_manager: None,
            security_policy: None,
            gc_config: None,
            enable_gc: false,
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
    
    /// Set a custom security manager
    pub fn with_security_manager(mut self, manager: Arc<SecurityManager>) -> Self {
        self.security_manager = Some(manager);
        self
    }
    
    /// Set a security policy
    pub fn with_security_policy(mut self, policy: SecurityPolicy) -> Self {
        self.security_policy = Some(policy);
        self
    }
    
    /// Enable sandbox mode with strict security
    pub fn with_sandbox_mode(mut self) -> Self {
        self.security_policy = Some(SecurityPolicy::sandbox());
        self
    }
    
    /// Enable garbage collection with default configuration
    pub fn with_gc(mut self) -> Self {
        self.enable_gc = true;
        self.gc_config = Some(GcConfig::default());
        self
    }
    
    /// Enable garbage collection with custom configuration
    pub fn with_gc_config(mut self, config: GcConfig) -> Self {
        self.enable_gc = true;
        self.gc_config = Some(config);
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
        
        if let Some(registry) = self.stdlib_registry {
            vm.set_stdlib_registry(registry);
        }
        
        // Module loader is created with the provided config
        if let Some(config) = self.module_config {
            let loader = ModuleLoader::new(config);
            vm.set_module_loader(loader);
        }
        
        // Configure VM
        if self.trace_mode {
            vm.enable_trace();
        }
        
        // Set initial globals
        for (name, value) in self.initial_globals {
            vm.set_global(name, value);
        }
        
        // Apply security configuration
        if let Some(manager) = self.security_manager {
            vm.set_security_manager(manager);
        } else if let Some(policy) = self.security_policy {
            vm.set_security_manager(Arc::new(SecurityManager::new(policy)));
        }
        
        // Apply GC configuration
        if self.enable_gc {
            if let Some(config) = self.gc_config {
                vm.with_gc_config(config);
            } else {
                vm.with_gc();
            }
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

/// Sandboxed configuration for untrusted code
pub struct SandboxConfig {
    pub max_memory: u64,
    pub max_instructions: u64,
    pub allowed_modules: Vec<String>,
}

impl Default for SandboxConfig {
    fn default() -> Self {
        Self {
            max_memory: 10 * 1024 * 1024, // 10MB
            max_instructions: 1_000_000,
            allowed_modules: vec![],
        }
    }
}

impl VMConfig for SandboxConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        let mut policy = SecurityPolicy::sandbox();
        policy.max_memory = self.max_memory;
        policy.max_instructions = self.max_instructions;
        for module in &self.allowed_modules {
            policy.allowed_modules.insert(module.clone());
        }
        builder.security_policy = Some(policy);
        builder.trace_mode = false;
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
        assert_eq!(vm.get_global("version"), Some(&Value::String("1.0.0".to_string())));
    }
    
    #[test]
    fn test_vm_builder_with_config() {
        let bytecode = Bytecode::new();
        let _vm = VMBuilder::new()
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
        if let Err(e) = result {
            assert!(e.to_string().contains("Bytecode is required"));
        }
    }
}