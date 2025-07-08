//! Dependency injection integration for the VM

use std::sync::Arc;
use fluentai_di::prelude::*;
use fluentai_effects::{EffectContext, runtime::EffectRuntime};
use fluentai_effects::provider::EffectHandlerProvider;
use fluentai_stdlib::StdlibRegistry;
use fluentai_modules::{ModuleLoader, ModuleResolver, ModuleConfig};
use anyhow::Result;

use crate::builder::{VMBuilder, VMConfig};

/// VM services registration extension trait
pub trait VMContainerBuilderExt {
    /// Register VM-related services
    fn register_vm_services(&mut self) -> &mut Self;
    
    /// Register VM services with custom configuration
    fn register_vm_services_with_config<C: VMConfig + Clone + Send + Sync + 'static>(&mut self, config: C) -> &mut Self;
}

impl VMContainerBuilderExt for ContainerBuilder {
    fn register_vm_services(&mut self) -> &mut Self {
        // Register default configurations
        self.register_instance(ModuleConfig::default());
        
        // Register effect context as singleton
        self.register_singleton(|| {
            Arc::new(EffectContext::default())
        });
        
        // Register effect runtime as singleton
        self.register_singleton(|| {
            Arc::new(EffectRuntime::default())
        });
        
        // Register VM builder factory
        self.register_transient(|| {
            VMBuilder::new()
        });
        
        self
    }
    
    fn register_vm_services_with_config<C: VMConfig + Clone + Send + Sync + 'static>(&mut self, config: C) -> &mut Self {
        self.register_vm_services();
        
        // Register the config as a service
        self.register_instance(config);
        
        self
    }
}

/// VM service provider trait
pub trait VMServiceProvider: Send + Sync {
    /// Create a VM builder with injected dependencies
    fn create_vm_builder(&self) -> Result<VMBuilder>;
    
    /// Get the module loader
    fn get_module_loader(&self) -> Result<ModuleLoader>;
    
    /// Get the module resolver
    fn get_module_resolver(&self) -> Result<ModuleResolver>;
    
    /// Get the effect context
    fn get_effect_context(&self) -> Result<Arc<EffectContext>>;
    
    /// Get the effect runtime
    fn get_effect_runtime(&self) -> Result<Arc<EffectRuntime>>;
}

/// Default VM service provider implementation
pub struct ContainerVMProvider {
    container: Arc<Container>,
}

impl ContainerVMProvider {
    pub fn new(container: Arc<Container>) -> Self {
        Self { container }
    }
}

impl VMServiceProvider for ContainerVMProvider {
    fn create_vm_builder(&self) -> Result<VMBuilder> {
        let mut builder = VMBuilder::new();
        
        // Try to inject optional services
        if let Ok(effect_context) = self.container.resolve::<Arc<EffectContext>>() {
            builder = builder.with_effect_context(effect_context);
        }
        
        if let Ok(effect_runtime) = self.container.resolve::<Arc<EffectRuntime>>() {
            builder = builder.with_effect_runtime(effect_runtime);
        }
        
        // Module config can be resolved from DI
        if let Ok(config) = self.container.resolve::<ModuleConfig>() {
            builder = builder.with_module_config(config);
        }
        
        // Check if effect handler provider is available
        // Note: In the future, we could enhance EffectRuntime to accept a provider
        if let Ok(_handler_provider) = self.container.resolve::<Arc<EffectHandlerProvider>>() {
            // For now, just use the default runtime
            // TODO: Integrate EffectHandlerProvider with EffectRuntime
        }
        
        // Check if stdlib registry is available directly
        if let Ok(registry) = self.container.resolve::<StdlibRegistry>() {
            builder = builder.with_stdlib_registry(registry);
        }
        
        Ok(builder)
    }
    
    fn get_module_loader(&self) -> Result<ModuleLoader> {
        // ModuleLoader doesn't implement Clone, so create a new instance
        Ok(ModuleLoader::new(ModuleConfig::default()))
    }
    
    fn get_module_resolver(&self) -> Result<ModuleResolver> {
        // ModuleResolver doesn't implement Clone, so create a new instance
        let loader = ModuleLoader::new(ModuleConfig::default());
        Ok(ModuleResolver::new(loader))
    }
    
    fn get_effect_context(&self) -> Result<Arc<EffectContext>> {
        self.container.resolve::<Arc<EffectContext>>()
            .map_err(|e| anyhow::anyhow!("Failed to resolve EffectContext: {}", e))
    }
    
    fn get_effect_runtime(&self) -> Result<Arc<EffectRuntime>> {
        self.container.resolve::<Arc<EffectRuntime>>()
            .map_err(|e| anyhow::anyhow!("Failed to resolve EffectRuntime: {}", e))
    }
}

/// Configuration for development VM
#[derive(Clone)]
pub struct DevelopmentVMConfig {
    pub trace_enabled: bool,
    pub stack_size: usize,
}

impl Default for DevelopmentVMConfig {
    fn default() -> Self {
        Self {
            trace_enabled: true,
            stack_size: 1024 * 1024, // 1MB
        }
    }
}

impl VMConfig for DevelopmentVMConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        *builder = builder.clone()
            .with_trace_mode(self.trace_enabled)
            .with_stack_size(self.stack_size);
    }
}

/// Configuration for production VM
#[derive(Clone)]
pub struct ProductionVMConfig {
    pub stack_size: usize,
    pub enable_optimizations: bool,
}

impl Default for ProductionVMConfig {
    fn default() -> Self {
        Self {
            stack_size: 10 * 1024 * 1024, // 10MB
            enable_optimizations: true,
        }
    }
}

impl VMConfig for ProductionVMConfig {
    fn configure(&self, builder: &mut VMBuilder) {
        *builder = builder.clone()
            .with_trace_mode(false)
            .with_stack_size(self.stack_size);
        // Future: Add optimization flags
    }
}

/// Factory for creating VM instances
pub trait VMFactory: Service {
    /// Create a new VM instance with the given bytecode
    fn create_vm(&self, bytecode: crate::Bytecode) -> Result<crate::VM>;
}

/// Default VM factory implementation
pub struct DefaultVMFactory {
    provider: Arc<dyn VMServiceProvider>,
}

impl DefaultVMFactory {
    pub fn new(provider: Arc<dyn VMServiceProvider>) -> Self {
        Self { provider }
    }
}

impl VMFactory for DefaultVMFactory {
    fn create_vm(&self, bytecode: crate::Bytecode) -> Result<crate::VM> {
        let builder = self.provider.create_vm_builder()?;
        builder.with_bytecode(bytecode).build()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::Bytecode;
    
    #[test]
    fn test_vm_di_registration() {
        let mut builder = ContainerBuilder::new();
        builder.register_vm_services();
        
        let container = Arc::new(builder.build());
        
        // Module loader resolution would require Clone implementation
        // assert!(container.resolve::<ModuleLoader>().is_ok());
        
        // Should be able to resolve effect context
        assert!(container.resolve::<Arc<EffectContext>>().is_ok());
        
        // Should be able to resolve VM builder
        assert!(container.resolve::<VMBuilder>().is_ok());
    }
    
    #[test]
    fn test_vm_provider() {
        let mut builder = ContainerBuilder::new();
        builder.register_vm_services();
        
        let container = Arc::new(builder.build());
        let provider = ContainerVMProvider::new(container);
        
        // Should be able to create VM builder
        let vm_builder = provider.create_vm_builder().unwrap();
        
        // Should be able to build VM with bytecode
        let bytecode = Bytecode::new();
        let vm = vm_builder.with_bytecode(bytecode).build();
        assert!(vm.is_ok());
    }
    
    #[test]
    fn test_vm_factory() {
        let mut builder = ContainerBuilder::new();
        builder.register_vm_services();
        
        let container = Arc::new(builder.build());
        let provider = Arc::new(ContainerVMProvider::new(container.clone()));
        let factory = DefaultVMFactory::new(provider);
        
        // Should be able to create VM through factory
        let bytecode = Bytecode::new();
        let vm = factory.create_vm(bytecode);
        assert!(vm.is_ok());
    }
}