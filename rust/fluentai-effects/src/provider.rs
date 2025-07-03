//! Effect handler provider for dynamic injection

use std::sync::Arc;
use std::collections::HashMap;
use parking_lot::RwLock;
use crate::{EffectHandler, EffectContext, EffectType};
use fluentai_core::error::{Error, Result};

/// Factory function for creating effect handlers
pub type EffectHandlerFactory = Box<dyn Fn() -> Arc<dyn EffectHandler> + Send + Sync>;

/// Provider for effect handlers with dynamic registration
#[derive(Clone)]
pub struct EffectHandlerProvider {
    /// Registered handler factories
    factories: Arc<RwLock<HashMap<EffectType, EffectHandlerFactory>>>,
    /// Handler instances (for singleton handlers)
    instances: Arc<RwLock<HashMap<EffectType, Arc<dyn EffectHandler>>>>,
    /// Parent provider for hierarchical resolution
    parent: Option<Arc<EffectHandlerProvider>>,
}

impl EffectHandlerProvider {
    /// Create a new provider
    pub fn new() -> Self {
        Self {
            factories: Arc::new(RwLock::new(HashMap::new())),
            instances: Arc::new(RwLock::new(HashMap::new())),
            parent: None,
        }
    }
    
    /// Create a child provider
    pub fn create_child(parent: Arc<EffectHandlerProvider>) -> Self {
        Self {
            factories: Arc::new(RwLock::new(HashMap::new())),
            instances: Arc::new(RwLock::new(HashMap::new())),
            parent: Some(parent),
        }
    }
    
    /// Register a handler factory
    pub fn register_factory<F>(&self, effect_type: EffectType, factory: F)
    where
        F: Fn() -> Arc<dyn EffectHandler> + Send + Sync + 'static,
    {
        self.factories.write().insert(effect_type, Box::new(factory));
    }
    
    /// Register a singleton handler
    pub fn register_singleton(&self, handler: Arc<dyn EffectHandler>) {
        let effect_type = handler.effect_type();
        self.instances.write().insert(effect_type, handler);
    }
    
    /// Get or create a handler
    pub fn get_handler(&self, effect_type: EffectType) -> Result<Arc<dyn EffectHandler>> {
        // Check instances first
        if let Some(handler) = self.instances.read().get(&effect_type) {
            return Ok(Arc::clone(handler));
        }
        
        // Check factories
        if let Some(factory) = self.factories.read().get(&effect_type) {
            let handler = factory();
            // Cache singleton instances
            self.instances.write().insert(effect_type, Arc::clone(&handler));
            return Ok(handler);
        }
        
        // Check parent
        if let Some(parent) = &self.parent {
            return parent.get_handler(effect_type);
        }
        
        Err(Error::Runtime(format!(
            "No handler registered for effect type {:?}",
            effect_type
        )))
    }
    
    /// Create an effect context with all registered handlers
    pub fn create_context(&self) -> Result<EffectContext> {
        let context = EffectContext::new();
        
        // Get all effect types from this provider and parents
        let mut effect_types = std::collections::HashSet::new();
        self.collect_effect_types(&mut effect_types);
        
        // Register handlers for all effect types
        for effect_type in effect_types {
            if let Ok(handler) = self.get_handler(effect_type) {
                context.register_handler(handler);
            }
        }
        
        Ok(context)
    }
    
    /// Collect all effect types from this provider and parents
    fn collect_effect_types(&self, types: &mut std::collections::HashSet<EffectType>) {
        // Add from instances
        for effect_type in self.instances.read().keys() {
            types.insert(*effect_type);
        }
        
        // Add from factories
        for effect_type in self.factories.read().keys() {
            types.insert(*effect_type);
        }
        
        // Add from parent
        if let Some(parent) = &self.parent {
            parent.collect_effect_types(types);
        }
    }
    
    /// Remove a handler
    pub fn remove_handler(&self, effect_type: EffectType) {
        self.instances.write().remove(&effect_type);
        self.factories.write().remove(&effect_type);
    }
    
    /// Clear all handlers
    pub fn clear(&self) {
        self.instances.write().clear();
        self.factories.write().clear();
    }
}

impl Default for EffectHandlerProvider {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for configuring effect handlers
pub struct EffectHandlerBuilder {
    provider: EffectHandlerProvider,
}

impl EffectHandlerBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            provider: EffectHandlerProvider::new(),
        }
    }
    
    /// Add a handler factory
    pub fn with_factory<F>(self, effect_type: EffectType, factory: F) -> Self
    where
        F: Fn() -> Arc<dyn EffectHandler> + Send + Sync + 'static,
    {
        self.provider.register_factory(effect_type, factory);
        self
    }
    
    /// Add a singleton handler
    pub fn with_handler(self, handler: Arc<dyn EffectHandler>) -> Self {
        self.provider.register_singleton(handler);
        self
    }
    
    /// Add default handlers
    pub fn with_defaults(self) -> Self {
        self.with_handler(Arc::new(crate::handlers::IOHandler::new()))
            .with_handler(Arc::new(crate::handlers::StateHandler::new()))
            .with_handler(Arc::new(crate::handlers::ErrorHandler::new()))
            .with_handler(Arc::new(crate::handlers::TimeHandler::new()))
            .with_handler(Arc::new(crate::handlers::RandomHandler::new()))
            .with_handler(Arc::new(crate::handlers::NetworkHandler::new()))
            .with_handler(Arc::new(crate::handlers::AsyncHandler::new()))
            .with_handler(Arc::new(crate::handlers::ConcurrentHandler::new()))
            .with_handler(Arc::new(crate::handlers::DomHandler::new()))
    }
    
    /// Build the provider
    pub fn build(self) -> EffectHandlerProvider {
        self.provider
    }
}

impl Default for EffectHandlerBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Integration with DI container
#[cfg(feature = "di")]
pub mod di {
    use super::*;
    use fluentai_di::prelude::*;
    
    /// Register effect services with DI container
    pub fn register_effect_services(builder: &mut ContainerBuilder) -> Result<()> {
        // Register provider as singleton
        builder.register_singleton(|| {
            Arc::new(EffectHandlerBuilder::new()
                .with_defaults()
                .build())
        });
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::EffectType;
    use fluentai_core::value::Value;
    
    /// Test handler for testing
    struct TestHandler {
        name: String,
    }
    
    impl TestHandler {
        fn new(name: &str) -> Self {
            Self { name: name.to_string() }
        }
    }
    
    #[async_trait::async_trait]
    impl EffectHandler for TestHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::IO
        }
        
        fn handle_sync(&self, operation: &str, _args: &[Value]) -> crate::EffectResult {
            Ok(Value::String(format!("{} handled {}", self.name, operation)))
        }
    }
    
    #[test]
    fn test_provider_registration() {
        let provider = EffectHandlerProvider::new();
        
        // Register singleton
        provider.register_singleton(Arc::new(TestHandler::new("singleton")));
        
        // Register factory
        provider.register_factory(EffectType::State, || {
            Arc::new(TestHandler::new("factory"))
        });
        
        // Get handlers
        let io_handler = provider.get_handler(EffectType::IO).unwrap();
        let state_handler = provider.get_handler(EffectType::State).unwrap();
        
        // Test handlers
        let io_result = io_handler.handle_sync("test", &[]);
        assert!(io_result.is_ok());
        
        let state_result = state_handler.handle_sync("test", &[]);
        assert!(state_result.is_ok());
    }
    
    #[test]
    fn test_hierarchical_providers() {
        let parent = Arc::new(EffectHandlerProvider::new());
        parent.register_singleton(Arc::new(TestHandler::new("parent")));
        
        let child = EffectHandlerProvider::create_child(parent);
        child.register_singleton(Arc::new(TestHandler::new("child")));
        
        // Child can access parent's handlers
        let handler = child.get_handler(EffectType::IO).unwrap();
        let result = handler.handle_sync("test", &[]);
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_builder() {
        let provider = EffectHandlerBuilder::new()
            .with_handler(Arc::new(TestHandler::new("test")))
            .with_factory(EffectType::State, || Arc::new(TestHandler::new("factory")))
            .build();
        
        let context = provider.create_context().unwrap();
        let result = context.perform_sync(EffectType::IO, "test", &[]);
        assert!(result.is_ok());
    }
}