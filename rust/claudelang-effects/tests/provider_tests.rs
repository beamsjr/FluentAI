//! Integration tests for EffectHandlerProvider

use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use claudelang_effects::{
    EffectHandler, EffectHandlerProvider, EffectHandlerBuilder,
    EffectResult,
};
use claudelang_core::{ast::EffectType, value::Value};
use async_trait::async_trait;

/// Test handler that tracks invocation count
struct CountingHandler {
    effect_type: EffectType,
    invocations: Arc<AtomicUsize>,
}

impl CountingHandler {
    fn new(effect_type: EffectType) -> Self {
        Self {
            effect_type,
            invocations: Arc::new(AtomicUsize::new(0)),
        }
    }
    
    fn invocation_count(&self) -> usize {
        self.invocations.load(Ordering::SeqCst)
    }
}

#[async_trait]
impl EffectHandler for CountingHandler {
    fn effect_type(&self) -> EffectType {
        self.effect_type
    }
    
    fn handle_sync(&self, operation: &str, _args: &[Value]) -> EffectResult {
        self.invocations.fetch_add(1, Ordering::SeqCst);
        Ok(Value::String(format!("Handled {} with count", operation)))
    }
}

#[test]
fn test_singleton_registration() {
    let provider = EffectHandlerProvider::new();
    let handler = Arc::new(CountingHandler::new(EffectType::IO));
    let handler_clone = handler.clone();
    
    provider.register_singleton(handler);
    
    // Get handler multiple times
    let h1 = provider.get_handler(EffectType::IO).unwrap();
    let h2 = provider.get_handler(EffectType::IO).unwrap();
    
    // Should be the same instance
    assert!(Arc::ptr_eq(&h1, &h2));
    
    // Test invocations
    h1.handle_sync("test", &[]).unwrap();
    h2.handle_sync("test", &[]).unwrap();
    
    assert_eq!(handler_clone.invocation_count(), 2);
}

#[test]
fn test_factory_registration() {
    let provider = EffectHandlerProvider::new();
    let creation_count = Arc::new(AtomicUsize::new(0));
    let creation_count_clone = creation_count.clone();
    
    provider.register_factory(EffectType::State, move || {
        creation_count_clone.fetch_add(1, Ordering::SeqCst);
        Arc::new(CountingHandler::new(EffectType::State))
    });
    
    // First call creates handler
    let h1 = provider.get_handler(EffectType::State).unwrap();
    assert_eq!(creation_count.load(Ordering::SeqCst), 1);
    
    // Second call reuses cached handler
    let h2 = provider.get_handler(EffectType::State).unwrap();
    assert_eq!(creation_count.load(Ordering::SeqCst), 1);
    
    // Should be the same instance
    assert!(Arc::ptr_eq(&h1, &h2));
}

#[test]
fn test_hierarchical_resolution() {
    let parent = Arc::new(EffectHandlerProvider::new());
    parent.register_singleton(Arc::new(CountingHandler::new(EffectType::IO)));
    parent.register_singleton(Arc::new(CountingHandler::new(EffectType::State)));
    
    let child = EffectHandlerProvider::create_child(parent.clone());
    child.register_singleton(Arc::new(CountingHandler::new(EffectType::State))); // Override
    child.register_singleton(Arc::new(CountingHandler::new(EffectType::Error))); // New
    
    // Child has its own State handler
    let child_state = child.get_handler(EffectType::State).unwrap();
    let parent_state = parent.get_handler(EffectType::State).unwrap();
    assert!(!Arc::ptr_eq(&child_state, &parent_state));
    
    // Child inherits IO handler from parent
    let child_io = child.get_handler(EffectType::IO).unwrap();
    let parent_io = parent.get_handler(EffectType::IO).unwrap();
    assert!(Arc::ptr_eq(&child_io, &parent_io));
    
    // Parent doesn't have Error handler
    assert!(parent.get_handler(EffectType::Error).is_err());
    assert!(child.get_handler(EffectType::Error).is_ok());
}

#[test]
fn test_create_context() {
    let provider = EffectHandlerBuilder::new()
        .with_handler(Arc::new(CountingHandler::new(EffectType::IO)))
        .with_handler(Arc::new(CountingHandler::new(EffectType::State)))
        .with_factory(EffectType::Error, || {
            Arc::new(CountingHandler::new(EffectType::Error))
        })
        .build();
    
    let context = provider.create_context().unwrap();
    
    // Test that all handlers are available in context
    assert!(context.perform_sync(EffectType::IO, "test", &[]).is_ok());
    assert!(context.perform_sync(EffectType::State, "test", &[]).is_ok());
    assert!(context.perform_sync(EffectType::Error, "test", &[]).is_ok());
}

#[test]
fn test_remove_handler() {
    let provider = EffectHandlerProvider::new();
    provider.register_singleton(Arc::new(CountingHandler::new(EffectType::IO)));
    
    // Handler exists
    assert!(provider.get_handler(EffectType::IO).is_ok());
    
    // Remove handler
    provider.remove_handler(EffectType::IO);
    
    // Handler no longer exists
    assert!(provider.get_handler(EffectType::IO).is_err());
}

#[test]
fn test_clear_handlers() {
    let provider = EffectHandlerProvider::new();
    provider.register_singleton(Arc::new(CountingHandler::new(EffectType::IO)));
    provider.register_factory(EffectType::State, || {
        Arc::new(CountingHandler::new(EffectType::State))
    });
    
    // Handlers exist
    assert!(provider.get_handler(EffectType::IO).is_ok());
    assert!(provider.get_handler(EffectType::State).is_ok());
    
    // Clear all handlers
    provider.clear();
    
    // No handlers exist
    assert!(provider.get_handler(EffectType::IO).is_err());
    assert!(provider.get_handler(EffectType::State).is_err());
}

#[test]
fn test_builder_with_defaults() {
    let provider = EffectHandlerBuilder::new()
        .with_defaults()
        .build();
    
    let _context = provider.create_context().unwrap();
    
    // Test that default handlers are registered
    let effect_types = vec![
        EffectType::IO,
        EffectType::State,
        EffectType::Error,
        EffectType::Time,
        EffectType::Random,
        EffectType::Network,
        EffectType::Async,
        EffectType::Concurrent,
        EffectType::Dom,
    ];
    
    for effect_type in effect_types {
        assert!(provider.get_handler(effect_type).is_ok(),
            "Default handler for {:?} should be registered", effect_type);
    }
}

/// Test handler that can be configured
struct ConfigurableHandler {
    config: parking_lot::RwLock<String>,
}

impl ConfigurableHandler {
    fn new(config: &str) -> Self {
        Self {
            config: parking_lot::RwLock::new(config.to_string()),
        }
    }
    
    fn update_config(&self, new_config: &str) {
        *self.config.write() = new_config.to_string();
    }
}

#[async_trait]
impl EffectHandler for ConfigurableHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::IO
    }
    
    fn handle_sync(&self, _operation: &str, _args: &[Value]) -> EffectResult {
        Ok(Value::String(self.config.read().clone()))
    }
}

#[test]
fn test_handler_reconfiguration() {
    let provider = EffectHandlerProvider::new();
    let handler = Arc::new(ConfigurableHandler::new("initial"));
    provider.register_singleton(handler.clone());
    
    let context = provider.create_context().unwrap();
    
    // Initial config
    let result = context.perform_sync(EffectType::IO, "test", &[]).unwrap();
    match result {
        Value::String(s) => assert_eq!(s, "initial"),
        _ => panic!("Expected string value"),
    }
    
    // Update config
    handler.update_config("updated");
    
    // Config change is reflected
    let result = context.perform_sync(EffectType::IO, "test", &[]).unwrap();
    match result {
        Value::String(s) => assert_eq!(s, "updated"),
        _ => panic!("Expected string value"),
    }
}

#[cfg(feature = "di")]
#[test]
fn test_di_integration() {
    use claudelang_di::prelude::*;
    use claudelang_effects::provider::di::register_effect_services;
    
    let mut builder = ContainerBuilder::new();
    register_effect_services(&mut builder).unwrap();
    
    let container = builder.build();
    
    // Resolve provider
    let provider = container.resolve::<EffectHandlerProvider>().unwrap();
    assert!(provider.get_handler(EffectType::IO).is_ok());
    
    // Resolve context
    let context = container.resolve::<claudelang_effects::EffectContext>().unwrap();
    assert!(context.perform_sync(EffectType::IO, "print", &[
        Value::String("Test".to_string())
    ]).is_ok());
}