//! Effect handler system for ClaudeLang
//! 
//! This module provides a comprehensive effect handling system that supports
//! all effect types including IO, State, Error, Time, Network, Random, Dom,
//! Async, and Concurrent effects.

use async_trait::async_trait;
use claudelang_core::{ast::EffectType, error::Error, value::Value, Result};
use dashmap::DashMap;
use std::sync::Arc;

pub mod handlers;
pub mod runtime;
pub mod provider;

pub use handlers::*;
pub use runtime::EffectRuntime;
pub use provider::{EffectHandlerProvider, EffectHandlerBuilder, EffectHandlerFactory};

/// Effect operation result
pub type EffectResult = Result<Value>;

/// Base trait for all effect handlers
#[async_trait]
pub trait EffectHandler: Send + Sync {
    /// Get the effect type this handler manages
    fn effect_type(&self) -> EffectType;
    
    /// Handle a synchronous effect operation
    fn handle_sync(&self, operation: &str, _args: &[Value]) -> EffectResult {
        Err(Error::Runtime(format!(
            "Synchronous operation '{}' not supported for {:?}",
            operation,
            self.effect_type()
        )))
    }
    
    /// Handle an asynchronous effect operation
    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        // Default to sync handler for backwards compatibility
        self.handle_sync(operation, args)
    }
    
    /// Check if an operation is async
    fn is_async_operation(&self, _operation: &str) -> bool {
        false
    }
}

/// Effect context that manages all handlers
#[derive(Clone)]
pub struct EffectContext {
    handlers: Arc<DashMap<EffectType, Arc<dyn EffectHandler>>>,
}

impl EffectContext {
    /// Create a new effect context
    pub fn new() -> Self {
        Self {
            handlers: Arc::new(DashMap::new()),
        }
    }
    
    /// Register an effect handler
    pub fn register_handler(&self, handler: Arc<dyn EffectHandler>) {
        self.handlers.insert(handler.effect_type(), handler);
    }
    
    /// Perform a synchronous effect
    pub fn perform_sync(
        &self,
        effect_type: EffectType,
        operation: &str,
        args: &[Value],
    ) -> EffectResult {
        match self.handlers.get(&effect_type) {
            Some(handler) => handler.handle_sync(operation, args),
            None => Err(Error::Runtime(format!(
                "No handler registered for effect type {:?}",
                effect_type
            ))),
        }
    }
    
    /// Perform an asynchronous effect
    pub async fn perform_async(
        &self,
        effect_type: EffectType,
        operation: &str,
        args: &[Value],
    ) -> EffectResult {
        match self.handlers.get(&effect_type) {
            Some(handler) => handler.handle_async(operation, args).await,
            None => Err(Error::Runtime(format!(
                "No handler registered for effect type {:?}",
                effect_type
            ))),
        }
    }
    
    /// Check if an operation is async
    pub fn is_async_operation(&self, effect_type: EffectType, operation: &str) -> bool {
        match self.handlers.get(&effect_type) {
            Some(handler) => handler.is_async_operation(operation),
            None => false,
        }
    }
}

impl Default for EffectContext {
    fn default() -> Self {
        let context = Self::new();
        
        // Register default handlers
        context.register_handler(Arc::new(handlers::IOHandler::new()));
        context.register_handler(Arc::new(handlers::StateHandler::new()));
        context.register_handler(Arc::new(handlers::ErrorHandler::new()));
        context.register_handler(Arc::new(handlers::TimeHandler::new()));
        context.register_handler(Arc::new(handlers::RandomHandler::new()));
        context.register_handler(Arc::new(handlers::NetworkHandler::new()));
        context.register_handler(Arc::new(handlers::AsyncHandler::new()));
        context.register_handler(Arc::new(handlers::ConcurrentHandler::new()));
        context.register_handler(Arc::new(handlers::DomHandler::new()));
        
        context
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_effect_context_creation() {
        let context = EffectContext::default();
        
        // Test that default handlers are registered
        let io_result = context.perform_sync(
            EffectType::IO,
            "print",
            &[Value::String("Hello".to_string())],
        );
        assert!(io_result.is_ok());
    }
    
    #[tokio::test]
    async fn test_async_effect() {
        let context = EffectContext::default();
        
        // Test async operation
        let result = context.perform_async(
            EffectType::Time,
            "now",
            &[],
        ).await;
        
        assert!(result.is_ok());
    }
}
