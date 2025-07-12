//! Effect handler system for FluentAi
//!
//! This module provides a comprehensive effect handling system that supports
//! all effect types including IO, State, Error, Time, Network, Random, Dom,
//! Async, and Concurrent effects.

#![warn(missing_docs)]

use async_trait::async_trait;
use dashmap::DashMap;
pub use fluentai_core::ast::EffectType;
use fluentai_core::{error::Error, value::Value, Result};
use std::fmt;
use std::sync::Arc;

pub mod handlers;
pub mod provider;
pub mod reactive;
pub mod runtime;

pub use handlers::*;
pub use provider::{EffectHandlerBuilder, EffectHandlerFactory, EffectHandlerProvider};
pub use runtime::EffectRuntime;

/// Effect operation result
pub type EffectResult = Result<Value>;

/// Helper function to format effect error messages consistently
pub fn format_effect_error(effect_type: &str, operation: &str, message: &str) -> String {
    format!("{}: '{}' - {}", effect_type, operation, message)
}

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

impl fmt::Display for EffectContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut handlers: Vec<String> = self
            .handlers
            .iter()
            .map(|entry| format!("{:?}", entry.key()))
            .collect();
        handlers.sort();

        if handlers.is_empty() {
            write!(f, "EffectContext {{ no handlers registered }}")
        } else {
            write!(f, "EffectContext {{ handlers: {} }}", handlers.join(", "))
        }
    }
}

impl fmt::Debug for EffectContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EffectContext")
            .field("handler_count", &self.handlers.len())
            .finish()
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
        let result = context.perform_async(EffectType::Time, "now", &[]).await;

        assert!(result.is_ok());
    }

    #[test]
    fn test_error_message_formatting() {
        // Test the format_effect_error helper
        let msg = format_effect_error("IO", "read", "file not found");
        assert_eq!(msg, "IO: 'read' - file not found");

        let msg2 = format_effect_error("State", "get", "key is required");
        assert_eq!(msg2, "State: 'get' - key is required");
    }

    #[test]
    fn test_unknown_operation_errors() {
        let context = EffectContext::default();

        // Test unknown IO operation
        let result = context.perform_sync(EffectType::IO, "unknown_op", &[]);
        assert!(result.is_err());
        match result {
            Err(Error::Runtime(msg)) => {
                assert!(msg.contains("IO: 'unknown_op' - operation not supported"));
            }
            _ => panic!("Expected Runtime error"),
        }

        // Test unknown State operation
        let result = context.perform_sync(EffectType::State, "unknown_state_op", &[]);
        assert!(result.is_err());
        match result {
            Err(Error::Runtime(msg)) => {
                assert!(msg.contains("State: 'unknown_state_op' - operation not supported"));
            }
            _ => panic!("Expected Runtime error"),
        }
    }

    #[test]
    fn test_missing_handler_error() {
        let context = EffectContext::new(); // Empty context

        let result = context.perform_sync(
            EffectType::IO,
            "print",
            &[Value::String("test".to_string())],
        );
        assert!(result.is_err());
        match result {
            Err(Error::Runtime(msg)) => {
                assert!(msg.contains("No handler registered for effect type"));
                assert!(msg.contains("IO"));
            }
            _ => panic!("Expected Runtime error for missing handler"),
        }
    }

    #[test]
    fn test_invalid_argument_errors() {
        let context = EffectContext::default();

        // Test IO read_file with non-string argument
        let result = context.perform_sync(EffectType::IO, "read_file", &[Value::Integer(42)]);
        assert!(result.is_err());
        match result {
            Err(Error::Runtime(msg)) => {
                assert!(msg.contains("IO: 'read_file' - requires a string path"));
            }
            _ => panic!("Expected Runtime error"),
        }

        // Test State get with non-string key
        let result = context.perform_sync(EffectType::State, "get", &[Value::Boolean(true)]);
        assert!(result.is_err());
        match result {
            Err(Error::Runtime(msg)) => {
                assert!(msg.contains("State: 'get' - requires a string key"));
            }
            _ => panic!("Expected Runtime error"),
        }

        // Test State set with missing value
        let result = context.perform_sync(
            EffectType::State,
            "set",
            &[Value::String("key".to_string())],
        );
        assert!(result.is_err());
        match result {
            Err(Error::Runtime(msg)) => {
                assert!(msg.contains("State: 'set' - requires key and value"));
            }
            _ => panic!("Expected Runtime error"),
        }
    }

    #[test]
    fn test_effect_context_display() {
        let context = EffectContext::default();
        let display = format!("{}", context);
        assert!(display.contains("EffectContext"));
        assert!(display.contains("handlers:"));

        let empty_context = EffectContext::new();
        let empty_display = format!("{}", empty_context);
        assert!(empty_display.contains("no handlers registered"));
    }

    #[test]
    fn test_effect_context_debug() {
        let context = EffectContext::default();
        let debug = format!("{:?}", context);
        assert!(debug.contains("EffectContext"));
        assert!(debug.contains("handler_count"));
    }
}
