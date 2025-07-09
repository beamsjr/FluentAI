//! Reactive effect handler for state management with automatic dependency tracking

use crate::reactive::{ReactiveContext, ReactiveState};
use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};

pub struct ReactiveHandler {
    context: ReactiveContext,
    state: ReactiveState,
}

impl ReactiveHandler {
    pub fn new() -> Self {
        let context = ReactiveContext::new();
        let state = ReactiveState::with_context(&context);

        Self { context, state }
    }

    /// Get the reactive context
    pub fn context(&self) -> &ReactiveContext {
        &self.context
    }

    /// Get the reactive state
    pub fn state(&self) -> &ReactiveState {
        &self.state
    }
}

#[async_trait]
impl EffectHandler for ReactiveHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::State
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            // Basic reactive state operations
            "reactive:ref" => {
                // Create a reactive reference
                let initial = args.first().cloned().unwrap_or(Value::Nil);
                let key = format!("ref_{}", uuid::Uuid::new_v4());
                self.state.set(key.clone(), initial);
                Ok(Value::String(key))
            }

            "reactive:get" => {
                // Get reactive value with dependency tracking
                if let Some(Value::String(key)) = args.first() {
                    Ok(self.state.get(key).unwrap_or(Value::Nil))
                } else {
                    Err(Error::Runtime(
                        "reactive:get requires a string key".to_string(),
                    ))
                }
            }

            "reactive:set" => {
                // Set reactive value and trigger updates
                if args.len() >= 2 {
                    if let Some(Value::String(key)) = args.first() {
                        let value = args[1].clone();
                        self.state.set(key.clone(), value.clone());
                        Ok(value)
                    } else {
                        Err(Error::Runtime(
                            "reactive:set requires a string key".to_string(),
                        ))
                    }
                } else {
                    Err(Error::Runtime(
                        "reactive:set requires key and value".to_string(),
                    ))
                }
            }

            "reactive:update" => {
                // Update reactive value with a function
                if args.len() >= 2 {
                    if let (Some(Value::String(key)), Some(update_fn)) = (args.get(0), args.get(1))
                    {
                        // In a real implementation, we'd apply the function
                        // For now, we'll just set the new value
                        self.state.set(key.clone(), update_fn.clone());
                        Ok(update_fn.clone())
                    } else {
                        Err(Error::Runtime(
                            "reactive:update requires key and function".to_string(),
                        ))
                    }
                } else {
                    Err(Error::Runtime(
                        "reactive:update requires key and function".to_string(),
                    ))
                }
            }

            "reactive:delete" => {
                // Delete reactive value
                if let Some(Value::String(key)) = args.first() {
                    Ok(self.state.delete(key).unwrap_or(Value::Nil))
                } else {
                    Err(Error::Runtime(
                        "reactive:delete requires a string key".to_string(),
                    ))
                }
            }

            "reactive:computed" => {
                // Create a computed value
                // In a real implementation, this would take a function
                // For now, return a placeholder
                Ok(Value::String(format!("computed_{}", uuid::Uuid::new_v4())))
            }

            "reactive:watch" => {
                // Create a watcher
                // In a real implementation, this would take a callback
                // For now, return a placeholder
                Ok(Value::String(format!("watcher_{}", uuid::Uuid::new_v4())))
            }

            "reactive:batch" => {
                // Batch multiple updates
                // Execute all operations in args within a batch
                let scheduler = self.context.scheduler.clone();
                let batch = crate::reactive::BatchScope::new(scheduler);

                // In a real implementation, we'd execute the batched operations
                batch.run(|| Ok(Value::Boolean(true)))
            }

            "reactive:flush" => {
                // Force synchronous flush of all pending updates
                self.context.scheduler.flush_sync();
                Ok(Value::Nil)
            }

            // Also handle regular state operations for compatibility
            "get" => {
                if let Some(Value::String(key)) = args.first() {
                    Ok(self.state.get(key).unwrap_or(Value::Nil))
                } else {
                    Err(Error::Runtime("get requires a string key".to_string()))
                }
            }

            "set" => {
                if args.len() >= 2 {
                    if let Some(Value::String(key)) = args.first() {
                        let value = args[1].clone();
                        self.state.set(key.clone(), value.clone());
                        Ok(value)
                    } else {
                        Err(Error::Runtime("set requires a string key".to_string()))
                    }
                } else {
                    Err(Error::Runtime("set requires key and value".to_string()))
                }
            }

            "delete" => {
                if let Some(Value::String(key)) = args.first() {
                    Ok(self.state.delete(key).unwrap_or(Value::Nil))
                } else {
                    Err(Error::Runtime("delete requires a string key".to_string()))
                }
            }

            "clear" => {
                self.state.clear();
                Ok(Value::Nil)
            }

            _ => Err(Error::Runtime(format!(
                "Unknown reactive operation: {}",
                operation
            ))),
        }
    }
}

impl Default for ReactiveHandler {
    fn default() -> Self {
        Self::new()
    }
}
