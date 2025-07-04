//! Async effect handler for promises and futures

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, value::Value, error::Error};
use dashmap::DashMap;
use std::sync::Arc;
use tokio::sync::{oneshot, Mutex};
use uuid::Uuid;

type PromiseReceiver = oneshot::Receiver<Result<Value, String>>;

pub struct AsyncHandler {
    promises: Arc<DashMap<String, Arc<Mutex<PromiseReceiver>>>>,
}

impl AsyncHandler {
    pub fn new() -> Self {
        Self {
            promises: Arc::new(DashMap::new()),
        }
    }
}

#[async_trait]
impl EffectHandler for AsyncHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Async
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "promise" => {
                // Create a new promise
                let (tx, rx) = oneshot::channel();
                let promise_id = Uuid::new_v4().to_string();
                
                self.promises.insert(
                    promise_id.clone(),
                    Arc::new(Mutex::new(rx))
                );
                
                // In a real implementation, we'd execute the executor function
                // For now, just return the promise ID
                if args.is_empty() {
                    // Auto-resolve with nil
                    let _ = tx.send(Ok(Value::Nil));
                }
                
                Ok(Value::String(promise_id))
            }
            "resolve" => {
                if args.len() >= 2 {
                    if let Some(Value::String(_promise_id)) = args.first() {
                        // In a real implementation, we'd resolve the promise
                        Ok(Value::Nil)
                    } else {
                        Err(Error::Runtime("async:resolve requires promise ID".to_string()))
                    }
                } else {
                    Err(Error::Runtime("async:resolve requires 2 arguments".to_string()))
                }
            }
            "reject" => {
                if args.len() >= 2 {
                    if let Some(Value::String(_promise_id)) = args.first() {
                        // In a real implementation, we'd reject the promise
                        Ok(Value::Nil)
                    } else {
                        Err(Error::Runtime("async:reject requires promise ID".to_string()))
                    }
                } else {
                    Err(Error::Runtime("async:reject requires 2 arguments".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!("Unknown Async operation: {}", operation))),
        }
    }
    
    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "await" => {
                if let Some(Value::String(promise_id)) = args.first() {
                    if let Some(promise_entry) = self.promises.get(promise_id) {
                        let receiver = promise_entry.clone();
                        drop(promise_entry); // Release the dashmap lock
                        
                        let mut rx = receiver.lock().await;
                        
                        // Try to receive the value
                        match rx.try_recv() {
                            Ok(Ok(value)) => Ok(value),
                            Ok(Err(error)) => Err(Error::Runtime(error)),
                            Err(_) => Err(Error::Runtime("Promise not yet resolved".to_string())),
                        }
                    } else {
                        Err(Error::Runtime("Unknown promise ID".to_string()))
                    }
                } else {
                    Err(Error::Runtime("async:await requires promise ID".to_string()))
                }
            }
            "all" => {
                // Wait for all promises
                let mut results = Vec::new();
                for arg in args {
                    if let Value::String(_promise_id) = arg {
                        let result = self.handle_async("await", &[arg.clone()]).await?;
                        results.push(result);
                    }
                }
                Ok(Value::List(results))
            }
            "race" => {
                // Return first resolved promise
                // In a real implementation, we'd use tokio::select!
                if let Some(first) = args.first() {
                    self.handle_async("await", &[first.clone()]).await
                } else {
                    Err(Error::Runtime("async:race requires at least one promise".to_string()))
                }
            }
            _ => self.handle_sync(operation, args),
        }
    }
    
    fn is_async_operation(&self, operation: &str) -> bool {
        matches!(operation, "await" | "all" | "race")
    }
}