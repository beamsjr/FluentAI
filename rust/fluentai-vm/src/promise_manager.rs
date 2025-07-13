//! Promise management for FluentAI VM
//!
//! This module provides the infrastructure for managing promise lifecycles,
//! including creation, resolution, rejection, and async execution.

use crate::error::{VMError, VMResult};
use crate::safety::PromiseId;
use fluentai_core::value::Value;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tokio::sync::{oneshot, Mutex};
use tokio::time::{Duration, timeout};

/// State of a promise
#[derive(Debug, Clone)]
pub enum PromiseState {
    /// Promise is still pending
    Pending,
    /// Promise resolved with a value
    Resolved(Value),
    /// Promise rejected with an error
    Rejected(String),
}

/// A promise handle that can be resolved or rejected
pub struct PromiseHandle {
    /// The promise ID
    pub id: PromiseId,
    /// Sender for resolving/rejecting the promise
    sender: Option<oneshot::Sender<VMResult<Value>>>,
}

impl PromiseHandle {
    /// Resolve the promise with a value
    pub fn resolve(mut self, value: Value) -> VMResult<()> {
        if let Some(sender) = self.sender.take() {
            sender.send(Ok(value)).map_err(|_| VMError::AsyncError {
                message: "Failed to resolve promise: receiver dropped".to_string(),
                stack_trace: None,
            })?;
        }
        Ok(())
    }
    
    /// Reject the promise with an error
    pub fn reject(mut self, error: String) -> VMResult<()> {
        if let Some(sender) = self.sender.take() {
            sender.send(Err(VMError::RuntimeError {
                message: error,
                stack_trace: None,
            })).map_err(|_| VMError::AsyncError {
                message: "Failed to reject promise: receiver dropped".to_string(),
                stack_trace: None,
            })?;
        }
        Ok(())
    }
}

/// Promise manager handles the lifecycle of promises
pub struct PromiseManager {
    /// Map of promise IDs to their receivers
    promises: FxHashMap<PromiseId, oneshot::Receiver<VMResult<Value>>>,
    /// Map of promise IDs to their current state (for debugging/inspection)
    states: Arc<Mutex<FxHashMap<PromiseId, PromiseState>>>,
}

impl PromiseManager {
    /// Create a new promise manager
    pub fn new() -> Self {
        Self {
            promises: FxHashMap::default(),
            states: Arc::new(Mutex::new(FxHashMap::default())),
        }
    }
    
    /// Create a new promise and return its ID and handle
    pub async fn create_promise(&mut self, promise_id: PromiseId) -> (PromiseId, PromiseHandle) {
        let (tx, rx) = oneshot::channel();
        self.promises.insert(promise_id, rx);
        
        // Mark as pending
        self.states.lock().await.insert(promise_id, PromiseState::Pending);
        
        let handle = PromiseHandle {
            id: promise_id,
            sender: Some(tx),
        };
        
        (promise_id, handle)
    }
    
    /// Wait for a promise to resolve
    pub async fn await_promise(&mut self, promise_id: PromiseId) -> VMResult<Value> {
        if let Some(receiver) = self.promises.remove(&promise_id) {
            match receiver.await {
                Ok(result) => {
                    // Update state
                    let mut states = self.states.lock().await;
                    match &result {
                        Ok(value) => {
                            states.insert(promise_id, PromiseState::Resolved(value.clone()));
                        }
                        Err(e) => {
                            states.insert(promise_id, PromiseState::Rejected(e.to_string()));
                        }
                    }
                    result
                }
                Err(_) => {
                    // Update state
                    self.states.lock().await.insert(
                        promise_id,
                        PromiseState::Rejected("Promise cancelled".to_string())
                    );
                    Err(VMError::AsyncError {
                        message: "Promise was cancelled".to_string(),
                        stack_trace: None,
                    })
                }
            }
        } else {
            // Check if we have a cached state
            let states = self.states.lock().await;
            if let Some(state) = states.get(&promise_id) {
                match state {
                    PromiseState::Resolved(value) => Ok(value.clone()),
                    PromiseState::Rejected(error) => Err(VMError::RuntimeError {
                        message: error.clone(),
                        stack_trace: None,
                    }),
                    PromiseState::Pending => Err(VMError::AsyncError {
                        message: "Promise already being awaited elsewhere".to_string(),
                        stack_trace: None,
                    }),
                }
            } else {
                Err(VMError::AsyncError {
                    message: format!("Unknown promise ID: {:?}", promise_id),
                    stack_trace: None,
                })
            }
        }
    }
    
    /// Wait for multiple promises to resolve (Promise.all)
    pub async fn await_all(&mut self, promise_ids: Vec<PromiseId>) -> VMResult<Vec<Value>> {
        let mut results = Vec::with_capacity(promise_ids.len());
        
        // Collect all futures
        let mut futures = Vec::new();
        for id in promise_ids {
            if let Some(receiver) = self.promises.remove(&id) {
                futures.push(receiver);
            } else {
                // Check cached state
                let states = self.states.lock().await;
                if let Some(state) = states.get(&id) {
                    match state {
                        PromiseState::Resolved(value) => {
                            results.push(value.clone());
                            continue;
                        }
                        PromiseState::Rejected(error) => {
                            return Err(VMError::RuntimeError {
                                message: format!("Promise rejected: {}", error),
                                stack_trace: None,
                            });
                        }
                        PromiseState::Pending => {
                            return Err(VMError::AsyncError {
                                message: "Promise already being awaited".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                } else {
                    return Err(VMError::AsyncError {
                        message: format!("Unknown promise ID: {:?}", id),
                        stack_trace: None,
                    });
                }
            }
        }
        
        // Wait for all promises
        for future in futures {
            match future.await {
                Ok(Ok(value)) => results.push(value),
                Ok(Err(e)) => return Err(e),
                Err(_) => {
                    return Err(VMError::AsyncError {
                        message: "Promise was cancelled".to_string(),
                        stack_trace: None,
                    });
                }
            }
        }
        
        Ok(results)
    }
    
    /// Wait for the first promise to resolve (Promise.race)
    pub async fn await_race(&mut self, promise_ids: Vec<PromiseId>) -> VMResult<Value> {
        if promise_ids.is_empty() {
            return Err(VMError::RuntimeError {
                message: "Promise.race requires at least one promise".to_string(),
                stack_trace: None,
            });
        }
        
        // Check for already resolved promises first
        let states = self.states.lock().await;
        for id in &promise_ids {
            if let Some(state) = states.get(id) {
                match state {
                    PromiseState::Resolved(value) => return Ok(value.clone()),
                    PromiseState::Rejected(error) => {
                        return Err(VMError::RuntimeError {
                            message: error.clone(),
                            stack_trace: None,
                        });
                    }
                    _ => {}
                }
            }
        }
        drop(states);
        
        // Create futures for all promises
        let mut futures = Vec::new();
        for id in promise_ids {
            if let Some(receiver) = self.promises.remove(&id) {
                futures.push(Box::pin(async move {
                    receiver.await.map_err(|_| VMError::AsyncError {
                        message: "Promise was cancelled".to_string(),
                        stack_trace: None,
                    })?
                }));
            }
        }
        
        if futures.is_empty() {
            return Err(VMError::AsyncError {
                message: "No pending promises to race".to_string(),
                stack_trace: None,
            });
        }
        
        // Wait for the first to complete
        let (result, _index, _remaining) = futures::future::select_all(futures).await;
        result
    }
    
    /// Add timeout to a promise
    pub async fn with_timeout(&mut self, promise_id: PromiseId, duration: Duration) -> VMResult<Value> {
        if let Some(receiver) = self.promises.remove(&promise_id) {
            match timeout(duration, receiver).await {
                Ok(Ok(result)) => result,
                Ok(Err(_)) => Err(VMError::AsyncError {
                    message: "Promise was cancelled".to_string(),
                    stack_trace: None,
                }),
                Err(_) => Err(VMError::AsyncError {
                    message: "Promise timed out".to_string(),
                    stack_trace: None,
                }),
            }
        } else {
            // Check cached state
            let states = self.states.lock().await;
            if let Some(state) = states.get(&promise_id) {
                match state {
                    PromiseState::Resolved(value) => Ok(value.clone()),
                    PromiseState::Rejected(error) => Err(VMError::RuntimeError {
                        message: error.clone(),
                        stack_trace: None,
                    }),
                    PromiseState::Pending => Err(VMError::AsyncError {
                        message: "Promise already being awaited".to_string(),
                        stack_trace: None,
                    }),
                }
            } else {
                Err(VMError::AsyncError {
                    message: format!("Unknown promise ID: {:?}", promise_id),
                    stack_trace: None,
                })
            }
        }
    }
    
    /// Get the current state of a promise
    pub async fn get_state(&self, promise_id: PromiseId) -> Option<PromiseState> {
        self.states.lock().await.get(&promise_id).cloned()
    }
}

impl Default for PromiseManager {
    fn default() -> Self {
        Self::new()
    }
}