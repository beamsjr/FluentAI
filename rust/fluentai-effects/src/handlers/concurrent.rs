//! Concurrent effect handler for channels and goroutines

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use dashmap::DashMap;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};
use uuid::Uuid;

type ChannelSender = mpsc::UnboundedSender<Value>;
type ChannelReceiver = Arc<Mutex<mpsc::UnboundedReceiver<Value>>>;

pub struct ConcurrentHandler {
    channels: Arc<DashMap<String, (ChannelSender, ChannelReceiver)>>,
    goroutines: Arc<DashMap<String, tokio::task::JoinHandle<()>>>,
    mutexes: Arc<DashMap<String, Arc<Mutex<Value>>>>,
}

impl ConcurrentHandler {
    pub fn new() -> Self {
        Self {
            channels: Arc::new(DashMap::new()),
            goroutines: Arc::new(DashMap::new()),
            mutexes: Arc::new(DashMap::new()),
        }
    }
}

#[async_trait]
impl EffectHandler for ConcurrentHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Concurrent
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "channel" => {
                // Create a new channel
                let (tx, rx) = mpsc::unbounded_channel();
                let channel_id = Uuid::new_v4().to_string();

                self.channels
                    .insert(channel_id.clone(), (tx, Arc::new(Mutex::new(rx))));

                Ok(Value::String(channel_id))
            }
            "send" => {
                if args.len() >= 2 {
                    if let Some(Value::String(channel_id)) = args.first() {
                        if let Some(channel) = self.channels.get(channel_id) {
                            let (tx, _) = channel.value();
                            let value = args[1].clone();

                            tx.send(value)
                                .map_err(|_| Error::Runtime("Channel closed".to_string()))?;

                            Ok(Value::Nil)
                        } else {
                            Err(Error::Runtime("Unknown channel ID".to_string()))
                        }
                    } else {
                        Err(Error::Runtime(
                            "concurrent:send requires channel ID".to_string(),
                        ))
                    }
                } else {
                    Err(Error::Runtime(
                        "concurrent:send requires 2 arguments".to_string(),
                    ))
                }
            }
            "close" => {
                if let Some(Value::String(channel_id)) = args.first() {
                    self.channels.remove(channel_id);
                    Ok(Value::Nil)
                } else {
                    Err(Error::Runtime(
                        "concurrent:close requires channel ID".to_string(),
                    ))
                }
            }
            "mutex" => {
                // Create a new mutex with optional initial value
                let mutex_id = Uuid::new_v4().to_string();
                let initial_value = args.first().cloned().unwrap_or(Value::Nil);

                self.mutexes
                    .insert(mutex_id.clone(), Arc::new(Mutex::new(initial_value)));

                Ok(Value::String(mutex_id))
            }
            _ => Err(Error::Runtime(format!(
                "Unknown Concurrent operation: {}",
                operation
            ))),
        }
    }

    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "receive" => {
                if let Some(Value::String(channel_id)) = args.first() {
                    if let Some(channel) = self.channels.get(channel_id) {
                        let (_, rx) = channel.value();
                        let receiver = rx.clone();
                        drop(channel); // Release the dashmap lock

                        let mut rx = receiver.lock().await;

                        match rx.recv().await {
                            Some(value) => Ok(value),
                            None => Err(Error::Runtime("Channel closed".to_string())),
                        }
                    } else {
                        Err(Error::Runtime("Unknown channel ID".to_string()))
                    }
                } else {
                    Err(Error::Runtime(
                        "concurrent:receive requires channel ID".to_string(),
                    ))
                }
            }
            "select" => {
                // Select from multiple channels
                let mut futures = Vec::new();

                for (i, arg) in args.iter().enumerate() {
                    if let Value::String(channel_id) = arg {
                        if let Some(channel) = self.channels.get(channel_id) {
                            let (_, rx) = channel.value();
                            let receiver = rx.clone();
                            drop(channel);

                            let future = async move {
                                let mut rx = receiver.lock().await;
                                rx.recv().await.map(|v| (i, v))
                            };

                            futures.push(Box::pin(future));
                        }
                    }
                }

                if futures.is_empty() {
                    return Err(Error::Runtime(
                        "concurrent:select requires at least one channel".to_string(),
                    ));
                }

                // Use tokio::select! to wait for first result
                // In a real implementation, we'd use a more sophisticated approach
                if let Some(first_future) = futures.into_iter().next() {
                    match first_future.await {
                        Some((index, value)) => {
                            let mut result = FxHashMap::default();
                            result.insert("index".to_string(), Value::Integer(index as i64));
                            result.insert("value".to_string(), value);
                            Ok(Value::Map(result))
                        }
                        None => Err(Error::Runtime("All channels closed".to_string())),
                    }
                } else {
                    Err(Error::Runtime("No channels to select from".to_string()))
                }
            }
            "spawn" => {
                // Spawn a goroutine
                let goroutine_id = Uuid::new_v4().to_string();

                // In a real implementation, we'd execute FluentAi code
                // For now, just spawn an empty task
                let handle = tokio::spawn(async move {
                    // Goroutine body would go here
                });

                self.goroutines.insert(goroutine_id.clone(), handle);
                Ok(Value::String(goroutine_id))
            }
            "join" => {
                if let Some(Value::String(goroutine_id)) = args.first() {
                    if let Some((_, handle)) = self.goroutines.remove(goroutine_id) {
                        handle
                            .await
                            .map_err(|e| Error::Runtime(format!("Goroutine panicked: {}", e)))?;
                        Ok(Value::Nil)
                    } else {
                        Err(Error::Runtime("Unknown goroutine ID".to_string()))
                    }
                } else {
                    Err(Error::Runtime(
                        "concurrent:join requires goroutine ID".to_string(),
                    ))
                }
            }
            _ => self.handle_sync(operation, args),
        }
    }

    fn is_async_operation(&self, operation: &str) -> bool {
        matches!(operation, "receive" | "select" | "spawn" | "join")
    }
}
