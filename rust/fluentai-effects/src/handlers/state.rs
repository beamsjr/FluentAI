//! State effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, value::Value, error::Error};
use dashmap::DashMap;
use std::sync::Arc;

pub struct StateHandler {
    state: Arc<DashMap<String, Value>>,
}

impl StateHandler {
    pub fn new() -> Self {
        Self {
            state: Arc::new(DashMap::new()),
        }
    }
}

#[async_trait]
impl EffectHandler for StateHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::State
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "get" => {
                if let Some(Value::String(key)) = args.first() {
                    Ok(self.state.get(key)
                        .map(|v| v.clone())
                        .unwrap_or(Value::Nil))
                } else {
                    Err(Error::Runtime("state:get requires a string key".to_string()))
                }
            }
            "set" => {
                if args.len() >= 2 {
                    if let Some(Value::String(key)) = args.first() {
                        let value = args[1].clone();
                        self.state.insert(key.clone(), value.clone());
                        Ok(value)
                    } else {
                        Err(Error::Runtime("state:set requires a string key".to_string()))
                    }
                } else {
                    Err(Error::Runtime("state:set requires key and value".to_string()))
                }
            }
            "update" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(key)), Some(func)) = (args.get(0), args.get(1)) {
                        // In a real implementation, we'd apply the function
                        // For now, just replace the value
                        self.state.insert(key.clone(), func.clone());
                        Ok(func.clone())
                    } else {
                        Err(Error::Runtime("state:update requires key and function".to_string()))
                    }
                } else {
                    Err(Error::Runtime("state:update requires 2 arguments".to_string()))
                }
            }
            "delete" => {
                if let Some(Value::String(key)) = args.first() {
                    self.state.remove(key);
                    Ok(Value::Nil)
                } else {
                    Err(Error::Runtime("state:delete requires a string key".to_string()))
                }
            }
            "clear" => {
                self.state.clear();
                Ok(Value::Nil)
            }
            _ => Err(Error::Runtime(format!("Unknown State operation: {}", operation))),
        }
    }
}