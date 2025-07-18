//! Time effect handler

use crate::{format_effect_error, EffectHandler, EffectResult};
use async_trait::async_trait;
use chrono::{Local, Utc};
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use tokio::time::{sleep, Duration};

pub struct TimeHandler;

impl TimeHandler {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl EffectHandler for TimeHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Time
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "now" => Ok(Value::Integer(Utc::now().timestamp_millis())),
            "now_local" => Ok(Value::Integer(Local::now().timestamp_millis())),
            "timestamp" => Ok(Value::Integer(
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map_err(|e| Error::Runtime(e.to_string()))?
                    .as_millis() as i64,
            )),
            "format" => {
                if args.len() >= 2 {
                    if let (Some(Value::Integer(timestamp)), Some(Value::String(format))) =
                        (args.get(0), args.get(1))
                    {
                        let dt = chrono::DateTime::<Utc>::from_timestamp_millis(*timestamp)
                            .ok_or_else(|| Error::Runtime("Invalid timestamp".to_string()))?;
                        Ok(Value::String(dt.format(format).to_string()))
                    } else {
                        Err(Error::Runtime(format_effect_error(
                            "Time",
                            operation,
                            "requires timestamp and format string",
                        )))
                    }
                } else if let Some(Value::Integer(timestamp)) = args.first() {
                    // Default to RFC3339 if no format string provided
                    let dt = chrono::DateTime::<Utc>::from_timestamp_millis(*timestamp)
                        .ok_or_else(|| Error::Runtime("Invalid timestamp".to_string()))?;
                    Ok(Value::String(dt.to_rfc3339()))
                } else {
                    Err(Error::Runtime(format_effect_error(
                        "Time",
                        operation,
                        "requires timestamp",
                    )))
                }
            }
            "sleep" => {
                // Support synchronous sleep for testing
                if let Some(Value::Integer(ms)) = args.first() {
                    if *ms < 0 {
                        Err(Error::Runtime(format_effect_error(
                            "Time",
                            operation,
                            "requires non-negative milliseconds",
                        )))
                    } else {
                        std::thread::sleep(std::time::Duration::from_millis(*ms as u64));
                        Ok(Value::Nil)
                    }
                } else {
                    Err(Error::Runtime(format_effect_error(
                        "Time",
                        operation,
                        "requires milliseconds",
                    )))
                }
            }
            _ => Err(Error::Runtime(format!(
                "Unknown Time operation: {}",
                operation
            ))),
        }
    }

    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "sleep" => {
                if let Some(Value::Integer(ms)) = args.first() {
                    if *ms < 0 {
                        Err(Error::Runtime(format_effect_error(
                            "Time",
                            operation,
                            "requires non-negative milliseconds",
                        )))
                    } else {
                        sleep(Duration::from_millis(*ms as u64)).await;
                        Ok(Value::Nil)
                    }
                } else {
                    Err(Error::Runtime(format_effect_error(
                        "Time",
                        operation,
                        "requires milliseconds",
                    )))
                }
            }
            _ => self.handle_sync(operation, args),
        }
    }

    fn is_async_operation(&self, operation: &str) -> bool {
        matches!(operation, "sleep")
    }
}
