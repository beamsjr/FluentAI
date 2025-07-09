//! Error effect handler

use crate::{format_effect_error, EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl EffectHandler for ErrorHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Error
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "raise" => {
                let msg = args
                    .first()
                    .map(|v| match v {
                        Value::String(s) => s.clone(),
                        other => other.to_string(),
                    })
                    .unwrap_or_else(|| "Error raised".to_string());
                Err(Error::Runtime(msg))
            }
            "try" => {
                // In a real implementation, this would execute code with error handling
                // For now, just return the first argument
                Ok(args.first().cloned().unwrap_or(Value::Nil))
            }
            "catch" => {
                // Would normally catch errors from a block
                Ok(Value::Nil)
            }
            "finally" => {
                // Would execute cleanup code
                Ok(Value::Nil)
            }
            _ => Err(Error::Runtime(format_effect_error(
                "Error",
                operation,
                "operation not supported",
            ))),
        }
    }
}
