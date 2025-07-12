//! IO effect handler

use crate::{format_effect_error, EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use std::io::{self, Write};

pub struct IOHandler;

impl IOHandler {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl EffectHandler for IOHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::IO
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "print" => {
                if let Some(arg) = args.first() {
                    // Special handling for string values - print without quotes
                    match arg {
                        Value::String(s) => print!("{}", s),
                        _ => print!("{}", arg),
                    }
                    io::stdout()
                        .flush()
                        .map_err(|e| Error::Runtime(e.to_string()))?;
                }
                Ok(Value::Nil)
            }
            "println" => {
                if let Some(arg) = args.first() {
                    // Special handling for string values - print without quotes
                    match arg {
                        Value::String(s) => println!("{}", s),
                        _ => println!("{}", arg),
                    }
                }
                Ok(Value::Nil)
            }
            "read_line" => {
                let mut buffer = String::new();
                io::stdin()
                    .read_line(&mut buffer)
                    .map_err(|e| Error::Runtime(e.to_string()))?;
                Ok(Value::String(buffer.trim_end().to_string()))
            }
            "read_file" => {
                if let Some(Value::String(path)) = args.first() {
                    std::fs::read_to_string(path)
                        .map(Value::String)
                        .map_err(|e| Error::Runtime(format!("Failed to read file: {}", e)))
                } else {
                    Err(Error::Runtime(format_effect_error(
                        "IO",
                        operation,
                        "requires a string path",
                    )))
                }
            }
            "write_file" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(path)), Some(content)) = (args.get(0), args.get(1)) {
                        let content_str = match content {
                            Value::String(s) => s.clone(),
                            other => other.to_string(),
                        };
                        std::fs::write(path, content_str)
                            .map(|_| Value::Nil)
                            .map_err(|e| Error::Runtime(format!("Failed to write file: {}", e)))
                    } else {
                        Err(Error::Runtime(format_effect_error(
                            "IO",
                            operation,
                            "requires path and content",
                        )))
                    }
                } else {
                    Err(Error::Runtime(format_effect_error(
                        "IO",
                        operation,
                        "requires 2 arguments",
                    )))
                }
            }
            _ => Err(Error::Runtime(format_effect_error(
                "IO",
                operation,
                "operation not supported",
            ))),
        }
    }
}
