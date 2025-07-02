//! IO effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use claudelang_core::{ast::EffectType, value::Value, error::Error};
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
                    print!("{}", arg);
                    io::stdout().flush().map_err(|e| Error::Runtime(e.to_string()))?;
                }
                Ok(Value::Nil)
            }
            "println" => {
                if let Some(arg) = args.first() {
                    println!("{}", arg);
                }
                Ok(Value::Nil)
            }
            "read_line" => {
                let mut buffer = String::new();
                io::stdin().read_line(&mut buffer)
                    .map_err(|e| Error::Runtime(e.to_string()))?;
                Ok(Value::String(buffer.trim_end().to_string()))
            }
            "read_file" => {
                if let Some(Value::String(path)) = args.first() {
                    std::fs::read_to_string(path)
                        .map(Value::String)
                        .map_err(|e| Error::Runtime(format!("Failed to read file: {}", e)))
                } else {
                    Err(Error::Runtime("read_file requires a string path".to_string()))
                }
            }
            "write_file" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(path)), Some(content)) = (args.get(0), args.get(1)) {
                        std::fs::write(path, content.to_string())
                            .map(|_| Value::Nil)
                            .map_err(|e| Error::Runtime(format!("Failed to write file: {}", e)))
                    } else {
                        Err(Error::Runtime("write_file requires path and content".to_string()))
                    }
                } else {
                    Err(Error::Runtime("write_file requires 2 arguments".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!("Unknown IO operation: {}", operation))),
        }
    }
}