//! Logger module for FluentAi
//! Provides structured logging with levels on top of the IO effect system

use chrono::{DateTime, Local};
use once_cell::sync::Lazy;
use serde_json::json;
use std::sync::RwLock;

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};
use fluentai_core::ast::EffectType;

/// Log levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Debug = 0,
    Info = 1,
    Warn = 2,
    Error = 3,
}

impl LogLevel {
    fn as_str(&self) -> &'static str {
        match self {
            LogLevel::Debug => "DEBUG",
            LogLevel::Info => "INFO",
            LogLevel::Warn => "WARN",
            LogLevel::Error => "ERROR",
        }
    }

    fn from_i64(value: i64) -> Result<Self> {
        match value {
            0 => Ok(LogLevel::Debug),
            1 => Ok(LogLevel::Info),
            2 => Ok(LogLevel::Warn),
            3 => Ok(LogLevel::Error),
            _ => Err(anyhow!("Invalid log level: {}", value)),
        }
    }
}

/// Global log level
static CURRENT_LOG_LEVEL: Lazy<RwLock<LogLevel>> = Lazy::new(|| RwLock::new(LogLevel::Info));

/// Format a log message with timestamp and level
fn format_log_message(level: LogLevel, message: &str, data: Option<&Value>) -> String {
    let timestamp: DateTime<Local> = Local::now();
    let timestamp_str = timestamp.format("%Y-%m-%d %H:%M:%S%.3f");

    let mut output = format!("[{}] [{}] {}", timestamp_str, level.as_str(), message);

    if let Some(data) = data {
        // Convert Value to JSON for structured data
        let json_str = match data {
            Value::Map(map) => {
                let mut json_map = serde_json::Map::new();
                for (k, v) in map {
                    json_map.insert(k.clone(), value_to_json(v));
                }
                serde_json::to_string(&json_map).unwrap_or_else(|_| "{}".to_string())
            }
            _ => value_to_json(data).to_string(),
        };
        output.push_str(&format!(" {}", json_str));
    }

    output
}

/// Convert FluentAi Value to serde_json::Value
fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Integer(i) => json!(i),
        Value::Float(f) => json!(f),
        Value::String(s) => json!(s),
        Value::Boolean(b) => json!(b),
        Value::Nil => json!(null),
        Value::List(list) => {
            let json_list: Vec<serde_json::Value> = list.iter().map(value_to_json).collect();
            json!(json_list)
        }
        Value::Map(map) => {
            let mut json_map = serde_json::Map::new();
            for (k, v) in map {
                json_map.insert(k.clone(), value_to_json(v));
            }
            json!(json_map)
        }
        _ => json!(format!("{:?}", value)),
    }
}

/// Core logging function
pub fn log(args: &[Value]) -> Result<Value> {
    if args.len() < 2 {
        return Err(anyhow!(
            "log requires at least 2 arguments, got {}",
            args.len()
        ));
    }

    let level = match &args[0] {
        Value::Integer(i) => LogLevel::from_i64(*i)?,
        _ => return Err(anyhow!("Log level must be an integer")),
    };

    let message = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("Message must be a string")),
    };

    let data = args.get(2);

    // Check if we should log this level
    let current_level = CURRENT_LOG_LEVEL.read().unwrap();
    if level >= *current_level {
        let formatted = format_log_message(level, message, data);
        println!("{}", formatted);
    }

    Ok(Value::Nil)
}

/// Debug log
pub fn debug(args: &[Value]) -> Result<Value> {
    let mut new_args = vec![Value::Integer(LogLevel::Debug as i64)];
    new_args.extend_from_slice(args);
    log(&new_args)
}

/// Info log
pub fn info(args: &[Value]) -> Result<Value> {
    let mut new_args = vec![Value::Integer(LogLevel::Info as i64)];
    new_args.extend_from_slice(args);
    log(&new_args)
}

/// Warn log
pub fn warn(args: &[Value]) -> Result<Value> {
    let mut new_args = vec![Value::Integer(LogLevel::Warn as i64)];
    new_args.extend_from_slice(args);
    log(&new_args)
}

/// Error log
pub fn error(args: &[Value]) -> Result<Value> {
    let mut new_args = vec![Value::Integer(LogLevel::Error as i64)];
    new_args.extend_from_slice(args);
    log(&new_args)
}

/// Set log level
pub fn set_log_level(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(anyhow!(
            "set-log-level requires exactly 1 argument, got {}",
            args.len()
        ));
    }

    let level = match &args[0] {
        Value::Integer(i) => LogLevel::from_i64(*i)?,
        _ => return Err(anyhow!("Log level must be an integer")),
    };

    let mut current_level = CURRENT_LOG_LEVEL.write().unwrap();
    *current_level = level;

    Ok(Value::Nil)
}

/// Get current log level
pub fn get_log_level(_args: &[Value]) -> Result<Value> {
    let level = CURRENT_LOG_LEVEL.read().unwrap();
    Ok(Value::Integer(*level as i64))
}

/// Log level constants
pub fn log_level_debug(_args: &[Value]) -> Result<Value> {
    Ok(Value::Integer(LogLevel::Debug as i64))
}

pub fn log_level_info(_args: &[Value]) -> Result<Value> {
    Ok(Value::Integer(LogLevel::Info as i64))
}

pub fn log_level_warn(_args: &[Value]) -> Result<Value> {
    Ok(Value::Integer(LogLevel::Warn as i64))
}

pub fn log_level_error(_args: &[Value]) -> Result<Value> {
    Ok(Value::Integer(LogLevel::Error as i64))
}

/// Register all logger functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Logging functions (all have IO effect)
        StdlibFunction::effectful(
            "logger:log",
            log,
            2,
            Some(3),
            vec![EffectType::IO],
            "Log a message with level and optional data",
        ),
        StdlibFunction::effectful(
            "logger:debug",
            debug,
            1,
            Some(2),
            vec![EffectType::IO],
            "Log a debug message",
        ),
        StdlibFunction::effectful(
            "logger:info",
            info,
            1,
            Some(2),
            vec![EffectType::IO],
            "Log an info message",
        ),
        StdlibFunction::effectful(
            "logger:warn",
            warn,
            1,
            Some(2),
            vec![EffectType::IO],
            "Log a warning message",
        ),
        StdlibFunction::effectful(
            "logger:error",
            error,
            1,
            Some(2),
            vec![EffectType::IO],
            "Log an error message",
        ),
        // Configuration functions (have State effect)
        StdlibFunction::effectful(
            "logger:set-log-level",
            set_log_level,
            1,
            Some(1),
            vec![EffectType::State],
            "Set the minimum log level",
        ),
        StdlibFunction::pure(
            "logger:get-log-level",
            get_log_level,
            0,
            Some(0),
            "Get the current log level",
        ),
        // Log level constants
        StdlibFunction::pure(
            "logger:DEBUG",
            log_level_debug,
            0,
            Some(0),
            "Debug log level constant",
        ),
        StdlibFunction::pure(
            "logger:INFO",
            log_level_info,
            0,
            Some(0),
            "Info log level constant",
        ),
        StdlibFunction::pure(
            "logger:WARN",
            log_level_warn,
            0,
            Some(0),
            "Warn log level constant",
        ),
        StdlibFunction::pure(
            "logger:ERROR",
            log_level_error,
            0,
            Some(0),
            "Error log level constant",
        ),
    ]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_level_ordering() {
        assert!(LogLevel::Debug < LogLevel::Info);
        assert!(LogLevel::Info < LogLevel::Warn);
        assert!(LogLevel::Warn < LogLevel::Error);
    }

    #[test]
    fn test_format_log_message() {
        let message = format_log_message(LogLevel::Info, "Test message", None);
        assert!(message.contains("[INFO]"));
        assert!(message.contains("Test message"));

        let data = Value::Map(
            vec![
                ("key".to_string(), Value::String("value".to_string())),
                ("count".to_string(), Value::Integer(42)),
            ]
            .into_iter()
            .collect(),
        );

        let message_with_data = format_log_message(LogLevel::Error, "Error occurred", Some(&data));
        assert!(message_with_data.contains("[ERROR]"));
        assert!(message_with_data.contains("Error occurred"));
        assert!(message_with_data.contains("\"key\":\"value\""));
        assert!(message_with_data.contains("\"count\":42"));
    }
}
