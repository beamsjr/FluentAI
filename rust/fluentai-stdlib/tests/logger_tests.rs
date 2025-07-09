//! Tests for the logger module

use fluentai_effects::EffectType;
use fluentai_stdlib::logger::*;
use fluentai_stdlib::value::Value;
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

// Helper to capture log output
struct LogCapture {
    captured: Arc<AtomicBool>,
    original_hook: Option<Box<dyn Fn(&std::panic::PanicHookInfo<'_>) + Sync + Send + 'static>>,
}

impl LogCapture {
    fn new() -> Self {
        Self {
            captured: Arc::new(AtomicBool::new(false)),
            original_hook: None,
        }
    }

    fn start(&mut self) {
        self.captured.store(true, Ordering::SeqCst);
    }

    fn stop(&mut self) {
        self.captured.store(false, Ordering::SeqCst);
    }
}

#[test]
fn test_log_levels() {
    // Test LogLevel ordering
    assert!(LogLevel::Debug < LogLevel::Info);
    assert!(LogLevel::Info < LogLevel::Warn);
    assert!(LogLevel::Warn < LogLevel::Error);

    // Test LogLevel conversion
    // from_i64 is private, so we can't test it directly
    // But we can test the ordering is correct
    assert!((LogLevel::Debug as i32) < (LogLevel::Info as i32));
    assert!((LogLevel::Info as i32) < (LogLevel::Warn as i32));
    assert!((LogLevel::Warn as i32) < (LogLevel::Error as i32));

    // as_str() is private, so we can't test string representation directly
}

#[test]
fn test_basic_logging() {
    // Test log function with different levels
    let result = log(&[
        Value::Integer(1),
        Value::String("Test info message".to_string()),
    ]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);

    // Test with data
    let mut data = FxHashMap::default();
    data.insert("user".to_string(), Value::String("alice".to_string()));
    data.insert("action".to_string(), Value::String("login".to_string()));

    let result = log(&[
        Value::Integer(1),
        Value::String("User action".to_string()),
        Value::Map(data),
    ]);
    assert!(result.is_ok());

    // Test error cases
    let result = log(&[Value::Integer(1)]); // Too few arguments
    assert!(result.is_err());

    let result = log(&[
        Value::String("not a number".to_string()),
        Value::String("message".to_string()),
    ]);
    assert!(result.is_err());
}

#[test]
fn test_level_specific_functions() {
    // Test debug
    let result = debug(&[Value::String("Debug message".to_string())]);
    assert!(result.is_ok());

    // Test info
    let result = info(&[Value::String("Info message".to_string())]);
    assert!(result.is_ok());

    // Test warn
    let result = warn(&[Value::String("Warning message".to_string())]);
    assert!(result.is_ok());

    // Test error
    let result = error(&[Value::String("Error message".to_string())]);
    assert!(result.is_ok());

    // Test with structured data
    let mut data = FxHashMap::default();
    data.insert("code".to_string(), Value::Integer(404));
    data.insert("path".to_string(), Value::String("/not/found".to_string()));

    let result = error(&[
        Value::String("Page not found".to_string()),
        Value::Map(data),
    ]);
    assert!(result.is_ok());
}

#[test]
fn test_log_level_management() {
    // Get current log level
    let result = get_log_level(&[]);
    assert!(result.is_ok());
    let initial_level = result.unwrap();

    // Set log level to Debug
    let result = set_log_level(&[Value::Integer(0)]);
    assert!(result.is_ok());

    let result = get_log_level(&[]);
    assert_eq!(result.unwrap(), Value::Integer(0));

    // Set log level to Error
    let result = set_log_level(&[Value::Integer(3)]);
    assert!(result.is_ok());

    let result = get_log_level(&[]);
    assert_eq!(result.unwrap(), Value::Integer(3));

    // Test invalid log level
    let result = set_log_level(&[Value::Integer(5)]);
    assert!(result.is_err());

    // Test non-integer argument
    let result = set_log_level(&[Value::String("info".to_string())]);
    assert!(result.is_err());

    // Restore initial level
    set_log_level(&[initial_level]).unwrap();
}

#[test]
fn test_log_filtering() {
    // Save initial log level
    let initial_level = get_log_level(&[]).unwrap();

    // Set log level to WARN
    set_log_level(&[Value::Integer(2)]).unwrap();

    // Debug and Info messages should be filtered (but function still returns Ok)
    let result = debug(&[Value::String("This should be filtered".to_string())]);
    assert!(result.is_ok());

    let result = info(&[Value::String("This should also be filtered".to_string())]);
    assert!(result.is_ok());

    // Warn and Error messages should pass through
    let result = warn(&[Value::String("This should be logged".to_string())]);
    assert!(result.is_ok());

    let result = error(&[Value::String("This should also be logged".to_string())]);
    assert!(result.is_ok());

    // Restore initial level
    set_log_level(&[initial_level]).unwrap();
}

#[test]
fn test_value_to_json_conversion() {
    // The logger internally converts values to JSON for structured logging
    // We can test this indirectly by passing various value types

    // Simple values
    info(&[
        Value::String("Test with integer".to_string()),
        Value::Integer(42),
    ])
    .unwrap();
    info(&[
        Value::String("Test with float".to_string()),
        Value::Float(3.14),
    ])
    .unwrap();
    info(&[
        Value::String("Test with boolean".to_string()),
        Value::Boolean(true),
    ])
    .unwrap();
    info(&[Value::String("Test with nil".to_string()), Value::Nil]).unwrap();

    // List values
    let list = Value::List(vec![
        Value::Integer(1),
        Value::String("two".to_string()),
        Value::Boolean(false),
    ]);
    info(&[Value::String("Test with list".to_string()), list]).unwrap();

    // Nested map
    let mut inner_map = FxHashMap::default();
    inner_map.insert("nested".to_string(), Value::String("value".to_string()));

    let mut outer_map = FxHashMap::default();
    outer_map.insert("level".to_string(), Value::Integer(1));
    outer_map.insert("data".to_string(), Value::Map(inner_map));

    info(&[
        Value::String("Test with nested map".to_string()),
        Value::Map(outer_map),
    ])
    .unwrap();
}

#[test]
fn test_register_logger_functions() {
    use fluentai_stdlib::registry::StdlibRegistry;

    let mut registry = StdlibRegistry::new();
    register(&mut registry);

    // Check that all logger functions are registered
    assert!(registry.contains("logger:log"));
    assert!(registry.contains("logger:debug"));
    assert!(registry.contains("logger:info"));
    assert!(registry.contains("logger:warn"));
    assert!(registry.contains("logger:error"));
    assert!(registry.contains("logger:get-log-level"));
    assert!(registry.contains("logger:set-log-level"));

    // Check that they have the correct effect type
    let log_func = registry.get("logger:log").unwrap();
    assert_eq!(log_func.effects, vec![EffectType::IO]);

    let get_level = registry.get("logger:get-log-level").unwrap();
    assert!(get_level.effects.is_empty()); // Pure function
}

#[test]
fn test_thread_safety() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use std::thread;

    let counter = Arc::new(AtomicUsize::new(0));
    let mut handles = vec![];

    // Spawn multiple threads that log concurrently
    for i in 0..10 {
        let counter_clone = counter.clone();
        let handle = thread::spawn(move || {
            for j in 0..100 {
                let msg = format!("Thread {} iteration {}", i, j);
                info(&[Value::String(msg)]).unwrap();
                counter_clone.fetch_add(1, Ordering::SeqCst);
            }
        });
        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }

    // Verify all logs were processed
    assert_eq!(counter.load(Ordering::SeqCst), 1000);
}
