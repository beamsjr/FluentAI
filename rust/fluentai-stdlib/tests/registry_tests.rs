//! Tests for the stdlib function registry

use fluentai_stdlib::registry::{StdlibFunction, StdlibRegistry};
use fluentai_stdlib::value::Value;
use fluentai_core::ast::EffectType;
use anyhow::Result;

// Test function implementations
fn test_add(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        _ => anyhow::bail!("add: expected two integers"),
    }
}

fn test_print(_args: &[Value]) -> Result<Value> {
    // Just return nil for testing
    Ok(Value::Nil)
}

fn test_variadic(args: &[Value]) -> Result<Value> {
    Ok(Value::Integer(args.len() as i64))
}

#[test]
fn test_stdlib_function_creation() {
    // Test pure function
    let add_func = StdlibFunction::pure(
        "add",
        test_add,
        2,
        Some(2),
        "Add two integers",
    );
    
    assert_eq!(add_func.name, "add");
    assert_eq!(add_func.min_args, 2);
    assert_eq!(add_func.max_args, Some(2));
    assert!(add_func.effects.is_empty());
    assert_eq!(add_func.doc, "Add two integers");
    
    // Test effectful function
    let print_func = StdlibFunction::effectful(
        "print",
        test_print,
        1,
        None,
        vec![EffectType::IO],
        "Print values",
    );
    
    assert_eq!(print_func.name, "print");
    assert_eq!(print_func.min_args, 1);
    assert_eq!(print_func.max_args, None);
    assert_eq!(print_func.effects, vec![EffectType::IO]);
}

#[test]
fn test_argument_validation() {
    let func = StdlibFunction::pure("test", test_add, 2, Some(3), "Test function");
    
    // Too few arguments
    assert!(func.validate_args(1).is_err());
    
    // Valid argument counts
    assert!(func.validate_args(2).is_ok());
    assert!(func.validate_args(3).is_ok());
    
    // Too many arguments
    assert!(func.validate_args(4).is_err());
    
    // Test variadic function
    let variadic = StdlibFunction::pure("variadic", test_variadic, 0, None, "Variadic function");
    assert!(variadic.validate_args(0).is_ok());
    assert!(variadic.validate_args(10).is_ok());
    assert!(variadic.validate_args(100).is_ok());
}

#[test]
fn test_function_call() {
    let add_func = StdlibFunction::pure("add", test_add, 2, Some(2), "Add two integers");
    
    // Valid call
    let result = add_func.call(&[Value::Integer(5), Value::Integer(3)]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(8));
    
    // Invalid argument count
    let result = add_func.call(&[Value::Integer(5)]);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("expected at least 2 arguments"));
    
    // Invalid argument types
    let result = add_func.call(&[Value::String("5".to_string()), Value::Integer(3)]);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("expected two integers"));
}

#[test]
fn test_stdlib_registry() {
    let mut registry = StdlibRegistry::new();
    
    // Register some functions
    let add_func = StdlibFunction::pure("add", test_add, 2, Some(2), "Add two integers");
    let print_func = StdlibFunction::effectful("print", test_print, 1, None, vec![EffectType::IO], "Print values");
    
    registry.register(add_func);
    registry.register(print_func);
    
    // Test lookup
    assert!(registry.get("add").is_some());
    assert!(registry.get("print").is_some());
    assert!(registry.get("nonexistent").is_none());
    
    // Test function properties
    let add = registry.get("add").unwrap();
    assert_eq!(add.min_args, 2);
    assert!(add.effects.is_empty());
    
    let print = registry.get("print").unwrap();
    assert_eq!(print.min_args, 1);
    assert_eq!(print.effects, vec![EffectType::IO]);
}

#[test]
fn test_registry_register_all() {
    let mut registry = StdlibRegistry::new();
    
    let functions = vec![
        StdlibFunction::pure("func1", test_add, 2, Some(2), "Function 1"),
        StdlibFunction::pure("func2", test_variadic, 0, None, "Function 2"),
        StdlibFunction::effectful("func3", test_print, 1, None, vec![EffectType::IO], "Function 3"),
    ];
    
    registry.register_all(functions);
    
    assert!(registry.get("func1").is_some());
    assert!(registry.get("func2").is_some());
    assert!(registry.get("func3").is_some());
}

#[test]
fn test_registry_list_functions() {
    let mut registry = StdlibRegistry::new();
    
    registry.register_all(vec![
        StdlibFunction::pure("zebra", test_add, 2, Some(2), "Z function"),
        StdlibFunction::pure("alpha", test_add, 2, Some(2), "A function"),
        StdlibFunction::pure("beta", test_add, 2, Some(2), "B function"),
    ]);
    
    let mut functions = registry.function_names();
    assert_eq!(functions.len(), 3);
    
    // Sort them for consistent testing
    functions.sort();
    assert_eq!(functions[0], "alpha");
    assert_eq!(functions[1], "beta");
    assert_eq!(functions[2], "zebra");
}

#[test]
fn test_registry_contains() {
    let mut registry = StdlibRegistry::new();
    
    registry.register(StdlibFunction::pure("exists", test_add, 2, Some(2), "Existing function"));
    
    assert!(registry.contains("exists"));
    assert!(!registry.contains("does_not_exist"));
}

#[test]
fn test_registry_thread_safety() {
    use std::sync::Arc;
    use std::thread;
    
    // Create registry and register function before making it Arc
    let mut registry = StdlibRegistry::new();
    registry.register(
        StdlibFunction::pure("initial", test_add, 2, Some(2), "Initial function")
    );
    
    let registry = Arc::new(registry);
    let mut handles = vec![];
    
    // Spawn readers
    for _i in 0..5 {
        let reg = registry.clone();
        let handle = thread::spawn(move || {
            for _ in 0..100 {
                let _func = reg.get("initial");
                thread::yield_now();
            }
        });
        handles.push(handle);
    }
    
    // Wait for all threads
    for handle in handles {
        handle.join().unwrap();
    }
}