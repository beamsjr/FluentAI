//! Tests for error handler stack resource limits

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;
use fluentai_vm::safety::ResourceLimits;
use fluentai_vm::error::VMError;

fn run_with_limits(source: &str, limits: ResourceLimits) -> Result<Value> {
    // Parse the source code
    let graph = fluentai_parser::parse(source)
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

    // Compile to bytecode without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;

    // Create VM with effect runtime and custom limits
    let runtime = Arc::new(EffectRuntime::new()?);
    let mut vm = VM::new(bytecode);
    vm.set_resource_limits(limits);
    vm.set_effect_runtime(runtime);

    // Run the VM
    Ok(vm.run()?)
}

#[test]
fn test_deeply_nested_try_catch_hits_limit() {
    // Create very low limits for testing
    let mut limits = ResourceLimits::testing();
    limits.max_error_handlers = 3;
    
    // Create deeply nested try-catch blocks
    let code = r#"
        (try 
            (try
                (try
                    (try
                        42
                        (catch e1 e1))
                    (catch e2 e2))
                (catch e3 e3))
            (catch e4 e4))
    "#;
    
    let result = run_with_limits(code, limits);
    match result {
        Err(e) => {
            // Check that we get the right error type
            if let Some(vm_error) = e.downcast_ref::<VMError>() {
                match vm_error {
                    VMError::ResourceLimitExceeded { resource, limit, requested, .. } => {
                        assert_eq!(resource, "error handlers");
                        assert_eq!(*limit, 3);
                        assert_eq!(*requested, 4);
                    }
                    _ => panic!("Expected ResourceLimitExceeded, got {:?}", vm_error),
                }
            } else {
                panic!("Expected VMError, got {:?}", e);
            }
        }
        Ok(_) => panic!("Expected error due to limit, but succeeded"),
    }
}

#[test]
fn test_deeply_nested_try_finally_hits_limit() {
    // Create very low limits for testing
    let mut limits = ResourceLimits::testing();
    limits.max_error_handlers = 3;
    
    // Create deeply nested try-finally blocks
    let code = r#"
        (try 
            (try
                (try
                    (try
                        42
                        (finally 1))
                    (finally 2))
                (finally 3))
            (finally 4))
    "#;
    
    let result = run_with_limits(code, limits);
    match result {
        Err(e) => {
            // Check that we get the right error type
            if let Some(vm_error) = e.downcast_ref::<VMError>() {
                match vm_error {
                    VMError::ResourceLimitExceeded { resource, limit, requested, .. } => {
                        assert_eq!(resource, "error handlers");
                        assert_eq!(*limit, 3);
                        assert_eq!(*requested, 4);
                    }
                    _ => panic!("Expected ResourceLimitExceeded, got {:?}", vm_error),
                }
            } else {
                panic!("Expected VMError, got {:?}", e);
            }
        }
        Ok(_) => panic!("Expected error due to limit, but succeeded"),
    }
}

#[test]
fn test_recursive_function_with_try_catch() {
    // Create low limits for testing
    let mut limits = ResourceLimits::testing();
    limits.max_error_handlers = 5;
    limits.max_call_depth = 10; // Also limit call depth
    
    // Create recursive function with try-catch
    let code = r#"
        (define (recurse n)
            (try
                (if (= n 0)
                    42
                    (recurse (- n 1)))
                (catch e e)))
        (recurse 10)
    "#;
    
    let result = run_with_limits(code, limits);
    match result {
        Err(e) => {
            // Could hit either error handler limit or call depth limit
            if let Some(vm_error) = e.downcast_ref::<VMError>() {
                match vm_error {
                    VMError::ResourceLimitExceeded { resource, .. } => {
                        assert_eq!(resource, "error handlers");
                    }
                    VMError::CallStackOverflow { .. } => {
                        // This is also acceptable - depends on which limit hits first
                    }
                    _ => panic!("Expected ResourceLimitExceeded or CallStackOverflow, got {:?}", vm_error),
                }
            } else {
                panic!("Expected VMError, got {:?}", e);
            }
        }
        Ok(_) => panic!("Expected error due to limit, but succeeded"),
    }
}

#[test]
fn test_exactly_at_limit_works() {
    // Create limits that allow exactly 3 handlers
    let mut limits = ResourceLimits::testing();
    limits.max_error_handlers = 3;
    
    // Create exactly 3 nested try-catch blocks
    let code = r#"
        (try 
            (try
                (try
                    42
                    (catch e1 e1))
                (catch e2 e2))
            (catch e3 e3))
    "#;
    
    let result = run_with_limits(code, limits).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_sandboxed_limits() {
    // Use sandboxed limits (100 error handlers)
    let limits = ResourceLimits::sandboxed();
    
    // Create a reasonable amount of nesting that should work
    let mut code = String::from("(try ");
    for _ in 0..50 {
        code.push_str("(try ");
    }
    code.push_str("42");
    for i in 0..50 {
        code.push_str(&format!(" (catch e{} e{}))", i, i));
    }
    code.push_str(" (catch e_outer e_outer))");
    
    let result = run_with_limits(&code, limits).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_mixed_try_catch_finally() {
    // Create low limits for testing
    let mut limits = ResourceLimits::testing();
    limits.max_error_handlers = 4;
    
    // Create mixed try-catch-finally blocks
    let code = r#"
        (try 
            (try
                (try
                    (try
                        (try
                            42
                            (catch e1 e1))
                        (finally 1))
                    (catch e2 e2))
                (finally 2))
            (catch e3 e3))
    "#;
    
    let result = run_with_limits(code, limits);
    match result {
        Err(e) => {
            // Check that we get the right error type
            if let Some(vm_error) = e.downcast_ref::<VMError>() {
                match vm_error {
                    VMError::ResourceLimitExceeded { resource, limit, requested, .. } => {
                        assert_eq!(resource, "error handlers");
                        assert_eq!(*limit, 4);
                        assert_eq!(*requested, 5);
                    }
                    _ => panic!("Expected ResourceLimitExceeded, got {:?}", vm_error),
                }
            } else {
                panic!("Expected VMError, got {:?}", e);
            }
        }
        Ok(_) => panic!("Expected error due to limit, but succeeded"),
    }
}

#[test]
fn test_error_handler_limit_prevents_attack() {
    // Create low limits for testing - simulating a malicious script
    let mut limits = ResourceLimits::testing();
    limits.max_error_handlers = 10;
    
    // Create a malicious script that tries to exhaust error handlers
    let mut code = String::from("(try ");
    for _ in 0..20 {
        code.push_str("(try ");
    }
    code.push_str("(throw \"error\")");
    for i in 0..20 {
        code.push_str(&format!(" (catch e{} e{}))", i, i));
    }
    code.push_str(" (catch e_outer e_outer))");
    
    let result = run_with_limits(&code, limits);
    match result {
        Err(e) => {
            // We expect to hit the limit during compilation/execution
            if let Some(vm_error) = e.downcast_ref::<VMError>() {
                match vm_error {
                    VMError::ResourceLimitExceeded { resource, limit, .. } => {
                        assert_eq!(resource, "error handlers");
                        assert_eq!(*limit, 10);
                    }
                    _ => panic!("Expected ResourceLimitExceeded, got {:?}", vm_error),
                }
            } else {
                panic!("Expected VMError, got {:?}", e);
            }
        }
        Ok(_) => panic!("Expected error due to limit, but succeeded"),
    }
}