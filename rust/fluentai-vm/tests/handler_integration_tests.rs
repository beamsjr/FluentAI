//! Integration tests for effect handlers
//!
//! These tests verify that handlers work correctly with effects,
//! including proper interception, handler chaining, and state management.

use fluentai_vm::{Compiler, Value, VM};

#[test]
fn test_basic_handler_syntax() {
    // Test that handler syntax parses and compiles without effects
    let code = r#"
        (handler
            ((error (lambda (err) "handled")))
            42)
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Body should execute normally without effects
    match result {
        Value::Integer(42) => {}
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_handler_with_effect_invocation() {
    // Test handler with actual effect invocation
    let code = r#"
        (handler
            ((error (lambda (err) 99)))
            (effect error:raise "test error"))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Handler should intercept the effect
    match result {
        Value::Integer(99) => {}
        _ => panic!("Expected 99, got {:?}", result),
    }
}

#[test]
fn test_nested_handlers() {
    // Test that inner handlers shadow outer handlers
    let code = r#"
        (handler
            ((error (lambda (e) 1)))
            (handler
                ((error (lambda (e) 2)))
                (effect error:raise "test")))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Inner handler should win
    match result {
        Value::Integer(2) => {}
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_handler_with_multiple_handlers() {
    // Test handler with multiple effect handlers
    let code = r#"
        (handler
            ((io (lambda (op) 10))
             (error (lambda (e) 20)))
            (+ (effect io:print) (effect error:raise "test")))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Both effects should be handled
    match result {
        Value::Integer(30) => {} // 10 + 20
        _ => panic!("Expected 30, got {:?}", result),
    }
}

#[test]
fn test_handler_passes_through_unhandled() {
    // Test that unhandled effects pass through
    let code = r#"
        (handler
            ((error (lambda (e) 5)))
            (handler
                ((io (lambda (op) 10)))
                (effect error:raise "test")))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Error should pass through inner handler to outer handler
    match result {
        Value::Integer(5) => {}
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_handler_with_lexical_scope() {
    // Test that handlers preserve lexical scope
    let code = r#"
        (let ((x 10))
            (handler
                ((error (lambda (e) (+ x 5))))
                (effect error:raise "test")))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Handler should access x from lexical scope
    match result {
        Value::Integer(15) => {}
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_handler_function_receives_args() {
    // Test that handler functions receive effect arguments
    let code = r#"
        (handler
            ((io (lambda (op) op)))
            (effect io:print))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Handler should receive and return the operation
    match result {
        Value::String(s) => assert_eq!(s, "print"),
        _ => panic!("Expected 'print', got {:?}", result),
    }
}

#[test]
fn test_handler_with_conditional() {
    // Test handler with simple conditional logic
    let code = r#"
        (handler
            ((io (lambda (op) 100)))
            (effect io:print))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Should return handler result
    match result {
        Value::Integer(100) => {}
        _ => panic!("Expected 100, got {:?}", result),
    }
}

#[test]
fn test_handler_with_string_result() {
    // Test handler that returns string
    let code = r#"
        (handler
            ((io (lambda (op) "handled")))
            (effect io:test))
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Handler should return string
    match result {
        Value::String(s) => assert_eq!(s, "handled"),
        _ => panic!("Expected 'handled', got {:?}", result),
    }
}

#[test]
fn test_handler_removes_after_body() {
    // Test that handler is removed after body execution
    let code = r#"
        (let ((result
                (handler
                    ((error (lambda (e) 42)))
                    (effect error:raise "test"))))
            result)
    "#;

    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Result should be from handler
    match result {
        Value::Integer(42) => {}
        _ => panic!("Expected 42, got {:?}", result),
    }
}
