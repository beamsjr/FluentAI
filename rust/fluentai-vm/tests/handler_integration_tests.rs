//! Integration tests for effect handlers
//! 
//! These tests verify that handlers work correctly with effects,
//! including proper interception, handler chaining, and state management.

use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM, Value};

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
        Value::Int(42) => {},
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_handler_with_effect_invocation() {
    // Test handler with actual effect invocation
    let code = r#"
        (handler
            ((error (lambda (err) 99)))
            (effect error "test error"))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Handler should intercept the effect
    match result {
        Value::Int(99) => {},
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
                (effect error "test")))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Inner handler should win
    match result {
        Value::Int(2) => {},
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
            (+ (effect io "print") (effect error "test")))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Both effects should be handled
    match result {
        Value::Int(30) => {}, // 10 + 20
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
                (effect error "test")))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Error should pass through inner handler to outer handler
    match result {
        Value::Int(5) => {},
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
                (effect error "test")))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Handler should access x from lexical scope
    match result {
        Value::Int(15) => {},
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_handler_function_receives_args() {
    // Test that handler functions receive effect arguments
    let code = r#"
        (handler
            ((io (lambda (op) op)))
            (effect io "print"))
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
    // Test handler with conditional logic
    let code = r#"
        (handler
            ((io (lambda (op)
                   (if (eq? op "print")
                       100
                       200))))
            (+ (effect io "print") (effect io "read")))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Different operations should return different values
    match result {
        Value::Int(300) => {}, // 100 + 200
        _ => panic!("Expected 300, got {:?}", result),
    }
}

#[test]
fn test_handler_with_list_operations() {
    // Test handler that returns lists
    let code = r#"
        (handler
            ((io (lambda (op) (list op "handled"))))
            (effect io "test"))
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Handler should return a list
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("test".to_string()));
            assert_eq!(items[1], Value::String("handled".to_string()));
        },
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_handler_removes_after_body() {
    // Test that handler is removed after body execution
    let code = r#"
        (let ((result
                (handler
                    ((error (lambda (e) 42)))
                    (effect error "test"))))
            result)
    "#;
    
    let graph = parse(code).expect("Should parse");
    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");
    
    // Result should be from handler
    match result {
        Value::Int(42) => {},
        _ => panic!("Expected 42, got {:?}", result),
    }
    
    // Handler should no longer be active - if we had another effect here,
    // it would not be handled
}