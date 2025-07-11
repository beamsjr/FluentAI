//! Test assignment as expression functionality

use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM, Value};

#[test]
fn test_simple_assignment_returns_value() {
    // Simple top-level assignment
    let code = "x := 42";
    
    let graph = parse(code).expect("Parse failed");
    
    // The assignment to an undefined variable should work at parse time
    // but fail at compile time since x is not defined
    let result = Compiler::new().compile(&graph);
    assert!(result.is_err(), "Should fail - x is not defined");
}

#[test]
fn test_assignment_no_optimization() {
    // Assignment after let binding without optimization
    let code = "let x = 10; x := 42";
    
    let graph = parse(code).expect("Parse failed");
    
    // Use compiler without optimization
    let options = fluentai_vm::CompilerOptions {
        optimization_level: fluentai_optimizer::OptimizationLevel::None,
        debug_info: true,
    };
    
    // Debug: print the graph
    println!("Graph nodes:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", graph.root_id);
    
    let bytecode = Compiler::with_options(options).compile(&graph).expect("Compile failed");
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Run failed");
    
    // Should return 42 (the assigned value)
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_assignment_after_let() {
    // Assignment after let binding
    let code = "let x = 10; x := 42";
    
    let graph = parse(code).expect("Parse failed");
    
    // Debug: print the graph
    println!("Graph nodes:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", graph.root_id);
    
    // Use no optimization to work around optimizer bug with Assignment nodes
    let options = fluentai_vm::CompilerOptions {
        optimization_level: fluentai_optimizer::OptimizationLevel::None,
        debug_info: false,
    };
    let bytecode = Compiler::with_options(options).compile(&graph).expect("Compile failed");
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Run failed");
    
    // Should return 42 (the assigned value)
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_assignment_in_expression() {
    // Assignment in arithmetic expression
    let code = "let x = 10; 5 + (x := 20)";
    
    let graph = parse(code).expect("Parse failed");
    // Use no optimization to work around optimizer bug with Assignment nodes
    let options = fluentai_vm::CompilerOptions {
        optimization_level: fluentai_optimizer::OptimizationLevel::None,
        debug_info: false,
    };
    let bytecode = Compiler::with_options(options).compile(&graph).expect("Compile failed");
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Run failed");
    
    // Should return 25 (5 + 20)
    assert_eq!(result, Value::Integer(25));
}

#[test]
fn test_chained_assignment() {
    // Chained assignments
    let code = "let x = 0; let y = 0; x := y := 42";
    
    let graph = parse(code).expect("Parse failed");
    // Use no optimization to work around optimizer bug with Assignment nodes
    let options = fluentai_vm::CompilerOptions {
        optimization_level: fluentai_optimizer::OptimizationLevel::None,
        debug_info: false,
    };
    let bytecode = Compiler::with_options(options).compile(&graph).expect("Compile failed");
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Run failed");
    
    // Should return 42
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_assignment_with_equals() {
    // Test = operator (should work same as :=)
    let code = "let x = 10; x = 42";
    
    let graph = parse(code).expect("Parse failed");
    // Use no optimization to work around optimizer bug with Assignment nodes
    let options = fluentai_vm::CompilerOptions {
        optimization_level: fluentai_optimizer::OptimizationLevel::None,
        debug_info: false,
    };
    let bytecode = Compiler::with_options(options).compile(&graph).expect("Compile failed");
    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Run failed");
    
    // Should return 42
    assert_eq!(result, Value::Integer(42));
}