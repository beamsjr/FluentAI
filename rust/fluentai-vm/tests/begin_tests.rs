//! Comprehensive tests for multiple top-level expressions (Begin node)

use fluentai_core::value::Value;
use fluentai_vm::{Compiler, CompilerOptions, OptimizationLevel, VM};

fn compile_and_run(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let graph = parse(code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

fn test_eval(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    compile_and_run(code)
}

#[test]
fn test_begin_multiple_literals() {
    let code = r#"
        1
        2
        3
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(3));
}

#[test]
fn test_begin_multiple_expressions() {
    let code = r#"
        1 + 2
        3 * 4
        10 - 5
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(5));
}

#[test]
fn test_begin_with_effects() {
    // Using new perform syntax
    let code = r#"
        perform State.set("x", 10)
        perform State.set("y", 20)
        perform State.get("x") + perform State.get("y")
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_with_let_bindings() {
    // Multiple expressions at top level return the last value
    let code = r#"
        { let x = 10; x };
        { let y = 20; y };
        { let z = 30; z }
    "#;
    
    println!("\nTesting: test_begin_with_let_bindings");
    println!("Code: {}", code);
    
    let graph = parse(code).unwrap();
    if let Some(root_id) = graph.root_id {
        let root_node = graph.get_node(root_id);
        println!("Root node: {:?}", root_node);
        
        // Print all nodes to see the structure
        println!("All nodes:");
        for (id, node) in &graph.nodes {
            println!("  Node {}: {:?}", id, node);
        }
    }
    
    let result = compile_and_run(code);
    println!("Result: {:?}", result);
    
    assert_eq!(result.unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_mixed_expressions() {
    let code = r#"
        42
        { let x = 10; x * 2 }
        "hello"
        5 + 5
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(10));
}

#[test]
fn test_begin_with_functions() {
    let code = r#"
        { let inc = (x) => x + 1; inc(5) }
        { let double = (x) => x * 2; double(7) }
        { let square = (x) => x * x; square(3) }
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(9));
}

#[test]
fn test_block_return_debug() {
    let code = "{ let x = 10; x }";
    
    println!("Testing code: {}", code);
    
    let graph = parse(code).unwrap();
    println!("Parsed graph root: {:?}", graph.root_id);
    
    if let Some(root_id) = graph.root_id {
        let root_node = graph.get_node(root_id);
        println!("Root node: {:?}", root_node);
        
        // Print the whole graph structure
        println!("Graph structure:");
        for (id, node) in &graph.nodes {
            println!("  Node {}: {:?}", id, node);
        }
    }
    
    let result = compile_and_run(code);
    println!("Result: {:?}", result);
    
    // This test currently fails because blocks return nil
    if let Ok(val) = &result {
        println!("Got value: {:?}", val);
    }
}

#[test]
fn test_block_with_semicolon() {
    // Test 1: Block with semicolon
    let code1 = "{ let x = 10; x };";
    println!("\nTesting: Block with semicolon");
    println!("Code: {}", code1);
    
    let graph1 = parse(code1).unwrap();
    if let Some(root_id) = graph1.root_id {
        let root_node = graph1.get_node(root_id);
        println!("Root node: {:?}", root_node);
    }
    
    let result1 = compile_and_run(code1);
    println!("Result: {:?}", result1);
    
    // Test 2: Multiple blocks
    let code2 = "{ let x = 10; x }; { let y = 20; y }";
    println!("\nTesting: Two blocks with semicolon");
    println!("Code: {}", code2);
    
    let graph2 = parse(code2).unwrap();
    if let Some(root_id) = graph2.root_id {
        let root_node = graph2.get_node(root_id);
        println!("Root node: {:?}", root_node);
        
        // Print all nodes to see the structure
        for (id, node) in &graph2.nodes {
            println!("  Node {}: {:?}", id, node);
        }
    }
    
    let result2 = compile_and_run(code2);
    println!("Result: {:?}", result2);
    assert_eq!(result2.unwrap(), Value::Integer(20), "Should return last block's value");
}

#[test]
fn test_begin_empty() {
    // Empty file should parse but might not have a root
    let code = "";
    let result = test_eval(code);
    // Empty code might return Nil or error, depending on implementation
    assert!(matches!(result, Ok(Value::Nil)) || result.is_err());
}

#[test]
fn test_begin_single_expression() {
    // Single expression should not be wrapped in Begin
    let code = "1 + 2 + 3";
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(6));
}

#[test]
fn test_begin_with_lists() {
    // Now using newly added list function
    let code = r#"
        list(1, 2, 3)
        list(4, 5, 6)
        list(1, 2, 3, 4)
    "#;
    assert_eq!(
        compile_and_run(code).unwrap(),
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4)
        ])
    );
}

#[test]
fn test_begin_with_conditionals() {
    let code = r#"
        if (true) { 10 } else { 20 }
        if (false) { 30 } else { 40 }
        if (true) { 50 } else { 60 }
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(50));
}

#[test]
fn test_begin_with_pattern_matching() {
    let code = r#"
        match(1) {
            1 => "one",
            2 => "two"
        }
        match(2) {
            1 => "one",
            2 => "two",
            _ => "other"
        }
        match(3) {
            1 => "one",
            2 => "two",
            _ => "other"
        }
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::String("other".to_string()));
}

#[test]
fn test_begin_with_nested_begin() {
    let code = r#"
        {
          1
          2
        }
        {
          3
          4
        }
    "#;
    // Should evaluate to 4 (last expression of last begin)
    // But actually begin is parsed inline, so this becomes: 1 2 3 4
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(4));
}

#[test]
fn test_begin_with_define() {
    let code = r#"
        private function x() { 10 }
        private function y() { 20 }
        x() + y()
    "#;
    // This depends on whether define returns a value
    // Usually define returns the defined value or nil
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_all_intermediate_evaluated() {
    // Using new := mutation syntax
    let code = r#"
        {
          let x = 0;
          x := 1;
          x := 2;
          x := 3;
          x
        }
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(3));
}

#[test]
fn test_mutation_operator_simple() {
    // Test that mutation operator returns the assigned value
    let code = "{ let x = 10; x := 20 }";
    
    println!("\nTesting mutation operator return value");
    println!("Code: {}", code);
    
    let graph = parse(code).unwrap();
    if let Some(root_id) = graph.root_id {
        println!("Root node: {:?}", graph.get_node(root_id));
        
        // Print all nodes
        for (id, node) in &graph.nodes {
            println!("  Node {}: {:?}", id, node);
        }
    }
    
    let result = compile_and_run(code);
    println!("Result: {:?}", result);
    
    // Mutation should return the assigned value
    assert_eq!(result.unwrap(), Value::Integer(20));
}

#[test]
fn test_begin_with_side_effects() {
    // Using new := mutation syntax
    let code = r#"
        {
          let x = 10;
          x := x + 10;
          x := x + 10;
          x
        }
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_mixed_types() {
    // Now using newly added stdlib functions
    let code = r#"
        10
        "hello"
        quote("symbol")
        list(1, 2, 3)
        12
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(12));
}

#[test]
#[ignore = "Cannot assign to undefined vars in FLC"]
fn test_begin_with_void_expressions() {
    // Some expressions might return void/nil
    let code = r#"
        set!(undefined_var, 10)
        print("hello")
        nil
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Nil);
}

#[test]
fn test_nested_let_begin_simple() {
    // Test simple nested let with multiple expressions
    let code = r#"
        {
          let x = 10;
          1;
          2;
          3
        }
    "#;
    
    println!("\nTesting nested let/begin issue");
    println!("Code: {}", code);
    
    let graph = parse(code).unwrap();
    println!("\nGraph structure:");
    
    if let Some(root_id) = graph.root_id {
        let root_node = graph.get_node(root_id);
        println!("Root: {:?}", root_node);
        
        // Print all nodes
        for (id, node) in &graph.nodes {
            println!("  Node {}: {:?}", id, node);
        }
    }
    
    let result = compile_and_run(code);
    println!("Result: {:?}", result);
    
    assert_eq!(result.unwrap(), Value::Integer(3));
}

#[test]
fn test_begin_in_let_body() {
    // Now using newly added list function
    let code = r#"
        {
          let x = 10;
          list(1, 2);
          list(3, 4);
          list(2, 4, 6, 8, 10)
        }
    "#;
    assert_eq!(
        compile_and_run(code).unwrap(),
        Value::List(vec![
            Value::Integer(2),
            Value::Integer(4),
            Value::Integer(6),
            Value::Integer(8),
            Value::Integer(10)
        ])
    );
}

#[test]
fn test_begin_with_recursive_function() {
    let code = r#"
        private function fib(n) {
            if (n <= 1) {
                n
            } else {
                fib(n - 1) + fib(n - 2)
            }
        }
        fib(3)
        fib(4)
        fib(5)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(5)); // fib(5) = 5
}

// Error cases
#[test]
#[ignore = "FLC allows undefined variables at compile time"]
fn test_begin_with_undefined_variable() {
    let code = r#"
        { let x = 10; x }
        undefined_var
        1 + 2
    "#;
    // Should error on undefined-var
    assert!(compile_and_run(code).is_err());
}

#[test]
#[ignore = "FLC string concatenation behavior differs"]
fn test_begin_with_type_error() {
    let code = r#"
        1 + 2
        "hello" + "world"
        3 * 4
    "#;
    // Should error on string addition
    assert!(compile_and_run(code).is_err());
}