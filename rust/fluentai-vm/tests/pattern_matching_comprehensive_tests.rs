//! Comprehensive tests for pattern matching functionality

use fluentai_core::ast::{Graph, Node, Literal, Pattern};
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    Value,
    VM,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

fn compile_and_run(graph: &Graph) -> Result<Value> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

fn compile_and_run_optimized(graph: &Graph, level: OptimizationLevel) -> Result<Value> {
    let options = CompilerOptions {
        optimization_level: level,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

// ===== Literal Pattern Tests =====

#[test]
fn test_match_integer_literal() -> Result<()> {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    let result1 = graph.add_node(Node::Literal(Literal::String("forty-two".to_string()))).expect("Failed to add node");
    let result2 = graph.add_node(Node::Literal(Literal::String("not 42".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Literal(Literal::Integer(42)), result1),
            (Pattern::Wildcard, result2),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("forty-two".to_string()));
    Ok(())
}

#[test]
fn test_match_string_literal() -> Result<()> {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::String("hello".to_string()))).expect("Failed to add node");
    let result1 = graph.add_node(Node::Literal(Literal::String("greeting".to_string()))).expect("Failed to add node");
    let result2 = graph.add_node(Node::Literal(Literal::String("not greeting".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Literal(Literal::String("hello".to_string())), result1),
            (Pattern::Wildcard, result2),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("greeting".to_string()));
    Ok(())
}

#[test]
fn test_match_boolean_literals() -> Result<()> {
    let mut graph = Graph::new();
    
    // Test matching true
    let true_val = graph.add_node(Node::Literal(Literal::Boolean(true))).expect("Failed to add node");
    let true_result = graph.add_node(Node::Literal(Literal::String("is true".to_string()))).expect("Failed to add node");
    let false_result = graph.add_node(Node::Literal(Literal::String("is false".to_string()))).expect("Failed to add node");
    
    let match_true = graph.add_node(Node::Match {
        expr: true_val,
        branches: vec![
            (Pattern::Literal(Literal::Boolean(true)), true_result),
            (Pattern::Literal(Literal::Boolean(false)), false_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_true);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("is true".to_string()));
    
    // Test matching false
    let mut graph = Graph::new();
    let false_val = graph.add_node(Node::Literal(Literal::Boolean(false))).expect("Failed to add node");
    let true_result = graph.add_node(Node::Literal(Literal::String("is true".to_string()))).expect("Failed to add node");
    let false_result = graph.add_node(Node::Literal(Literal::String("is false".to_string()))).expect("Failed to add node");
    
    let match_false = graph.add_node(Node::Match {
        expr: false_val,
        branches: vec![
            (Pattern::Literal(Literal::Boolean(true)), true_result),
            (Pattern::Literal(Literal::Boolean(false)), false_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_false);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("is false".to_string()));
    Ok(())
}

// ===== Variable Pattern Tests =====

#[test]
fn test_match_variable_binding() -> Result<()> {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::Integer(100))).expect("Failed to add node");
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Variable("x".to_string()), x_var),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Integer(100));
    Ok(())
}

// ===== List Pattern Tests =====

#[test]
fn test_match_empty_list_with_nil() -> Result<()> {
    let mut graph = Graph::new();
    
    let empty_list = graph.add_node(Node::List(vec![])).expect("Failed to add node");
    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string()))).expect("Failed to add node");
    let non_empty_result = graph.add_node(Node::Literal(Literal::String("not empty".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: empty_list,
        branches: vec![
            (Pattern::Constructor { name: "nil".to_string(), patterns: vec![] }, empty_result),
            (Pattern::Wildcard, non_empty_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("empty".to_string()));
    Ok(())
}

#[test]
fn test_match_non_empty_list_with_cons() -> Result<()> {
    let mut graph = Graph::new();
    
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let three = graph.add_node(Node::Literal(Literal::Integer(3))).expect("Failed to add node");
    let list = graph.add_node(Node::List(vec![one, two, three])).expect("Failed to add node");
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: list,
        branches: vec![
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, x_var),
            (Pattern::Constructor { name: "nil".to_string(), patterns: vec![] }, empty_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Integer(1)); // First element
    Ok(())
}

#[test]
fn test_match_list_tail() -> Result<()> {
    let mut graph = Graph::new();
    
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let three = graph.add_node(Node::Literal(Literal::Integer(3))).expect("Failed to add node");
    let list = graph.add_node(Node::List(vec![one, two, three])).expect("Failed to add node");
    
    let xs_var = graph.add_node(Node::Variable { name: "xs".to_string() }).expect("Failed to add node");
    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: list,
        branches: vec![
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, xs_var),
            (Pattern::Constructor { name: "nil".to_string(), patterns: vec![] }, empty_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::List(vec![Value::Integer(2), Value::Integer(3)]));
    Ok(())
}

#[test]
fn test_match_single_element_list() -> Result<()> {
    let mut graph = Graph::new();
    
    let one = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    let list = graph.add_node(Node::List(vec![one])).expect("Failed to add node");
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    let xs_var = graph.add_node(Node::Variable { name: "xs".to_string() }).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: list,
        branches: vec![
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, xs_var), // Return the tail
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::List(vec![])); // Tail of single-element list is empty
    Ok(())
}

// ===== Wildcard Pattern Tests =====

#[test]
fn test_wildcard_pattern() -> Result<()> {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::Integer(999))).expect("Failed to add node");
    let default_result = graph.add_node(Node::Literal(Literal::String("default".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Wildcard, default_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("default".to_string()));
    Ok(())
}

#[test]
fn test_wildcard_with_other_patterns() -> Result<()> {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::Integer(999))).expect("Failed to add node");
    let specific_result = graph.add_node(Node::Literal(Literal::String("found 42".to_string()))).expect("Failed to add node");
    let default_result = graph.add_node(Node::Literal(Literal::String("not 42".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Literal(Literal::Integer(42)), specific_result),
            (Pattern::Wildcard, default_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("not 42".to_string()));
    Ok(())
}

// ===== Complex Pattern Tests =====

#[test]
fn test_multiple_pattern_branches() -> Result<()> {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let result1 = graph.add_node(Node::Literal(Literal::String("one".to_string()))).expect("Failed to add node");
    let result2 = graph.add_node(Node::Literal(Literal::String("two".to_string()))).expect("Failed to add node");
    let result3 = graph.add_node(Node::Literal(Literal::String("three".to_string()))).expect("Failed to add node");
    let default = graph.add_node(Node::Literal(Literal::String("other".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Literal(Literal::Integer(1)), result1),
            (Pattern::Literal(Literal::Integer(2)), result2),
            (Pattern::Literal(Literal::Integer(3)), result3),
            (Pattern::Wildcard, default),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("two".to_string()));
    Ok(())
}

// ===== Optimization Tests =====

#[test]
fn test_pattern_matching_with_standard_optimization() -> Result<()> {
    let mut graph = Graph::new();
    
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let list = graph.add_node(Node::List(vec![one, two])).expect("Failed to add node");
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    let default = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: list,
        branches: vec![
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, x_var),
            (Pattern::Wildcard, default),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run_optimized(&graph, OptimizationLevel::Standard)?;
    assert_eq!(result, Value::Integer(1));
    Ok(())
}

#[test]
fn test_pattern_matching_with_aggressive_optimization() -> Result<()> {
    let mut graph = Graph::new();
    
    let empty_list = graph.add_node(Node::List(vec![])).expect("Failed to add node");
    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string()))).expect("Failed to add node");
    let non_empty = graph.add_node(Node::Literal(Literal::String("has items".to_string()))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: empty_list,
        branches: vec![
            (Pattern::Constructor { name: "nil".to_string(), patterns: vec![] }, empty_result),
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, non_empty),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run_optimized(&graph, OptimizationLevel::Aggressive)?;
    assert_eq!(result, Value::String("empty".to_string()));
    Ok(())
}

// ===== Edge Case Tests =====

#[test]
fn test_match_with_no_branches_should_error() {
    let mut graph = Graph::new();
    
    let value = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    let match_node = graph.add_node(Node::Match {
        expr: value,
        branches: vec![], // No branches
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    // This should fail during compilation or execution
    let result = compile_and_run(&graph);
    assert!(result.is_err());
}

#[test]
fn test_match_list_with_wrong_pattern_order() -> Result<()> {
    let mut graph = Graph::new();
    
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let list = graph.add_node(Node::List(vec![one])).expect("Failed to add node");
    
    let nil_result = graph.add_node(Node::Literal(Literal::String("nil".to_string()))).expect("Failed to add node");
    let cons_result = graph.add_node(Node::Literal(Literal::String("cons".to_string()))).expect("Failed to add node");
    
    // Put nil pattern first, cons pattern second (opposite of typical order)
    let match_node = graph.add_node(Node::Match {
        expr: list,
        branches: vec![
            (Pattern::Constructor { name: "nil".to_string(), patterns: vec![] }, nil_result),
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, cons_result),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("cons".to_string())); // Should match cons, not nil
    Ok(())
}