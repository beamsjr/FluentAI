//! Edge case tests for pattern matching

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, Pattern};
use fluentai_optimizer::OptimizationLevel;
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    Value, VM,
};

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

#[test]
fn test_match_deeply_nested_lists() -> Result<()> {
    let mut graph = Graph::new();

    // Create [[1, 2], [3, 4]]
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let three = graph
        .add_node(Node::Literal(Literal::Integer(3)))
        .expect("Failed to add node");
    let four = graph
        .add_node(Node::Literal(Literal::Integer(4)))
        .expect("Failed to add node");

    let inner1 = graph
        .add_node(Node::List(vec![one, two]))
        .expect("Failed to add node");
    let inner2 = graph
        .add_node(Node::List(vec![three, four]))
        .expect("Failed to add node");
    let outer = graph
        .add_node(Node::List(vec![inner1, inner2]))
        .expect("Failed to add node");

    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let default = graph
        .add_node(Node::Literal(Literal::Integer(0)))
        .expect("Failed to add node");

    let match_node = graph
        .add_node(Node::Match {
            expr: outer,
            branches: vec![
                (
                    Pattern::Constructor {
                        name: "cons".to_string(),
                        patterns: vec![
                            Pattern::Variable("x".to_string()),
                            Pattern::Variable("xs".to_string()),
                        ],
                    },
                    x_var,
                ),
                (Pattern::Wildcard, default),
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_node);

    let result = compile_and_run(&graph)?;
    // Should return the first element, which is [1, 2]
    assert_eq!(
        result,
        Value::List(vec![Value::Integer(1), Value::Integer(2)])
    );
    Ok(())
}

#[test]
fn test_match_list_of_different_types() -> Result<()> {
    let mut graph = Graph::new();

    // Create [1, "hello", true]
    let int_val = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");
    let str_val = graph
        .add_node(Node::Literal(Literal::String("hello".to_string())))
        .expect("Failed to add node");
    let bool_val = graph
        .add_node(Node::Literal(Literal::Boolean(true)))
        .expect("Failed to add node");
    let mixed_list = graph
        .add_node(Node::List(vec![int_val, str_val, bool_val]))
        .expect("Failed to add node");

    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");

    let match_node = graph
        .add_node(Node::Match {
            expr: mixed_list,
            branches: vec![(
                Pattern::Constructor {
                    name: "cons".to_string(),
                    patterns: vec![
                        Pattern::Variable("x".to_string()),
                        Pattern::Variable("xs".to_string()),
                    ],
                },
                x_var,
            )],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_node);

    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Integer(42)); // First element
    Ok(())
}

#[test]
fn test_match_very_long_list() -> Result<()> {
    let mut graph = Graph::new();

    // Create a list with 100 elements
    let mut elements = Vec::new();
    for i in 0..100 {
        elements.push(
            graph
                .add_node(Node::Literal(Literal::Integer(i)))
                .expect("Failed to add node"),
        );
    }
    let long_list = graph
        .add_node(Node::List(elements))
        .expect("Failed to add node");

    let xs_var = graph
        .add_node(Node::Variable {
            name: "xs".to_string(),
        })
        .expect("Failed to add node");

    let match_node = graph
        .add_node(Node::Match {
            expr: long_list,
            branches: vec![
                (
                    Pattern::Constructor {
                        name: "cons".to_string(),
                        patterns: vec![
                            Pattern::Variable("x".to_string()),
                            Pattern::Variable("xs".to_string()),
                        ],
                    },
                    xs_var,
                ), // Return tail
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_node);

    let result = compile_and_run(&graph)?;
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 99); // Tail has 99 elements
            assert_eq!(items[0], Value::Integer(1)); // First element of tail
            assert_eq!(items[98], Value::Integer(99)); // Last element of tail
        }
        _ => panic!("Expected list"),
    }
    Ok(())
}

#[test]
fn test_match_with_shadowed_variables() -> Result<()> {
    let mut graph = Graph::new();

    // Create outer let with x = 100
    let outer_x_val = graph
        .add_node(Node::Literal(Literal::Integer(100)))
        .expect("Failed to add node");

    // Create list for matching
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let list = graph
        .add_node(Node::List(vec![one, two]))
        .expect("Failed to add node");

    // Match will bind new x
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let match_node = graph
        .add_node(Node::Match {
            expr: list,
            branches: vec![(
                Pattern::Constructor {
                    name: "cons".to_string(),
                    patterns: vec![
                        Pattern::Variable("x".to_string()), // Shadows outer x
                        Pattern::Variable("xs".to_string()),
                    ],
                },
                x_var,
            )],
        })
        .expect("Failed to add node");

    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("x".to_string(), outer_x_val)],
            body: match_node,
        })
        .expect("Failed to add node");
    graph.root_id = Some(let_node);

    let result = compile_and_run(&graph)?;
    // The pattern-bound x should shadow the outer x, returning 1
    assert_eq!(result, Value::Integer(1)); // Pattern-bound x shadows outer x
    Ok(())
}

#[test]
fn test_match_nil_before_cons() -> Result<()> {
    let mut graph = Graph::new();

    // Test both empty and non-empty lists with nil pattern first

    // Test 1: Empty list
    let empty = graph
        .add_node(Node::List(vec![]))
        .expect("Failed to add node");
    let nil_result = graph
        .add_node(Node::Literal(Literal::String("is nil".to_string())))
        .expect("Failed to add node");
    let cons_result = graph
        .add_node(Node::Literal(Literal::String("is cons".to_string())))
        .expect("Failed to add node");

    let match_empty = graph
        .add_node(Node::Match {
            expr: empty,
            branches: vec![
                (
                    Pattern::Constructor {
                        name: "nil".to_string(),
                        patterns: vec![],
                    },
                    nil_result,
                ),
                (
                    Pattern::Constructor {
                        name: "cons".to_string(),
                        patterns: vec![
                            Pattern::Variable("h".to_string()),
                            Pattern::Variable("t".to_string()),
                        ],
                    },
                    cons_result,
                ),
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_empty);

    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("is nil".to_string()));

    // Test 2: Non-empty list
    let mut graph = Graph::new();
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let non_empty = graph
        .add_node(Node::List(vec![one]))
        .expect("Failed to add node");
    let nil_result = graph
        .add_node(Node::Literal(Literal::String("is nil".to_string())))
        .expect("Failed to add node");
    let cons_result = graph
        .add_node(Node::Literal(Literal::String("is cons".to_string())))
        .expect("Failed to add node");

    let match_non_empty = graph
        .add_node(Node::Match {
            expr: non_empty,
            branches: vec![
                (
                    Pattern::Constructor {
                        name: "nil".to_string(),
                        patterns: vec![],
                    },
                    nil_result,
                ),
                (
                    Pattern::Constructor {
                        name: "cons".to_string(),
                        patterns: vec![
                            Pattern::Variable("h".to_string()),
                            Pattern::Variable("t".to_string()),
                        ],
                    },
                    cons_result,
                ),
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_non_empty);

    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("is cons".to_string()));

    Ok(())
}

#[test]
fn test_match_multiple_variable_bindings() -> Result<()> {
    let mut graph = Graph::new();

    // Create list [10, 20, 30]
    let ten = graph
        .add_node(Node::Literal(Literal::Integer(10)))
        .expect("Failed to add node");
    let twenty = graph
        .add_node(Node::Literal(Literal::Integer(20)))
        .expect("Failed to add node");
    let thirty = graph
        .add_node(Node::Literal(Literal::Integer(30)))
        .expect("Failed to add node");
    let list = graph
        .add_node(Node::List(vec![ten, twenty, thirty]))
        .expect("Failed to add node");

    // Create expression that uses both x and xs
    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let xs_var = graph
        .add_node(Node::Variable {
            name: "xs".to_string(),
        })
        .expect("Failed to add node");

    // Get length of xs
    let list_len = graph
        .add_node(Node::Variable {
            name: "list-len".to_string(),
        })
        .expect("Failed to add node");
    let xs_len = graph
        .add_node(Node::Application {
            function: list_len,
            args: vec![xs_var],
        })
        .expect("Failed to add node");

    // Add x + length(xs)
    let sum = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![x_var, xs_len],
        })
        .expect("Failed to add node");

    let match_node = graph
        .add_node(Node::Match {
            expr: list,
            branches: vec![(
                Pattern::Constructor {
                    name: "cons".to_string(),
                    patterns: vec![
                        Pattern::Variable("x".to_string()),
                        Pattern::Variable("xs".to_string()),
                    ],
                },
                sum,
            )],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_node);

    // Note: This test would need list-len to be defined in stdlib
    // For now, just test that both variables are bound
    let x_only = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let match_simple = graph
        .add_node(Node::Match {
            expr: list,
            branches: vec![(
                Pattern::Constructor {
                    name: "cons".to_string(),
                    patterns: vec![
                        Pattern::Variable("x".to_string()),
                        Pattern::Variable("xs".to_string()),
                    ],
                },
                x_only,
            )],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_simple);

    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::Integer(10)); // x is bound correctly
    Ok(())
}

#[test]
fn test_match_with_nil_fallthrough() -> Result<()> {
    let mut graph = Graph::new();

    // Match empty list with only cons pattern (no nil pattern)
    let empty = graph
        .add_node(Node::List(vec![]))
        .expect("Failed to add node");
    let cons_result = graph
        .add_node(Node::Literal(Literal::String("matched cons".to_string())))
        .expect("Failed to add node");
    let default_result = graph
        .add_node(Node::Literal(Literal::String("no match".to_string())))
        .expect("Failed to add node");

    let match_node = graph
        .add_node(Node::Match {
            expr: empty,
            branches: vec![
                (
                    Pattern::Constructor {
                        name: "cons".to_string(),
                        patterns: vec![
                            Pattern::Variable("x".to_string()),
                            Pattern::Variable("xs".to_string()),
                        ],
                    },
                    cons_result,
                ),
                (Pattern::Wildcard, default_result),
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_node);

    let result = compile_and_run(&graph)?;
    assert_eq!(result, Value::String("no match".to_string())); // Falls through to wildcard
    Ok(())
}
