//! Example demonstrating symbolic execution for contract verification

use fluentai_parser::parse;
use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
    SymbolicExecutor, SymbolicValue,
};

/// Helper to find function definitions in the graph
fn find_definition(graph: &fluentai_core::ast::Graph, function_name: &str) -> Option<fluentai_core::ast::NodeId> {
    use fluentai_core::ast::Node;
    // Look through all nodes for a Let or Letrec binding with the given name
    for (_id, node) in &graph.nodes {
        match node {
            Node::Let { bindings, .. } | Node::Letrec { bindings, .. } => {
                for (name, value_id) in bindings {
                    if name == function_name {
                        return Some(*value_id);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Simple arithmetic function with branches
    println!("=== Example 1: Absolute Value Function ===");
    {
        let program = r#"
            (define (abs x)
              (if (< x 0)
                  (- 0 x)
                  x))
        "#;
        
        let graph = parse(program)?;
        
        // Find the function node
        let function_id = find_definition(&graph, "abs")
            .expect("Function 'abs' not found");
        
        // Create a contract: result should always be non-negative
        let contract = Contract {
            function_name: "abs".to_string(),
            preconditions: vec![],
            postconditions: vec![
                ContractCondition {
                    expression: graph.add_node(fluentai_core::ast::Node::Application {
                        function: graph.add_node(fluentai_core::ast::Node::Variable { name: ">=".to_string() } ),
                        args: vec![
                            graph.add_node(fluentai_core::ast::Node::Variable { name: "result".to_string() } ),
                            graph.add_node(fluentai_core::ast::Node::Literal(
                                fluentai_core::ast::Literal::Integer(0)
                            )),
                        ],
                    }),
                    message: Some("Result must be non-negative".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: None,
            pure: true,
            node_id: function_id,
        };
        
        // Create symbolic executor
        let executor = SymbolicExecutor::new();
        
        // Execute symbolically
        let states = executor.execute_function(&graph, function_id, &["x".to_string()])?;
        
        println!("Explored {} execution paths", states.len());
        
        for (i, state) in states.iter().enumerate() {
            println!("\nPath {}:", i + 1);
            println!("  Path constraints:");
            for constraint in &state.path_constraints {
                println!("    {:?} should be {}", constraint.constraint, constraint.expected);
            }
            println!("  Final bindings:");
            for (var, value) in &state.bindings {
                println!("    {} = {:?}", var, value);
            }
        }
    }
    
    // Example 2: Function with multiple branches
    println!("\n\n=== Example 2: Sign Function ===");
    {
        let program = r#"
            (define (sign x)
              (if (< x 0)
                  -1
                  (if (> x 0)
                      1
                      0)))
        "#;
        
        let graph = parse(program)?;
        
        let function_id = find_definition(&graph, "sign")
            .expect("Function 'sign' not found");
        
        let executor = SymbolicExecutor::new();
        let states = executor.execute_function(&graph, function_id, &["x".to_string()])?;
        
        println!("Explored {} execution paths", states.len());
        
        for (i, state) in states.iter().enumerate() {
            println!("\nPath {}:", i + 1);
            println!("  Path constraints:");
            for constraint in &state.path_constraints {
                match &constraint.constraint {
                    SymbolicValue::BinOp { op, left, right } => {
                        println!("    {:?} {} {:?} should be {}", left, op, right, constraint.expected);
                    }
                    _ => println!("    {:?} should be {}", constraint.constraint, constraint.expected),
                }
            }
        }
    }
    
    // Example 3: Function with loops (unrolled)
    println!("\n\n=== Example 3: Sum Function ===");
    {
        let program = r#"
            (define (sum-to-n n)
              (define (sum-helper i acc)
                (if (> i n)
                    acc
                    (sum-helper (+ i 1) (+ acc i))))
              (sum-helper 1 0))
        "#;
        
        let graph = parse(program)?;
        
        let function_id = find_definition(&graph, "sum-to-n")
            .expect("Function 'sum-to-n' not found");
        
        // Use limited depth to avoid infinite recursion
        let executor = SymbolicExecutor::with_limits(10, 20);
        
        match executor.execute_function(&graph, function_id, &["n".to_string()]) {
            Ok(states) => {
                println!("Explored {} execution paths (with depth limit)", states.len());
            }
            Err(e) => {
                println!("Execution stopped due to limits: {}", e);
            }
        }
    }
    
    // Example 4: Contract verification
    println!("\n\n=== Example 4: Contract Verification ===");
    {
        let program = r#"
            (define (safe-divide x y)
              (if (= y 0)
                  0
                  (/ x y)))
        "#;
        
        let graph = parse(program)?;
        
        let function_id = find_definition(&graph, "safe-divide")
            .expect("Function 'safe-divide' not found");
        
        // Contract: if y != 0, result should be x/y
        let contract = Contract {
            function_name: "safe-divide".to_string(),
            preconditions: vec![],
            postconditions: vec![
                ContractCondition {
                    expression: graph.add_node(fluentai_core::ast::Node::Application {
                        function: graph.add_node(fluentai_core::ast::Node::Variable { name: "or".to_string() } ),
                        args: vec![
                            graph.add_node(fluentai_core::ast::Node::Application {
                                function: graph.add_node(fluentai_core::ast::Node::Variable { name: "=".to_string() } ),
                                args: vec![
                                    graph.add_node(fluentai_core::ast::Node::Variable { name: "y".to_string() } ),
                                    graph.add_node(fluentai_core::ast::Node::Literal(
                                        fluentai_core::ast::Literal::Integer(0)
                                    )),
                                ],
                            }),
                            graph.add_node(fluentai_core::ast::Node::Application {
                                function: graph.add_node(fluentai_core::ast::Node::Variable { name: "=".to_string() } ),
                                args: vec![
                                    graph.add_node(fluentai_core::ast::Node::Variable { name: "result".to_string() } ),
                                    graph.add_node(fluentai_core::ast::Node::Application {
                                        function: graph.add_node(fluentai_core::ast::Node::Variable { name: "/".to_string() } ),
                                        args: vec![
                                            graph.add_node(fluentai_core::ast::Node::Variable { name: "x".to_string() } ),
                                            graph.add_node(fluentai_core::ast::Node::Variable { name: "y".to_string() } ),
                                        ],
                                    }),
                                ],
                            }),
                        ],
                    }),
                    message: Some("Result should be x/y when y != 0".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: None,
            pure: true,
            node_id: function_id,
        };
        
        let executor = SymbolicExecutor::new();
        let result = executor.verify_contract(&graph, &contract)?;
        
        println!("Contract verification result:");
        println!("  Total paths: {}", result.total_paths);
        println!("  Verified paths: {}", result.verified_paths);
        println!("  Violations: {}", result.violations.len());
        
        if !result.violations.is_empty() {
            println!("\nViolations found:");
            for (i, violation) in result.violations.iter().enumerate() {
                println!("  Violation {}: {}", i + 1, violation.condition.message.as_deref().unwrap_or("No message"));
            }
        }
    }
    
    Ok(())
}