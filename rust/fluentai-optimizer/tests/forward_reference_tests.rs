//! Tests for forward reference handling in optimizer passes

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, Pattern};
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};

#[test]
fn test_optimizer_handles_forward_references_in_match() -> Result<()> {
    // This test verifies the fix for the forward reference bug
    // where nodes referenced in match branches weren't properly mapped

    let mut graph = Graph::new();

    // Create a variable node that will be referenced later
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");

    // Create a list for matching
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let list_val = graph
        .add_node(Node::List(vec![one, two]))
        .expect("Failed to add node");

    // Create a match that references x_var in its branch
    let zero = graph
        .add_node(Node::Literal(Literal::Integer(0)))
        .expect("Failed to add node");
    let match_node = graph
        .add_node(Node::Match {
            expr: list_val,
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
                ), // Forward reference to x_var
                (Pattern::Wildcard, zero),
            ],
        })
        .expect("Failed to add node");

    graph.root_id = Some(match_node);

    // Run optimizer - should not fail with "Invalid node ID"
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let result = pipeline.optimize(&graph)?;

    // Verify the optimized graph is valid
    assert!(result.root_id.is_some());
    if let Some(root) = result.root_id {
        assert!(result.get_node(root).is_some());
    }

    Ok(())
}

#[test]
fn test_optimizer_handles_complex_forward_references() -> Result<()> {
    let mut graph = Graph::new();

    // Create multiple variables that will be referenced
    let a_var = graph
        .add_node(Node::Variable {
            name: "a".to_string(),
        })
        .expect("Failed to add node");
    let b_var = graph
        .add_node(Node::Variable {
            name: "b".to_string(),
        })
        .expect("Failed to add node");
    let c_var = graph
        .add_node(Node::Variable {
            name: "c".to_string(),
        })
        .expect("Failed to add node");

    // Create nested structure
    let inner_list = graph
        .add_node(Node::List(vec![a_var, b_var]))
        .expect("Failed to add node");
    let outer_list = graph
        .add_node(Node::List(vec![inner_list, c_var]))
        .expect("Failed to add node");

    // Create application that uses the variables
    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let app = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![a_var, b_var],
        })
        .expect("Failed to add node");

    // Create match that references the application
    let match_node = graph
        .add_node(Node::Match {
            expr: outer_list,
            branches: vec![(Pattern::Variable("lst".to_string()), app)],
        })
        .expect("Failed to add node");

    graph.root_id = Some(match_node);

    // Test with different optimization levels
    for level in [
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ] {
        let config = OptimizationConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);
        let result = pipeline.optimize(&graph)?;
        assert!(result.root_id.is_some());
    }

    Ok(())
}

#[test]
fn test_optimizer_preserves_variable_references_in_let() -> Result<()> {
    let mut graph = Graph::new();

    // Create variables that will be referenced in different scopes
    let x_var1 = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let x_var2 = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");

    // Create a value
    let value = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");

    // Create inner let that references x from outer scope
    let inner_let = graph
        .add_node(Node::Let {
            bindings: vec![("y".to_string(), x_var1)],
            body: x_var2,
        })
        .expect("Failed to add node");

    // Create outer let
    let outer_let = graph
        .add_node(Node::Let {
            bindings: vec![("x".to_string(), value)],
            body: inner_let,
        })
        .expect("Failed to add node");

    graph.root_id = Some(outer_let);

    // Run optimization pipeline
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let result = pipeline.optimize(&graph)?;

    assert!(result.root_id.is_some());
    Ok(())
}

#[test]
fn test_optimizer_handles_forward_references_in_lambda() -> Result<()> {
    let mut graph = Graph::new();

    // Create a variable that will be captured
    let free_var = graph
        .add_node(Node::Variable {
            name: "free".to_string(),
        })
        .expect("Failed to add node");

    // Create lambda body that references the variable
    let plus_var = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let one_val = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let body = graph
        .add_node(Node::Application {
            function: plus_var,
            args: vec![free_var, one_val],
        })
        .expect("Failed to add node");

    // Create lambda
    let lambda = graph
        .add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body,
        })
        .expect("Failed to add node");

    // Create let binding that provides the free variable
    let free_val = graph
        .add_node(Node::Literal(Literal::Integer(10)))
        .expect("Failed to add node");
    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("free".to_string(), free_val)],
            body: lambda,
        })
        .expect("Failed to add node");

    graph.root_id = Some(let_node);

    // Run optimizer
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let result = pipeline.optimize(&graph)?;

    assert!(result.root_id.is_some());
    Ok(())
}

#[test]
fn test_optimizer_handles_circular_references() -> Result<()> {
    let mut graph = Graph::new();

    // Create a letrec with mutual recursion
    let f_var = graph
        .add_node(Node::Variable {
            name: "f".to_string(),
        })
        .expect("Failed to add node");
    let g_var = graph
        .add_node(Node::Variable {
            name: "g".to_string(),
        })
        .expect("Failed to add node");

    // f calls g
    let arg1 = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let f_body = graph
        .add_node(Node::Application {
            function: g_var,
            args: vec![arg1],
        })
        .expect("Failed to add node");
    let f_lambda = graph
        .add_node(Node::Lambda {
            params: vec!["n".to_string()],
            body: f_body,
        })
        .expect("Failed to add node");

    // g calls f
    let arg2 = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let g_body = graph
        .add_node(Node::Application {
            function: f_var,
            args: vec![arg2],
        })
        .expect("Failed to add node");
    let g_lambda = graph
        .add_node(Node::Lambda {
            params: vec!["n".to_string()],
            body: g_body,
        })
        .expect("Failed to add node");

    let letrec = graph
        .add_node(Node::Letrec {
            bindings: vec![("f".to_string(), f_lambda), ("g".to_string(), g_lambda)],
            body: f_var,
        })
        .expect("Failed to add node");

    graph.root_id = Some(letrec);

    // Run optimizer - should handle circular references correctly
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let result = pipeline.optimize(&graph)?;

    assert!(result.root_id.is_some());
    Ok(())
}

#[test]
fn test_all_optimization_passes_handle_forward_references() -> Result<()> {
    // Test that each individual pass handles forward references correctly
    let mut graph = Graph::new();

    // Create a pattern that uses forward references
    let var_ref = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let list = graph
        .add_node(Node::List(vec![one]))
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
                var_ref,
            )],
        })
        .expect("Failed to add node");

    graph.root_id = Some(match_node);

    // Test each optimization level separately
    let levels = vec![
        ("None", OptimizationLevel::None),
        ("Basic", OptimizationLevel::Basic),
        ("Standard", OptimizationLevel::Standard),
        ("Aggressive", OptimizationLevel::Aggressive),
    ];

    for (name, level) in levels {
        let config = OptimizationConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);
        let result = pipeline.optimize(&graph);

        assert!(result.is_ok(), "Optimization level {} failed", name);
        let optimized = result?;
        assert!(
            optimized.root_id.is_some(),
            "Lost root node at level {}",
            name
        );
    }

    Ok(())
}
