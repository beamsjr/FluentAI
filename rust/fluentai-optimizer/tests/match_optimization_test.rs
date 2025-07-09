//! Test Match node optimization specifically

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, NodeId, Pattern};
use fluentai_optimizer::pipeline::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};

#[test]
fn test_match_optimization_simple() -> Result<()> {
    let mut graph = Graph::new();

    // Create a simple match: (match [1, 2] ((cons x xs) 99) (_ 0))
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let list_val = graph
        .add_node(Node::List(vec![one, two]))
        .expect("Failed to add node");

    let result_val = graph
        .add_node(Node::Literal(Literal::Integer(99)))
        .expect("Failed to add node");
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
                    result_val,
                ),
                (Pattern::Wildcard, zero),
            ],
        })
        .expect("Failed to add node");
    graph.root_id = Some(match_node);

    println!("Original graph nodes:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }

    // Optimize with standard level
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph)?;

    println!("\nOptimized graph nodes:");
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }

    // Verify the optimized graph structure
    assert!(optimized.root_id.is_some());
    let root = optimized.root_id.unwrap();
    assert!(optimized.get_node(root).is_some());

    // Check that all node references are valid
    check_node_references(&optimized, root)?;

    Ok(())
}

fn check_node_references(graph: &Graph, node_id: NodeId) -> Result<()> {
    let node = graph
        .get_node(node_id)
        .ok_or_else(|| anyhow::anyhow!("Invalid node ID: {:?}", node_id))?;

    match node {
        Node::Match { expr, branches } => {
            // Check expr reference
            if graph.get_node(*expr).is_none() {
                return Err(anyhow::anyhow!(
                    "Match expr references invalid node: {:?}",
                    expr
                ));
            }
            check_node_references(graph, *expr)?;

            // Check branch references
            for (_, branch_body) in branches {
                if graph.get_node(*branch_body).is_none() {
                    return Err(anyhow::anyhow!(
                        "Match branch references invalid node: {:?}",
                        branch_body
                    ));
                }
                check_node_references(graph, *branch_body)?;
            }
        }
        Node::List(items) => {
            for item in items {
                if graph.get_node(*item).is_none() {
                    return Err(anyhow::anyhow!(
                        "List item references invalid node: {:?}",
                        item
                    ));
                }
                check_node_references(graph, *item)?;
            }
        }
        _ => {}
    }

    Ok(())
}
