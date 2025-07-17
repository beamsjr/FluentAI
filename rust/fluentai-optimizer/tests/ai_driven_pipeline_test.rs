//! Tests for AI-driven optimization pipeline

use fluentai_optimizer::ai_driven::{create_ai_driven_pipeline, AIDrivenConfig};
use fluentai_optimizer::passes::effect_reordering::EffectReorderingPass;
use fluentai_optimizer::passes::subgraph_fusion::SubgraphFusionPass;
use fluentai_core::ast::{Graph, Node, NodeId, EffectType};

#[test]
fn test_ai_driven_pipeline_creation() {
    let pipeline = create_ai_driven_pipeline();
    
    // Should have 2 passes: effect reordering and subgraph fusion
    assert_eq!(pipeline.len(), 2);
    
    // Check pass names
    assert_eq!(pipeline[0].name(), "Effect Reordering");
    assert_eq!(pipeline[1].name(), "Subgraph Fusion");
}

#[test]
fn test_effect_reordering_in_pipeline() {
    let mut graph = Graph::new();
    
    // Create multiple IO effects that could be batched
    let print1 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    let compute = graph.add_node(Node::Literal(fluentai_core::ast::Literal::Integer(42))).unwrap();
    
    let print2 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    // Create a sequence: print -> compute -> print
    let seq = graph.add_node(Node::Begin {
        exprs: vec![print1, compute, print2],
    }).unwrap();
    
    graph.root_id = Some(seq);
    
    // Run the pipeline
    let mut pipeline = create_ai_driven_pipeline();
    let mut result = graph.clone();
    
    for pass in &mut pipeline {
        match pass.run(&result) {
            Ok(optimized) => result = optimized,
            Err(e) => panic!("Optimization failed: {}", e),
        }
    }
    
    // The result should still be valid
    assert!(result.root_id.is_some());
}

#[test]
fn test_subgraph_fusion_in_pipeline() {
    let mut graph = Graph::new();
    
    // Create a simple computation that might benefit from fusion
    let list = graph.add_node(Node::List(vec![])).unwrap();
    graph.root_id = Some(list);
    
    // Run the pipeline
    let mut pipeline = create_ai_driven_pipeline();
    let mut result = graph.clone();
    
    for pass in &mut pipeline {
        match pass.run(&result) {
            Ok(optimized) => result = optimized,
            Err(e) => panic!("Optimization failed: {}", e),
        }
    }
    
    // The result should still be valid
    assert!(result.root_id.is_some());
}

#[test]
fn test_ai_driven_config() {
    let config = AIDrivenConfig::default();
    
    assert!(config.enable_effect_reordering);
    assert!(config.enable_subgraph_fusion);
    assert!(config.enable_pattern_optimization);
    assert_eq!(config.confidence_threshold, 0.8);
}

#[test]
fn test_pipeline_with_effects_and_handlers() {
    let mut graph = Graph::new();
    
    // Create an effect within a handler
    let io_effect = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "read".to_string(),
        args: vec![],
    }).unwrap();
    
    let handler_body = graph.add_node(Node::Variable {
        name: "result".to_string(),
    }).unwrap();
    
    let handler = graph.add_node(Node::Handler {
        handlers: vec![(
            EffectType::IO,
            Some("read".to_string()),
            handler_body,
        )],
        body: io_effect,
    }).unwrap();
    
    graph.root_id = Some(handler);
    
    // Run the pipeline
    let mut pipeline = create_ai_driven_pipeline();
    let mut result = graph.clone();
    
    for pass in &mut pipeline {
        match pass.run(&result) {
            Ok(optimized) => result = optimized,
            Err(e) => panic!("Optimization failed: {}", e),
        }
        
        // Print stats for debugging
        println!("{}", pass.stats());
    }
    
    // The result should still be valid
    assert!(result.root_id.is_some());
}