//! Tests for runtime-guided optimization

use fluentai_core::ast::{Graph, Node, NodeId};
use fluentai_optimizer::runtime_guided::{
    create_runtime_guided_pipeline, RuntimeGuidedConfig, HotPathInliningPass,
    ValueSpecializationPass, AdaptiveLoopUnrollingPass,
};
use fluentai_vm::profiler::Profiler;
use std::sync::Arc;

#[test]
fn test_runtime_guided_pipeline() {
    // Create a profiler with some mock data
    let profiler = Arc::new(Profiler::new());
    profiler.enable();
    
    // Create a simple AST graph
    let mut graph = Graph::new();
    let func_node = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: NodeId(std::num::NonZeroU32::new(2).unwrap()),
    }).unwrap();
    
    // Create runtime-guided optimization pipeline
    let pipeline = create_runtime_guided_pipeline(profiler);
    
    // The pipeline should have 3 passes
    assert_eq!(pipeline.len(), 3);
    
    // Run each pass
    for mut pass in pipeline {
        let result = pass.run(&graph);
        assert!(result.is_ok());
        let stats = pass.stats();
        println!("Pass stats: {}", stats);
    }
}

#[test]
fn test_hot_path_inlining_pass() {
    let profiler = Arc::new(Profiler::new());
    profiler.enable();
    
    // Simulate hot function by recording many calls
    for _ in 0..1100 {
        let start = profiler.record_function_entry(1, Some("hot_func".to_string()));
        if let Some(start_time) = start {
            profiler.record_function_exit(1, start_time);
        }
    }
    
    let config = RuntimeGuidedConfig {
        profiler: profiler.clone(),
        hot_function_threshold: 1000,
        ..Default::default()
    };
    
    let mut pass = HotPathInliningPass::new(config);
    let graph = Graph::new();
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    
    // Check that hot functions were detected
    let hot_functions = profiler.get_hot_functions();
    assert_eq!(hot_functions.len(), 1);
    assert_eq!(hot_functions[0].call_count, 1100);
}

#[test]
fn test_value_specialization_pass() {
    let profiler = Arc::new(Profiler::new());
    profiler.enable();
    
    let node_id = NodeId(std::num::NonZeroU32::new(1).unwrap());
    
    // Record skewed values (90% are "42")
    for i in 0..100 {
        if i < 90 {
            profiler.record_value(node_id, "int:42");
        } else {
            profiler.record_value(node_id, &format!("int:{}", i));
        }
    }
    
    let config = RuntimeGuidedConfig {
        profiler: profiler.clone(),
        value_specialization_threshold: 0.8,
        ..Default::default()
    };
    
    let mut pass = ValueSpecializationPass::new(config);
    let graph = Graph::new();
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    
    // Check that skewed values were detected
    let skewed_values = profiler.get_skewed_values(0.8);
    assert_eq!(skewed_values.len(), 1);
    assert_eq!(skewed_values[0].most_common.as_ref().unwrap().0, "int:42");
}

#[test]
fn test_adaptive_loop_unrolling_pass() {
    let profiler = Arc::new(Profiler::new());
    profiler.enable();
    
    let loop_id = NodeId(std::num::NonZeroU32::new(1).unwrap());
    
    // Record loop iterations with consistent count
    for _ in 0..20 {
        profiler.record_loop_iteration(loop_id, 8);
    }
    
    let config = RuntimeGuidedConfig {
        profiler: profiler.clone(),
        loop_unroll_threshold: 4.0,
        ..Default::default()
    };
    
    let mut pass = AdaptiveLoopUnrollingPass::new(config);
    let graph = Graph::new();
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    
    // Check that hot loops were detected
    let hot_loops = profiler.get_hot_loops();
    assert_eq!(hot_loops.len(), 1);
    assert_eq!(hot_loops[0].avg_iterations, 8.0);
}