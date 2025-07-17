//! Tests for ML-driven optimization integration

use fluentai_optimizer::ml_integration::{
    MLOptimizationCoordinator, MLOptimizationConfig, PerformanceMetrics
};
use fluentai_optimizer::runtime_guided::RuntimeGuidedConfig;
use fluentai_core::ast::{Graph, Node, NodeId, Literal, EffectType};
use fluentai_core::profiling::{FunctionProfileData, ValueProfileData, LoopProfileData};

/// Create a test graph with various optimization opportunities
fn create_test_graph() -> Graph {
    let mut graph = Graph::new();
    
    // Hot function that should be inlined
    let hot_func_body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let hot_func = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: hot_func_body,
    }).unwrap();
    
    // Multiple calls to hot function
    let mut calls = vec![];
    for i in 0..10 {
        let arg = graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
        let call = graph.add_node(Node::Application {
            function: hot_func,
            args: vec![arg],
        }).unwrap();
        calls.push(call);
    }
    
    // Effects that can be reordered
    let effect1 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    let pure_computation = graph.add_node(Node::Literal(Literal::Integer(100))).unwrap();
    
    let effect2 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    // Memory-intensive operations
    let list_elements: Vec<NodeId> = (0..100).map(|i| {
        graph.add_node(Node::Literal(Literal::Integer(i))).unwrap()
    }).collect();
    let large_list = graph.add_node(Node::List(list_elements)).unwrap();
    
    // Create main program structure
    let mut all_nodes = calls;
    all_nodes.push(effect1);
    all_nodes.push(pure_computation);
    all_nodes.push(effect2);
    all_nodes.push(large_list);
    
    let main_body = graph.add_node(Node::Begin {
        exprs: all_nodes,
    }).unwrap();
    
    graph.root_id = Some(main_body);
    graph
}

/// Create mock runtime profiling data
fn create_runtime_data() -> RuntimeGuidedConfig {
    RuntimeGuidedConfig {
        hot_functions: vec![
            FunctionProfileData {
                chunk_id: 1,
                name: Some("hot_func".to_string()),
                call_count: 10000,
                total_time_ns: 1_000_000_000,
                avg_time_ns: 100_000,
            }
        ],
        skewed_values: vec![
            ValueProfileData {
                node_id: NodeId(std::num::NonZeroU32::new(1).unwrap()),
                total_count: 1000,
                most_common: Some(("42".to_string(), 800)),
                skew_percentage: 0.8,
            }
        ],
        hot_loops: vec![
            LoopProfileData {
                loop_id: NodeId(std::num::NonZeroU32::new(2).unwrap()),
                execution_count: 500,
                avg_iterations: 10.0,
                max_iterations: 20,
            }
        ],
        hot_function_threshold: 1000,
        value_specialization_threshold: 0.7,
        loop_unroll_threshold: 5.0,
    }
}

#[test]
fn test_ml_coordinator_with_runtime_data() {
    let graph = create_test_graph();
    let runtime_data = create_runtime_data();
    
    let config = MLOptimizationConfig {
        confidence_threshold: 0.6,
        enable_learning: true,
        model_path: None,
        runtime_data: Some(runtime_data),
    };
    
    let mut coordinator = MLOptimizationCoordinator::new(config);
    
    // Analyze and get optimization passes
    let passes = coordinator.analyze_and_optimize(&graph).unwrap();
    
    // Should have multiple optimization passes
    assert!(passes.len() > 0);
    
    // Print what optimizations were selected
    for pass in &passes {
        println!("Selected optimization: {}", pass.name());
    }
}

#[test]
fn test_ml_learning_feedback() {
    let graph = create_test_graph();
    
    let config = MLOptimizationConfig {
        confidence_threshold: 0.7,
        enable_learning: true,
        model_path: None,
        runtime_data: None,
    };
    
    let mut coordinator = MLOptimizationCoordinator::new(config);
    
    // Get initial optimizations
    let _passes = coordinator.analyze_and_optimize(&graph).unwrap();
    
    // Simulate performance improvement
    let metrics = PerformanceMetrics {
        execution_time_before: 1000.0,
        execution_time_after: 750.0,
        improvement_percentage: 25.0,
        memory_usage_before: 1_000_000,
        memory_usage_after: 800_000,
    };
    
    // Update coordinator with results
    coordinator.update_with_results(metrics);
    
    // In a real implementation, this would update the ML model
    // to learn that these optimizations were effective
}

#[test]
fn test_feature_extraction() {
    let mut graph = Graph::new();
    
    // Add various node types
    for _ in 0..5 {
        graph.add_node(Node::Effect {
            effect_type: EffectType::IO,
            operation: "print".to_string(),
            args: vec![],
        }).unwrap();
    }
    
    for i in 0..10 {
        graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
    }
    
    let config = MLOptimizationConfig::default();
    let coordinator = MLOptimizationCoordinator::new(config);
    
    // Test effect density calculation
    let density = coordinator.calculate_effect_density(&graph);
    assert!((density - 0.333).abs() < 0.01); // 5/15 ≈ 0.333
}

#[test]
fn test_memory_pressure_estimation() {
    let mut graph = Graph::new();
    
    // Add allocation-heavy nodes
    let list1 = graph.add_node(Node::List(vec![])).unwrap();
    let list2 = graph.add_node(Node::List(vec![])).unwrap();
    let map = graph.add_node(Node::Map(vec![])).unwrap();
    
    let lambda_body = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: lambda_body,
    }).unwrap();
    
    // Add some non-allocation nodes
    for i in 0..6 {
        graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
    }
    
    let config = MLOptimizationConfig::default();
    let coordinator = MLOptimizationCoordinator::new(config);
    
    let pressure = coordinator.estimate_memory_pressure(&graph);
    println!("Memory pressure: {}, expected: 0.363636", pressure);
    println!("Total nodes: {}", graph.nodes.len());
    assert!((pressure - 0.363636).abs() < 0.01); // 4/11 ≈ 0.363636
}

#[test]
#[cfg(feature = "ai-analysis")]
fn test_ai_integration() {
    use fluentai_optimizer::ai_driven::ai_hints_to_optimization_config;
    
    let graph = create_test_graph();
    let analysis = serde_json::json!({
        "optimizations": [
            {
                "type": "inline",
                "confidence": 0.9,
                "targets": [1, 2, 3]
            },
            {
                "type": "memoize",
                "confidence": 0.8,
                "targets": [4]
            }
        ],
        "patterns": [
            {
                "type": "map_reduce",
                "confidence": 0.85
            }
        ],
        "confidence": 0.82
    });
    
    // This should use the ML coordinator internally
    let config = ai_hints_to_optimization_config(&graph, &analysis);
    
    // Should return a valid config
    assert!(config.constant_folding || config.dead_code_elimination);
}