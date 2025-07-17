//! Integration tests for FluentAI AI analysis

use fluentai_ai::{
    analyze_ast, create_default_analyzer, AiAnalyzerBuilder,
    metadata::{MetadataInjector, MetadataExtractor, InjectorConfig},
    cli::{AiCli, AiCliConfig, OutputFormat},
};
use fluentai_core::ast::{Graph, Node, Literal, NodeId};
use std::collections::HashMap;

/// Create a sample AST for testing
fn create_sample_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create a simple function: factorial(n) = if n <= 1 then 1 else n * factorial(n-1)
    let n_var = graph.add_node(Node::Variable { name: "n".to_string() }).unwrap();
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    
    // n <= 1
    let le_op = graph.add_node(Node::Variable { name: "<=".to_string() }).unwrap();
    let condition = graph.add_node(Node::Application {
        function: le_op,
        args: vec![n_var, one],
    }).unwrap();
    
    // n - 1
    let minus_op = graph.add_node(Node::Variable { name: "-".to_string() }).unwrap();
    let n_minus_1 = graph.add_node(Node::Application {
        function: minus_op,
        args: vec![n_var, one],
    }).unwrap();
    
    // factorial(n-1)
    let factorial_var = graph.add_node(Node::Variable { name: "factorial".to_string() }).unwrap();
    let recursive_call = graph.add_node(Node::Application {
        function: factorial_var,
        args: vec![n_minus_1],
    }).unwrap();
    
    // n * factorial(n-1)
    let mult_op = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
    let else_branch = graph.add_node(Node::Application {
        function: mult_op,
        args: vec![n_var, recursive_call],
    }).unwrap();
    
    // if n <= 1 then 1 else n * factorial(n-1)
    let _if_expr = graph.add_node(Node::If {
        condition,
        then_branch: one,
        else_branch,
    }).unwrap();
    
    graph
}

/// Create a map-reduce example
fn create_map_reduce_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create a list [1, 2, 3]
    let items = vec![
        graph.add_node(Node::Literal(Literal::Integer(1))).unwrap(),
        graph.add_node(Node::Literal(Literal::Integer(2))).unwrap(),
        graph.add_node(Node::Literal(Literal::Integer(3))).unwrap(),
    ];
    let list = graph.add_node(Node::List(items)).unwrap();
    
    // Create map function: x => x * 2
    let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let mult = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
    let double = graph.add_node(Node::Application {
        function: mult,
        args: vec![x, two],
    }).unwrap();
    let map_fn = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: double,
    }).unwrap();
    
    // map(list, x => x * 2)
    let map_var = graph.add_node(Node::Variable { name: "map".to_string() }).unwrap();
    let mapped = graph.add_node(Node::Application {
        function: map_var,
        args: vec![list, map_fn],
    }).unwrap();
    
    // reduce(mapped, 0, +)
    let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
    let reduce_var = graph.add_node(Node::Variable { name: "reduce".to_string() }).unwrap();
    let _result = graph.add_node(Node::Application {
        function: reduce_var,
        args: vec![mapped, zero, plus],
    }).unwrap();
    
    graph
}

#[test]
fn test_basic_analysis() {
    let graph = create_sample_ast();
    let result = analyze_ast(&graph).unwrap();
    
    assert!(result.confidence > 0.0);
    assert!(result.performance_score >= 0.0 && result.performance_score <= 1.0);
}

#[test]
fn test_pattern_detection() {
    let graph = create_map_reduce_ast();
    let analyzer = create_default_analyzer();
    let result = analyzer.analyze_graph(&graph).unwrap();
    
    // Should detect the map-reduce pattern
    assert!(!result.patterns.is_empty());
    let has_map_reduce = result.patterns.iter()
        .any(|p| matches!(p.pattern_type, fluentai_ai::analysis::PatternType::MapReduce));
    assert!(has_map_reduce);
}

#[test]
fn test_optimization_detection() {
    let mut graph = Graph::new();
    
    // Create a constant expression: 2 + 3
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let three = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
    let _expr = graph.add_node(Node::Application {
        function: plus,
        args: vec![two, three],
    }).unwrap();
    
    let result = analyze_ast(&graph).unwrap();
    
    // Should detect constant folding opportunity
    assert!(!result.optimizations.is_empty());
    let has_constant_fold = result.optimizations.iter()
        .any(|o| matches!(o.optimization_type, fluentai_ai::analysis::OptimizationType::ConstantFold));
    assert!(has_constant_fold);
}

#[test]
fn test_metadata_injection_extraction() {
    let mut graph = create_sample_ast();
    let analysis = analyze_ast(&graph).unwrap();
    
    // Inject metadata
    let injector = MetadataInjector::new(InjectorConfig::default());
    injector.inject(&mut graph, &analysis).unwrap();
    
    // Extract metadata
    let extractor = MetadataExtractor::new(InjectorConfig::default());
    
    // Check that we can extract global metadata
    let global_meta = extractor.extract_global_metadata(&graph);
    assert!(global_meta.performance_score.is_some());
    assert!(global_meta.analysis_confidence.is_some());
}

#[test]
fn test_embedding_generation() {
    let graph = create_sample_ast();
    
    let analyzer = AiAnalyzerBuilder::new()
        .generate_embeddings(true)
        .build();
    
    let result = analyzer.analyze_graph(&graph).unwrap();
    
    // Should have embeddings for all nodes
    assert!(!result.embeddings.is_empty());
    assert!(result.embeddings.len() > 5); // At least a few nodes
    
    // Check embedding dimensions
    for (_, embedding) in &result.embeddings {
        assert_eq!(embedding.len(), 16); // Default feature vector size
    }
}

#[test]
fn test_cli_output_formats() {
    let mut graph = create_sample_ast();
    
    // Test human-readable output
    let cli = AiCli::new(AiCliConfig {
        output_format: OutputFormat::Human,
        ..Default::default()
    });
    // Should not panic
    cli.analyze_and_output(&mut graph).unwrap();
    
    // Test JSON output
    let cli_json = AiCli::new(AiCliConfig {
        output_format: OutputFormat::Json,
        ..Default::default()
    });
    // Should not panic
    cli_json.analyze_and_output(&mut graph).unwrap();
    
    // Test compact output
    let cli_compact = AiCli::new(AiCliConfig {
        output_format: OutputFormat::Compact,
        ..Default::default()
    });
    // Should not panic
    cli_compact.analyze_and_output(&mut graph).unwrap();
}

#[test]
fn test_cache_effectiveness() {
    let graph = create_map_reduce_ast();
    let analyzer = create_default_analyzer();
    
    // First analysis - should miss cache
    let _result1 = analyzer.analyze_graph(&graph).unwrap();
    let stats1 = analyzer.cache_stats();
    assert_eq!(stats1.misses, 1);
    assert_eq!(stats1.hits, 0);
    
    // Second analysis - should hit cache
    let _result2 = analyzer.analyze_graph(&graph).unwrap();
    let stats2 = analyzer.cache_stats();
    assert_eq!(stats2.misses, 1); // Still 1
    assert_eq!(stats2.hits, 1); // Now we have a hit
    
    // Clear cache
    analyzer.clear_cache();
    
    // Third analysis - should miss again
    let _result3 = analyzer.analyze_graph(&graph).unwrap();
    let stats3 = analyzer.cache_stats();
    assert_eq!(stats3.misses, 2); // Incremented
}

#[test]
fn test_analyzer_configuration() {
    let graph = create_sample_ast();
    
    // Test with different configurations
    let analyzer_no_opt = AiAnalyzerBuilder::new()
        .detect_optimizations(false)
        .build();
    
    let result_no_opt = analyzer_no_opt.analyze_graph(&graph).unwrap();
    assert!(result_no_opt.optimizations.is_empty());
    
    let analyzer_no_patterns = AiAnalyzerBuilder::new()
        .detect_patterns(false)
        .build();
    
    let result_no_patterns = analyzer_no_patterns.analyze_graph(&graph).unwrap();
    assert!(result_no_patterns.patterns.is_empty());
}

#[test]
fn test_complex_ast_analysis() {
    let mut graph = Graph::new();
    
    // Create a more complex AST with nested structures
    let mut nodes = Vec::new();
    
    // Create 10 variables
    for i in 0..10 {
        let var = graph.add_node(Node::Variable { 
            name: format!("var_{}", i) 
        }).unwrap();
        nodes.push(var);
    }
    
    // Create nested applications
    let mut current = nodes[0];
    for i in 1..nodes.len() {
        let app = graph.add_node(Node::Application {
            function: current,
            args: vec![nodes[i]],
        }).unwrap();
        current = app;
    }
    
    // Create a lambda that wraps everything
    let _lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: current,
    }).unwrap();
    
    // Analysis should complete without errors
    let result = analyze_ast(&graph).unwrap();
    assert!(result.confidence > 0.0);
    
    // Should have reasonable performance score for complex graph
    assert!(result.performance_score < 1.0); // Not perfect due to complexity
}