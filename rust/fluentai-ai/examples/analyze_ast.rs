//! Example of using FluentAI AI analysis on an AST

use fluentai_ai::{analyze_ast, AiAnalyzerBuilder};
use fluentai_ai::metadata::{MetadataInjector, InjectorConfig};
use fluentai_core::ast::{Graph, Node, Literal};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create a sample AST with a map-reduce pattern
    let mut graph = Graph::new();
    
    // Create a list [1, 2, 3, 4, 5]
    let n1 = graph.add_node(Node::Literal(Literal::Integer(1)))?;
    let n2 = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let n3 = graph.add_node(Node::Literal(Literal::Integer(3)))?;
    let n4 = graph.add_node(Node::Literal(Literal::Integer(4)))?;
    let n5 = graph.add_node(Node::Literal(Literal::Integer(5)))?;
    let list = graph.add_node(Node::List(vec![n1, n2, n3, n4, n5]))?;
    
    // Create a lambda: x => x * 2
    let x = graph.add_node(Node::Variable { name: "x".to_string() })?;
    let two = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let mult = graph.add_node(Node::Variable { name: "*".to_string() })?;
    let double = graph.add_node(Node::Application {
        function: mult,
        args: vec![x, two],
    })?;
    let map_fn = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: double,
    })?;
    
    // Create map(list, x => x * 2)
    let map_var = graph.add_node(Node::Variable { name: "map".to_string() })?;
    let mapped = graph.add_node(Node::Application {
        function: map_var,
        args: vec![list, map_fn],
    })?;
    
    // Create reduce(mapped, 0, +)
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)))?;
    let plus = graph.add_node(Node::Variable { name: "+".to_string() })?;
    let reduce_var = graph.add_node(Node::Variable { name: "reduce".to_string() })?;
    let result = graph.add_node(Node::Application {
        function: reduce_var,
        args: vec![mapped, zero, plus],
    })?;
    
    println!("=== FluentAI AST Analysis Demo ===\n");
    
    // 1. Basic analysis
    println!("1. Basic Analysis:");
    let analysis = analyze_ast(&graph)?;
    println!("   Performance Score: {:.2}", analysis.performance_score);
    println!("   Confidence: {:.2}", analysis.confidence);
    println!("   Patterns Found: {}", analysis.patterns.len());
    println!("   Optimizations Found: {}", analysis.optimizations.len());
    
    // 2. Pattern details
    if !analysis.patterns.is_empty() {
        println!("\n2. Detected Patterns:");
        for pattern in &analysis.patterns {
            println!("   - {:?} (confidence: {:.2})", pattern.pattern_type, pattern.confidence);
        }
    }
    
    // 3. Optimization suggestions
    if !analysis.optimizations.is_empty() {
        println!("\n3. Optimization Suggestions:");
        for opt in &analysis.optimizations {
            println!("   - {:?}: {}", opt.optimization_type, opt.description);
            println!("     Expected improvement: {:.1}%", opt.expected_improvement * 100.0);
        }
    }
    
    // 4. Inject metadata into AST
    println!("\n4. Injecting AI metadata into AST...");
    let injector = MetadataInjector::new(InjectorConfig::default());
    injector.inject(&mut graph, &analysis)?;
    println!("   Metadata injected successfully!");
    
    // 5. Show node details with embeddings
    println!("\n5. Node Feature Details:");
    if let Some(node_id) = graph.node_ids().next() {
        if let Some(metadata) = graph.get_metadata(node_id) {
            let features = metadata.feature_vector();
            println!("   First node features (first 5): {:?}", &features[..5.min(features.len())]);
        }
    }
    
    // 6. Custom analyzer with embeddings
    println!("\n6. Advanced Analysis with Embeddings:");
    let analyzer = AiAnalyzerBuilder::new()
        .generate_embeddings(true)
        .build();
    
    let advanced_analysis = analyzer.analyze_graph(&graph)?;
    println!("   Generated embeddings for {} nodes", advanced_analysis.embeddings.len());
    
    Ok(())
}