//! Example demonstrating AI-driven optimization
//!
//! This example shows how the optimizer can use AI analysis results
//! to dynamically configure optimization passes.

use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId, NodeMetadata};
use fluentai_core::value::Value;
use fluentai_optimizer::{OptimizationPipeline, OptimizationSource, OptimizationLevel};

#[cfg(feature = "ai-analysis")]
use fluentai_ai::analyze_ast;

fn create_example_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create a more complex example with patterns
    // This represents: [1, 2, 3, 4, 5].map(x => x * 2).filter(x => x > 5).reduce(+)
    
    // List literal [1, 2, 3, 4, 5]
    let list_values: Vec<NodeId> = (1..=5)
        .map(|i| {
            graph.add_node(Node::Literal {
                value: Value::Integer(i),
                metadata: NodeMetadata::default(),
            })
        })
        .collect();
    
    let list_node = graph.add_node(Node::List {
        elements: list_values,
        metadata: NodeMetadata::default(),
    });
    
    // Lambda for map: x => x * 2
    let x_param = graph.add_node(Node::Symbol {
        name: "x".to_string(),
        metadata: NodeMetadata::default(),
    });
    
    let const_2 = graph.add_node(Node::Literal {
        value: Value::Integer(2),
        metadata: NodeMetadata::default(),
    });
    
    let multiply = graph.add_node(Node::Application {
        function: NodeId::placeholder(), // Would be multiply function
        args: vec![x_param, const_2],
        metadata: NodeMetadata::default(),
    });
    
    let map_lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: multiply,
        captures: vec![],
        metadata: NodeMetadata::default(),
    });
    
    // Map application
    let map_node = graph.add_node(Node::Application {
        function: NodeId::placeholder(), // Would be map function
        args: vec![list_node, map_lambda],
        metadata: NodeMetadata::default(),
    });
    
    // Lambda for filter: x => x > 5
    let const_5 = graph.add_node(Node::Literal {
        value: Value::Integer(5),
        metadata: NodeMetadata::default(),
    });
    
    let greater_than = graph.add_node(Node::Application {
        function: NodeId::placeholder(), // Would be > function
        args: vec![x_param, const_5],
        metadata: NodeMetadata::default(),
    });
    
    let filter_lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: greater_than,
        captures: vec![],
        metadata: NodeMetadata::default(),
    });
    
    // Filter application
    let filter_node = graph.add_node(Node::Application {
        function: NodeId::placeholder(), // Would be filter function
        args: vec![map_node, filter_lambda],
        metadata: NodeMetadata::default(),
    });
    
    // Reduce with + operation
    let reduce_node = graph.add_node(Node::Application {
        function: NodeId::placeholder(), // Would be reduce function
        args: vec![filter_node],
        metadata: NodeMetadata::default(),
    });
    
    graph.root_id = Some(reduce_node);
    graph
}

fn main() -> Result<()> {
    println!("=== AI-Driven Optimization Demo ===\n");
    
    let graph = create_example_ast();
    println!("Created AST with {} nodes", graph.nodes.len());
    
    #[cfg(feature = "ai-analysis")]
    {
        println!("\n--- Running AI Analysis ---");
        
        match analyze_ast(&graph) {
            Ok(analysis) => {
                println!("Performance score: {:.2}", analysis.performance_score);
                println!("Confidence: {:.2}", analysis.confidence);
                
                println!("\nDetected patterns:");
                for pattern in &analysis.patterns {
                    println!("  - {:?} (confidence: {:.2})", pattern.pattern_type, pattern.confidence);
                }
                
                println!("\nOptimization suggestions:");
                for suggestion in &analysis.optimizations {
                    println!("  - {:?}: {} (impact: {:.2})", 
                        suggestion.optimization_type,
                        suggestion.description,
                        suggestion.expected_improvement
                    );
                }
                
                // Run optimization with AI hints
                println!("\n--- Running AI-Driven Optimization ---");
                let mut ai_pipeline = OptimizationPipeline::from_source(
                    OptimizationSource::AIHints,
                    &graph
                )?;
                
                let ai_optimized = ai_pipeline.optimize(&graph)?;
                println!("AI-optimized graph: {} nodes", ai_optimized.nodes.len());
                println!("Nodes removed: {}", graph.nodes.len() - ai_optimized.nodes.len());
                
                // Compare with standard optimization
                println!("\n--- Comparing with Standard Optimization ---");
                let mut std_pipeline = OptimizationPipeline::from_source(
                    OptimizationSource::Manual(OptimizationLevel::Standard),
                    &graph
                )?;
                
                let std_optimized = std_pipeline.optimize(&graph)?;
                println!("Standard-optimized graph: {} nodes", std_optimized.nodes.len());
                
                // Try hybrid mode
                println!("\n--- Running Hybrid Optimization ---");
                let mut hybrid_pipeline = OptimizationPipeline::from_source(
                    OptimizationSource::Hybrid(OptimizationLevel::Basic),
                    &graph
                )?;
                
                let hybrid_optimized = hybrid_pipeline.optimize(&graph)?;
                println!("Hybrid-optimized graph: {} nodes", hybrid_optimized.nodes.len());
                
                println!("\n--- Summary ---");
                println!("Original nodes: {}", graph.nodes.len());
                println!("After AI optimization: {} nodes", ai_optimized.nodes.len());
                println!("After Standard optimization: {} nodes", std_optimized.nodes.len());
                println!("After Hybrid optimization: {} nodes", hybrid_optimized.nodes.len());
            }
            Err(e) => {
                eprintln!("AI analysis failed: {}", e);
                eprintln!("Falling back to standard optimization...");
                
                let mut pipeline = OptimizationPipeline::from_source(
                    OptimizationSource::Manual(OptimizationLevel::Standard),
                    &graph
                )?;
                
                let optimized = pipeline.optimize(&graph)?;
                println!("Standard-optimized graph: {} nodes", optimized.nodes.len());
            }
        }
    }
    
    #[cfg(not(feature = "ai-analysis"))]
    {
        println!("\nAI analysis feature not enabled. Using standard optimization...");
        
        let mut pipeline = OptimizationPipeline::from_source(
            OptimizationSource::Manual(OptimizationLevel::Standard),
            &graph
        )?;
        
        let optimized = pipeline.optimize(&graph)?;
        println!("Optimized graph: {} nodes", optimized.nodes.len());
        println!("Nodes removed: {}", graph.nodes.len() - optimized.nodes.len());
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_example_ast_creation() {
        let graph = create_example_ast();
        assert!(graph.root_id.is_some());
        assert!(graph.nodes.len() > 10);
    }
}