//! Example demonstrating context-aware optimization

use fluentai_core::ast::{ContextMemory, PerformanceHint, PerformanceHintType, UsageStatistics};
use fluentai_optimizer::OptimizationPipeline;
use fluentai_optimizer::passes::context_aware::ContextAwarePass;

fn main() -> anyhow::Result<()> {
    // Sample program with a hot function
    let source = r#"
        (letrec ((hot-function (lambda (x)
                                 (+ (* x x) (* 2 x) 1)))
                 (process-list (lambda (lst)
                                (map hot-function lst))))
          (process-list (list 1 2 3 4 5)))
    "#;
    
    // Parse the source code
    let mut graph = parse(source)?;
    
    println!("=== Original AST ===");
    println!("Nodes: {}", graph.nodes().count());
    
    // Find the hot-function node
    let hot_function_id = graph.nodes()
        .find(|(_, node)| {
            if let fluentai_core::ast::Node::Lambda { .. } = node {
                // This is a simple heuristic - in reality we'd use better identification
                true
            } else {
                false
            }
        })
        .map(|(id, _)| *id);
    
    // Add context memory to simulate runtime data
    if let Some(node_id) = hot_function_id {
        let context = ContextMemory {
            embedding_id: None,
            usage_stats: UsageStatistics {
                execution_count: 5000,
                avg_execution_time_ns: 150,
                error_count: 0,
                is_hot_path: true,
            },
            rationale: Some("Critical path function for list processing".to_string()),
            performance_hints: vec![
                PerformanceHint {
                    hint_type: PerformanceHintType::ShouldInline,
                    confidence: 0.9,
                    context: Some("Small hot function".to_string()),
                }
            ],
            semantic_tags: vec!["arithmetic".to_string(), "pure".to_string()],
            last_modified: None,
        };
        
        graph.set_context_memory(node_id, context);
        println!("\nAdded context memory to hot function (node {})", node_id.0);
    }
    
    // Find the map application
    let map_node_id = graph.nodes()
        .find(|(_, node)| {
            if let fluentai_core::ast::Node::Application { function, .. } = node {
                if let Some(fluentai_core::ast::Node::Variable { name }) = graph.get_node(*function) {
                    name == "map"
                } else {
                    false
                }
            } else {
                false
            }
        })
        .map(|(id, _)| *id);
    
    // Add context for vectorization
    if let Some(node_id) = map_node_id {
        let context = ContextMemory {
            embedding_id: None,
            usage_stats: UsageStatistics {
                execution_count: 1000,
                avg_execution_time_ns: 5000,
                error_count: 0,
                is_hot_path: true,
            },
            rationale: Some("Map over numeric list can be vectorized".to_string()),
            performance_hints: vec![
                PerformanceHint {
                    hint_type: PerformanceHintType::CanVectorize,
                    confidence: 0.85,
                    context: Some("SIMD operations available".to_string()),
                }
            ],
            semantic_tags: vec!["simd-compatible".to_string(), "list-processing".to_string()],
            last_modified: None,
        };
        
        graph.set_context_memory(node_id, context);
        println!("Added context memory to map operation (node {})", node_id.0);
    }
    
    // Run context-aware optimization
    println!("\n=== Running Context-Aware Optimization ===");
    
    let mut pass = ContextAwarePass::new();
    let optimized = pass.run(&graph)?;
    
    println!("\nOptimized nodes: {}", optimized.nodes().count());
    
    // Check what optimizations were applied
    println!("\n=== Applied Optimizations ===");
    
    for (node_id, _) in optimized.nodes() {
        if let Some(metadata) = optimized.get_metadata(*node_id) {
            if !metadata.hints.is_empty() {
                println!("\nNode {}: {:?}", node_id.0, metadata.hints);
                
                if let Some(context) = optimized.get_context_memory(*node_id) {
                    println!("  Semantic tags: {:?}", context.semantic_tags);
                    if let Some(rationale) = &context.rationale {
                        println!("  Rationale: {}", rationale);
                    }
                }
            }
        }
    }
    
    // Show statistics
    println!("\n=== Optimization Statistics ===");
    
    let mut inline_count = 0;
    let mut vectorize_count = 0;
    
    for (_, _) in optimized.nodes() {
        if let Some(metadata) = optimized.get_metadata(*node_id) {
            for hint in &metadata.hints {
                match hint {
                    fluentai_core::ast::OptimizationHint::ForceInline => inline_count += 1,
                    fluentai_core::ast::OptimizationHint::Vectorize => vectorize_count += 1,
                    _ => {}
                }
            }
        }
    }
    
    println!("Functions marked for inlining: {}", inline_count);
    println!("Operations marked for vectorization: {}", vectorize_count);
    
    // Demonstrate how this integrates with the full pipeline
    println!("\n=== Full Optimization Pipeline ===");
    
    let config = fluentai_optimizer::OptimizationConfig {
        level: fluentai_optimizer::OptimizationLevel::Aggressive,
        constant_folding: true,
        dead_code_elimination: true,
        cse: true,
        inline: true,
        inline_threshold: 20,
        tail_call_optimization: true,
        loop_optimization: true,
        beta_reduction: true,
        partial_evaluation: true,
        max_iterations: 3,
        debug_mode: false,
    };
    
    let mut pipeline = OptimizationPipeline::new(config);
    
    // The context-aware pass would be added to the pipeline
    // In a real implementation, it would be integrated into the standard pipeline
    
    let final_result = pipeline.optimize(optimized)?;
    
    println!("\nFinal optimized nodes: {}", final_result.nodes().count());
    println!("Pipeline stats:\n{}", pipeline.stats());
    
    Ok(())
}