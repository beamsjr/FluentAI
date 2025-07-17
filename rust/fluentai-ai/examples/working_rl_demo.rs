//! Working RL Optimization Demo
//! 
//! This example shows the RL system learning to optimize FluentAI programs.

use fluentai_ai::rl::{
    OptimizationAction, OptimizationState, OptimizationConfig,
    InlineLevel, ResourceMetrics,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use fluentai_parser::parse_flc;
use fluentai_optimizer::{
    OptimizationPipeline, OptimizationConfig as OptimizerConfig,
    passes::{
        constant_folding::ConstantFoldingPass,
        dead_code::DeadCodeEliminationPass,
        cse::{CommonSubexpressionElimination, CSEConfig},
    }
};
use std::collections::HashMap;

/// Sample program with optimization opportunities
fn create_test_program() -> &'static str {
    r#"
    private function demo() {
        // Constant folding opportunities
        let x = 2 + 3;
        let y = 4 * 5;
        
        // Common subexpression
        let a = x + y;
        let b = x + y;
        
        // Dead code
        if (false) {
            $("This is dead code").print();
        }
        
        // Algebraic simplification
        let result = a * 1 + 0;
        
        $(f"Result: {result}").print();
    }
    
    demo()
    "#
}

/// Simple demonstration of the RL optimization concept
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI RL Optimization Demo ===\n");
    
    // Parse the program
    let code = create_test_program();
    let original_graph = parse_flc(code)?;
    let original_nodes = original_graph.nodes().count();
    println!("Original program: {} nodes", original_nodes);
    
    // Create optimization state
    let mut state = OptimizationState {
        ast_features: vec![original_nodes as f32],
        current_config: OptimizationConfig::default(),
        performance_history: vec![],
        resource_history: vec![],
    };
    
    // Simulate RL agent decisions
    println!("\n=== Simulating RL Agent Optimization Process ===");
    
    let mut current_graph = original_graph.clone();
    let actions = vec![
        OptimizationAction::ConstantFolding,
        OptimizationAction::CSE,
        OptimizationAction::DeadCodeElimination,
    ];
    
    for (step, action) in actions.iter().enumerate() {
        println!("\nStep {}: Applying {:?}", step + 1, action);
        
        // Apply the optimization
        let optimized = match action {
            OptimizationAction::ConstantFolding => {
                let mut pass = ConstantFoldingPass::new();
                pass.run(&current_graph)?
            }
            OptimizationAction::CSE => {
                let mut pass = CommonSubexpressionElimination::new(CSEConfig::default());
                pass.run(&current_graph)?
            }
            OptimizationAction::DeadCodeElimination => {
                let mut pass = DeadCodeEliminationPass::new();
                pass.run(&current_graph)?
            }
            _ => current_graph.clone(),
        };
        
        let new_nodes = optimized.nodes().count();
        let reduction = original_nodes - new_nodes;
        let improvement = (reduction as f32 / original_nodes as f32) * 100.0;
        
        println!("  Nodes: {} -> {} ({}% reduction)", 
                 current_graph.nodes().count(), new_nodes, improvement as i32);
        
        // Update state (as the RL agent would)
        state.current_config.apply_action(*action);
        state.performance_history.push(new_nodes as f32);
        state.resource_history.push(ResourceMetrics {
            memory_bytes: (new_nodes * 64) as u64,
            compilation_time_us: (new_nodes * 10) as u64,
            binary_size: (new_nodes * 32) as u64,
        });
        
        current_graph = optimized;
    }
    
    println!("\n=== Optimization Results ===");
    println!("Original nodes: {}", original_nodes);
    println!("Final nodes: {}", current_graph.nodes().count());
    println!("Total reduction: {}%", 
             ((original_nodes - current_graph.nodes().count()) as f32 / original_nodes as f32 * 100.0) as i32);
    
    println!("\n=== RL Learning Insights ===");
    println!("In a full RL system, the agent would:");
    println!("1. Learn which optimizations work best for different AST patterns");
    println!("2. Discover optimal ordering of optimization passes");
    println!("3. Adapt to specific program characteristics");
    println!("4. Balance compilation time vs runtime performance");
    
    println!("\nState evolution during optimization:");
    for (i, perf) in state.performance_history.iter().enumerate() {
        println!("  After step {}: {} nodes", i + 1, *perf as i32);
    }
    
    println!("\nDemo complete!");
    
    Ok(())
}