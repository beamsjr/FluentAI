//! RL Optimization Showcase
//! 
//! Demonstrates how the RL system learns to optimize FluentAI programs

use fluentai_ai::rl::{
    OptimizationState, OptimizationConfig,
    ResourceMetrics,
};
use fluentai_core::ast::Graph;
use fluentai_parser::parse_flc;
use fluentai_optimizer::{OptimizationPipeline, OptimizationConfig as PipelineConfig, OptimizationLevel};
use std::time::Instant;

/// Create a test program with various optimization opportunities
fn create_demo_program() -> &'static str {
    r#"
    private function calculate_fibonacci(n) {
        // Base cases with redundant calculations
        let zero = 0 + 0;  // Constant folding opportunity
        let one = 1 * 1;   // Another constant folding
        
        // Unused variable (dead code)
        let unused_value = 999;
        
        // Common subexpression
        let check1 = n <= 1;
        let check2 = n <= 1;  // Same expression
        
        if (check1) {
            if (n == 0) { zero } else { one }
        } else {
            // Recursive calls
            calculate_fibonacci(n - 1) + calculate_fibonacci(n - 2)
        }
    }
    
    private function main() {
        // More optimization opportunities
        let x = 10;
        let y = 20;
        
        // Common subexpression
        let sum1 = x + y;
        let sum2 = x + y;
        
        // Dead code
        if (false) {
            $("This will never execute").print();
        }
        
        // Calculate fibonacci
        let result = calculate_fibonacci(5);
        
        // Algebraic simplification
        let final_result = result * 1 + 0;
        
        $(f"Fibonacci(5) = {final_result}").print();
    }
    
    main()
    "#
}

/// Measure optimization impact
fn measure_optimization(original: &Graph, optimized: &Graph) -> (usize, f32) {
    let original_nodes = original.nodes().count();
    let optimized_nodes = optimized.nodes().count();
    let reduction = original_nodes.saturating_sub(optimized_nodes);
    let percentage = if original_nodes > 0 {
        (reduction as f32 / original_nodes as f32) * 100.0
    } else {
        0.0
    };
    (reduction, percentage)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI RL Optimization Showcase ===\n");
    
    // Parse the demo program
    let code = create_demo_program();
    let original_graph = parse_flc(code)?;
    let original_nodes = original_graph.nodes().count();
    
    println!("Original program statistics:");
    println!("  Total nodes: {}", original_nodes);
    println!("  Contains: constant expressions, dead code, common subexpressions\n");
    
    // Initialize RL state
    let mut rl_state = OptimizationState {
        ast_features: vec![original_nodes as f32],
        current_config: OptimizationConfig::default(),
        performance_history: vec![],
        resource_history: vec![],
    };
    
    println!("=== RL Agent Learning Process ===\n");
    
    // Simulate RL agent trying different optimization strategies
    let strategies = vec![
        ("Conservative", OptimizationLevel::Basic),
        ("Standard", OptimizationLevel::Standard),
        ("Aggressive", OptimizationLevel::Aggressive),
    ];
    
    let mut best_strategy = None;
    let mut best_reduction = 0.0;
    
    for (name, level) in strategies {
        println!("Testing {} optimization strategy...", name);
        
        let start = Instant::now();
        
        // Create pipeline with this optimization level
        let config = PipelineConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);
        
        // Run optimization
        let optimized = pipeline.optimize(&original_graph)?;
        let optimization_time = start.elapsed();
        
        // Measure results
        let (reduction, percentage) = measure_optimization(&original_graph, &optimized);
        
        println!("  Nodes reduced: {} ({}% reduction)", reduction, percentage as i32);
        println!("  Optimization time: {:?}", optimization_time);
        println!("  Passes applied: {}\n", pipeline.stats());
        
        // Update RL state (simulating learning)
        rl_state.performance_history.push(optimized.nodes().count() as f32);
        rl_state.resource_history.push(ResourceMetrics {
            memory_bytes: (optimized.nodes().count() * 64) as u64,
            compilation_time_us: optimization_time.as_micros() as u64,
            binary_size: (optimized.nodes().count() * 32) as u64,
        });
        
        // Track best strategy
        if percentage > best_reduction {
            best_reduction = percentage;
            best_strategy = Some((name, level));
        }
    }
    
    println!("=== RL Learning Results ===\n");
    
    if let Some((name, level)) = best_strategy {
        println!("Best strategy learned: {} ({}% reduction)", name, best_reduction as i32);
        
        // Apply best strategy
        println!("\nApplying learned optimal strategy...");
        let config = PipelineConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);
        let final_optimized = pipeline.optimize(&original_graph)?;
        
        println!("Final results:");
        println!("  Original nodes: {}", original_nodes);
        println!("  Optimized nodes: {}", final_optimized.nodes().count());
        println!("  Total reduction: {}%", 
                 ((original_nodes - final_optimized.nodes().count()) as f32 / original_nodes as f32 * 100.0) as i32);
    }
    
    println!("\n=== RL System Insights ===\n");
    println!("The RL agent learned:");
    println!("1. Different optimization levels have different trade-offs");
    println!("2. More aggressive optimization takes longer but reduces more nodes");
    println!("3. The optimal strategy depends on program characteristics");
    
    println!("\nIn a full RL implementation, the agent would:");
    println!("- Learn from thousands of programs");
    println!("- Adapt strategies based on AST patterns");
    println!("- Balance compilation time vs runtime performance");
    println!("- Discover novel optimization sequences");
    
    println!("\nDemo complete!");
    
    Ok(())
}