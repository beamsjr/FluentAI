//! Simplified Real-World RL Optimization Demo: Data Processing Pipeline
//! 
//! This demonstrates optimizing a realistic data processing pipeline.

use fluentai_ai::rl::{OptimizationState, OptimizationConfig, ResourceMetrics};
use fluentai_core::ast::Graph;
use fluentai_parser::parse_flc;
use fluentai_optimizer::{OptimizationPipeline, OptimizationConfig as PipelineConfig, OptimizationLevel};
use fluentai_vm::{VM, compiler::Compiler};
use std::time::{Duration, Instant};
use std::collections::HashMap;

/// Real-world data processing with optimization opportunities
fn create_data_processor() -> &'static str {
    r#"
    // Process a list of numbers with various transformations
    private function process_data(numbers) {
        // Constant expressions that can be folded
        let multiplier = 2 * 5;  // = 10
        let offset = 100 + 0;    // = 100
        let divisor = 20 / 2;    // = 10
        
        // Dead code that will never execute
        if (false) {
            $(("This is dead code")).print();
            let unused = 999;
        }
        
        // Common subexpressions
        let threshold = multiplier * 5;  // = 50
        let check_val = multiplier * 5;  // Same as threshold
        
        // Process the data with redundant operations
        let processed = numbers
            .map(n => n * multiplier + offset)  // Scale and offset
            .filter(n => n > threshold)          // Filter by threshold
            .map(n => n / divisor)               // Normalize
            .map(n => n * 1 + 0);                // Algebraic identity (no-op)
        
        // More dead code
        if (1 == 2) {
            processed.map(n => n * 999);
        }
        
        // Calculate statistics with redundant computations
        let sum = processed.reduce(0, (acc, n) => acc + n);
        let count = processed.length();
        let avg = sum / count;
        
        // Redundant calculation
        let sum2 = processed.reduce(0, (acc, n) => acc + n);
        let redundant_avg = sum2 / count;
        
        {
            "processed_count": count,
            "average": avg,
            "sum": sum,
            "threshold_used": check_val
        }
    }
    
    // Run the processing
    private function main() {
        let test_data = [5, 10, 15, 20, 25, 30, 35, 40, 45, 50];
        let result = process_data(test_data);
        
        $(("Processing complete")).print();
        $(("Count: " + result.processed_count)).print();
        $(("Average: " + result.average)).print();
    }
    
    main()
    "#
}

/// Compile and run a graph, measuring actual execution time
fn benchmark_execution(graph: &Graph, iterations: usize) -> Result<Duration, Box<dyn std::error::Error>> {
    // Compile the graph
    let compiler = Compiler::new();
    let bytecode = compiler.compile(graph)?;
    
    // Run multiple times to get stable measurements
    let mut total_duration = Duration::from_secs(0);
    
    for _ in 0..iterations {
        // Create a fresh VM for each run
        let mut vm = VM::new(bytecode.clone());
        
        let start = Instant::now();
        vm.run()?;
        total_duration += start.elapsed();
    }
    
    Ok(total_duration / iterations as u32)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Real-World RL Optimization: Data Processing Pipeline ===\n");
    
    // Parse the data processor program
    let code = create_data_processor();
    let original_graph = parse_flc(code)?;
    
    println!("Data Processing Pipeline Features:");
    println!("- Constant folding opportunities (2*5, 100+0, 20/2)");
    println!("- Dead code elimination (if false blocks)");
    println!("- Common subexpression elimination");
    println!("- Algebraic simplification (n*1+0)");
    println!("- Redundant calculations\n");
    
    println!("Original program: {} AST nodes", original_graph.nodes().count());
    
    // Benchmark unoptimized version
    println!("\nBenchmarking unoptimized version...");
    let unopt_time = benchmark_execution(&original_graph, 100)?;
    println!("Average execution time: {:?}", unopt_time);
    
    // Initialize RL state for learning
    let mut rl_state = OptimizationState {
        ast_features: vec![original_graph.nodes().count() as f32],
        current_config: OptimizationConfig::default(),
        performance_history: vec![unopt_time.as_micros() as f32],
        resource_history: vec![],
    };
    
    println!("\n=== RL Agent Testing Optimization Strategies ===\n");
    
    // Test different optimization levels
    let strategies = vec![
        ("Conservative", OptimizationLevel::Basic),
        ("Standard", OptimizationLevel::Standard),
        ("Aggressive", OptimizationLevel::Aggressive),
    ];
    
    let mut results = HashMap::new();
    let mut best_time = unopt_time;
    let mut best_strategy = None;
    
    for (name, level) in strategies {
        println!("Testing {} optimization...", name);
        
        // Optimize
        let config = PipelineConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);
        let optimized = pipeline.optimize(&original_graph)?;
        
        // Measure actual performance
        let opt_time = benchmark_execution(&optimized, 100)?;
        let speedup = unopt_time.as_micros() as f64 / opt_time.as_micros() as f64;
        let improvement = ((unopt_time.as_micros() - opt_time.as_micros()) as f64 
                          / unopt_time.as_micros() as f64) * 100.0;
        
        println!("  AST nodes: {} -> {}", 
                 original_graph.nodes().count(), 
                 optimized.nodes().count());
        println!("  Execution time: {:?} -> {:?}", unopt_time, opt_time);
        println!("  Speedup: {:.2}x", speedup);
        println!("  Performance improvement: {:.1}%\n", improvement);
        
        // Update RL state
        rl_state.performance_history.push(opt_time.as_micros() as f32);
        rl_state.resource_history.push(ResourceMetrics {
            memory_bytes: (optimized.nodes().count() * 64) as u64,
            compilation_time_us: 0,
            binary_size: (optimized.nodes().count() * 32) as u64,
        });
        
        results.insert(name, (opt_time, speedup, optimized.nodes().count()));
        
        if opt_time < best_time {
            best_time = opt_time;
            best_strategy = Some((name, level));
        }
    }
    
    println!("=== RL Learning Results ===\n");
    
    if let Some((name, _)) = best_strategy {
        let (time, speedup, nodes) = results[name];
        println!("Best strategy: {} optimization", name);
        println!("  Final execution time: {:?}", time);
        println!("  Total speedup: {:.2}x", speedup);
        println!("  AST node reduction: {} -> {}", 
                 original_graph.nodes().count(), nodes);
        
        println!("\nReal performance gains:");
        println!("  - {:.1}% faster execution", ((speedup - 1.0) * 100.0));
        println!("  - Processing 1M items would save ~{:.1} seconds",
                 (unopt_time.as_secs_f64() - time.as_secs_f64()) * 1_000_000.0 / 10.0);
    }
    
    println!("\n=== Why These Optimizations Matter ===\n");
    println!("In this data processing pipeline:");
    println!("1. Constant folding removes runtime arithmetic");
    println!("2. Dead code elimination removes unreachable code");
    println!("3. CSE eliminates redundant threshold calculations");
    println!("4. Algebraic simplification removes identity operations");
    println!("\nThese optimizations compound when processing large datasets!");
    
    println!("\nDemo complete!");
    
    Ok(())
}