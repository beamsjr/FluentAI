use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};
use fluentai_optimizer::{
    ml_integration::{MLOptimizationCoordinator, MLOptimizationConfig, PerformanceMetrics},
};
use std::time::Instant;

fn main() {
    println!("=== FluentAI ML Optimization Demo ===");
    println!();
    
    // Load the minimal demo
    let source = include_str!("rl_optimization_demo_minimal.flc");
    
    // Parse the program
    let graph = match parse_flc(source) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Failed to parse demo: {:?}", e);
            return;
        }
    };
    
    println!("✅ Demo parsed successfully ({} nodes)", graph.nodes.len());
    println!();
    
    // Phase 1: Run without optimization
    println!("Phase 1: Running without optimization...");
    println!("=======================================");
    
    let compiler = Compiler::new();
    let chunk = match compiler.compile(&graph) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to compile: {:?}", e);
            return;
        }
    };
    
    let mut baseline_time = std::time::Duration::from_secs(0);
    let mut vm = VM::new(chunk.clone());
    
    let start = Instant::now();
    match vm.run() {
        Ok(_) => {
            baseline_time = start.elapsed();
            println!("✅ Baseline execution completed");
            println!("   Time: {:?}", baseline_time);
        }
        Err(e) => {
            println!("⚠️  Runtime error: {:?}", e);
            println!("   (Some VM features may be missing)");
        }
    }
    println!();
    
    // Phase 2: Apply ML-based optimizations
    println!("Phase 2: Applying ML-based optimizations...");
    println!("==========================================");
    
    // Create ML optimization coordinator
    let ml_config = MLOptimizationConfig {
        confidence_threshold: 0.7,
        enable_learning: true,
        model_path: None,
        runtime_data: None, // In a real scenario, we'd have profiling data
    };
    
    let mut ml_coordinator = MLOptimizationCoordinator::new(ml_config);
    
    // Analyze and get optimization passes
    let passes = match ml_coordinator.analyze_and_optimize(&graph) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Failed to analyze: {:?}", e);
            vec![]
        }
    };
    
    println!("Selected {} optimization passes:", passes.len());
    for pass in &passes {
        println!("  - {}", pass.name());
    }
    
    // Apply optimizations
    let mut optimized_graph = graph.clone();
    for mut pass in passes {
        match pass.run(&optimized_graph) {
            Ok(new_graph) => {
                optimized_graph = new_graph;
                println!("✅ Applied: {}", pass.name());
            }
            Err(e) => {
                println!("❌ Failed {}: {:?}", pass.name(), e);
            }
        }
    }
    println!();
    
    // Compile optimized version
    let new_compiler = Compiler::new();
    let optimized_chunk = match new_compiler.compile(&optimized_graph) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to compile optimized version: {:?}", e);
            return;
        }
    };
    
    // Phase 3: Run optimized version
    println!("Phase 3: Running optimized version...");
    println!("====================================");
    
    vm = VM::new(optimized_chunk);
    let start = Instant::now();
    
    match vm.run() {
        Ok(_) => {
            let optimized_time = start.elapsed();
            println!("✅ Optimized execution completed");
            println!("   Time: {:?}", optimized_time);
            
            // Calculate improvement
            if baseline_time.as_millis() > 0 {
                let improvement = ((baseline_time.as_millis() as f64 - optimized_time.as_millis() as f64) 
                    / baseline_time.as_millis() as f64) * 100.0;
                println!();
                println!("=== Results ===");
                println!("Baseline:  {:?}", baseline_time);
                println!("Optimized: {:?}", optimized_time);
                println!("Improvement: {:.1}%", improvement);
                
                // Update ML model with results
                let metrics = PerformanceMetrics {
                    execution_time_before: baseline_time.as_millis() as f64,
                    execution_time_after: optimized_time.as_millis() as f64,
                    improvement_percentage: improvement,
                    memory_usage_before: 1_000_000, // Placeholder
                    memory_usage_after: 800_000,     // Placeholder
                };
                
                ml_coordinator.update_with_results(metrics);
                println!();
                println!("✅ ML model updated with performance results");
            }
        }
        Err(e) => {
            println!("⚠️  Runtime error: {:?}", e);
        }
    }
    
    println!();
    println!("=== How ML Optimization Works ===");
    println!("1. The ML system analyzes the AST structure");
    println!("2. It identifies optimization opportunities:");
    println!("   - Hot functions → Inlining");
    println!("   - Pure functions → Memoization");
    println!("   - Repeated patterns → CSE");
    println!("   - Effects → Reordering");
    println!("3. Each optimization has a confidence score");
    println!("4. The system learns from execution results");
    println!("5. Future runs benefit from past learning!");
    
    println!();
    println!("Note: With profiling data, the ML system can make");
    println!("even better decisions based on actual runtime behavior.");
}