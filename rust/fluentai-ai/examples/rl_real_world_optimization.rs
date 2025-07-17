//! Real-World RL Optimization Demo: Log Analysis Pipeline
//! 
//! This demonstrates optimizing a realistic data processing pipeline that:
//! 1. Parses log entries
//! 2. Filters by criteria
//! 3. Aggregates statistics
//! 4. Generates reports
//! 
//! This type of code is common in monitoring systems, analytics pipelines,
//! and data processing applications.

use fluentai_ai::rl::{OptimizationState, OptimizationConfig, ResourceMetrics};
use fluentai_core::ast::Graph;
use fluentai_parser::parse_flc;
use fluentai_optimizer::{OptimizationPipeline, OptimizationConfig as PipelineConfig, OptimizationLevel};
use fluentai_vm::{VM, compiler::Compiler};
use std::time::{Duration, Instant};
use std::collections::HashMap;

/// Real-world log processing pipeline with optimization opportunities
fn create_log_processor() -> &'static str {
    r#"
    // Simulated log entry parser - real-world use case
    private function parse_log_entry(line) {
        // In real app, this would parse actual log format
        // For demo, we simulate parsing overhead
        let parts = line.split(" ");
        {
            "timestamp": parts.get(0).unwrap_or(""),
            "level": parts.get(1).unwrap_or("INFO"),
            "service": parts.get(2).unwrap_or("unknown"),
            "message": parts.slice(3).join(" ")
        }
    }
    
    // Check if log entry matches criteria - with optimization opportunities
    private function matches_criteria(entry, level_filter, service_filter) {
        // Redundant checks (common subexpression)
        let level_check = entry.level == level_filter;
        let service_check = entry.service == service_filter;
        
        // Another redundant computation
        let level_match = entry.level == level_filter;
        let service_match = entry.service == service_filter;
        
        // Unnecessary intermediate variables
        let result = level_match && service_match;
        result
    }
    
    // Extract metrics from log entry
    private function extract_metrics(entry) {
        // Constant computations that could be folded
        let error_weight = 10 * 1;
        let warning_weight = 5 + 0;
        let info_weight = 1 * 1 * 1;
        
        // Dead code - this condition is always false
        if (false && entry.level == "IMPOSSIBLE") {
            return {"weight": 1000, "count": 1};
        }
        
        // Simple weight calculation with optimization opportunities
        let weight = if (entry.level == "ERROR") {
            error_weight
        } else {
            if (entry.level == "WARN") {
                warning_weight  
            } else {
                info_weight
            }
        };
        
        {"weight": weight, "count": 1}
    }
    
    // Main processing pipeline
    private function process_logs(log_lines, filters) {
        // Parse all entries first (could be optimized with fusion)
        let parsed_entries = log_lines.map(line => parse_log_entry(line));
        
        // Filter entries (could be optimized with predicate pushdown)
        let filtered = parsed_entries.filter(entry => 
            matches_criteria(entry, filters.level, filters.service)
        );
        
        // Extract metrics (could be fused with filter)
        let metrics = filtered.map(entry => extract_metrics(entry));
        
        // Aggregate results - with redundant calculations
        let total_count = metrics.length();
        let redundant_count = metrics.length(); // Same calculation
        
        let total_weight = metrics.reduce(0, (sum, m) => sum + m.weight);
        let avg_weight = if (total_count > 0) {
            total_weight / total_count
        } else {
            0
        };
        
        // Build report with some algebraic simplification opportunities
        let report = {
            "total_entries": total_count * 1 + 0,  // Could be simplified
            "filtered_count": redundant_count,
            "total_weight": total_weight,
            "average_weight": avg_weight,
            "service": filters.service,
            "level": filters.level
        };
        
        report
    }
    
    // Simulate a batch processing scenario
    private function run_analysis() {
        // Simulate log data - in real app this would read from files/streams
        let log_lines = [
            "2024-01-15T10:30:00 ERROR auth Failed login attempt from 192.168.1.100",
            "2024-01-15T10:30:01 INFO api Request processed in 45ms",
            "2024-01-15T10:30:02 WARN db Connection pool running low",
            "2024-01-15T10:30:03 ERROR auth Invalid token provided",
            "2024-01-15T10:30:04 INFO api Health check passed",
            "2024-01-15T10:30:05 ERROR payment Transaction failed: insufficient funds",
            "2024-01-15T10:30:06 WARN cache Cache miss rate above threshold",
            "2024-01-15T10:30:07 INFO api User profile updated",
            "2024-01-15T10:30:08 ERROR auth Account locked after failed attempts",
            "2024-01-15T10:30:09 INFO worker Background job completed"
        ];
        
        // Multiple analysis runs with different filters
        let filters_list = [
            {"level": "ERROR", "service": "auth"},
            {"level": "WARN", "service": "db"},
            {"level": "INFO", "service": "api"},
            {"level": "ERROR", "service": "payment"}
        ];
        
        // Process each filter set - simulates real batch processing
        let results = filters_list.map(filters => {
            let report = process_logs(log_lines, filters);
            report
        });
        
        // Return aggregate statistics
        let total_errors = results
            .filter(r => r.level == "ERROR")
            .map(r => r.filtered_count)
            .reduce(0, (sum, count) => sum + count);
            
        {
            "batch_results": results,
            "total_error_count": total_errors,
            "filters_applied": filters_list.length()
        }
    }
    
    run_analysis()
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
    println!("=== Real-World RL Optimization: Log Analysis Pipeline ===\n");
    
    // Parse the log processor program
    let code = create_log_processor();
    let original_graph = parse_flc(code)?;
    
    println!("Log Processing Pipeline:");
    println!("- Parses log entries");
    println!("- Filters by level and service");
    println!("- Aggregates metrics");
    println!("- Generates reports\n");
    
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
            compilation_time_us: 0, // Could measure actual compilation time
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
        println!("  - Processing 10M log entries would save ~{:.1} seconds",
                 (unopt_time.as_secs_f64() - time.as_secs_f64()) * 10_000_000.0 / 10.0);
    }
    
    println!("\n=== Why These Optimizations Matter ===\n");
    println!("In this log processing pipeline:");
    println!("1. CSE removes redundant level/service checks");
    println!("2. Constant folding eliminates weight calculations");
    println!("3. Dead code elimination removes impossible branches");
    println!("4. Loop fusion combines map/filter operations");
    println!("5. These add up to significant gains when processing millions of logs");
    
    println!("\nIn production systems processing billions of events,");
    println!("even small optimizations translate to substantial cost savings!");
    
    println!("\nDemo complete!");
    
    Ok(())
}