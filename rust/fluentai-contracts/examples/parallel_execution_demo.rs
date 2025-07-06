//! Example demonstrating parallel symbolic execution

use fluentai_parser::parse;
use fluentai_contracts::{
    SymbolicExecutor,
    parallel_execution::{ParallelSymbolicExecutor, ParallelConfig, benchmark_parallel_execution},
};
use std::time::Instant;
use std::num::NonZero;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Parallel Symbolic Execution Demo ===\n");
    
    // Example 1: Complex branching function
    {
        let program = r#"
            (define (complex-branch a b c)
              (if (> a 0)
                  (if (> b 0)
                      (if (> c 0)
                          'all-positive
                          (if (= c 0)
                              'c-zero
                              'c-negative))
                      (if (> c 0)
                          'only-a-c-positive
                          'only-a-positive))
                  (if (< a 0)
                      (if (> b 0)
                          'b-positive-a-negative
                          (if (> c 0)
                              'only-c-positive
                              'none-positive))
                      'a-zero)))
        "#;
        
        let graph = parse(program)?;
        
        println!("Testing complex branching function with 3 parameters...\n");
        
        // Sequential execution
        let start = Instant::now();
        let sequential_executor = SymbolicExecutor::new();
        let sequential_states = sequential_executor.execute_function_by_name(&graph, "complex-branch")?;
        let sequential_time = start.elapsed();
        
        println!("Sequential Execution:");
        println!("  States explored: {}", sequential_states.len());
        println!("  Time: {:?}", sequential_time);
        println!("  States/second: {:.2}", sequential_states.len() as f64 / sequential_time.as_secs_f64());
        
        // Parallel execution
        let parallel_executor = ParallelSymbolicExecutor::new(graph.clone());
        
        // Find function ID (simplified)
        let function_id = fluentai_core::ast::NodeId(NonZero::new(1).unwrap()); // Would need proper lookup
        
        let start = Instant::now();
        let parallel_states = parallel_executor.execute_function(
            function_id,
            &["a".to_string(), "b".to_string(), "c".to_string()]
        )?;
        let parallel_time = start.elapsed();
        
        println!("\nParallel Execution:");
        println!("  States explored: {}", parallel_states.len());
        println!("  Time: {:?}", parallel_time);
        println!("  States/second: {:.2}", parallel_states.len() as f64 / parallel_time.as_secs_f64());
        println!("  Speedup: {:.2}x", sequential_time.as_secs_f64() / parallel_time.as_secs_f64());
        println!("  CPU cores used: {}", num_cpus::get());
    }
    
    // Example 2: Recursive function with parallel exploration
    println!("\n\n=== Recursive Function Parallel Exploration ===\n");
    {
        let program = r#"
            (define (tree-sum n)
              (if (<= n 0)
                  0
                  (if (= n 1)
                      1
                      (+ (tree-sum (- n 1))
                         (tree-sum (- n 2))))))
        "#;
        
        let graph = parse(program)?;
        
        // Configure parallel execution
        let config = ParallelConfig {
            num_threads: num_cpus::get(),
            max_depth: 15,
            max_states_per_worker: 100,
            work_stealing_threshold: 8,
            batch_size: 4,
        };
        
        println!("Configuration:");
        println!("  Threads: {}", config.num_threads);
        println!("  Max depth: {}", config.max_depth);
        println!("  Work stealing threshold: {}", config.work_stealing_threshold);
        
        let parallel_executor = ParallelSymbolicExecutor::with_config(graph.clone(), config);
        
        let start = Instant::now();
        let states = parallel_executor.execute_function(
            fluentai_core::ast::NodeId(NonZero::new(1).unwrap()),
            &["n".to_string()]
        )?;
        let duration = start.elapsed();
        
        println!("\nResults:");
        println!("  Paths explored: {}", states.len());
        println!("  Time: {:?}", duration);
        println!("  Paths/second: {:.2}", states.len() as f64 / duration.as_secs_f64());
    }
    
    // Example 3: Batch execution of multiple functions
    println!("\n\n=== Batch Function Execution ===\n");
    {
        let program = r#"
            (define (f1 x) (if (> x 0) 'pos 'neg))
            (define (f2 x y) (if (> x y) x y))
            (define (f3 a b c) (+ a (+ b c)))
        "#;
        
        let graph = parse(program)?;
        let parallel_executor = ParallelSymbolicExecutor::new(graph);
        
        // Execute multiple functions in parallel
        let functions = vec![
            (fluentai_core::ast::NodeId(NonZero::new(1).unwrap()), vec!["x".to_string()]),
            (fluentai_core::ast::NodeId(NonZero::new(2).unwrap()), vec!["x".to_string(), "y".to_string()]),
            (fluentai_core::ast::NodeId(NonZero::new(3).unwrap()), vec!["a".to_string(), "b".to_string(), "c".to_string()]),
        ];
        
        let start = Instant::now();
        let batch_results = parallel_executor.execute_functions_batch(functions)?;
        let duration = start.elapsed();
        
        println!("Batch execution of {} functions:", batch_results.len());
        for (i, states) in batch_results.iter().enumerate() {
            println!("  Function {}: {} paths", i + 1, states.len());
        }
        println!("Total time: {:?}", duration);
    }
    
    Ok(())
}