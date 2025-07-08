//! Throughput Benchmark
//! 
//! Demonstrates FluentAI's ability to execute 100,000+ operations per second.
//! This benchmark measures the throughput of various operations including:
//! - Simple arithmetic operations
//! - Function calls
//! - Pattern matching
//! - List operations

use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler};
use std::time::{Duration, Instant};

const ITERATIONS: usize = 100_000;

fn benchmark_operation(name: &str, code: &str) -> Result<(Duration, f64), Box<dyn std::error::Error>> {
    // Parse and compile the code
    let graph = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create a single VM
    let mut vm = VM::new(bytecode);
    
    // Warm up
    for _ in 0..100 {
        vm.run()?;
        vm.reset(); // Reset VM state between runs
    }
    
    // Benchmark
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        vm.run()?;
        vm.reset(); // Reset VM state between runs
    }
    let duration = start.elapsed();
    
    let ops_per_second = ITERATIONS as f64 / duration.as_secs_f64();
    
    println!("{}: {:>10.0} ops/sec ({}μs per op)", 
             name, 
             ops_per_second,
             duration.as_micros() as f64 / ITERATIONS as f64);
    
    Ok((duration, ops_per_second))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("FluentAI Throughput Benchmark");
    println!("============================");
    println!("Running {} iterations of each operation\n", ITERATIONS);
    
    let benchmarks = vec![
        ("Simple arithmetic", "(+ 1 2)"),
        ("Complex arithmetic", "(+ (* 3 4) (* 5 6))"),
        ("Function call", "((lambda (x) (* x x)) 7)"),
        ("Conditional", "(if (> 5 3) 10 20)"),
        ("Let binding", "(let ((x 5) (y 10)) (+ x y))"),
        ("List creation", "(list 1 2 3 4 5)"),
        ("Pattern matching", "(match 5 (0 \"zero\") (5 \"five\") (_ \"other\"))"),
        ("Recursive function", "(letrec ((f (lambda (n) (if (= n 0) 0 (+ n (f (- n 1))))))) (f 10))"),
    ];
    
    let mut total_ops = 0.0;
    let mut total_duration = Duration::new(0, 0);
    
    for (name, code) in benchmarks {
        match benchmark_operation(name, code) {
            Ok((duration, ops)) => {
                total_ops += ops;
                total_duration += duration;
            }
            Err(e) => {
                eprintln!("Error benchmarking {}: {}", name, e);
            }
        }
    }
    
    println!("\n========================================");
    println!("Average throughput: {:.0} ops/sec", total_ops / 8.0);
    println!("Total time for {} operations: {:.2}s", ITERATIONS * 8, total_duration.as_secs_f64());
    
    // Verify we meet the 100,000+ ops/sec claim
    let avg_throughput = total_ops / 8.0;
    if avg_throughput >= 100_000.0 {
        println!("\n✓ VERIFIED: FluentAI achieves 100,000+ operations/second");
    } else {
        println!("\n✗ Performance below expected threshold");
    }
    
    Ok(())
}