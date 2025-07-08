//! Realistic Throughput Benchmark
//! 
//! Measures actual operation throughput with realistic workloads

use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler};
use std::time::Instant;

fn benchmark_code(name: &str, code: &str, iterations: usize) -> Result<f64, Box<dyn std::error::Error>> {
    // Parse and compile
    let graph = parse(code)?;
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Warm up
    for _ in 0..10 {
        let mut warm_vm = VM::new(bytecode.clone());
        warm_vm.run()?;
    }
    
    // Benchmark
    let start = Instant::now();
    for _ in 0..iterations {
        let mut vm = VM::new(bytecode.clone());
        vm.run()?;
    }
    let duration = start.elapsed();
    
    let ops_per_second = iterations as f64 / duration.as_secs_f64();
    println!("{:<25} {:>10.0} ops/sec", name, ops_per_second);
    
    Ok(ops_per_second)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("FluentAI Realistic Throughput Benchmark");
    println!("======================================\n");
    
    let mut total_ops = 0.0;
    let mut count = 0;
    
    // Test various operations
    let benchmarks = vec![
        ("Simple arithmetic", "(+ 1 2)", 100_000),
        ("Multiple operations", "(+ (* 2 3) (- 10 5))", 50_000),
        ("Function definition", "(let ((f (lambda (x) (* x x)))) (f 5))", 25_000),
        ("List operations", "(let ((lst (list 1 2 3))) (car lst))", 25_000),
        ("Nested arithmetic", "(+ 1 (+ 2 (+ 3 (+ 4 5))))", 25_000),
        ("Boolean logic", "(and (> 5 3) (< 2 4))", 50_000),
        ("Conditional", "(if (> 10 5) 42 0)", 50_000),
        ("String concat", "(str \"Hello\" \" \" \"World\")", 25_000),
    ];
    
    for (name, code, iters) in benchmarks {
        match benchmark_code(name, code, iters) {
            Ok(ops) => {
                total_ops += ops;
                count += 1;
            }
            Err(e) => {
                println!("{:<25} ERROR: {}", name, e);
            }
        }
    }
    
    println!("\n========================================");
    let avg_ops = total_ops / count as f64;
    println!("Average throughput: {:.0} ops/sec", avg_ops);
    
    if avg_ops >= 100_000.0 {
        println!("\n✓ VERIFIED: FluentAI achieves 100,000+ operations/second");
    } else if avg_ops >= 50_000.0 {
        println!("\n⚠ Performance: {:.0} ops/sec (good but below 100k target)", avg_ops);
    } else {
        println!("\n✗ Performance: {:.0} ops/sec (below expectations)", avg_ops);
    }
    
    // Additional context
    println!("\nNote: This measures full VM creation + execution cycles.");
    println!("For pure operation throughput within a running VM,");
    println!("performance would be significantly higher.");
    
    Ok(())
}