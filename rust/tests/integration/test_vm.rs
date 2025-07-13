//! Test the VM with simple examples

use fluentai_vm::{Compiler, VM};

fn main() {
    println!("Testing FluentAi VM\n");
    
    let test_cases = vec![
        ("Integer literal", "42"),
        ("Float literal", "3.14"),
        ("String literal", r#""hello world""#),
        ("Boolean true", "true"),
        ("Boolean false", "false"),
        ("Simple arithmetic", "(+ 1 2)"),
        ("Nested arithmetic", "(* (+ 1 2) (- 4 3))"),
        ("Integer operations", "(+ (* 2 3) (/ 8 2))"),
        ("Comparison", "(< 5 10)"),
        ("String concat", r#"(str-concat "hello" " world")"#),
        ("List creation", "[1 2 3]"),
    ];
    
    for (name, code) in test_cases {
        println!("Test: {}", name);
        println!("Code: {}", code);
        
        match run_code(code) {
            Ok(result) => println!("Result: {}\n", result),
            Err(e) => println!("Error: {}\n", e),
        }
    }
    
    // Benchmark VM execution
    println!("Benchmarking VM...");
    let start = std::time::Instant::now();
    let iterations = 10_000;
    
    for _ in 0..iterations {
        let _ = run_code("(+ (* 2 3) (* 4 5))");
    }
    
    let elapsed = start.elapsed();
    let per_iteration = elapsed.as_micros() as f64 / iterations as f64;
    
    println!("Average execution time: {:.1} µs", per_iteration);
    println!("\nCompare to Python VM baseline: ~3.2 µs");
    println!("Expected speedup: {:.1}x", 3.2 / per_iteration);
}

fn run_code(code: &str) -> anyhow::Result<String> {
    // Parse
    let ast = parse(code)?;
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    // Execute
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    Ok(format!("{}", result))
}