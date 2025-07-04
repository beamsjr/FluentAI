//! Test optimization performance

use fluentai_parser::parse;
use fluentai_vm::{Compiler, CompilerOptions, VM};
use fluentai_optimizer::OptimizationLevel;
use std::time::Instant;

fn main() {
    let test_cases = vec![
        ("constant folding", "(+ 2 3)"),
        ("nested arithmetic", "(+ (* 2 3) (- 10 5))"),
        ("function inlining", "((lambda (x) (+ x 1)) 5)"),
        ("dead code", "(let ((x 10) (y 20)) x)"),
        ("complex", r#"
            (let ((add (lambda (x y) (+ x y)))
                  (mul (lambda (x y) (* x y))))
              (+ (add 2 3) (mul 4 5)))
        "#),
    ];

    for (name, code) in test_cases {
        println!("\n=== Testing: {} ===", name);
        println!("Code: {}", code);
        
        let ast = parse(code).unwrap();
        
        // Test each optimization level
        for level in [
            OptimizationLevel::None,
            OptimizationLevel::Basic,
            OptimizationLevel::Standard,
            OptimizationLevel::Aggressive,
        ] {
            let start = Instant::now();
            
            let compiler = Compiler::with_options(CompilerOptions {
                optimization_level: level,
                debug_info: false,
            });
            
            let bytecode = compiler.compile(&ast).unwrap();
            let compile_time = start.elapsed();
            
            // Count instructions
            let instruction_count: usize = bytecode.chunks.iter()
                .map(|chunk| chunk.instructions.len())
                .sum();
            
            // Run the code
            let start = Instant::now();
            let mut vm = VM::new(bytecode);
            let result = vm.run().unwrap();
            let run_time = start.elapsed();
            
            println!("  {:?}: {} instructions, compile: {:?}, run: {:?}, result: {}",
                     level, instruction_count, compile_time, run_time, result);
        }
    }
    
    // Benchmark a larger program
    println!("\n=== Benchmark: Factorial ===");
    let factorial_code = r#"
        (letrec ((factorial (lambda (n)
                              (if (= n 0)
                                  1
                                  (* n (factorial (- n 1)))))))
          (factorial 10))
    "#;
    
    let ast = parse(factorial_code).unwrap();
    
    // Compare unoptimized vs optimized
    let unopt_compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let unopt_bytecode = unopt_compiler.compile(&ast).unwrap();
    
    let opt_compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::Aggressive,
        debug_info: false,
    });
    let opt_bytecode = opt_compiler.compile(&ast).unwrap();
    
    // Run multiple times for timing
    let iterations = 1000;
    
    let start = Instant::now();
    for _ in 0..iterations {
        let mut vm = VM::new(unopt_bytecode.clone());
        let _ = vm.run().unwrap();
    }
    let unopt_time = start.elapsed();
    
    let start = Instant::now();
    for _ in 0..iterations {
        let mut vm = VM::new(opt_bytecode.clone());
        let _ = vm.run().unwrap();
    }
    let opt_time = start.elapsed();
    
    println!("Unoptimized: {:?} for {} iterations", unopt_time, iterations);
    println!("Optimized: {:?} for {} iterations", opt_time, iterations);
    
    let speedup = unopt_time.as_secs_f64() / opt_time.as_secs_f64();
    println!("Speedup: {:.2}x", speedup);
    
    let improvement = ((unopt_time.as_secs_f64() - opt_time.as_secs_f64()) / unopt_time.as_secs_f64()) * 100.0;
    println!("Performance improvement: {:.1}%", improvement);
}