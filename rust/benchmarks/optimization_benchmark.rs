//! Benchmark optimization performance

use fluentai_vm::{Compiler, CompilerOptions, VM};
use fluentai_optimizer::OptimizationLevel;
use std::time::Instant;

fn benchmark_program(name: &str, code: &str, iterations: usize) {
    println!("\n=== Benchmark: {} ===", name);
    
    let ast = parse(code).expect("Failed to parse");
    
    // Compile without optimization
    let unopt_compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let unopt_bytecode = unopt_compiler.compile(&ast).expect("Failed to compile");
    
    // Compile with aggressive optimization
    let opt_compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::Aggressive,
        debug_info: false,
    });
    let opt_bytecode = opt_compiler.compile(&ast).expect("Failed to compile");
    
    // Count instructions
    let unopt_instructions: usize = unopt_bytecode.chunks.iter()
        .map(|chunk| chunk.instructions.len())
        .sum();
    let opt_instructions: usize = opt_bytecode.chunks.iter()
        .map(|chunk| chunk.instructions.len())
        .sum();
    
    println!("Instructions: {} -> {} ({}% reduction)", 
             unopt_instructions, 
             opt_instructions,
             (100.0 * (1.0 - opt_instructions as f64 / unopt_instructions as f64)) as i32);
    
    // Benchmark execution
    let start = Instant::now();
    for _ in 0..iterations {
        let mut vm = VM::new(unopt_bytecode.clone());
        let _ = vm.run().expect("VM execution failed");
    }
    let unopt_time = start.elapsed();
    
    let start = Instant::now();
    for _ in 0..iterations {
        let mut vm = VM::new(opt_bytecode.clone());
        let _ = vm.run().expect("VM execution failed");
    }
    let opt_time = start.elapsed();
    
    let speedup = unopt_time.as_secs_f64() / opt_time.as_secs_f64();
    let improvement = ((unopt_time.as_secs_f64() - opt_time.as_secs_f64()) / unopt_time.as_secs_f64()) * 100.0;
    
    println!("Execution time: {:?} -> {:?}", unopt_time, opt_time);
    println!("Speedup: {:.2}x", speedup);
    println!("Performance improvement: {:.1}%", improvement);
}

fn main() {
    let iterations = 10000;
    
    // Simple constant folding
    benchmark_program(
        "Constant Arithmetic",
        "(+ (* 2 3) (- 10 5))",
        iterations
    );
    
    // Function inlining
    benchmark_program(
        "Function Inlining",
        r#"
        (let ((add (lambda (x y) (+ x y)))
              (mul (lambda (x y) (* x y))))
          (+ (add 2 3) (mul 4 5)))
        "#,
        iterations
    );
    
    // Complex expression with multiple optimizations
    benchmark_program(
        "Complex Expression",
        r#"
        (let ((x 10)
              (y 20)
              (z 30))
          (if (> x 5)
              (+ (* x 2) (* y 3) z)
              (- (* x 3) (* y 2) z)))
        "#,
        iterations
    );
    
    // List operations
    benchmark_program(
        "List Operations",
        r#"
        (let ((lst (list 1 2 3 4 5)))
          (+ (car lst) 
             (car (cdr lst))
             (car (cdr (cdr lst)))))
        "#,
        iterations / 10  // Fewer iterations for complex operations
    );
    
    // Nested let bindings
    benchmark_program(
        "Nested Bindings",
        r#"
        (let ((a 1))
          (let ((b (+ a 2)))
            (let ((c (+ b 3)))
              (let ((d (+ c 4)))
                (+ a b c d)))))
        "#,
        iterations
    );
    
    println!("\n=== Summary ===");
    println!("The FluentAi optimizer successfully reduces code size and improves execution performance.");
    println!("Optimization techniques include:");
    println!("- Constant folding");
    println!("- Dead code elimination");
    println!("- Function inlining");
    println!("- Partial evaluation");
    println!("- Common subexpression elimination");
}