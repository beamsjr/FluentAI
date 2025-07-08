//! VM Performance Benchmark
//! 
//! Detailed performance analysis of VM execution characteristics

use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler};
use std::time::Instant;

fn measure_vm_overhead() -> Result<(), Box<dyn std::error::Error>> {
    println!("VM Overhead Analysis");
    println!("===================\n");
    
    // Simple expression for minimal overhead
    let code = "(+ 1 2)";
    let graph = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Measure VM creation time
    let create_start = Instant::now();
    for _ in 0..1000 {
        let _ = VM::new(bytecode.clone());
    }
    let create_time = create_start.elapsed();
    println!("VM creation time: {:.3} µs", create_time.as_secs_f64() * 1_000_000.0 / 1000.0);
    
    // Measure execution time with new VM each time
    let new_vm_start = Instant::now();
    for _ in 0..1000 {
        let mut vm = VM::new(bytecode.clone());
        vm.run()?;
    }
    let new_vm_time = new_vm_start.elapsed();
    println!("Execution with new VM: {:.3} µs", new_vm_time.as_secs_f64() * 1_000_000.0 / 1000.0);
    
    // Measure execution time with VM reset
    let mut vm = VM::new(bytecode.clone());
    let reset_start = Instant::now();
    for _ in 0..1000 {
        vm.run()?;
        vm.reset();
    }
    let reset_time = reset_start.elapsed();
    println!("Execution with reset: {:.3} µs", reset_time.as_secs_f64() * 1_000_000.0 / 1000.0);
    
    // Calculate overhead percentages
    let create_overhead = (create_time.as_secs_f64() / new_vm_time.as_secs_f64()) * 100.0;
    let speedup = new_vm_time.as_secs_f64() / reset_time.as_secs_f64();
    
    println!("\nOverhead Analysis:");
    println!("- VM creation is {:.1}% of total time", create_overhead);
    println!("- Using reset() is {:.1}x faster than new VM", speedup);
    
    Ok(())
}

fn benchmark_instruction_types() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n\nInstruction Performance");
    println!("======================\n");
    
    struct Benchmark {
        name: &'static str,
        code: &'static str,
    }
    
    let benchmarks = vec![
        Benchmark { name: "Push constant", code: "42" },
        Benchmark { name: "Arithmetic", code: "(+ 1 2)" },
        Benchmark { name: "Variable access", code: "(let ((x 42)) x)" },
        Benchmark { name: "Function call", code: "((lambda (x) x) 42)" },
        Benchmark { name: "Conditional", code: "(if true 1 2)" },
        Benchmark { name: "List creation", code: "(list 1 2 3)" },
        Benchmark { name: "List access", code: "(car (list 1 2 3))" },
    ];
    
    for bench in benchmarks {
        let graph = parse(bench.code)?;
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&graph)?;
        let mut vm = VM::new(bytecode);
        
        // Warm up
        for _ in 0..100 {
            vm.run()?;
            vm.reset();
        }
        
        // Measure
        let start = Instant::now();
        let iterations = 100_000;
        for _ in 0..iterations {
            vm.run()?;
            vm.reset();
        }
        let duration = start.elapsed();
        
        let ns_per_op = duration.as_nanos() as f64 / iterations as f64;
        println!("{:<20} {:.1} ns/op", bench.name, ns_per_op);
    }
    
    Ok(())
}

fn analyze_scaling() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n\nScaling Analysis");
    println!("================\n");
    
    println!("List size vs. performance:");
    for size in [10, 100, 1000] {
        let mut code = String::from("(list");
        for i in 0..size {
            code.push_str(&format!(" {}", i));
        }
        code.push(')');
        
        let graph = parse(&code)?;
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&graph)?;
        let mut vm = VM::new(bytecode);
        
        let start = Instant::now();
        let iterations = 1000;
        for _ in 0..iterations {
            vm.run()?;
            vm.reset();
        }
        let duration = start.elapsed();
        
        let us_per_op = duration.as_secs_f64() * 1_000_000.0 / iterations as f64;
        println!("  {} elements: {:.2} µs/op", size, us_per_op);
    }
    
    println!("\nNested depth vs. performance:");
    for depth in [5, 10, 20] {
        let mut code = String::new();
        for _ in 0..depth {
            code.push_str("(+ 1 ");
        }
        code.push('0');
        for _ in 0..depth {
            code.push(')');
        }
        
        let graph = parse(&code)?;
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&graph)?;
        let mut vm = VM::new(bytecode);
        
        let start = Instant::now();
        let iterations = 10000;
        for _ in 0..iterations {
            vm.run()?;
            vm.reset();
        }
        let duration = start.elapsed();
        
        let us_per_op = duration.as_secs_f64() * 1_000_000.0 / iterations as f64;
        println!("  {} levels: {:.2} µs/op", depth, us_per_op);
    }
    
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("FluentAI VM Performance Analysis");
    println!("================================");
    
    measure_vm_overhead()?;
    benchmark_instruction_types()?;
    analyze_scaling()?;
    
    println!("\n✓ Performance analysis complete");
    
    Ok(())
}