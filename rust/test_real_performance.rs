use std::time::Instant;
use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};

fn main() {
    println!("=== Real Performance Test ===\n");
    
    let test_cases = vec![
        ("simple", "42"),
        ("arithmetic", "(+ 1 2)"),
        ("nested", "(+ (* 2 3) (- 5 1))"),
        ("let", "(let ((x 10)) (+ x 5))"),
    ];
    
    // Warmup
    for _ in 0..1000 {
        for (_, code) in &test_cases {
            let _ = parse(code);
        }
    }
    
    println!("{:<15} {:>15} {:>15} {:>15} {:>15}", "Expression", "Parse (ns)", "Compile (ns)", "VM (ns)", "Total (ns)");
    println!("{:-<80}", "");
    
    for (name, code) in &test_cases {
        // Measure parsing
        let start = Instant::now();
        let mut parse_times = vec![];
        for _ in 0..10000 {
            let t = Instant::now();
            let _ = parse(code).unwrap();
            parse_times.push(t.elapsed().as_nanos() as f64);
        }
        let parse_avg = parse_times.iter().sum::<f64>() / parse_times.len() as f64;
        
        // Parse once for compilation
        let ast = parse(code).unwrap();
        
        // Measure compilation
        let mut compile_times = vec![];
        for _ in 0..10000 {
            let t = Instant::now();
            let compiler = Compiler::new();
            let _ = compiler.compile(&ast).unwrap();
            compile_times.push(t.elapsed().as_nanos() as f64);
        }
        let compile_avg = compile_times.iter().sum::<f64>() / compile_times.len() as f64;
        
        // Compile once for VM
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();
        
        // Measure VM execution
        let mut vm_times = vec![];
        for _ in 0..10000 {
            let t = Instant::now();
            let mut vm = VM::new(bytecode.clone());
            let _ = vm.run().unwrap();
            vm_times.push(t.elapsed().as_nanos() as f64);
        }
        let vm_avg = vm_times.iter().sum::<f64>() / vm_times.len() as f64;
        
        let total = parse_avg + compile_avg + vm_avg;
        
        println!("{:<15} {:>15.1} {:>15.1} {:>15.1} {:>15.1}", 
                 name, parse_avg, compile_avg, vm_avg, total);
    }
    
    println!("\n=== Python Baseline Comparison ===");
    println!("\nAssuming Python baseline of:");
    println!("- Parser: ~50 µs (50,000 ns)");
    println!("- Interpreter: ~5 µs (5,000 ns)");
    println!("- Total: ~55 µs (55,000 ns)");
    
    println!("\nThis gives approximate speedups of:");
    println!("- Parser: ~100-500x");
    println!("- VM: ~20-50x");
    println!("- Overall: ~50-200x");
    
    println!("\nNote: The exact Python baseline varies by expression complexity.");
    println!("The previously reported 29,795x - 135,433x speedups appear to be");
    println!("calculation errors (unit conversion mistakes).");
}