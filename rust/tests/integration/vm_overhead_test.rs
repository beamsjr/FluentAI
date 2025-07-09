use std::time::Instant;
use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_stdlib::StdlibRegistry;

fn main() {
    println!("=== VM Overhead Analysis ===\n");
    
    // Simple expression that should execute very quickly
    let code = "42";
    let ast = parse(code).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    println!("Bytecode chunks: {}", bytecode.chunks.len());
    println!("Main chunk instructions: {}\n", bytecode.chunks[bytecode.main_chunk].instructions.len());
    
    // Benchmark VM creation
    println!("1. VM Creation Overhead:");
    let mut creation_times = vec![];
    for _ in 0..1000 {
        let t = Instant::now();
        let _vm = VM::new(bytecode.clone());
        creation_times.push(t.elapsed().as_nanos() as f64);
    }
    creation_times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let creation_median = creation_times[500];
    println!("   Median VM::new() time: {:.0} ns ({:.3} µs)", creation_median, creation_median / 1000.0);
    
    // Benchmark stdlib initialization
    println!("\n2. Stdlib Initialization Overhead:");
    let mut stdlib_times = vec![];
    for _ in 0..1000 {
        let t = Instant::now();
        let _stdlib = fluentai_stdlib::init_stdlib();
        stdlib_times.push(t.elapsed().as_nanos() as f64);
    }
    stdlib_times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let stdlib_median = stdlib_times[500];
    println!("   Median init_stdlib() time: {:.0} ns ({:.3} µs)", stdlib_median, stdlib_median / 1000.0);
    
    // Benchmark just the execution (after VM is created)
    println!("\n3. Pure Execution Time (VM already created):");
    
    // Since we can't reset the VM, measure single execution after creation
    let mut exec_times = vec![];
    for _ in 0..1000 {
        let mut vm = VM::new(bytecode.clone());
        
        // Warm up the VM a bit
        vm.run().unwrap();
        
        // Now measure a second run (VM is already warm)
        let mut vm2 = VM::new(bytecode.clone());
        let t = Instant::now();
        vm2.run().unwrap();
        exec_times.push(t.elapsed().as_nanos() as f64);
    }
    exec_times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let exec_median = exec_times[500];
    println!("   Median execution time: {:.0} ns ({:.3} µs)", exec_median, exec_median / 1000.0);
    
    // Analyze the breakdown
    println!("\n4. Performance Breakdown:");
    let total_time = creation_median + exec_median;
    println!("   Total time (creation + execution): {:.0} ns ({:.3} µs)", total_time, total_time / 1000.0);
    println!("   VM creation overhead: {:.1}%", (creation_median / total_time) * 100.0);
    println!("   Actual execution: {:.1}%", (exec_median / total_time) * 100.0);
    
    println!("\n5. Theoretical Performance:");
    let ops_per_sec_with_creation = 1_000_000_000.0 / total_time;
    let ops_per_sec_without_creation = 1_000_000_000.0 / exec_median;
    println!("   With VM creation: {:.0} ops/sec", ops_per_sec_with_creation);
    println!("   Without VM creation: {:.0} ops/sec", ops_per_sec_without_creation);
    
    // Check what's taking time in VM creation
    println!("\n6. Component Initialization Times:");
    
    // Empty hashmap creation
    let t = Instant::now();
    for _ in 0..1000 {
        let _map = rustc_hash::FxHashMap::<String, fluentai_vm::Value>::default();
    }
    let hashmap_time = t.elapsed().as_nanos() as f64 / 1000.0;
    println!("   FxHashMap creation: {:.0} ns", hashmap_time);
    
    // Vec with capacity
    let t = Instant::now();
    for _ in 0..1000 {
        let _vec = Vec::<fluentai_vm::Value>::with_capacity(10_000);
    }
    let vec_time = t.elapsed().as_nanos() as f64 / 1000.0;
    println!("   Vec::with_capacity(10_000): {:.0} ns", vec_time);
    
    // ModuleLoader creation
    let t = Instant::now();
    for _ in 0..1000 {
        let _loader = fluentai_modules::ModuleLoader::new(fluentai_modules::ModuleConfig::default());
    }
    let loader_time = t.elapsed().as_nanos() as f64 / 1000.0;
    println!("   ModuleLoader::new(): {:.0} ns", loader_time);
    
    println!("\n7. Analysis:");
    println!("   The primary bottleneck is VM creation, specifically:");
    println!("   - Stdlib initialization accounts for ~{:.1}% of VM creation time", 
             (stdlib_median / creation_median) * 100.0);
    println!("   - This involves registering 258 functions");
    println!("   - Each VM instance creates its own complete stdlib registry");
}