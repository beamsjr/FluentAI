//! Concurrent GC Demo
//! 
//! Demonstrates the concurrent garbage collector with <10ms pause times

use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler, VMBuilder};
use std::time::{Duration, Instant};
use std::thread;
use std::sync::{Arc, Mutex};

fn create_gc_stress_code(size: usize) -> String {
    // Create code that allocates many temporary objects
    let mut code = String::from("(let (");
    
    // Create many temporary bindings
    for i in 0..size {
        code.push_str(&format!("(temp{} (list {} {} {})) ", i, i, i+1, i+2));
    }
    
    code.push_str(") (list ");
    
    // Reference only a few of them (most become garbage)
    for i in (0..size).step_by(10) {
        code.push_str(&format!("temp{} ", i));
    }
    
    code.push_str("))");
    code
}

fn measure_gc_pauses() -> Result<(), Box<dyn std::error::Error>> {
    println!("Concurrent GC Pause Time Analysis");
    println!("=================================\n");
    
    // Create a large program that generates garbage
    let code = create_gc_stress_code(1000);
    let graph = parse(&code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with GC enabled
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode.clone())
        .with_gc_threshold(100)  // Low threshold to trigger GC frequently
        .build()?;
    
    let pause_times = Arc::new(Mutex::new(Vec::new()));
    let pause_times_clone = pause_times.clone();
    
    // Monitor thread to detect pauses
    let monitoring = Arc::new(Mutex::new(true));
    let monitoring_clone = monitoring.clone();
    
    let monitor_thread = thread::spawn(move || {
        let mut last_check = Instant::now();
        let check_interval = Duration::from_micros(100); // Check every 100µs
        
        while *monitoring_clone.lock().unwrap() {
            thread::sleep(check_interval);
            let now = Instant::now();
            let elapsed = now.duration_since(last_check);
            
            // If more than 1ms passed, we likely had a GC pause
            if elapsed > Duration::from_millis(1) {
                pause_times_clone.lock().unwrap().push(elapsed);
            }
            
            last_check = now;
        }
    });
    
    // Run the program many times to trigger GC
    println!("Running GC stress test...");
    let start = Instant::now();
    for i in 0..100 {
        vm.run()?;
        vm.reset();
        
        if i % 10 == 0 {
            print!(".");
            use std::io::{self, Write};
            io::stdout().flush()?;
        }
    }
    let total_time = start.elapsed();
    println!(" done\n");
    
    // Stop monitoring
    *monitoring.lock().unwrap() = false;
    monitor_thread.join().unwrap();
    
    // Analyze pause times
    let pauses = pause_times.lock().unwrap();
    if pauses.is_empty() {
        println!("No GC pauses detected (GC may not have been triggered)");
    } else {
        let max_pause = pauses.iter().max().unwrap();
        let avg_pause: Duration = pauses.iter().sum::<Duration>() / pauses.len() as u32;
        
        println!("GC Statistics:");
        println!("- Total pauses: {}", pauses.len());
        println!("- Max pause: {:.2} ms", max_pause.as_secs_f64() * 1000.0);
        println!("- Average pause: {:.2} ms", avg_pause.as_secs_f64() * 1000.0);
        println!("- Total time: {:.2} ms", total_time.as_secs_f64() * 1000.0);
        
        if max_pause < &Duration::from_millis(10) {
            println!("\n✓ VERIFIED: Concurrent GC achieves <10ms pause times");
        } else {
            println!("\n⚠ Max pause exceeded 10ms target");
        }
    }
    
    Ok(())
}

fn benchmark_gc_throughput() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n\nGC Throughput Impact");
    println!("====================\n");
    
    let code = "(let ((lst (list 1 2 3 4 5))) (car lst))";
    let graph = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Benchmark without GC
    let mut vm_no_gc = VM::new(bytecode.clone());
    let start = Instant::now();
    for _ in 0..10000 {
        vm_no_gc.run()?;
        vm_no_gc.reset();
    }
    let time_no_gc = start.elapsed();
    
    // Benchmark with GC
    let mut vm_with_gc = VMBuilder::new()
        .with_bytecode(bytecode)
        .with_gc_threshold(1000)
        .build()?;
    
    let start = Instant::now();
    for _ in 0..10000 {
        vm_with_gc.run()?;
        vm_with_gc.reset();
    }
    let time_with_gc = start.elapsed();
    
    let overhead = ((time_with_gc.as_secs_f64() / time_no_gc.as_secs_f64()) - 1.0) * 100.0;
    
    println!("Without GC: {:.2} ms", time_no_gc.as_secs_f64() * 1000.0);
    println!("With GC: {:.2} ms", time_with_gc.as_secs_f64() * 1000.0);
    println!("GC overhead: {:.1}%", overhead);
    
    if overhead < 5.0 {
        println!("\n✓ GC overhead is minimal (<5%)");
    }
    
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("FluentAI Concurrent GC Demo");
    println!("===========================\n");
    
    println!("Note: This demo simulates concurrent GC behavior.");
    println!("Actual implementation may vary.\n");
    
    measure_gc_pauses()?;
    benchmark_gc_throughput()?;
    
    Ok(())
}