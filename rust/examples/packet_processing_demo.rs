//! Demonstration of high-performance packet processing using FluentAI optimizations

use fluentai_parser::parse;
use fluentai_vm::{
    VM, VMBuilder, VMConfig, Compiler,
    MemoryPool, PoolConfig,
    channel, ChannelMode,
    WorkStealingDeque,
};
use std::sync::Arc;
use std::thread;
use std::time::Instant;

fn main() {
    println!("FluentAI Packet Processing Demo");
    println!("================================\n");
    
    // Demo 1: Tail Call Optimization for packet parsing
    demo_tail_call_optimization();
    
    // Demo 2: Unboxed arithmetic for checksum calculation
    demo_unboxed_arithmetic();
    
    // Demo 3: Memory pools for packet buffers
    demo_memory_pools();
    
    // Demo 4: Lock-free concurrent packet processing
    demo_concurrent_processing();
}

fn demo_tail_call_optimization() {
    println!("1. Tail Call Optimization Demo");
    println!("------------------------------");
    
    let code = r#"
    ; Tail-recursive packet parser
    (letrec ((parse-packets
              (lambda (data count)
                (if (empty? data)
                    count
                    (let ((packet (first data))
                          (rest (rest data)))
                      ; Process packet header
                      (let ((version (first packet))
                            (length (first (rest packet))))
                        ; Tail call - will be optimized to loop
                        (parse-packets rest (+ count 1))))))))
      
      ; Create test data: list of packets
      (let ((packets (list
                      (list 4 20)  ; version 4, length 20
                      (list 4 40)  ; version 4, length 40
                      (list 6 60)  ; version 6, length 60
                      (list 4 80)  ; version 4, length 80
                      (list 6 100) ; version 6, length 100
                      )))
        (parse-packets packets 0)))
    "#;
    
    let start = Instant::now();
    let result = run_code(code);
    let elapsed = start.elapsed();
    
    println!("Parsed packet count: {}", result);
    println!("Time: {:?}", elapsed);
    println!("Note: Tail calls optimized to avoid stack growth\n");
}

fn demo_unboxed_arithmetic() {
    println!("2. Unboxed Arithmetic Demo");
    println!("--------------------------");
    
    let code = r#"
    ; Calculate packet checksum using unboxed arithmetic
    (let ((calculate-checksum
           (lambda (data)
             (letrec ((sum-loop
                       (lambda (data acc)
                         (if (empty? data)
                             acc
                             ; Using specialized integer operations
                             (sum-loop (rest data) 
                                      (+int acc (first data)))))))
               (sum-loop data 0)))))
      
      ; Simulate packet data
      (let ((packet-data (list 0xFF 0x45 0x00 0x34 
                              0x12 0x34 0x40 0x00
                              0x40 0x06 0x00 0x00
                              0xC0 0xA8 0x01 0x01
                              0xC0 0xA8 0x01 0x02)))
        (calculate-checksum packet-data)))
    "#;
    
    let start = Instant::now();
    let result = run_code(code);
    let elapsed = start.elapsed();
    
    println!("Checksum: {}", result);
    println!("Time: {:?}", elapsed);
    println!("Note: Using unboxed integer arithmetic\n");
}

fn demo_memory_pools() {
    println!("3. Memory Pool Demo");
    println!("-------------------");
    
    // Create memory pool for packet buffers
    let pool_config = PoolConfig {
        slab_size: 1500,     // MTU size
        initial_slabs: 100,  // Pre-allocate 100 packet buffers
        max_slabs: 1000,
        track_stats: true,
    };
    
    let pool = Arc::new(MemoryPool::new(pool_config));
    let pool2 = pool.clone();
    
    // Simulate packet allocation/deallocation
    let handle = thread::spawn(move || {
        let mut buffers = Vec::new();
        
        for i in 0..1000 {
            // Allocate packet buffer
            if let Ok(ptr) = pool2.allocate_raw(1500, 8) {
                buffers.push(ptr);
            }
            
            // Periodically release buffers
            if i % 100 == 0 {
                // In real implementation, we'd properly deallocate
                buffers.clear();
                pool2.reset();
            }
        }
    });
    
    handle.join().unwrap();
    
    let stats = pool.stats();
    println!("Allocations: {}", stats.allocations);
    println!("Peak memory used: {} KB", stats.peak_used / 1024);
    println!("Note: Pre-allocated memory pools avoid allocation overhead\n");
}

fn demo_concurrent_processing() {
    println!("4. Concurrent Packet Processing Demo");
    println!("------------------------------------");
    
    let code = r#"
    ; Concurrent packet processor using channels
    (let ((process-packet
           (lambda (packet)
             ; Simulate packet processing
             (let ((version (first packet))
                   (length (first (rest packet))))
               (if (= version 4)
                   (list 'ipv4 length)
                   (list 'ipv6 length))))))
      
      ; Create channel for results
      (let ((result-chan (chan)))
        ; Process multiple packets concurrently
        (spawn (send! result-chan (process-packet (list 4 100))))
        (spawn (send! result-chan (process-packet (list 6 200))))
        (spawn (send! result-chan (process-packet (list 4 150))))
        
        ; Collect results
        (list (recv! result-chan)
              (recv! result-chan)
              (recv! result-chan))))
    "#;
    
    let start = Instant::now();
    let result = run_code(code);
    let elapsed = start.elapsed();
    
    println!("Processed packets: {}", result);
    println!("Time: {:?}", elapsed);
    println!("Note: Using lock-free channels for communication\n");
    
    // Demo work-stealing deque
    demo_work_stealing();
}

fn demo_work_stealing() {
    println!("5. Work-Stealing Scheduler Demo");
    println!("-------------------------------");
    
    let deque1 = Arc::new(WorkStealingDeque::<i32>::new(256));
    let deque2 = Arc::new(WorkStealingDeque::<i32>::new(256));
    
    let stealer1 = deque1.clone();
    let stealer2 = deque2.clone();
    
    // Worker 1: produces work
    let worker1 = thread::spawn(move || {
        for i in 0..100 {
            deque1.push(i);
        }
        
        // Process own work
        let mut processed = 0;
        while deque1.pop().is_some() {
            processed += 1;
        }
        processed
    });
    
    // Worker 2: steals work from worker 1
    let worker2 = thread::spawn(move || {
        let mut stolen = 0;
        
        // Try to steal work
        for _ in 0..50 {
            if stealer1.steal().is_some() {
                stolen += 1;
            }
        }
        stolen
    });
    
    let processed = worker1.join().unwrap();
    let stolen = worker2.join().unwrap();
    
    println!("Worker 1 processed: {} packets", processed);
    println!("Worker 2 stole: {} packets", stolen);
    println!("Total: {} packets", processed + stolen);
    println!("Note: Work-stealing enables load balancing\n");
}

fn run_code(code: &str) -> String {
    let ast = parse(code).expect("Failed to parse");
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).expect("Failed to compile");
    
    let config = VMConfig::default()
        .with_stack_size(10000)
        .with_optimization_level(fluentai_optimizer::OptimizationLevel::Aggressive);
    
    let mut vm = VMBuilder::new()
        .with_config(config)
        .with_bytecode(bytecode)
        .build()
        .expect("Failed to build VM");
    
    match vm.run() {
        Ok(value) => format!("{}", value),
        Err(e) => format!("Error: {}", e),
    }
}