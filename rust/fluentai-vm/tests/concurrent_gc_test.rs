//! Tests for concurrent garbage collection

use fluentai_vm::{ConcurrentGc, ConcurrentGcConfig};
use fluentai_core::value::Value;
use std::thread;
use std::time::Duration;

#[test]
fn test_concurrent_gc_sweep() {
    // Create GC with concurrent sweeping enabled
    let config = ConcurrentGcConfig {
        young_gen_size: 1024 * 1024,     // 1MB
        old_gen_size: 4 * 1024 * 1024,   // 4MB
        promotion_threshold: 2,
        gc_trigger_percent: 0.8,
        concurrent_marking: false,        // Test only concurrent sweep
        concurrent_sweeping: true,
        target_pause_ms: 10,
        gc_thread_priority: 0,
    };
    
    let gc = ConcurrentGc::new(config);
    
    // Allocate some values
    let mut handles = vec![];
    for i in 0..1000 {
        let value = Value::String(format!("test string {}", i));
        let handle = gc.allocate(value).unwrap();
        handles.push(handle);
    }
    
    // Drop half of the handles to make them garbage
    handles.truncate(500);
    
    // Force a full GC
    gc.force_gc().unwrap();
    
    // Give the concurrent sweep time to complete
    thread::sleep(Duration::from_millis(100));
    
    // Check that bytes were freed
    let stats = gc.stats();
    assert!(stats.bytes_freed > 0, "Concurrent sweep should have freed memory");
    assert!(stats.major_collections > 0, "Should have performed at least one major GC");
}

#[test]
fn test_concurrent_gc_allocation_during_sweep() {
    let config = ConcurrentGcConfig {
        young_gen_size: 512 * 1024,      // 512KB
        old_gen_size: 2 * 1024 * 1024,   // 2MB
        promotion_threshold: 1,
        gc_trigger_percent: 0.5,          // Trigger GC earlier
        concurrent_marking: false,
        concurrent_sweeping: true,
        target_pause_ms: 10,
        gc_thread_priority: 0,
    };
    
    let gc = ConcurrentGc::new(config);
    let gc_clone = gc.clone();
    
    // Spawn thread that continuously allocates
    let alloc_thread = thread::spawn(move || {
        for i in 0..500 {
            let value = Value::Integer(i);
            let _ = gc_clone.allocate(value);
            thread::sleep(Duration::from_micros(100));
        }
    });
    
    // Main thread creates garbage and triggers GC
    let mut handles = vec![];
    for i in 0..200 {
        let value = Value::String(format!("garbage {}", i));
        let handle = gc.allocate(value).unwrap();
        handles.push(handle);
    }
    
    // Drop all handles to create garbage
    handles.clear();
    
    // Force GC while allocations are happening
    gc.force_gc().unwrap();
    
    // Wait for allocations to complete
    alloc_thread.join().unwrap();
    
    // Verify GC completed successfully
    let stats = gc.stats();
    assert!(stats.bytes_freed > 0, "Should have freed memory despite concurrent allocations");
}

#[test]
fn test_concurrent_sweep_performance() {
    // Compare performance of concurrent vs synchronous sweep
    let sync_config = ConcurrentGcConfig {
        young_gen_size: 1024 * 1024,
        old_gen_size: 4 * 1024 * 1024,
        promotion_threshold: 2,
        gc_trigger_percent: 0.8,
        concurrent_marking: false,
        concurrent_sweeping: false,  // Synchronous
        target_pause_ms: 10,
        gc_thread_priority: 0,
    };
    
    let concurrent_config = ConcurrentGcConfig {
        concurrent_sweeping: true,   // Concurrent
        ..sync_config.clone()
    };
    
    // Test synchronous GC
    let sync_gc = ConcurrentGc::new(sync_config);
    let sync_time = measure_gc_time(&sync_gc, 5000);
    
    // Test concurrent GC
    let concurrent_gc = ConcurrentGc::new(concurrent_config);
    let concurrent_time = measure_gc_time(&concurrent_gc, 5000);
    
    println!("Synchronous GC time: {:?}", sync_time);
    println!("Concurrent GC time: {:?}", concurrent_time);
    
    // Concurrent should generally be faster for large heaps
    // but we can't guarantee this in all environments
}

fn measure_gc_time(gc: &ConcurrentGc, num_objects: usize) -> Duration {
    use std::time::Instant;
    
    // Allocate objects
    let mut handles = vec![];
    for i in 0..num_objects {
        let value = Value::String(format!("test object {}", i));
        let handle = gc.allocate(value).unwrap();
        handles.push(handle);
    }
    
    // Drop all handles
    handles.clear();
    
    // Measure GC time
    let start = Instant::now();
    gc.force_gc().unwrap();
    start.elapsed()
}