//! Comprehensive tests for RuntimeHandle and EffectRuntime

use fluentai_effects::{EffectRuntime, runtime::RuntimeHandle};
use std::sync::{Arc, atomic::{AtomicUsize, Ordering}};
use std::time::Duration;
use tokio::time::sleep;

#[test]
fn test_effect_runtime_creation() {
    let runtime = EffectRuntime::new();
    assert!(runtime.is_ok());
    
    // Test that we can create multiple runtimes
    let runtime2 = EffectRuntime::new();
    assert!(runtime2.is_ok());
}

#[test]
fn test_effect_runtime_default() {
    // Default should not panic
    let runtime = EffectRuntime::default();
    
    // Should be able to use it
    let _handle = runtime.handle();
}

#[test]
fn test_runtime_handle_creation() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = RuntimeHandle::new(&runtime);
    
    // Should be able to clone handle
    let handle2 = handle.clone();
    
    // Both handles should work
    let _guard1 = handle.enter();
    drop(_guard1);
    let _guard2 = handle2.enter();
}

#[test]
fn test_runtime_block_on() {
    let runtime = EffectRuntime::new().unwrap();
    
    // Test blocking on a simple future
    let result = runtime.block_on(async {
        42
    });
    
    assert_eq!(result, 42);
    
    // Test blocking on an async operation
    let result = runtime.block_on(async {
        sleep(Duration::from_millis(10)).await;
        "done"
    });
    
    assert_eq!(result, "done");
}

#[test]
fn test_runtime_spawn() {
    let runtime = EffectRuntime::new().unwrap();
    let counter = Arc::new(AtomicUsize::new(0));
    let counter_clone = counter.clone();
    
    // Spawn a task
    let handle = runtime.spawn(async move {
        counter_clone.fetch_add(1, Ordering::SeqCst);
        42
    });
    
    // Wait for task to complete
    let result = runtime.block_on(handle);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 42);
    assert_eq!(counter.load(Ordering::SeqCst), 1);
}

#[test]
fn test_runtime_handle_spawn() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = RuntimeHandle::new(&runtime);
    let counter = Arc::new(AtomicUsize::new(0));
    
    // Spawn multiple tasks via handle
    let mut join_handles = vec![];
    
    for i in 0..5 {
        let counter_clone = counter.clone();
        let jh = handle.spawn(async move {
            sleep(Duration::from_millis(10)).await;
            counter_clone.fetch_add(1, Ordering::SeqCst);
            i
        });
        join_handles.push(jh);
    }
    
    // Wait for all tasks
    runtime.block_on(async {
        for (i, jh) in join_handles.into_iter().enumerate() {
            let result = jh.await;
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), i);
        }
    });
    
    assert_eq!(counter.load(Ordering::SeqCst), 5);
}

#[test]
fn test_runtime_handle_enter_guard() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = RuntimeHandle::new(&runtime);
    
    // Test enter guard
    {
        let _guard = handle.enter();
        // Within the guard, we should be in the runtime context
        // We can spawn blocking tasks
        let result = tokio::task::block_in_place(|| {
            42
        });
        assert_eq!(result, 42);
    }
    // Guard dropped, context exited
}

#[test]
fn test_multiple_runtime_handles() {
    let runtime = EffectRuntime::new().unwrap();
    
    // Create multiple handles
    let handle1 = RuntimeHandle::new(&runtime);
    let handle2 = RuntimeHandle::new(&runtime);
    let handle3 = handle1.clone();
    
    let counter = Arc::new(AtomicUsize::new(0));
    
    // All handles should work
    for handle in [handle1, handle2, handle3] {
        let counter_clone = counter.clone();
        let jh = handle.spawn(async move {
            counter_clone.fetch_add(1, Ordering::SeqCst);
        });
        runtime.block_on(jh).unwrap();
    }
    
    assert_eq!(counter.load(Ordering::SeqCst), 3);
}

#[test]
fn test_runtime_spawn_panic_handling() {
    let runtime = EffectRuntime::new().unwrap();
    
    // Spawn a task that panics
    let handle = runtime.spawn(async {
        panic!("Task panic!");
    });
    
    // The join handle should return an error
    let result = runtime.block_on(handle);
    assert!(result.is_err());
    
    // Runtime should still be functional
    let result = runtime.block_on(async { 42 });
    assert_eq!(result, 42);
}

#[test]
fn test_runtime_concurrent_spawns() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = RuntimeHandle::new(&runtime);
    let counter = Arc::new(AtomicUsize::new(0));
    
    // Spawn many tasks concurrently
    let tasks: Vec<_> = (0..100).map(|_| {
        let counter_clone = counter.clone();
        handle.spawn(async move {
            // Small delay to ensure concurrency
            sleep(Duration::from_micros(100)).await;
            counter_clone.fetch_add(1, Ordering::SeqCst);
        })
    }).collect();
    
    // Wait for all tasks
    runtime.block_on(async {
        for task in tasks {
            task.await.unwrap();
        }
    });
    
    assert_eq!(counter.load(Ordering::SeqCst), 100);
}

#[test]
fn test_runtime_handle_across_threads() {
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let handle = RuntimeHandle::new(&runtime);
    let counter = Arc::new(AtomicUsize::new(0));
    
    // Use handle from multiple threads
    let mut thread_handles = vec![];
    
    for _ in 0..5 {
        let handle_clone = handle.clone();
        let counter_clone = counter.clone();
        let runtime_clone = runtime.clone();
        
        let th = std::thread::spawn(move || {
            // Each thread spawns a task
            let jh = handle_clone.spawn(async move {
                counter_clone.fetch_add(1, Ordering::SeqCst);
            });
            
            // Wait for it using the runtime
            runtime_clone.block_on(jh).unwrap();
        });
        
        thread_handles.push(th);
    }
    
    // Wait for all threads
    for th in thread_handles {
        th.join().unwrap();
    }
    
    assert_eq!(counter.load(Ordering::SeqCst), 5);
}

#[test]
fn test_runtime_nested_spawns() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = RuntimeHandle::new(&runtime);
    let counter = Arc::new(AtomicUsize::new(0));
    
    // Spawn a task that spawns more tasks
    let counter_clone = counter.clone();
    let handle_clone = handle.clone();
    
    let outer_task = handle.spawn(async move {
        let mut inner_tasks = vec![];
        
        for _ in 0..3 {
            let counter_clone2 = counter_clone.clone();
            let inner = handle_clone.spawn(async move {
                counter_clone2.fetch_add(1, Ordering::SeqCst);
            });
            inner_tasks.push(inner);
        }
        
        // Wait for inner tasks
        for inner in inner_tasks {
            inner.await.unwrap();
        }
        
        "outer done"
    });
    
    let result = runtime.block_on(outer_task);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "outer done");
    assert_eq!(counter.load(Ordering::SeqCst), 3);
}

#[test]
fn test_runtime_handle_drop_safety() {
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let counter = Arc::new(AtomicUsize::new(0));
    
    // Create and drop handles
    for _ in 0..10 {
        let handle = RuntimeHandle::new(&runtime);
        let counter_clone = counter.clone();
        
        // Spawn a task
        let jh = handle.spawn(async move {
            sleep(Duration::from_millis(1)).await;
            counter_clone.fetch_add(1, Ordering::SeqCst);
        });
        
        // Drop handle immediately
        drop(handle);
        
        // Task should still complete
        runtime.block_on(jh).unwrap();
    }
    
    assert_eq!(counter.load(Ordering::SeqCst), 10);
}

#[test]
fn test_runtime_error_scenarios() {
    let runtime = EffectRuntime::new().unwrap();
    
    // Test spawning a future that returns an error
    let result = runtime.block_on(async {
        Result::<i32, String>::Err("Test error".to_string())
    });
    
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Test error");
    
    // Runtime should still work
    let result = runtime.block_on(async { Ok::<i32, String>(42) });
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 42);
}

#[test] 
fn test_runtime_handle_multiple_enters() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = RuntimeHandle::new(&runtime);
    
    // Test nested enters (should work)
    let _guard1 = handle.enter();
    {
        let _guard2 = handle.enter();
        // Should be able to use runtime context here
        let result = tokio::task::block_in_place(|| {
            "nested"
        });
        assert_eq!(result, "nested");
    }
    // Still in outer enter
    let result = tokio::task::block_in_place(|| {
        "outer"
    });
    assert_eq!(result, "outer");
}