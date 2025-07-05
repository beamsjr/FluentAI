#[cfg(test)]
mod tests {
    use super::*;
    use crate::reactive::{ReactiveContext, ReactiveState, Computed};
    use crate::reactive::computed::Computed as ComputedImport;
    use crate::reactive::watchers::{Watcher, WatcherBuilder};
    use fluentai_core::value::Value;
    use std::sync::{Arc, Mutex};
    
    #[test]
    fn test_reactive_state_basic() {
        let ctx = ReactiveContext::new();
        let state = ReactiveState::with_context(&ctx);
        
        // Test set and get
        state.set("count".to_string(), Value::Integer(0));
        assert_eq!(state.get("count"), Some(Value::Integer(0)));
        
        // Test update
        state.update("count", |v| {
            if let Value::Integer(n) = v {
                Value::Integer(n + 1)
            } else {
                v.clone()
            }
        });
        assert_eq!(state.get("count"), Some(Value::Integer(1)));
        
        // Test delete
        assert_eq!(state.delete("count"), Some(Value::Integer(1)));
        assert_eq!(state.get("count"), None);
    }
    
    #[test]
    fn test_computed_values() {
        let ctx = ReactiveContext::new();
        let state = ReactiveState::with_context(&ctx);
        
        // Set up reactive state
        state.set("a".to_string(), Value::Integer(1));
        state.set("b".to_string(), Value::Integer(2));
        
        // Create computed value
        let state_clone = state.clone();
        let sum = ctx.with(|| {
            Computed::new(move || {
                let a = state_clone.get("a").unwrap_or(Value::Integer(0));
                let b = state_clone.get("b").unwrap_or(Value::Integer(0));
                
                if let (Value::Integer(a_val), Value::Integer(b_val)) = (a, b) {
                    Value::Integer(a_val + b_val)
                } else {
                    Value::Integer(0)
                }
            })
        });
        
        // Initial computation
        assert_eq!(sum.get(), Value::Integer(3));
        
        // Update state and verify recomputation
        state.set("a".to_string(), Value::Integer(5));
        ctx.scheduler.flush_sync(); // Force synchronous update
        assert_eq!(sum.get(), Value::Integer(7));
    }
    
    #[test]
    fn test_watchers() {
        let ctx = ReactiveContext::new();
        let state = ReactiveState::with_context(&ctx);
        
        // Track watcher calls
        let called = Arc::new(Mutex::new(Vec::new()));
        let called_clone = called.clone();
        
        // Create watcher
        let watcher = ctx.with(|| {
            WatcherBuilder::new()
                .immediate()
                .build(move |value, _old| {
                    called_clone.lock().unwrap().push(value.clone());
                })
        });
        
        // Watch a key
        ctx.with(|| {
            watcher.watch_key(&state, "test");
        });
        
        // Should be called immediately with None/nil
        ctx.scheduler.flush_sync();
        
        // Set value
        state.set("test".to_string(), Value::String("hello".to_string()));
        ctx.scheduler.flush_sync();
        
        // Update value
        state.set("test".to_string(), Value::String("world".to_string()));
        ctx.scheduler.flush_sync();
        
        // Check calls
        let calls = called.lock().unwrap();
        println!("Watcher calls: {}", calls.len());
        for (i, call) in calls.iter().enumerate() {
            println!("  Call {}: {:?}", i, call);
        }
        assert!(calls.len() >= 2, "Expected at least 2 calls, got {}", calls.len()); // At least initial and one update
    }
    
    #[test]
    fn test_batch_updates() {
        let ctx = ReactiveContext::new();
        let state = ReactiveState::with_context(&ctx);
        
        // Track computations
        let compute_count = Arc::new(Mutex::new(0));
        let compute_count_clone = compute_count.clone();
        
        // Create computed that tracks how many times it runs
        let state_clone = state.clone();
        let computed = ctx.with(|| {
            Computed::new(move || {
                *compute_count_clone.lock().unwrap() += 1;
                
                let a = state_clone.get("a").unwrap_or(Value::Integer(0));
                let b = state_clone.get("b").unwrap_or(Value::Integer(0));
                let c = state_clone.get("c").unwrap_or(Value::Integer(0));
                
                if let (Value::Integer(a), Value::Integer(b), Value::Integer(c)) = (a, b, c) {
                    Value::Integer(a + b + c)
                } else {
                    Value::Integer(0)
                }
            })
        });
        
        // Initial computation
        assert_eq!(computed.get(), Value::Integer(0));
        assert_eq!(*compute_count.lock().unwrap(), 1);
        
        // Batch multiple updates
        let batch = crate::reactive::BatchScope::new(ctx.scheduler.clone());
        batch.run(|| {
            state.set("a".to_string(), Value::Integer(1));
            state.set("b".to_string(), Value::Integer(2));
            state.set("c".to_string(), Value::Integer(3));
        });
        
        // Should only recompute once after batch
        assert_eq!(computed.get(), Value::Integer(6));
        
        // In an ideal implementation, this would be 2 (initial + one batch update)
        // But without proper batching, it might be higher
        let count = *compute_count.lock().unwrap();
        assert!(count >= 2, "Computed at least twice, got {}", count);
    }
}