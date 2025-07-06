#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_pool::*;
    use std::sync::{Arc, Mutex};
    use std::thread;
    
    #[test]
    fn test_pool_config_default() {
        let config = PoolConfig::default();
        assert_eq!(config.slab_size, 4096);
        assert_eq!(config.initial_slabs, 16);
        assert_eq!(config.max_slabs, 1024);
        assert!(!config.track_stats);
    }
    
    #[test]
    fn test_pool_config_custom() {
        let config = PoolConfig {
            slab_size: 8192,
            initial_slabs: 32,
            max_slabs: 2048,
            track_stats: true,
        };
        assert_eq!(config.slab_size, 8192);
        assert_eq!(config.initial_slabs, 32);
        assert_eq!(config.max_slabs, 2048);
        assert!(config.track_stats);
    }
    
    #[test]
    fn test_pool_stats_default() {
        let stats = PoolStats::default();
        assert_eq!(stats.allocations, 0);
        assert_eq!(stats.deallocations, 0);
        assert_eq!(stats.current_used, 0);
        assert_eq!(stats.peak_used, 0);
        assert_eq!(stats.failed_allocations, 0);
    }
    
    
    #[test]
    fn test_object_pool_new() {
        #[derive(Default, Debug, PartialEq)]
        struct TestObject {
            value: i32,
        }
        
        let config = PoolConfig {
            initial_slabs: 5,
            ..Default::default()
        };
        
        let _pool = ObjectPool::<TestObject>::new(config);
        // Pool is created with initial objects
    }
    
    #[test]
    fn test_object_pool_allocate_deallocate() {
        #[derive(Default, Debug)]
        struct TestObject {
            value: i32,
        }
        
        let config = PoolConfig {
            initial_slabs: 2,
            track_stats: true,
            ..Default::default()
        };
        
        let mut pool = ObjectPool::<TestObject>::new(config);
        
        // Allocate an object
        let mut obj = pool.allocate().unwrap();
        obj.value = 42;
        assert_eq!(pool.free_list.len(), 1);
        
        // Deallocate the object
        pool.deallocate(obj);
        assert_eq!(pool.free_list.len(), 2);
        
        // Check stats
        let stats = pool.stats();
        assert_eq!(stats.allocations, 1);
        assert_eq!(stats.deallocations, 1);
    }
    
    #[test]
    fn test_object_pool_grow() {
        #[derive(Default)]
        struct TestObject;
        
        let config = PoolConfig {
            initial_slabs: 1,
            max_slabs: 5,
            track_stats: true,
            ..Default::default()
        };
        
        let mut pool = ObjectPool::<TestObject>::new(config);
        
        // Allocate all initial objects
        let obj1 = pool.allocate().unwrap();
        
        // Pool should grow when allocating more
        let obj2 = pool.allocate().unwrap();
        
        // Return objects
        pool.deallocate(obj1);
        pool.deallocate(obj2);
        
        // Pool should have grown
        assert!(pool.free_list.len() > 1);
    }
    
    #[test]
    fn test_object_pool_max_limit() {
        #[derive(Default)]
        struct TestObject;
        
        let config = PoolConfig {
            initial_slabs: 1,
            max_slabs: 2,
            track_stats: true,
            ..Default::default()
        };
        
        let mut pool = ObjectPool::<TestObject>::new(config);
        
        // Allocate maximum objects
        let obj1 = pool.allocate().unwrap();
        let obj2 = pool.allocate().unwrap();
        
        // Should fail to allocate more
        let obj3 = pool.allocate();
        assert!(obj3.is_err());
        
        // Check stats
        let stats = pool.stats();
        assert_eq!(stats.failed_allocations, 1);
        
        // Return one object and try again
        pool.deallocate(obj1);
        let obj3 = pool.allocate();
        assert!(obj3.is_ok());
    }
    
    #[test]
    fn test_slab_allocator_new() {
        let config = PoolConfig::default();
        let allocator = SlabAllocator::new(config);
        assert_eq!(allocator.slabs.len(), allocator.config.initial_slabs);
    }
    
    #[test]
    fn test_slab_allocator_allocate() {
        let config = PoolConfig {
            slab_size: 1024,
            initial_slabs: 2,
            track_stats: true,
            ..Default::default()
        };
        
        let mut allocator = SlabAllocator::new(config);
        
        // Allocate some memory
        let ptr1 = allocator.allocate(100, 8);
        assert!(ptr1.is_ok());
        
        let ptr2 = allocator.allocate(200, 8);
        assert!(ptr2.is_ok());
        
        // Check stats
        let stats = allocator.stats();
        assert_eq!(stats.allocations, 2);
        assert!(stats.current_used > 0);
    }
    
    #[test]
    fn test_slab_allocator_reset() {
        let config = PoolConfig {
            slab_size: 1024,
            initial_slabs: 1,
            track_stats: true,
            ..Default::default()
        };
        
        let mut allocator = SlabAllocator::new(config);
        
        // Allocate memory
        allocator.allocate(100, 8);
        allocator.allocate(200, 8);
        
        // Reset allocator
        allocator.reset();
        
        // Stats should show reset
        let stats = allocator.stats();
        assert_eq!(stats.current_used, 0);
        
        // Should be able to allocate again
        let ptr = allocator.allocate(100, 8);
        assert!(ptr.is_ok());
    }
    
    #[test]
    fn test_slab_allocator_thread_safety() {
        let config = PoolConfig {
            slab_size: 4096,
            initial_slabs: 10,
            max_slabs: 20,
            track_stats: true,
            ..Default::default()
        };
        
        let allocator = Arc::new(Mutex::new(SlabAllocator::new(config)));
        let mut handles = vec![];
        
        // Spawn multiple threads to allocate concurrently
        for _ in 0..4 {
            let alloc = Arc::clone(&allocator);
            let handle = thread::spawn(move || {
                for _ in 0..100 {
                    let _ptr = alloc.lock().allocate(64, 8);
                    thread::yield_now();
                }
            });
            handles.push(handle);
        }
        
        // Wait for all threads
        for handle in handles {
            handle.join().unwrap();
        }
        
        // Check stats
        let stats = allocator.lock().stats();
        assert!(stats.allocations > 0);
    }
    
    #[test]
    fn test_memory_pool_new() {
        let config = PoolConfig::default();
        let pool = MemoryPool::new(config);
        // Pool is created directly, not wrapped in Result
        assert!(pool.slab_allocator.lock().slabs.len() > 0);
    }
    
    #[test]
    fn test_memory_pool_allocate_deallocate() {
        let config = PoolConfig {
            track_stats: true,
            ..Default::default()
        };
        
        let pool = MemoryPool::new(config);
        
        // Allocate memory
        let ptr = pool.allocate_raw(256, 8).unwrap();
        assert!(!ptr.is_null());
        
        // Test string pool allocation/deallocation
        let string = pool.allocate_string().unwrap();
        pool.deallocate_string(string);
        
        // Check stats
        let stats = pool.stats();
        assert!(stats.allocations >= 1);
        assert!(stats.deallocations >= 1);
    }
    
    #[test]
    fn test_memory_pool_allocate_aligned() {
        let pool = MemoryPool::new(PoolConfig::default());
        
        // Test various alignments
        let ptr1 = pool.allocate_raw(100, 8).unwrap();
        assert_eq!(ptr1 as usize % 8, 0);
        
        let ptr2 = pool.allocate_raw(100, 16).unwrap();
        assert_eq!(ptr2 as usize % 16, 0);
        
        let ptr3 = pool.allocate_raw(100, 32).unwrap();
        assert_eq!(ptr3 as usize % 32, 0);
    }
    
    #[test]
    fn test_memory_pool_stats_tracking() {
        let config = PoolConfig {
            track_stats: true,
            ..Default::default()
        };
        
        let pool = MemoryPool::new(config);
        
        // Perform allocations
        let _ptr1 = pool.allocate_raw(100, 8).unwrap();
        let _ptr2 = pool.allocate_raw(200, 8).unwrap();
        
        let stats = pool.stats();
        assert!(stats.allocations >= 2);
        assert!(stats.current_used >= 300);
        assert!(stats.peak_used >= 300);
        
        // Test vector pool
        let vec = pool.allocate_vec().unwrap();
        pool.deallocate_vec(vec);
        
        let stats = pool.stats();
        assert!(stats.deallocations >= 1);
    }
    
    #[test]
    fn test_pool_config_clone() {
        let config1 = PoolConfig {
            slab_size: 8192,
            initial_slabs: 32,
            max_slabs: 2048,
            track_stats: true,
        };
        
        let config2 = config1.clone();
        assert_eq!(config1.slab_size, config2.slab_size);
        assert_eq!(config1.initial_slabs, config2.initial_slabs);
        assert_eq!(config1.max_slabs, config2.max_slabs);
        assert_eq!(config1.track_stats, config2.track_stats);
    }
    
    #[test]
    fn test_pool_stats_clone() {
        let stats1 = PoolStats {
            allocations: 100,
            deallocations: 50,
            current_used: 1024,
            peak_used: 2048,
            failed_allocations: 5,
        };
        
        let stats2 = stats1.clone();
        assert_eq!(stats1.allocations, stats2.allocations);
        assert_eq!(stats1.deallocations, stats2.deallocations);
        assert_eq!(stats1.current_used, stats2.current_used);
        assert_eq!(stats1.peak_used, stats2.peak_used);
        assert_eq!(stats1.failed_allocations, stats2.failed_allocations);
    }
}