//! Memory pool implementation for efficient allocation

use anyhow::{anyhow, Result};
use parking_lot::Mutex;
use std::cell::RefCell;
use std::sync::Arc;

/// Configuration for memory pools
#[derive(Debug, Clone)]
pub struct PoolConfig {
    /// Size of each slab in the pool
    pub slab_size: usize,
    /// Number of slabs to pre-allocate
    pub initial_slabs: usize,
    /// Maximum number of slabs
    pub max_slabs: usize,
    /// Enable statistics tracking
    pub track_stats: bool,
}

impl Default for PoolConfig {
    fn default() -> Self {
        Self {
            slab_size: 4096,   // 4KB slabs
            initial_slabs: 16, // 64KB initial allocation
            max_slabs: 1024,   // 4MB maximum
            track_stats: false,
        }
    }
}

/// Statistics for pool usage
#[derive(Debug, Default, Clone)]
pub struct PoolStats {
    pub allocations: u64,
    pub deallocations: u64,
    pub current_used: usize,
    pub peak_used: usize,
    pub failed_allocations: u64,
}

/// A slab of memory
struct Slab {
    data: Vec<u8>,
    used: usize,
}

impl Slab {
    fn new(size: usize) -> Self {
        Self {
            data: vec![0; size],
            used: 0,
        }
    }

    /// Try to allocate from this slab
    fn allocate(&mut self, size: usize, align: usize) -> Option<*mut u8> {
        // Align the current position
        let aligned_pos = (self.used + align - 1) & !(align - 1);
        let end_pos = aligned_pos + size;

        if end_pos <= self.data.len() {
            self.used = end_pos;
            Some(unsafe { self.data.as_mut_ptr().add(aligned_pos) })
        } else {
            None
        }
    }

    /// Reset the slab for reuse
    fn reset(&mut self) {
        self.used = 0;
    }
}

/// Object pool for a specific type
pub struct ObjectPool<T> {
    free_list: Vec<Box<T>>,
    config: PoolConfig,
    stats: RefCell<PoolStats>,
    total_allocated: RefCell<usize>,
}

impl<T: Default> ObjectPool<T> {
    /// Create a new object pool
    pub fn new(config: PoolConfig) -> Self {
        let mut free_list = Vec::with_capacity(config.initial_slabs);
        let initial_slabs = config.initial_slabs;

        // Pre-allocate objects
        for _ in 0..initial_slabs {
            free_list.push(Box::new(T::default()));
        }

        Self {
            free_list,
            config,
            stats: RefCell::new(PoolStats::default()),
            total_allocated: RefCell::new(initial_slabs),
        }
    }

    /// Allocate an object from the pool
    pub fn allocate(&mut self) -> Result<Box<T>> {
        if let Some(obj) = self.free_list.pop() {
            if self.config.track_stats {
                let mut stats = self.stats.borrow_mut();
                stats.allocations += 1;
                stats.current_used += 1;
                if stats.current_used > stats.peak_used {
                    stats.peak_used = stats.current_used;
                }
            }
            Ok(obj)
        } else if *self.total_allocated.borrow() < self.config.max_slabs {
            // Allocate new object
            *self.total_allocated.borrow_mut() += 1;
            if self.config.track_stats {
                let mut stats = self.stats.borrow_mut();
                stats.allocations += 1;
                stats.current_used += 1;
                if stats.current_used > stats.peak_used {
                    stats.peak_used = stats.current_used;
                }
            }
            Ok(Box::new(T::default()))
        } else {
            if self.config.track_stats {
                self.stats.borrow_mut().failed_allocations += 1;
            }
            Err(anyhow!("Object pool exhausted"))
        }
    }

    /// Return an object to the pool
    pub fn deallocate(&mut self, mut obj: Box<T>) {
        if self.free_list.len() < self.config.max_slabs {
            // Reset object to default state
            *obj = T::default();
            self.free_list.push(obj);

            if self.config.track_stats {
                let mut stats = self.stats.borrow_mut();
                stats.deallocations += 1;
                stats.current_used = stats.current_used.saturating_sub(1);
            }
        }
        // If pool is full, just drop the object
    }

    /// Get current statistics
    pub fn stats(&self) -> PoolStats {
        self.stats.borrow().clone()
    }
}

/// Slab allocator for fixed-size allocations
pub struct SlabAllocator {
    slabs: Vec<Slab>,
    current_slab: usize,
    config: PoolConfig,
    stats: RefCell<PoolStats>,
}

impl SlabAllocator {
    /// Create a new slab allocator
    pub fn new(config: PoolConfig) -> Self {
        let mut slabs = Vec::with_capacity(config.initial_slabs);

        // Pre-allocate initial slabs
        for _ in 0..config.initial_slabs {
            slabs.push(Slab::new(config.slab_size));
        }

        Self {
            slabs,
            current_slab: 0,
            config,
            stats: RefCell::new(PoolStats::default()),
        }
    }

    /// Allocate memory of given size and alignment
    pub fn allocate(&mut self, size: usize, align: usize) -> Result<*mut u8> {
        if size > self.config.slab_size {
            return Err(anyhow!(
                "Allocation size {} exceeds slab size {}",
                size,
                self.config.slab_size
            ));
        }

        // Try current slab first
        if let Some(ptr) = self.slabs[self.current_slab].allocate(size, align) {
            if self.config.track_stats {
                let mut stats = self.stats.borrow_mut();
                stats.allocations += 1;
                stats.current_used += size;
                if stats.current_used > stats.peak_used {
                    stats.peak_used = stats.current_used;
                }
            }
            return Ok(ptr);
        }

        // Try other slabs
        for i in 0..self.slabs.len() {
            if i != self.current_slab {
                if let Some(ptr) = self.slabs[i].allocate(size, align) {
                    self.current_slab = i;
                    if self.config.track_stats {
                        let mut stats = self.stats.borrow_mut();
                        stats.allocations += 1;
                        stats.current_used += size;
                        if stats.current_used > stats.peak_used {
                            stats.peak_used = stats.current_used;
                        }
                    }
                    return Ok(ptr);
                }
            }
        }

        // Allocate new slab if possible
        if self.slabs.len() < self.config.max_slabs {
            self.slabs.push(Slab::new(self.config.slab_size));
            self.current_slab = self.slabs.len() - 1;

            if let Some(ptr) = self.slabs[self.current_slab].allocate(size, align) {
                if self.config.track_stats {
                    let mut stats = self.stats.borrow_mut();
                    stats.allocations += 1;
                    stats.current_used += size;
                    if stats.current_used > stats.peak_used {
                        stats.peak_used = stats.current_used;
                    }
                }
                return Ok(ptr);
            }
        }

        if self.config.track_stats {
            self.stats.borrow_mut().failed_allocations += 1;
        }
        Err(anyhow!("Slab allocator exhausted"))
    }

    /// Reset all slabs (deallocate everything)
    pub fn reset(&mut self) {
        for slab in &mut self.slabs {
            slab.reset();
        }
        self.current_slab = 0;

        if self.config.track_stats {
            self.stats.borrow_mut().current_used = 0;
        }
    }

    /// Get current statistics
    pub fn stats(&self) -> PoolStats {
        self.stats.borrow().clone()
    }
}

/// Thread-safe memory pool
pub struct MemoryPool {
    slab_allocator: Arc<Mutex<SlabAllocator>>,
    // Specialized pools for common types
    string_pool: Arc<Mutex<ObjectPool<String>>>,
    vec_pool: Arc<Mutex<ObjectPool<Vec<u8>>>>,
}

impl MemoryPool {
    /// Create a new memory pool
    pub fn new(config: PoolConfig) -> Self {
        Self {
            slab_allocator: Arc::new(Mutex::new(SlabAllocator::new(config.clone()))),
            string_pool: Arc::new(Mutex::new(ObjectPool::new(config.clone()))),
            vec_pool: Arc::new(Mutex::new(ObjectPool::new(config))),
        }
    }

    /// Allocate raw memory
    pub fn allocate_raw(&self, size: usize, align: usize) -> Result<*mut u8> {
        self.slab_allocator.lock().allocate(size, align)
    }

    /// Allocate a string
    pub fn allocate_string(&self) -> Result<Box<String>> {
        self.string_pool.lock().allocate()
    }

    /// Deallocate a string
    pub fn deallocate_string(&self, s: Box<String>) {
        self.string_pool.lock().deallocate(s)
    }

    /// Allocate a vector
    pub fn allocate_vec(&self) -> Result<Box<Vec<u8>>> {
        self.vec_pool.lock().allocate()
    }

    /// Deallocate a vector
    pub fn deallocate_vec(&self, v: Box<Vec<u8>>) {
        self.vec_pool.lock().deallocate(v)
    }

    /// Reset all allocations
    pub fn reset(&self) {
        self.slab_allocator.lock().reset();
    }

    /// Get combined statistics
    pub fn stats(&self) -> PoolStats {
        let slab_stats = self.slab_allocator.lock().stats();
        let string_stats = self.string_pool.lock().stats();
        let vec_stats = self.vec_pool.lock().stats();

        PoolStats {
            allocations: slab_stats.allocations + string_stats.allocations + vec_stats.allocations,
            deallocations: slab_stats.deallocations
                + string_stats.deallocations
                + vec_stats.deallocations,
            current_used: slab_stats.current_used
                + string_stats.current_used
                + vec_stats.current_used,
            peak_used: slab_stats
                .peak_used
                .max(string_stats.peak_used)
                .max(vec_stats.peak_used),
            failed_allocations: slab_stats.failed_allocations
                + string_stats.failed_allocations
                + vec_stats.failed_allocations,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_object_pool() {
        let config = PoolConfig {
            initial_slabs: 2,
            max_slabs: 5,
            track_stats: true,
            ..Default::default()
        };

        let mut pool: ObjectPool<Vec<i32>> = ObjectPool::new(config);

        // Allocate objects
        let mut objects = vec![];
        for i in 0..3 {
            let mut obj = pool.allocate().unwrap();
            obj.push(i);
            objects.push(obj);
        }

        // Check stats
        let stats = pool.stats();
        assert_eq!(stats.allocations, 3);
        assert_eq!(stats.current_used, 3);

        // Return objects
        for obj in objects {
            pool.deallocate(obj);
        }

        let stats = pool.stats();
        assert_eq!(stats.deallocations, 3);
        assert_eq!(stats.current_used, 0);
    }

    #[test]
    fn test_slab_allocator() {
        let config = PoolConfig {
            slab_size: 1024,
            initial_slabs: 1,
            max_slabs: 2,
            track_stats: true,
        };

        let mut allocator = SlabAllocator::new(config);

        // Allocate some memory
        let ptr1 = allocator.allocate(100, 8).unwrap();
        let ptr2 = allocator.allocate(200, 8).unwrap();

        // Pointers should be different
        assert_ne!(ptr1, ptr2);

        let stats = allocator.stats();
        assert_eq!(stats.allocations, 2);
        assert_eq!(stats.current_used, 300);

        // Reset and check
        allocator.reset();
        let stats = allocator.stats();
        assert_eq!(stats.current_used, 0);
    }
}
