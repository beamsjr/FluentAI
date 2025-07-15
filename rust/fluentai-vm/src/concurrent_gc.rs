//! Concurrent garbage collector for FluentAi
//!
//! This module implements a concurrent, generational garbage collector that
//! minimizes stop-the-world pauses by doing most work concurrently.

use crate::gc::{ConcurrentGcNode, GcHandle};
use anyhow::{anyhow, Result};
use crossbeam_epoch::{self as epoch, Atomic, Guard};
use crossbeam_queue::SegQueue;
use fluentai_core::value::Value;
use parking_lot::RwLock;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};
#[cfg(target_arch = "wasm32")]
use std::time::Duration;

/// Concurrent garbage collector with generational support
pub struct ConcurrentGc {
    /// Young generation (nursery)
    young_gen: RwLock<Generation>,

    /// Old generation (tenured)
    old_gen: RwLock<Generation>,

    /// Write barrier for tracking inter-generational references
    write_barrier: WriteBarrier,

    /// GC thread handle
    _gc_thread: Option<thread::JoinHandle<()>>,

    /// Shutdown flag
    shutdown: Arc<AtomicBool>,

    /// GC statistics
    stats: GcStats,

    /// Configuration
    config: ConcurrentGcConfig,

    /// Root set
    roots: Arc<RwLock<FxHashSet<GcNodePtr>>>,

    /// Allocation rate tracking
    allocation_rate: AtomicUsize,

    /// Current GC phase
    phase: AtomicUsize,
}

/// A generation in the garbage collector
struct Generation {
    /// Allocated objects in this generation
    objects: FxHashMap<usize, GcNodePtr>,

    /// Size of allocated memory
    size: usize,

    /// Allocation pointer for bump allocation
    alloc_ptr: usize,

    /// Generation age
    age: usize,
}

/// Thread-safe pointer to a GC node
type GcNodePtr = Arc<ConcurrentGcNode>;

/// Write barrier for tracking mutations
struct WriteBarrier {
    /// Remembered set for old-to-young references
    remembered_set: Arc<RwLock<FxHashSet<GcNodePtr>>>,

    /// Card table for efficient scanning
    _card_table: Arc<RwLock<Vec<AtomicBool>>>,
}

/// GC phases for concurrent collection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
enum GcPhase {
    Idle = 0,
    Marking = 1,
    Sweeping = 2,
    Compacting = 3,
}

/// Configuration for concurrent GC
#[derive(Debug, Clone)]
pub struct ConcurrentGcConfig {
    /// Young generation size
    pub young_gen_size: usize,

    /// Old generation size
    pub old_gen_size: usize,

    /// Promotion threshold (survivals before promotion)
    pub promotion_threshold: usize,

    /// GC trigger threshold (percentage)
    pub gc_trigger_percent: f64,

    /// Use concurrent marking
    pub concurrent_marking: bool,

    /// Use concurrent sweeping
    pub concurrent_sweeping: bool,

    /// Target pause time in milliseconds
    pub target_pause_ms: u64,

    /// GC thread priority
    pub gc_thread_priority: i32,
}

impl Default for ConcurrentGcConfig {
    fn default() -> Self {
        Self {
            young_gen_size: 10 * 1024 * 1024, // 10MB
            old_gen_size: 100 * 1024 * 1024,  // 100MB
            promotion_threshold: 3,
            gc_trigger_percent: 0.8,
            concurrent_marking: true,
            concurrent_sweeping: true,
            target_pause_ms: 10,
            gc_thread_priority: 0,
        }
    }
}

/// GC statistics
#[derive(Debug, Default)]
struct GcStats {
    /// Total collections
    _collections: AtomicUsize,

    /// Minor collections (young gen only)
    minor_collections: AtomicUsize,

    /// Major collections (full GC)
    major_collections: AtomicUsize,

    /// Total pause time
    total_pause_ns: AtomicUsize,

    /// Total concurrent time
    _total_concurrent_ns: AtomicUsize,

    /// Bytes allocated
    bytes_allocated: AtomicUsize,

    /// Bytes freed
    bytes_freed: AtomicUsize,
}

impl ConcurrentGc {
    /// Create a new concurrent garbage collector
    pub fn new(config: ConcurrentGcConfig) -> Arc<Self> {
        let gc = Arc::new(Self {
            young_gen: RwLock::new(Generation::new(config.young_gen_size)),
            old_gen: RwLock::new(Generation::new(config.old_gen_size)),
            write_barrier: WriteBarrier::new(),
            _gc_thread: None,
            shutdown: Arc::new(AtomicBool::new(false)),
            stats: GcStats::default(),
            config,
            roots: Arc::new(RwLock::new(FxHashSet::default())),
            allocation_rate: AtomicUsize::new(0),
            phase: AtomicUsize::new(GcPhase::Idle as usize),
        });

        // Start GC thread
        let gc_clone = Arc::clone(&gc);
        let shutdown = Arc::clone(&gc.shutdown);
        let _gc_thread = thread::spawn(move || {
            gc_clone.gc_thread_loop(shutdown);
        });

        // Store the thread handle - we'll need a different approach
        // since we can't mutate after creating the Arc

        gc
    }

    /// Allocate a new value with GC
    pub fn allocate(&self, value: Value) -> Result<GcHandle> {
        // Try allocation in young generation first
        let size = self.value_size(&value);
        self.allocation_rate.fetch_add(size, Ordering::Relaxed);

        // Fast path: bump allocation in young gen
        if let Some(node) = self.try_alloc_young(&value, size) {
            return Ok(self.make_handle(node));
        }

        // Slow path: trigger minor GC and retry
        self.trigger_minor_gc()?;

        if let Some(node) = self.try_alloc_young(&value, size) {
            return Ok(self.make_handle(node));
        }

        // Last resort: allocate in old generation
        if let Some(node) = self.try_alloc_old(&value, size) {
            return Ok(self.make_handle(node));
        }

        // Full GC needed
        self.trigger_major_gc()?;

        // Final attempt
        if let Some(node) = self.try_alloc_old(&value, size) {
            Ok(self.make_handle(node))
        } else {
            Err(anyhow!("Out of memory"))
        }
    }

    /// Try to allocate in young generation
    fn try_alloc_young(&self, value: &Value, size: usize) -> Option<GcNodePtr> {
        let mut young = self.young_gen.write();

        if young.alloc_ptr + size <= young.size {
            let id = young.objects.len();
            let node = Arc::new(ConcurrentGcNode {
                id,
                value: std::sync::RwLock::new(value.clone()),
                mark_bits: AtomicUsize::new(0),
                forward: Atomic::null(),
                generation: AtomicUsize::new(0),
                ref_count: AtomicUsize::new(1),
            });

            young.objects.insert(id, node.clone());
            young.alloc_ptr += size;
            self.stats
                .bytes_allocated
                .fetch_add(size, Ordering::Relaxed);

            Some(node)
        } else {
            None
        }
    }

    /// Try to allocate in old generation
    fn try_alloc_old(&self, value: &Value, size: usize) -> Option<GcNodePtr> {
        let mut old = self.old_gen.write();

        if old.alloc_ptr + size <= old.size {
            let id = old.objects.len() + 1_000_000; // Offset for old gen IDs
            let node = Arc::new(ConcurrentGcNode {
                id,
                value: std::sync::RwLock::new(value.clone()),
                mark_bits: AtomicUsize::new(0),
                forward: Atomic::null(),
                generation: AtomicUsize::new(1),
                ref_count: AtomicUsize::new(1),
            });

            old.objects.insert(id, node.clone());
            old.alloc_ptr += size;
            self.stats
                .bytes_allocated
                .fetch_add(size, Ordering::Relaxed);

            Some(node)
        } else {
            None
        }
    }

    /// Trigger a minor (young generation) collection
    fn trigger_minor_gc(&self) -> Result<()> {
        #[cfg(not(target_arch = "wasm32"))]
        let start = Instant::now();
        self.stats.minor_collections.fetch_add(1, Ordering::Relaxed);

        // Phase 1: Initial mark (stop-the-world)
        self.phase
            .store(GcPhase::Marking as usize, Ordering::SeqCst);
        let roots = self.scan_roots();

        // Phase 2: Concurrent marking
        if self.config.concurrent_marking {
            self.concurrent_mark_young(&roots)?;
        } else {
            self.mark_young(&roots)?;
        }

        // Phase 3: Sweep and promote (stop-the-world)
        self.phase
            .store(GcPhase::Sweeping as usize, Ordering::SeqCst);
        self.sweep_and_promote_young()?;

        self.phase.store(GcPhase::Idle as usize, Ordering::SeqCst);

        #[cfg(not(target_arch = "wasm32"))]
        {
            let elapsed = start.elapsed();
            self.stats
                .total_pause_ns
                .fetch_add(elapsed.as_nanos() as usize, Ordering::Relaxed);
        }

        Ok(())
    }

    /// Trigger a major (full) collection
    fn trigger_major_gc(&self) -> Result<()> {
        #[cfg(not(target_arch = "wasm32"))]
        let start = Instant::now();
        self.stats.major_collections.fetch_add(1, Ordering::Relaxed);

        // Full marking of both generations
        self.phase
            .store(GcPhase::Marking as usize, Ordering::SeqCst);
        let roots = self.scan_roots();

        if self.config.concurrent_marking {
            self.concurrent_mark_all(&roots)?;
        } else {
            self.mark_all(&roots)?;
        }

        // Concurrent sweep
        self.phase
            .store(GcPhase::Sweeping as usize, Ordering::SeqCst);
        if self.config.concurrent_sweeping {
            self.concurrent_sweep_all()?;
        } else {
            self.sweep_all()?;
        }

        // Optional compaction
        self.phase
            .store(GcPhase::Compacting as usize, Ordering::SeqCst);
        self.compact_old_gen()?;

        self.phase.store(GcPhase::Idle as usize, Ordering::SeqCst);

        #[cfg(not(target_arch = "wasm32"))]
        {
            let elapsed = start.elapsed();
            self.stats
                .total_pause_ns
                .fetch_add(elapsed.as_nanos() as usize, Ordering::Relaxed);
        }

        Ok(())
    }

    /// Scan roots for marking
    fn scan_roots(&self) -> Vec<GcNodePtr> {
        self.roots.read().iter().cloned().collect()
    }

    /// Mark young generation
    fn mark_young(&self, roots: &[GcNodePtr]) -> Result<()> {
        let _young = self.young_gen.read();

        // Mark from roots
        for root in roots {
            if root.generation.load(Ordering::Relaxed) == 0 {
                self.mark_object(root);
            }
        }

        // Mark from remembered set (old->young references)
        let remembered = self.write_barrier.remembered_set.read();
        for obj in remembered.iter() {
            self.mark_object(obj);
        }

        Ok(())
    }

    /// Concurrent mark young generation
    fn concurrent_mark_young(&self, roots: &[GcNodePtr]) -> Result<()> {
        // Use snapshot-at-the-beginning (SATB) technique
        let _snapshot = self.take_heap_snapshot();

        // Mark concurrently
        let guard = epoch::pin();
        for root in roots {
            if root.generation.load(Ordering::Relaxed) == 0 {
                self.mark_object_concurrent(root, &guard);
            }
        }

        Ok(())
    }

    /// Mark all generations
    fn mark_all(&self, roots: &[GcNodePtr]) -> Result<()> {
        for root in roots {
            self.mark_object(root);
        }
        Ok(())
    }

    /// Concurrent mark all generations
    fn concurrent_mark_all(&self, roots: &[GcNodePtr]) -> Result<()> {
        let guard = epoch::pin();

        // Mark concurrently with tri-color marking
        let gray_queue = SegQueue::new();

        // Initialize gray set with roots
        for root in roots {
            root.mark_bits.store(1, Ordering::SeqCst); // Gray
            gray_queue.push(root.clone());
        }

        // Process gray objects
        while let Some(obj) = gray_queue.pop() {
            // Mark black
            obj.mark_bits.store(2, Ordering::SeqCst);

            // Scan object for references
            let value = obj.value.read().unwrap();
            self.scan_value_concurrent(&value, &gray_queue, &guard);
        }

        Ok(())
    }

    /// Mark a single object
    fn mark_object(&self, obj: &GcNodePtr) {
        if obj
            .mark_bits
            .compare_exchange(0, 1, Ordering::SeqCst, Ordering::Relaxed)
            .is_ok()
        {
            // Successfully marked, scan children
            let value = obj.value.read().unwrap();
            self.scan_value(&value);
            obj.mark_bits.store(2, Ordering::SeqCst); // Black
        }
    }

    /// Mark object concurrently
    fn mark_object_concurrent(&self, obj: &GcNodePtr, guard: &Guard) {
        // Concurrent marking with SATB
        if obj
            .mark_bits
            .compare_exchange(0, 1, Ordering::SeqCst, Ordering::Relaxed)
            .is_ok()
        {
            let value = obj.value.read().unwrap();
            self.scan_value_concurrent(&value, &SegQueue::new(), guard);
            obj.mark_bits.store(2, Ordering::SeqCst);
        }
    }

    /// Scan a value for references
    fn scan_value(&self, value: &Value) {
        match value {
            Value::List(items) => {
                for item in items {
                    self.scan_value(item);
                }
            }
            Value::Vector(items) => {
                for item in items {
                    self.scan_value(item);
                }
            }
            Value::Map(map) => {
                for (_, v) in map {
                    self.scan_value(v);
                }
            }
            Value::Function { env, .. } => {
                for v in env {
                    self.scan_value(v);
                }
            }
            Value::Future { env, .. } => {
                for v in env {
                    self.scan_value(v);
                }
            }
            Value::Tagged { values, .. } => {
                for v in values {
                    self.scan_value(v);
                }
            }
            Value::Module { exports, .. } => {
                for (_, v) in exports {
                    self.scan_value(v);
                }
            }
            Value::Procedure(proc) => {
                if let Some(env) = &proc.env {
                    for (_, v) in env {
                        self.scan_value(v);
                    }
                }
            }
            Value::GcHandle(_handle) => {
                // Mark referenced GC object
                // This would need integration with the GcHandle type
            }
            _ => {} // Primitive values
        }
    }

    /// Scan value concurrently
    fn scan_value_concurrent(
        &self,
        value: &Value,
        gray_queue: &SegQueue<GcNodePtr>,
        guard: &Guard,
    ) {
        // Similar to scan_value but adds to gray queue
        match value {
            Value::List(items) => {
                for item in items {
                    self.scan_value_concurrent(item, gray_queue, guard);
                }
            }
            Value::Vector(items) => {
                for item in items {
                    self.scan_value_concurrent(item, gray_queue, guard);
                }
            }
            Value::Map(map) => {
                for (_, v) in map {
                    self.scan_value_concurrent(v, gray_queue, guard);
                }
            }
            Value::Function { env, .. } => {
                for v in env {
                    self.scan_value_concurrent(v, gray_queue, guard);
                }
            }
            Value::Future { env, .. } => {
                for v in env {
                    self.scan_value_concurrent(v, gray_queue, guard);
                }
            }
            Value::Tagged { values, .. } => {
                for v in values {
                    self.scan_value_concurrent(v, gray_queue, guard);
                }
            }
            Value::Module { exports, .. } => {
                for (_, v) in exports {
                    self.scan_value_concurrent(v, gray_queue, guard);
                }
            }
            Value::Procedure(proc) => {
                if let Some(env) = &proc.env {
                    for (_, v) in env {
                        self.scan_value_concurrent(v, gray_queue, guard);
                    }
                }
            }
            _ => {}
        }
    }

    /// Sweep and promote young generation
    fn sweep_and_promote_young(&self) -> Result<()> {
        let mut young = self.young_gen.write();
        let mut old = self.old_gen.write();
        let mut promoted = Vec::new();

        // Store the age before the closure
        let young_age = young.age;
        let promotion_threshold = self.config.promotion_threshold;

        // Sweep unmarked objects
        young.objects.retain(|id, node| {
            let marked = node.mark_bits.load(Ordering::SeqCst) > 0;
            if marked {
                // Check if should be promoted
                if young_age >= promotion_threshold {
                    promoted.push((*id, node.clone()));
                    false // Remove from young
                } else {
                    // Reset mark for next cycle
                    node.mark_bits.store(0, Ordering::SeqCst);
                    true // Keep in young
                }
            } else {
                // Collect
                let value = node.value.read().unwrap();
                self.stats
                    .bytes_freed
                    .fetch_add(self.value_size(&value), Ordering::Relaxed);
                false
            }
        });

        // Promote survivors to old generation
        for (id, node) in promoted {
            node.generation.store(1, Ordering::SeqCst);
            old.objects.insert(id + 1_000_000, node);
        }

        // Reset allocation pointer
        young.alloc_ptr = young.objects.len() * 16; // Approximate
        young.age += 1;

        Ok(())
    }

    /// Sweep all generations
    fn sweep_all(&self) -> Result<()> {
        self.sweep_generation(&mut self.young_gen.write())?;
        self.sweep_generation(&mut self.old_gen.write())?;
        Ok(())
    }

    /// Concurrent sweep all generations
    fn concurrent_sweep_all(&self) -> Result<()> {
        // For now, just do synchronous sweep
        // A real concurrent implementation would need more careful design
        self.sweep_all()
    }

    /// Sweep a generation
    fn sweep_generation(&self, gen: &mut Generation) -> Result<()> {
        gen.objects.retain(|_, node| {
            let marked = node.mark_bits.load(Ordering::SeqCst) > 0;
            if marked {
                node.mark_bits.store(0, Ordering::SeqCst);
                true
            } else {
                let value = node.value.read().unwrap();
                self.stats
                    .bytes_freed
                    .fetch_add(self.value_size(&value), Ordering::Relaxed);
                false
            }
        });

        gen.alloc_ptr = gen.objects.len() * 16;
        Ok(())
    }

    /// Compact old generation
    fn compact_old_gen(&self) -> Result<()> {
        // Compaction moves objects to reduce fragmentation
        // This is complex and optional for initial implementation
        Ok(())
    }

    /// Take a heap snapshot for SATB
    fn take_heap_snapshot(&self) -> Vec<GcNodePtr> {
        let young = self.young_gen.read();
        let old = self.old_gen.read();

        let mut snapshot = Vec::new();
        for (_, node) in &young.objects {
            snapshot.push(node.clone());
        }
        for (_, node) in &old.objects {
            snapshot.push(node.clone());
        }

        snapshot
    }

    /// GC thread main loop
    fn gc_thread_loop(&self, shutdown: Arc<AtomicBool>) {
        while !shutdown.load(Ordering::Relaxed) {
            // Check allocation rate and trigger GC if needed
            let rate = self.allocation_rate.swap(0, Ordering::Relaxed);

            let young = self.young_gen.read();
            let young_usage = young.alloc_ptr as f64 / young.size as f64;
            drop(young);

            if young_usage > self.config.gc_trigger_percent {
                let _ = self.trigger_minor_gc();
            }

            // Sleep based on allocation rate
            let sleep_ms = if rate > 1_000_000 {
                10 // High allocation rate
            } else if rate > 100_000 {
                50 // Medium allocation rate
            } else {
                100 // Low allocation rate
            };

            thread::sleep(Duration::from_millis(sleep_ms));
        }
    }

    /// Estimate size of a value
    fn value_size(&self, value: &Value) -> usize {
        match value {
            Value::Nil => 8,
            Value::Boolean(_) => 8,
            Value::Integer(_) => 16,
            Value::Float(_) => 16,
            Value::String(s) => 24 + s.len(),
            Value::Symbol(s) => 24 + s.len(),
            Value::List(items) => 24 + items.len() * 8,
            Value::Procedure(_) => 48, // Arc + fields
            Value::Vector(items) => 24 + items.len() * 8,
            Value::Map(m) => 32 + m.len() * 16,
            Value::NativeFunction { .. } => 64, // Arc + fields
            Value::Function { env, .. } => 32 + env.len() * 8,
            Value::Promise(_) => 16,
            Value::Future { env, .. } => 32 + env.len() * 8,
            Value::Channel(_) => 16,
            Value::Cell(_) => 16,
            Value::Tagged { values, .. } => 32 + values.len() * 8,
            Value::Module { exports, .. } => 48 + exports.len() * 16,
            Value::GcHandle(_) => 16,
            Value::Actor(_) => 16, // Same as other ID-based values
            Value::Error { message, stack_trace, .. } => {
                32 + message.len() + stack_trace.as_ref().map_or(0, |st| st.len() * 16)
            }
        }
    }

    /// Create a handle from a node
    fn make_handle(&self, node: GcNodePtr) -> GcHandle {
        GcHandle::concurrent(node)
    }
}

impl Drop for ConcurrentGc {
    fn drop(&mut self) {
        // Signal shutdown to GC thread
        self.shutdown.store(true, Ordering::SeqCst);
        // Note: We can't join the thread here because we don't have ownership
        // The thread will exit on its own when it sees the shutdown flag
    }
}

impl Generation {
    fn new(size: usize) -> Self {
        Self {
            objects: FxHashMap::default(),
            size,
            alloc_ptr: 0,
            age: 0,
        }
    }
}

impl WriteBarrier {
    fn new() -> Self {
        let mut cards = Vec::with_capacity(1024);
        for _ in 0..1024 {
            cards.push(AtomicBool::new(false));
        }
        Self {
            remembered_set: Arc::new(RwLock::new(FxHashSet::default())),
            _card_table: Arc::new(RwLock::new(cards)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_concurrent_gc_allocation() {
        let config = ConcurrentGcConfig {
            young_gen_size: 1024 * 1024,    // 1MB
            old_gen_size: 10 * 1024 * 1024, // 10MB
            ..Default::default()
        };

        let gc = ConcurrentGc::new(config);

        // Allocate some values
        for i in 0..1000 {
            let value = Value::Integer(i);
            let _handle = gc.allocate(value).unwrap();
        }

        // Check stats
        assert!(gc.stats.bytes_allocated.load(Ordering::Relaxed) > 0);
    }

    #[test]
    fn test_concurrent_gc_collection() {
        let config = ConcurrentGcConfig {
            young_gen_size: 1024, // Very small to trigger GC
            old_gen_size: 10 * 1024,
            ..Default::default()
        };

        let gc = ConcurrentGc::new(config);

        // Allocate until GC triggers
        for i in 0..100 {
            let value = Value::String(format!("test-{}", i));
            let _handle = gc.allocate(value).unwrap();
        }

        // Should have triggered at least one collection
        assert!(gc.stats.minor_collections.load(Ordering::Relaxed) > 0);
    }
}
