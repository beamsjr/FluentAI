//! Opt-in garbage collection for FluentAi
//!
//! This module provides an optional garbage collector that can be explicitly
//! used via special forms like (gc:let ...) while keeping Rust's ownership
//! model as the default.

use crate::bytecode::Value;
use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::{Arc, Weak, RwLock, Mutex};
use std::sync::atomic::{AtomicUsize, AtomicBool, Ordering};
use crossbeam_epoch;

/// GC handle to a value
#[derive(Debug, Clone)]
pub struct GcHandle {
    inner: GcHandleInner,
}

/// Internal representation of GC handle
#[derive(Debug, Clone)]
enum GcHandleInner {
    /// Standard GC implementation
    Standard(Arc<GcCell>),
    /// Concurrent GC implementation
    Concurrent(Arc<ConcurrentGcNode>),
}

/// Node type for concurrent GC integration
#[derive(Debug)]
pub struct ConcurrentGcNode {
    pub id: usize,
    pub value: std::sync::RwLock<Value>,
    pub mark_bits: std::sync::atomic::AtomicUsize,
    pub forward: crossbeam_epoch::Atomic<ConcurrentGcNode>,
    pub generation: std::sync::atomic::AtomicUsize,
    pub ref_count: std::sync::atomic::AtomicUsize,
}

impl PartialEq for ConcurrentGcNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ConcurrentGcNode {}

impl std::hash::Hash for ConcurrentGcNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for GcHandle {
    fn eq(&self, other: &Self) -> bool {
        match (&self.inner, &other.inner) {
            (GcHandleInner::Standard(a), GcHandleInner::Standard(b)) => Arc::ptr_eq(a, b),
            (GcHandleInner::Concurrent(a), GcHandleInner::Concurrent(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for GcHandle {}

/// Internal GC cell
#[derive(Debug)]
struct GcCell {
    value: RwLock<Value>,
    metadata: GcMetadata,
}

/// Metadata for GC tracking
#[derive(Debug)]
struct GcMetadata {
    /// Unique ID for this allocation
    id: usize,
    /// Reference count (for cycle detection)
    _ref_count: AtomicUsize,
    /// Color for tri-color marking
    color: Mutex<Color>,
    /// Whether this object is pinned (cannot be collected)
    pinned: AtomicBool,
}

/// Tri-color marking for garbage collection
#[derive(Debug, Clone, Copy, PartialEq)]
enum Color {
    White, // Not visited (garbage)
    Gray,  // Visited but children not processed
    Black, // Visited and children processed
}

/// Garbage collector
pub struct GarbageCollector {
    /// All allocated objects
    objects: RwLock<FxHashMap<usize, Weak<GcCell>>>,
    /// Root set
    roots: RwLock<FxHashSet<usize>>,
    /// Next allocation ID
    next_id: AtomicUsize,
    /// Collection statistics
    stats: GcStats,
    /// Configuration
    config: GcConfig,
}

/// GC statistics
#[derive(Debug, Default)]
pub struct GcStats {
    /// Total allocations
    pub allocations: AtomicUsize,
    /// Total collections
    pub collections: AtomicUsize,
    /// Total objects collected
    pub collected: AtomicUsize,
    /// Current heap size
    pub heap_size: AtomicUsize,
}

/// GC configuration
#[derive(Debug, Clone)]
pub struct GcConfig {
    /// Threshold for triggering collection (number of allocations)
    pub collection_threshold: usize,
    /// Whether to use incremental collection
    pub incremental: bool,
    /// Maximum heap size in bytes
    pub max_heap_size: usize,
    /// Whether to collect cycles
    pub collect_cycles: bool,
}

impl Default for GcConfig {
    fn default() -> Self {
        Self {
            collection_threshold: 1000,
            incremental: false,
            max_heap_size: 100 * 1024 * 1024, // 100MB
            collect_cycles: true,
        }
    }
}

impl GarbageCollector {
    /// Create a new garbage collector
    pub fn new(config: GcConfig) -> Self {
        Self {
            objects: RwLock::new(FxHashMap::default()),
            roots: RwLock::new(FxHashSet::default()),
            next_id: AtomicUsize::new(1),
            stats: GcStats::default(),
            config,
        }
    }
    
    /// Allocate a new GC-managed value
    pub fn allocate(&self, value: Value) -> Result<GcHandle> {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        
        let cell = Arc::new(GcCell {
            value: RwLock::new(value),
            metadata: GcMetadata {
                id,
                _ref_count: AtomicUsize::new(1),
                color: Mutex::new(Color::Black),
                pinned: AtomicBool::new(false),
            },
        });
        
        self.objects.write().unwrap().insert(id, Arc::downgrade(&cell));
        self.stats.allocations.fetch_add(1, Ordering::Relaxed);
        
        // Check if we need to collect
        if self.should_collect() {
            self.collect()?;
        }
        
        Ok(GcHandle { inner: GcHandleInner::Standard(cell) })
    }
    
    /// Add a root reference
    pub fn add_root(&self, handle: &GcHandle) {
        let id = match &handle.inner {
            GcHandleInner::Standard(cell) => cell.metadata.id,
            GcHandleInner::Concurrent(node) => node.id,
        };
        self.roots.write().unwrap().insert(id);
    }
    
    /// Remove a root reference
    pub fn remove_root(&self, handle: &GcHandle) {
        let id = match &handle.inner {
            GcHandleInner::Standard(cell) => cell.metadata.id,
            GcHandleInner::Concurrent(node) => node.id,
        };
        self.roots.write().unwrap().remove(&id);
    }
    
    /// Pin an object (prevent collection)
    pub fn pin(&self, handle: &GcHandle) {
        match &handle.inner {
            GcHandleInner::Standard(cell) => {
                cell.metadata.pinned.store(true, Ordering::Relaxed);
            }
            GcHandleInner::Concurrent(_) => {
                // Concurrent GC handles pinning differently
            }
        }
    }
    
    /// Unpin an object
    pub fn unpin(&self, handle: &GcHandle) {
        match &handle.inner {
            GcHandleInner::Standard(cell) => {
                cell.metadata.pinned.store(false, Ordering::Relaxed);
            }
            GcHandleInner::Concurrent(_) => {
                // Concurrent GC handles pinning differently
            }
        }
    }
    
    /// Check if collection should be triggered
    fn should_collect(&self) -> bool {
        let allocations = self.stats.allocations.load(Ordering::Relaxed);
        let collections = self.stats.collections.load(Ordering::Relaxed);
        let since_last = allocations.saturating_sub(collections * self.config.collection_threshold);
        
        since_last >= self.config.collection_threshold
    }
    
    /// Perform garbage collection
    pub fn collect(&self) -> Result<()> {
        self.stats.collections.fetch_add(1, Ordering::Relaxed);
        
        if self.config.incremental {
            self.incremental_collect()
        } else {
            self.full_collect()
        }
    }
    
    /// Full mark-and-sweep collection
    fn full_collect(&self) -> Result<()> {
        // Phase 1: Mark all objects as white
        for (_, weak) in self.objects.read().unwrap().iter() {
            if let Some(obj) = weak.upgrade() {
                *obj.metadata.color.lock().unwrap() = Color::White;
            }
        }
        
        // Phase 2: Mark roots and their descendants
        let roots = self.roots.read().unwrap().clone();
        for root_id in roots {
            if let Some(weak) = self.objects.read().unwrap().get(&root_id) {
                if let Some(obj) = weak.upgrade() {
                    self.mark(&obj)?;
                }
            }
        }
        
        // Phase 3: Mark pinned objects
        for (_, weak) in self.objects.read().unwrap().iter() {
            if let Some(obj) = weak.upgrade() {
                if obj.metadata.pinned.load(Ordering::Relaxed) && *obj.metadata.color.lock().unwrap() == Color::White {
                    self.mark(&obj)?;
                }
            }
        }
        
        // Phase 4: Sweep unmarked objects
        let mut collected = 0;
        self.objects.write().unwrap().retain(|_, weak| {
            if let Some(obj) = weak.upgrade() {
                if *obj.metadata.color.lock().unwrap() == Color::White {
                    collected += 1;
                    false // Remove from tracking
                } else {
                    true // Keep alive
                }
            } else {
                collected += 1;
                false // Already dead, remove
            }
        });
        
        self.stats.collected.fetch_add(collected, Ordering::Relaxed);
        
        // Phase 5: Cycle detection if enabled
        if self.config.collect_cycles {
            self.detect_cycles()?;
        }
        
        Ok(())
    }
    
    /// Mark phase of GC
    fn mark(&self, obj: &Arc<GcCell>) -> Result<()> {
        if *obj.metadata.color.lock().unwrap() != Color::White {
            return Ok(()); // Already marked
        }
        
        *obj.metadata.color.lock().unwrap() = Color::Gray;
        
        // Mark children
        let value = obj.value.read().unwrap().clone();
        self.mark_value_children(&value)?;
        
        *obj.metadata.color.lock().unwrap() = Color::Black;
        Ok(())
    }
    
    /// Mark children of a value
    fn mark_value_children(&self, value: &Value) -> Result<()> {
        match value {
            Value::List(items) => {
                for item in items {
                    self.mark_value_children(item)?;
                }
            }
            Value::Map(map) => {
                for (_, v) in map {
                    self.mark_value_children(v)?;
                }
            }
            Value::Function { env, .. } => {
                for v in env {
                    self.mark_value_children(v)?;
                }
            }
            Value::Cell(cell_id) => {
                // Mark referenced cell
                if let Some(weak) = self.objects.read().unwrap().get(&(*cell_id as usize)) {
                    if let Some(obj) = weak.upgrade() {
                        self.mark(&obj)?;
                    }
                }
            }
            _ => {} // Primitive values have no children
        }
        Ok(())
    }
    
    /// Incremental collection (placeholder)
    fn incremental_collect(&self) -> Result<()> {
        // For now, just do full collection
        self.full_collect()
    }
    
    /// Detect and collect cycles
    fn detect_cycles(&self) -> Result<()> {
        // Simplified cycle detection using reference counting
        // This is a placeholder - proper cycle detection would use
        // algorithms like Bacon-Rajan concurrent cycle collection
        Ok(())
    }
    
    /// Get current statistics
    pub fn stats(&self) -> GcStatsSnapshot {
        GcStatsSnapshot {
            allocations: self.stats.allocations.load(Ordering::Relaxed),
            collections: self.stats.collections.load(Ordering::Relaxed),
            collected: self.stats.collected.load(Ordering::Relaxed),
            heap_size: self.stats.heap_size.load(Ordering::Relaxed),
            live_objects: self.objects.read().unwrap().len(),
        }
    }
}

/// Snapshot of GC statistics
#[derive(Debug, Clone)]
pub struct GcStatsSnapshot {
    pub allocations: usize,
    pub collections: usize,
    pub collected: usize,
    pub heap_size: usize,
    pub live_objects: usize,
}

impl GcHandle {
    /// Get the value
    pub fn get(&self) -> Value {
        match &self.inner {
            GcHandleInner::Standard(cell) => cell.value.read().unwrap().clone(),
            GcHandleInner::Concurrent(node) => node.value.read().unwrap().clone(),
        }
    }
    
    /// Set the value
    pub fn set(&self, value: Value) {
        match &self.inner {
            GcHandleInner::Standard(cell) => {
                *cell.value.write().unwrap() = value;
            }
            GcHandleInner::Concurrent(node) => {
                *node.value.write().unwrap() = value;
            }
        }
    }
    
    /// Get mutable reference to the value
    pub fn with_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Value) -> R,
    {
        match &self.inner {
            GcHandleInner::Standard(cell) => f(&mut cell.value.write().unwrap()),
            GcHandleInner::Concurrent(node) => f(&mut node.value.write().unwrap()),
        }
    }
    
    /// Create a concurrent GC handle
    pub fn concurrent(node: Arc<ConcurrentGcNode>) -> Self {
        GcHandle {
            inner: GcHandleInner::Concurrent(node),
        }
    }
}

/// GC scope for automatic root management
pub struct GcScope<'gc> {
    gc: &'gc GarbageCollector,
    handles: Vec<GcHandle>,
}

impl<'gc> GcScope<'gc> {
    /// Create a new GC scope
    pub fn new(gc: &'gc GarbageCollector) -> Self {
        Self {
            gc,
            handles: Vec::new(),
        }
    }
    
    /// Allocate in this scope
    pub fn alloc(&mut self, value: Value) -> Result<GcHandle> {
        let handle = self.gc.allocate(value)?;
        self.gc.add_root(&handle);
        self.handles.push(handle.clone());
        Ok(handle)
    }
}

impl<'gc> Drop for GcScope<'gc> {
    fn drop(&mut self) {
        // Remove roots when scope ends
        for handle in &self.handles {
            self.gc.remove_root(handle);
        }
    }
}

/// Extension trait for Value to support GC operations
pub trait GcValue {
    /// Check if this value contains GC references
    fn has_gc_refs(&self) -> bool;
    
    /// Visit GC references in this value
    fn visit_gc_refs<F>(&self, visitor: F) where F: FnMut(&GcHandle);
}

impl GcValue for Value {
    fn has_gc_refs(&self) -> bool {
        match self {
            Value::List(items) => items.iter().any(|v| v.has_gc_refs()),
            Value::Map(map) => map.values().any(|v| v.has_gc_refs()),
            Value::Function { env, .. } => {
                env.iter().any(|v| v.has_gc_refs())
            }
            Value::GcHandle(_) => true,
            _ => false,
        }
    }
    
    fn visit_gc_refs<F>(&self, mut visitor: F) where F: FnMut(&GcHandle) {
        match self {
            Value::List(items) => {
                for item in items {
                    item.visit_gc_refs(&mut visitor);
                }
            }
            Value::Map(map) => {
                for v in map.values() {
                    v.visit_gc_refs(&mut visitor);
                }
            }
            Value::Function { env, .. } => {
                for v in env {
                    v.visit_gc_refs(&mut visitor);
                }
            }
            Value::GcHandle(handle) => {
                visitor(handle);
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_gc_allocation() {
        let gc = GarbageCollector::new(GcConfig::default());
        let handle = gc.allocate(Value::Int(42)).unwrap();
        assert_eq!(handle.get(), Value::Int(42));
        
        handle.set(Value::Int(43));
        assert_eq!(handle.get(), Value::Int(43));
    }
    
    #[test]
    fn test_gc_collection() {
        let gc = GarbageCollector::new(GcConfig {
            collection_threshold: 10,
            ..Default::default()
        });
        
        // Allocate some objects
        let mut roots = vec![];
        for i in 0..5 {
            let handle = gc.allocate(Value::Int(i)).unwrap();
            gc.add_root(&handle);
            roots.push(handle);
        }
        
        // Allocate garbage
        for i in 5..15 {
            let _ = gc.allocate(Value::Int(i)).unwrap();
        }
        
        // Should trigger collection
        let stats = gc.stats();
        assert!(stats.collections > 0);
        assert!(stats.collected > 0);
    }
    
    #[test]
    fn test_gc_scope() {
        let gc = GarbageCollector::new(GcConfig::default());
        
        {
            let mut scope = GcScope::new(&gc);
            let handle = scope.alloc(Value::Int(42)).unwrap();
            assert_eq!(handle.get(), Value::Int(42));
        } // Roots automatically removed
        
        gc.collect().unwrap();
        let stats = gc.stats();
        assert!(stats.collected > 0);
    }
    
    #[test]
    fn test_gc_with_complex_values() {
        let gc = GarbageCollector::new(GcConfig {
            collection_threshold: 5,
            ..Default::default()
        });
        
        // Create nested structures
        let list = Value::List(vec![
            Value::Int(1),
            Value::String("hello".to_string()),
            Value::List(vec![Value::Int(2), Value::Int(3)]),
        ]);
        
        let handle = gc.allocate(list.clone()).unwrap();
        gc.add_root(&handle);
        
        // Verify we can retrieve the value
        assert_eq!(handle.get(), list);
        
        // Create more allocations to trigger collection
        for i in 0..10 {
            let _ = gc.allocate(Value::Int(i)).unwrap();
        }
        
        // Original handle should still be valid
        assert_eq!(handle.get(), list);
        
        // Check that collection happened
        let stats = gc.stats();
        assert!(stats.collections > 0);
    }
}