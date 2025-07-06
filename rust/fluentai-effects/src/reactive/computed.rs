//! Computed values that automatically update when dependencies change

use std::sync::Arc;
use parking_lot::{RwLock, Mutex};
use fluentai_core::value::Value;
use std::fmt::Debug;

use super::{ReactiveContext, DependencyTracker, UpdateScheduler};

/// A computed value that automatically recalculates when dependencies change
#[derive(Clone)]
pub struct Computed {
    inner: Arc<ComputedInner>,
}

struct ComputedInner {
    id: String,
    compute_fn: Arc<dyn Fn() -> Value + Send + Sync>,
    cached_value: RwLock<Option<Value>>,
    is_dirty: Mutex<bool>,
    tracker: Arc<DependencyTracker>,
    scheduler: Arc<UpdateScheduler>,
}

impl Debug for ComputedInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ComputedInner")
            .field("id", &self.id)
            .field("cached_value", &self.cached_value)
            .field("is_dirty", &self.is_dirty)
            .finish()
    }
}

impl Computed {
    /// Create a new computed value
    pub fn new<F>(compute_fn: F) -> Self
    where
        F: Fn() -> Value + Send + Sync + 'static,
    {
        let id = format!("computed_{}", uuid::Uuid::new_v4());
        
        let (tracker, scheduler) = ReactiveContext::current()
            .map(|ctx| (ctx.tracker.clone(), ctx.scheduler.clone()))
            .unwrap_or_else(|| (Arc::new(DependencyTracker::new()), Arc::new(UpdateScheduler::new())));
            
        let inner = Arc::new(ComputedInner {
            id: id.clone(),
            compute_fn: Arc::new(compute_fn),
            cached_value: RwLock::new(None),
            is_dirty: Mutex::new(true),
            tracker,
            scheduler,
        });
        
        // Register this computed value for updates
        if let Some(ctx) = ReactiveContext::current() {
            let weak_inner = Arc::downgrade(&inner);
            ctx.scheduler.register_computed(id, move || {
                if let Some(inner) = weak_inner.upgrade() {
                    inner.invalidate();
                }
            });
        }
        
        Computed { inner }
    }
    
    /// Get the computed value, recalculating if necessary
    pub fn get(&self) -> Value {
        // Check if we need to recompute
        let needs_recompute = {
            let is_dirty = self.inner.is_dirty.lock();
            *is_dirty || self.inner.cached_value.read().is_none()
        };
        
        if needs_recompute {
            self.recompute()
        } else {
            self.inner.cached_value.read().clone().unwrap_or(Value::Nil)
        }
    }
    
    /// Force recomputation of the value
    pub fn recompute(&self) -> Value {
        // Clear old dependencies
        self.inner.tracker.clear_computation_deps(&self.inner.id);
        
        // Create a context with our tracker and scheduler
        let ctx = ReactiveContext {
            current_computation: None,
            tracker: self.inner.tracker.clone(),
            scheduler: self.inner.scheduler.clone(),
        };
        
        // Compute within a tracking context
        let value = ctx.with_computation(self.inner.id.clone(), || {
            (self.inner.compute_fn)()
        });
        
        // Update cached value and clear dirty flag
        *self.inner.cached_value.write() = Some(value.clone());
        *self.inner.is_dirty.lock() = false;
        
        value
    }
    
    /// Mark this computed value as dirty
    pub fn invalidate(&self) {
        *self.inner.is_dirty.lock() = true;
    }
    
    /// Get the ID of this computed value
    pub fn id(&self) -> &str {
        &self.inner.id
    }
}

impl ComputedInner {
    fn invalidate(&self) {
        *self.is_dirty.lock() = true;
    }
}

/// A typed computed value
pub struct ComputedValue<T> {
    computed: Computed,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> ComputedValue<T>
where
    T: Into<Value> + TryFrom<Value> + Clone,
    T::Error: Debug,
{
    /// Create a new typed computed value
    pub fn new<F>(compute_fn: F) -> Self
    where
        F: Fn() -> T + Send + Sync + 'static,
    {
        let computed = Computed::new(move || compute_fn().into());
        Self {
            computed,
            _phantom: std::marker::PhantomData,
        }
    }
    
    /// Get the computed value
    pub fn get(&self) -> Option<T> {
        T::try_from(self.computed.get()).ok()
    }
    
    /// Force recomputation
    pub fn recompute(&self) -> Option<T> {
        T::try_from(self.computed.recompute()).ok()
    }
    
    /// Invalidate the cached value
    pub fn invalidate(&self) {
        self.computed.invalidate();
    }
}

// We need to add uuid to dependencies
// This will be added to Cargo.toml when we update it