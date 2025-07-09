//! Watchers for reactive state changes

use fluentai_core::value::Value;
use parking_lot::Mutex;
use std::sync::Arc;

use super::{ReactiveContext, ReactiveState};

/// Callback function for watchers
pub type WatchCallback = Arc<dyn Fn(&Value, Option<&Value>) + Send + Sync>;

/// A watcher that executes side effects when dependencies change
#[derive(Clone)]
pub struct Watcher {
    id: String,
    watch_fn: WatchCallback,
    dependencies: Arc<Mutex<Vec<String>>>,
    immediate: bool,
    _deep: bool,
}

impl Watcher {
    /// Create a new watcher
    pub fn new<F>(watch_fn: F, immediate: bool, deep: bool) -> Self
    where
        F: Fn(&Value, Option<&Value>) + Send + Sync + 'static,
    {
        Self {
            id: format!("watcher_{}", uuid::Uuid::new_v4()),
            watch_fn: Arc::new(watch_fn),
            dependencies: Arc::new(Mutex::new(Vec::new())),
            immediate,
            _deep: deep,
        }
    }

    /// Watch a specific key in reactive state
    pub fn watch_key(&self, state: &ReactiveState, key: &str) {
        // Add to dependencies
        self.dependencies.lock().push(key.to_string());

        if let Some(ctx) = ReactiveContext::current() {
            let watch_fn = self.watch_fn.clone();
            let key_str = key.to_string();
            let state_clone = state.clone();
            let watcher_id = self.id.clone();

            // Track dependencies by executing in a computation context
            let initial = ctx.with_computation(watcher_id.clone(), || {
                // This get() call should register the dependency
                state.get(key)
            });

            // Register watcher callback that will be called when dependencies change
            let watch_fn_for_callback = watch_fn.clone();
            let state_for_callback = state_clone.clone();
            let key_for_callback = key_str.clone();

            ctx.scheduler
                .register_computed(watcher_id.clone(), move || {
                    // Get the new value directly (we don't need to track dependencies in the callback)
                    let value = state_for_callback.get(&key_for_callback);
                    // Handle both Some and None cases
                    match value {
                        Some(ref v) => watch_fn_for_callback(v, None),
                        None => watch_fn_for_callback(&Value::Nil, None),
                    }
                });

            // Execute immediately if requested
            if self.immediate {
                match initial {
                    Some(ref value) => (self.watch_fn)(value, None),
                    None => (self.watch_fn)(&Value::Nil, None),
                }
            }
        }
    }

    /// Watch multiple keys
    pub fn watch_keys(&self, state: &ReactiveState, keys: &[String]) {
        for key in keys {
            self.watch_key(state, key);
        }
    }

    /// Watch a computed value
    pub fn watch_computed<F>(&self, compute_fn: F)
    where
        F: Fn() -> Value + Send + Sync + 'static,
    {
        // Create a computed value that we watch
        let computed = super::computed::Computed::new(compute_fn);

        // Get initial value
        let initial = computed.get();

        // Register for updates
        if let Some(ctx) = ReactiveContext::current() {
            let watch_fn = self.watch_fn.clone();
            let computed = computed.clone();

            ctx.scheduler.register_watcher(self.id.clone(), move || {
                let new_value = computed.get();
                watch_fn(&new_value, None);
            });

            // Execute immediately if requested
            if self.immediate {
                (self.watch_fn)(&initial, None);
            }
        }
    }

    /// Stop watching (cleanup)
    pub fn stop(&self) {
        if let Some(ctx) = ReactiveContext::current() {
            ctx.scheduler.unregister_watcher(&self.id);
        }
        self.dependencies.lock().clear();
    }
}

/// Builder for creating watchers
pub struct WatcherBuilder {
    immediate: bool,
    deep: bool,
}

impl WatcherBuilder {
    /// Create a new watcher builder
    pub fn new() -> Self {
        Self {
            immediate: false,
            deep: false,
        }
    }

    /// Execute the watcher immediately
    pub fn immediate(mut self) -> Self {
        self.immediate = true;
        self
    }

    /// Watch for deep changes in nested structures
    pub fn deep(mut self) -> Self {
        self.deep = true;
        self
    }

    /// Build the watcher with a callback
    pub fn build<F>(self, watch_fn: F) -> Watcher
    where
        F: Fn(&Value, Option<&Value>) + Send + Sync + 'static,
    {
        Watcher::new(watch_fn, self.immediate, self.deep)
    }
}

impl Default for WatcherBuilder {
    fn default() -> Self {
        Self::new()
    }
}
