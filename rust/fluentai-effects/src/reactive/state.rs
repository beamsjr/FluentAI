//! Reactive state container

use dashmap::DashMap;
use fluentai_core::value::Value;
use std::sync::Arc;

use super::scheduler::UpdateScheduler;
use super::{DependencyTracker, ReactiveContext};

/// A reactive state container that tracks access and notifies dependents
#[derive(Debug, Clone)]
pub struct ReactiveState {
    /// The actual state storage
    store: Arc<DashMap<String, Value>>,
    /// Dependency tracker
    tracker: Arc<DependencyTracker>,
    /// Update scheduler
    scheduler: Arc<UpdateScheduler>,
}

impl ReactiveState {
    /// Create a new reactive state container
    pub fn new() -> Self {
        Self {
            store: Arc::new(DashMap::new()),
            tracker: Arc::new(DependencyTracker::new()),
            scheduler: Arc::new(UpdateScheduler::new()),
        }
    }

    /// Create a reactive state with a specific context
    pub fn with_context(context: &ReactiveContext) -> Self {
        Self {
            store: Arc::new(DashMap::new()),
            tracker: context.tracker.clone(),
            scheduler: context.scheduler.clone(),
        }
    }

    /// Get a value, tracking the access if in a reactive context
    pub fn get(&self, key: &str) -> Option<Value> {
        // Track dependency if we're in a reactive computation
        if let Some(ctx) = ReactiveContext::current() {
            if let Some(computation_id) = &ctx.current_computation {
                self.tracker.add_dependency(key, computation_id);
            }
        }

        self.store.get(key).map(|v| v.clone())
    }

    /// Set a value and trigger updates
    pub fn set(&self, key: String, value: Value) {
        let changed = {
            let mut entry = self.store.entry(key.clone()).or_insert(Value::Nil);
            let changed = *entry != value;
            if changed {
                *entry = value;
            }
            changed
        };

        if changed {
            self.trigger_updates(&key);
        }
    }

    /// Update a value using a function
    pub fn update<F>(&self, key: &str, f: F)
    where
        F: FnOnce(&Value) -> Value,
    {
        let new_value = {
            let entry = self.store.entry(key.to_string()).or_insert(Value::Nil);
            f(&*entry)
        };

        self.set(key.to_string(), new_value);
    }

    /// Delete a value
    pub fn delete(&self, key: &str) -> Option<Value> {
        let removed = self.store.remove(key).map(|(_, v)| v);
        if removed.is_some() {
            self.trigger_updates(key);
        }
        removed
    }

    /// Clear all state
    pub fn clear(&self) {
        let keys: Vec<String> = self.store.iter().map(|entry| entry.key().clone()).collect();

        self.store.clear();

        // Trigger updates for all cleared keys
        for key in keys {
            self.trigger_updates(&key);
        }
    }

    /// Check if a key exists
    pub fn contains(&self, key: &str) -> bool {
        self.store.contains_key(key)
    }

    /// Get the number of entries
    pub fn len(&self) -> usize {
        self.store.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.store.is_empty()
    }

    /// Trigger updates for all dependents of a key
    fn trigger_updates(&self, key: &str) {
        let dependents = self.tracker.get_dependents(key);
        if !dependents.is_empty() {
            self.scheduler.schedule_updates(dependents);
        }
    }
}

impl Default for ReactiveState {
    fn default() -> Self {
        Self::new()
    }
}

/// A reactive reference to a single value
#[derive(Debug, Clone)]
pub struct ReactiveRef<T> {
    key: String,
    state: ReactiveState,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> ReactiveRef<T>
where
    T: Into<Value> + TryFrom<Value>,
    T::Error: std::fmt::Debug,
{
    /// Create a new reactive reference
    pub fn new(key: String, initial: T, state: ReactiveState) -> Self {
        state.set(key.clone(), initial.into());
        Self {
            key,
            state,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Get the current value
    pub fn get(&self) -> Option<T> {
        self.state.get(&self.key).and_then(|v| T::try_from(v).ok())
    }

    /// Set a new value
    pub fn set(&self, value: T) {
        self.state.set(self.key.clone(), value.into());
    }

    /// Update the value using a function
    pub fn update<F>(&self, f: F)
    where
        F: FnOnce(&T) -> T,
    {
        self.state.update(&self.key, |v| {
            if let Ok(current) = T::try_from(v.clone()) {
                f(&current).into()
            } else {
                v.clone()
            }
        });
    }
}
