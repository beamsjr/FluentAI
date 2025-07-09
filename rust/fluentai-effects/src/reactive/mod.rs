//! Reactive state system for FluentAi
//!
//! This module provides reactive primitives that automatically track dependencies
//! and trigger updates when state changes, enabling efficient declarative UIs.

pub mod computed;
pub mod deps;
pub mod scheduler;
pub mod state;
pub mod watchers;

pub use computed::{Computed, ComputedValue};
pub use deps::{DependencyGraph, DependencyTracker};
pub use scheduler::{BatchScope, UpdateScheduler};
pub use state::{ReactiveRef, ReactiveState};
pub use watchers::{WatchCallback, Watcher};

use parking_lot::RwLock;
use std::sync::Arc;

thread_local! {
    /// Global reactive context for the current execution
    static REACTIVE_CONTEXT: Arc<RwLock<Option<ReactiveContext>>> = Arc::new(RwLock::new(None));
}

/// Context for reactive computations
#[derive(Debug, Clone)]
pub struct ReactiveContext {
    /// Current computation being tracked
    pub current_computation: Option<String>,
    /// Dependency tracker
    pub tracker: Arc<DependencyTracker>,
    /// Update scheduler
    pub scheduler: Arc<UpdateScheduler>,
}

impl ReactiveContext {
    /// Create a new reactive context
    pub fn new() -> Self {
        Self {
            current_computation: None,
            tracker: Arc::new(DependencyTracker::new()),
            scheduler: Arc::new(UpdateScheduler::new()),
        }
    }

    /// Run a function within this reactive context
    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        REACTIVE_CONTEXT.with(|ctx| {
            let mut guard = ctx.write();
            let prev = guard.clone();
            *guard = Some(self.clone());
            drop(guard);

            let result = f();

            let mut guard = ctx.write();
            *guard = prev;

            result
        })
    }

    /// Get the current reactive context if any
    pub fn current() -> Option<ReactiveContext> {
        REACTIVE_CONTEXT.with(|ctx| ctx.read().clone())
    }

    /// Run a function with a specific computation context
    pub fn with_computation<F, R>(&self, computation_id: String, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let mut ctx = self.clone();
        ctx.current_computation = Some(computation_id);
        ctx.with(f)
    }
}

/// Initialize the reactive system
pub fn init_reactive_system() -> ReactiveContext {
    ReactiveContext::new()
}

#[cfg(test)]
mod tests;
