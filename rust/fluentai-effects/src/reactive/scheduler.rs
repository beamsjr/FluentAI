//! Update scheduler for batching reactive updates

use parking_lot::{Mutex, RwLock};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

/// Scheduler for batching and ordering reactive updates
pub struct UpdateScheduler {
    /// Pending updates to be processed
    pending: Arc<Mutex<HashSet<String>>>,
    /// Update callbacks for computed values
    computed_callbacks: Arc<RwLock<HashMap<String, Box<dyn Fn() + Send + Sync>>>>,
    /// Watcher callbacks
    watcher_callbacks: Arc<RwLock<HashMap<String, Box<dyn Fn() + Send + Sync>>>>,
    /// Whether we're currently flushing updates
    is_flushing: Arc<Mutex<bool>>,
    /// Update queue for ordering
    update_queue: Arc<Mutex<VecDeque<String>>>,
}

impl UpdateScheduler {
    /// Create a new update scheduler
    pub fn new() -> Self {
        Self {
            pending: Arc::new(Mutex::new(HashSet::new())),
            computed_callbacks: Arc::new(RwLock::new(HashMap::new())),
            watcher_callbacks: Arc::new(RwLock::new(HashMap::new())),
            is_flushing: Arc::new(Mutex::new(false)),
            update_queue: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    /// Schedule updates for a set of computation IDs
    pub fn schedule_updates(&self, computation_ids: Vec<String>) {
        let mut pending = self.pending.lock();
        let mut queue = self.update_queue.lock();

        for id in computation_ids {
            if pending.insert(id.clone()) {
                queue.push_back(id);
            }
        }

        drop(pending);
        drop(queue);

        // Schedule flush on next tick
        self.schedule_flush();
    }

    /// Register a computed value callback
    pub fn register_computed<F>(&self, id: String, callback: F)
    where
        F: Fn() + Send + Sync + 'static,
    {
        self.computed_callbacks
            .write()
            .insert(id, Box::new(callback));
    }

    /// Register a watcher callback
    pub fn register_watcher<F>(&self, id: String, callback: F)
    where
        F: Fn() + Send + Sync + 'static,
    {
        self.watcher_callbacks
            .write()
            .insert(id, Box::new(callback));
    }

    /// Unregister a watcher
    pub fn unregister_watcher(&self, id: &str) {
        self.watcher_callbacks.write().remove(id);
    }

    /// Schedule a flush of pending updates
    fn schedule_flush(&self) {
        let is_flushing = self.is_flushing.clone();
        let scheduler = Arc::new(self.clone());

        // Use a simple next-tick simulation
        // In production, this would use a proper event loop
        thread::spawn(move || {
            thread::sleep(Duration::from_micros(1));

            let mut flushing = is_flushing.lock();
            if !*flushing {
                *flushing = true;
                drop(flushing);

                scheduler.flush_updates();

                *is_flushing.lock() = false;
            }
        });
    }

    /// Flush all pending updates
    pub fn flush_updates(&self) {
        let mut processed = HashSet::new();

        loop {
            let next = {
                let mut queue = self.update_queue.lock();
                queue.pop_front()
            };

            match next {
                Some(id) => {
                    if processed.insert(id.clone()) {
                        self.process_update(&id);
                    }
                }
                None => break,
            }
        }

        // Clear pending set
        self.pending.lock().clear();
    }

    /// Process a single update
    fn process_update(&self, id: &str) {
        // Try computed callbacks first
        if let Some(callback) = self.computed_callbacks.read().get(id) {
            callback();
        }

        // Then try watcher callbacks
        if let Some(callback) = self.watcher_callbacks.read().get(id) {
            callback();
        }
    }

    /// Run updates synchronously (for testing)
    pub fn flush_sync(&self) {
        let mut flushing = self.is_flushing.lock();
        if !*flushing {
            *flushing = true;
            drop(flushing);

            self.flush_updates();

            *self.is_flushing.lock() = false;
        }
    }
}

impl Clone for UpdateScheduler {
    fn clone(&self) -> Self {
        Self {
            pending: self.pending.clone(),
            computed_callbacks: self.computed_callbacks.clone(),
            watcher_callbacks: self.watcher_callbacks.clone(),
            is_flushing: self.is_flushing.clone(),
            update_queue: self.update_queue.clone(),
        }
    }
}

impl Default for UpdateScheduler {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for UpdateScheduler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UpdateScheduler")
            .field("pending_count", &self.pending.lock().len())
            .field("computed_count", &self.computed_callbacks.read().len())
            .field("watcher_count", &self.watcher_callbacks.read().len())
            .field("is_flushing", &*self.is_flushing.lock())
            .field("queue_size", &self.update_queue.lock().len())
            .finish()
    }
}

/// Scope for batching multiple updates
pub struct BatchScope {
    scheduler: Arc<UpdateScheduler>,
}

impl BatchScope {
    /// Create a new batch scope
    pub fn new(scheduler: Arc<UpdateScheduler>) -> Self {
        Self { scheduler }
    }

    /// Run a function within a batch, deferring all updates until the end
    pub fn run<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        // In a full implementation, we'd pause automatic flushing
        let result = f();

        // Flush all updates at once
        self.scheduler.flush_sync();

        result
    }
}
