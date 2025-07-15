/// Enhanced reactive system for UI macros
use std::sync::Arc;
use std::collections::{HashMap, HashSet};
use parking_lot::{RwLock, Mutex};
use std::cell::RefCell;

pub struct ReactiveNode {
    pub id: String,
    pub dependencies: Vec<String>,
    pub update_fn: Box<dyn Fn() + Send + Sync>,
}

pub struct ReactiveSystem {
    nodes: Arc<RwLock<HashMap<String, ReactiveNode>>>,
    dependency_graph: Arc<RwLock<HashMap<String, Vec<String>>>>,
    current_tracking: Arc<Mutex<Option<TrackingContext>>>,
    effects: Arc<RwLock<Vec<Effect>>>,
}

struct TrackingContext {
    dependencies: RefCell<HashSet<String>>,
}

struct Effect {
    dependencies: Vec<String>,
    callback: Box<dyn Fn() + Send + Sync>,
}

impl ReactiveSystem {
    pub fn new() -> Self {
        Self {
            nodes: Arc::new(RwLock::new(HashMap::new())),
            dependency_graph: Arc::new(RwLock::new(HashMap::new())),
            current_tracking: Arc::new(Mutex::new(None)),
            effects: Arc::new(RwLock::new(Vec::new())),
        }
    }
    
    pub fn register_node(&self, node: ReactiveNode) {
        let id = node.id.clone();
        let deps = node.dependencies.clone();
        
        self.nodes.write().insert(id.clone(), node);
        
        // Update dependency graph
        let mut graph = self.dependency_graph.write();
        for dep in deps {
            graph.entry(dep).or_insert_with(Vec::new).push(id.clone());
        }
    }
    
    pub fn trigger_update(&self, source_id: &str) {
        // Update reactive nodes
        if let Some(dependents) = self.dependency_graph.read().get(source_id) {
            let nodes = self.nodes.read();
            for dependent_id in dependents {
                if let Some(node) = nodes.get(dependent_id) {
                    (node.update_fn)();
                }
            }
        }
        
        // Trigger effects
        let effects = self.effects.read();
        for effect in effects.iter() {
            if effect.dependencies.contains(&source_id.to_string()) {
                (effect.callback)();
            }
        }
    }
    
    /// Start tracking dependencies for computed properties
    pub fn start_tracking(&self) -> TrackingGuard {
        let context = TrackingContext {
            dependencies: RefCell::new(HashSet::new()),
        };
        *self.current_tracking.lock() = Some(context);
        TrackingGuard { system: self }
    }
    
    /// Track a dependency access
    pub fn track_dependency(&self, dep: &str) {
        if let Some(ref context) = *self.current_tracking.lock() {
            context.dependencies.borrow_mut().insert(dep.to_string());
        }
    }
    
    /// Get tracked dependencies
    pub fn get_tracked_dependencies(&self) -> Vec<String> {
        if let Some(ref context) = *self.current_tracking.lock() {
            context.dependencies.borrow().iter().cloned().collect()
        } else {
            Vec::new()
        }
    }
    
    /// Create an effect that runs when dependencies change
    pub fn create_effect(&self, dependencies: Vec<String>, callback: Box<dyn Fn() + Send + Sync>) {
        let effect = Effect {
            dependencies,
            callback,
        };
        self.effects.write().push(effect);
    }
}

pub struct TrackingGuard<'a> {
    system: &'a ReactiveSystem,
}

impl<'a> Drop for TrackingGuard<'a> {
    fn drop(&mut self) {
        *self.system.current_tracking.lock() = None;
    }
}

// Global reactive system instance
lazy_static::lazy_static! {
    static ref REACTIVE_SYSTEM: ReactiveSystem = ReactiveSystem::new();
}

/// Trigger an update for a reactive variable
pub fn trigger_update(var_name: &str) {
    REACTIVE_SYSTEM.trigger_update(var_name);
}

/// Execute a function with dependency tracking
pub fn with_tracking<T, F: FnOnce() -> T>(f: F) -> T {
    let _guard = REACTIVE_SYSTEM.start_tracking();
    let result = f();
    // Dependencies are automatically tracked during execution
    result
}

/// Create an effect that runs when dependencies change
pub fn create_effect(dependencies: Vec<&str>, callback: Box<dyn Fn() + Send + Sync>) {
    let deps: Vec<String> = dependencies.iter().map(|&s| s.to_string()).collect();
    REACTIVE_SYSTEM.create_effect(deps, callback);
}

/// Track a dependency access (called by reactive getters)
pub fn track_dependency(dep: &str) {
    REACTIVE_SYSTEM.track_dependency(dep);
}