//! Dependency tracking for reactive computations

use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Tracks dependencies between reactive values and computations
#[derive(Debug, Default)]
pub struct DependencyTracker {
    /// Map from state keys to dependent computations
    dependencies: RwLock<HashMap<String, HashSet<String>>>,
    /// Map from computations to their dependencies
    reverse_deps: RwLock<HashMap<String, HashSet<String>>>,
}

impl DependencyTracker {
    /// Create a new dependency tracker
    pub fn new() -> Self {
        Self::default()
    }

    /// Record that a computation depends on a state value
    pub fn add_dependency(&self, state_key: &str, computation_id: &str) {
        let mut deps = self.dependencies.write();
        deps.entry(state_key.to_string())
            .or_insert_with(HashSet::new)
            .insert(computation_id.to_string());

        let mut reverse = self.reverse_deps.write();
        reverse
            .entry(computation_id.to_string())
            .or_insert_with(HashSet::new)
            .insert(state_key.to_string());
    }

    /// Remove all dependencies for a computation
    pub fn clear_computation_deps(&self, computation_id: &str) {
        let mut reverse = self.reverse_deps.write();
        if let Some(state_keys) = reverse.remove(computation_id) {
            let mut deps = self.dependencies.write();
            for state_key in state_keys {
                if let Some(computations) = deps.get_mut(&state_key) {
                    computations.remove(computation_id);
                    if computations.is_empty() {
                        deps.remove(&state_key);
                    }
                }
            }
        }
    }

    /// Get all computations that depend on a state value
    pub fn get_dependents(&self, state_key: &str) -> Vec<String> {
        self.dependencies
            .read()
            .get(state_key)
            .map(|deps| deps.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// Get all state values that a computation depends on
    pub fn get_dependencies(&self, computation_id: &str) -> Vec<String> {
        self.reverse_deps
            .read()
            .get(computation_id)
            .map(|deps| deps.iter().cloned().collect())
            .unwrap_or_default()
    }
}

/// Represents a dependency graph for optimizing updates
#[derive(Debug)]
pub struct DependencyGraph {
    tracker: Arc<DependencyTracker>,
}

impl DependencyGraph {
    /// Create a new dependency graph
    pub fn new(tracker: Arc<DependencyTracker>) -> Self {
        Self { tracker }
    }

    /// Get the update order for a set of changed state keys
    /// Returns computations in topological order to avoid redundant updates
    pub fn get_update_order(&self, changed_keys: &[String]) -> Vec<String> {
        let mut to_update = HashSet::new();
        let mut visited = HashSet::new();

        // Collect all affected computations
        for key in changed_keys {
            let dependents = self.tracker.get_dependents(key);
            for dep in dependents {
                if !visited.contains(&dep) {
                    self.collect_transitive_deps(&dep, &mut to_update, &mut visited);
                }
            }
        }

        // Sort by dependency order (simplified - proper topological sort needed)
        let mut result: Vec<String> = to_update.into_iter().collect();
        result.sort();
        result
    }

    fn collect_transitive_deps(
        &self,
        computation_id: &str,
        to_update: &mut HashSet<String>,
        visited: &mut HashSet<String>,
    ) {
        if visited.insert(computation_id.to_string()) {
            to_update.insert(computation_id.to_string());

            // In a full implementation, we'd also track computation-to-computation deps
            // For now, we just add the direct computation
        }
    }
}
