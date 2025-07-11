//! Copy-on-write globals for efficient VM cloning

use std::sync::Arc;
use rustc_hash::FxHashMap;
use fluentai_core::value::Value;

/// Copy-on-write wrapper for global variables
/// Allows sharing globals between VM instances until a write occurs
#[derive(Debug, Clone)]
pub struct CowGlobals {
    /// Shared read-only globals
    shared: Arc<FxHashMap<String, Value>>,
    /// Local modifications (copy-on-write)
    local: Option<FxHashMap<String, Value>>,
}

impl CowGlobals {
    /// Create new empty globals
    pub fn new() -> Self {
        Self {
            shared: Arc::new(FxHashMap::default()),
            local: None,
        }
    }
    
    /// Create globals from an existing map
    pub fn from_map(map: FxHashMap<String, Value>) -> Self {
        Self {
            shared: Arc::new(map),
            local: None,
        }
    }
    
    /// Get a value from globals
    pub fn get(&self, key: &str) -> Option<&Value> {
        // Check local modifications first
        if let Some(ref local) = self.local {
            if let Some(value) = local.get(key) {
                return Some(value);
            }
        }
        // Fall back to shared globals
        self.shared.get(key)
    }
    
    /// Insert a value, triggering copy-on-write if needed
    pub fn insert(&mut self, key: String, value: Value) {
        // Ensure we have a local copy
        if self.local.is_none() {
            // Clone the shared map for local modifications
            self.local = Some((*self.shared).clone());
        }
        
        // Insert into local copy
        if let Some(ref mut local) = self.local {
            local.insert(key, value);
        }
    }
    
    /// Remove a value, triggering copy-on-write if needed
    pub fn remove(&mut self, key: &str) -> Option<Value> {
        // Ensure we have a local copy
        if self.local.is_none() {
            self.local = Some((*self.shared).clone());
        }
        
        // Remove from local copy
        if let Some(ref mut local) = self.local {
            local.remove(key)
        } else {
            None
        }
    }
    
    /// Check if a key exists
    pub fn contains_key(&self, key: &str) -> bool {
        if let Some(ref local) = self.local {
            local.contains_key(key) || self.shared.contains_key(key)
        } else {
            self.shared.contains_key(key)
        }
    }
    
    /// Clear all globals (local only)
    pub fn clear(&mut self) {
        self.local = Some(FxHashMap::default());
    }
    
    /// Get the effective globals map (for iteration)
    pub fn as_map(&self) -> FxHashMap<String, Value> {
        if let Some(ref local) = self.local {
            local.clone()
        } else {
            (*self.shared).clone()
        }
    }
    
    /// Get the number of globals
    pub fn len(&self) -> usize {
        if let Some(ref local) = self.local {
            local.len()
        } else {
            self.shared.len()
        }
    }
    
    /// Check if globals are empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for CowGlobals {
    fn default() -> Self {
        Self::new()
    }
}