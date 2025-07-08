//! Module caching system

use crate::{ModuleInfo, Result};
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
use tracing::{debug, trace};

/// Thread-safe module cache
#[derive(Debug, Clone)]
pub struct ModuleCache {
    inner: Arc<RwLock<ModuleCacheInner>>,
}

#[derive(Debug)]
struct ModuleCacheInner {
    modules: FxHashMap<String, Arc<ModuleInfo>>,
    max_size: usize,
    access_order: Vec<String>,
}

impl ModuleCache {
    /// Create a new module cache with the specified maximum size
    pub fn new(max_size: usize) -> Self {
        Self {
            inner: Arc::new(RwLock::new(ModuleCacheInner {
                modules: FxHashMap::default(),
                max_size,
                access_order: Vec::new(),
            })),
        }
    }
    
    /// Get a module from the cache
    pub fn get(&self, id: &str) -> Option<Arc<ModuleInfo>> {
        let mut cache = self.inner.write().unwrap();
        
        if let Some(module) = cache.modules.get(id).cloned() {
            // Update access order for LRU
            cache.access_order.retain(|x| x != id);
            cache.access_order.push(id.to_string());
            
            trace!("Cache hit for module: {}", id);
            Some(module)
        } else {
            trace!("Cache miss for module: {}", id);
            None
        }
    }
    
    /// Insert a module into the cache
    pub fn insert(&self, module: ModuleInfo) -> Result<()> {
        let mut cache = self.inner.write().unwrap();
        let id = module.id.clone();
        
        // Check if already exists - just return Ok if it does
        if cache.modules.contains_key(&id) {
            return Ok(());
        }
        
        // Evict oldest if at capacity
        if cache.modules.len() >= cache.max_size && cache.max_size > 0 {
            if let Some(oldest) = cache.access_order.first().cloned() {
                cache.modules.remove(&oldest);
                cache.access_order.remove(0);
                debug!("Evicted module from cache: {}", oldest);
            }
        }
        
        // Insert new module
        cache.modules.insert(id.clone(), Arc::new(module));
        cache.access_order.push(id.clone());
        
        debug!("Cached module: {}", id);
        Ok(())
    }
    
    /// Remove a module from the cache
    pub fn remove(&self, id: &str) -> Option<Arc<ModuleInfo>> {
        let mut cache = self.inner.write().unwrap();
        cache.access_order.retain(|x| x != id);
        cache.modules.remove(id)
    }
    
    /// Clear all cached modules
    pub fn clear(&self) {
        let mut cache = self.inner.write().unwrap();
        cache.modules.clear();
        cache.access_order.clear();
        debug!("Cleared module cache");
    }
    
    /// Get the number of cached modules
    pub fn size(&self) -> usize {
        self.inner.read().unwrap().modules.len()
    }
    
    /// Check if a module is cached
    pub fn contains(&self, id: &str) -> bool {
        self.inner.read().unwrap().modules.contains_key(id)
    }
    
    /// Get all cached module IDs
    pub fn cached_modules(&self) -> Vec<String> {
        self.inner.read().unwrap().modules.keys().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::Graph;
    use std::path::PathBuf;
    
    fn create_test_module(id: &str) -> ModuleInfo {
        ModuleInfo {
            id: id.to_string(),
            name: id.to_string(),
            path: PathBuf::from(format!("{}.ai", id)),
            graph: Graph::new(),
            root: fluentai_core::ast::NodeId::new(1).unwrap(),
            exports: vec![],
            dependencies: vec![],
            metadata: FxHashMap::default(),
        }
    }
    
    #[test]
    fn test_cache_basic_operations() {
        let cache = ModuleCache::new(3);
        
        // Test insert and get
        let module1 = create_test_module("module1");
        cache.insert(module1).unwrap();
        
        assert!(cache.contains("module1"));
        assert!(cache.get("module1").is_some());
        assert_eq!(cache.size(), 1);
    }
    
    #[test]
    fn test_cache_lru_eviction() {
        let cache = ModuleCache::new(2);
        
        // Fill cache
        cache.insert(create_test_module("module1")).unwrap();
        cache.insert(create_test_module("module2")).unwrap();
        
        // Access module1 to make it more recent
        cache.get("module1");
        
        // Insert module3, should evict module2
        cache.insert(create_test_module("module3")).unwrap();
        
        assert!(cache.contains("module1"));
        assert!(!cache.contains("module2"));
        assert!(cache.contains("module3"));
    }
    
    #[test]
    fn test_cache_clear() {
        let cache = ModuleCache::new(10);
        
        cache.insert(create_test_module("module1")).unwrap();
        cache.insert(create_test_module("module2")).unwrap();
        
        cache.clear();
        assert_eq!(cache.size(), 0);
        assert!(!cache.contains("module1"));
        assert!(!cache.contains("module2"));
    }
}