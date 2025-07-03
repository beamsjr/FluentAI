//! Savepoint management for nested transaction support

use std::collections::VecDeque;
use tokio::sync::RwLock;

/// Manages savepoints within a transaction
pub struct SavepointManager {
    /// Stack of savepoint names
    savepoints: RwLock<VecDeque<String>>,
}

impl SavepointManager {
    /// Create a new savepoint manager
    pub fn new() -> Self {
        Self {
            savepoints: RwLock::new(VecDeque::new()),
        }
    }
    
    /// Add a new savepoint
    pub async fn add_savepoint(&self, name: &str) {
        self.savepoints.write().await.push_back(name.to_string());
    }
    
    /// Remove a savepoint
    pub async fn remove_savepoint(&self, name: &str) {
        let mut savepoints = self.savepoints.write().await;
        savepoints.retain(|sp| sp != name);
    }
    
    /// Rollback to a savepoint (removes all savepoints after it)
    pub async fn rollback_to(&self, name: &str) {
        let mut savepoints = self.savepoints.write().await;
        
        // Find the savepoint
        if let Some(pos) = savepoints.iter().position(|sp| sp == name) {
            // Remove all savepoints after this one
            savepoints.truncate(pos + 1);
        }
    }
    
    /// Get all savepoints
    pub async fn all_savepoints(&self) -> Vec<String> {
        self.savepoints.read().await.iter().cloned().collect()
    }
    
    /// Get the most recent savepoint
    pub async fn latest_savepoint(&self) -> Option<String> {
        self.savepoints.read().await.back().cloned()
    }
    
    /// Clear all savepoints
    pub async fn clear(&self) {
        self.savepoints.write().await.clear();
    }
    
    /// Get savepoint count
    pub async fn count(&self) -> usize {
        self.savepoints.read().await.len()
    }
    
    /// Generate a unique savepoint name
    pub fn generate_name(&self) -> String {
        format!("sp_{}", uuid::Uuid::new_v4().to_string().replace("-", ""))
    }
}

impl Default for SavepointManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Savepoint guard for automatic cleanup
pub struct SavepointGuard<'a> {
    manager: &'a SavepointManager,
    name: String,
    released: bool,
}

impl<'a> SavepointGuard<'a> {
    /// Create a new savepoint guard
    pub async fn new(manager: &'a SavepointManager, name: String) -> Self {
        manager.add_savepoint(&name).await;
        Self {
            manager,
            name,
            released: false,
        }
    }
    
    /// Get the savepoint name
    pub fn name(&self) -> &str {
        &self.name
    }
    
    /// Release the savepoint (prevents automatic rollback)
    pub async fn release(mut self) {
        self.released = true;
        self.manager.remove_savepoint(&self.name).await;
    }
}

impl<'a> Drop for SavepointGuard<'a> {
    fn drop(&mut self) {
        if !self.released {
            // Note: We can't perform async operations in Drop
            // The savepoint will remain until the transaction ends
            eprintln!("Warning: SavepointGuard dropped without release - savepoint '{}' remains", self.name);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_savepoint_manager() {
        let manager = SavepointManager::new();
        
        // Add savepoints
        manager.add_savepoint("sp1").await;
        manager.add_savepoint("sp2").await;
        manager.add_savepoint("sp3").await;
        
        assert_eq!(manager.count().await, 3);
        assert_eq!(manager.latest_savepoint().await, Some("sp3".to_string()));
        
        // Rollback to sp2
        manager.rollback_to("sp2").await;
        assert_eq!(manager.count().await, 2);
        assert_eq!(manager.latest_savepoint().await, Some("sp2".to_string()));
        
        // Remove specific savepoint
        manager.remove_savepoint("sp1").await;
        assert_eq!(manager.count().await, 1);
        
        // Clear all
        manager.clear().await;
        assert_eq!(manager.count().await, 0);
    }
    
    #[test]
    fn test_generate_name() {
        let manager = SavepointManager::new();
        let name1 = manager.generate_name();
        let name2 = manager.generate_name();
        
        assert!(name1.starts_with("sp_"));
        assert!(name2.starts_with("sp_"));
        assert_ne!(name1, name2);
    }
}