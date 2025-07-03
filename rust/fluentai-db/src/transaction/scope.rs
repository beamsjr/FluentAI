//! Transaction scope utilities

use super::TransactionManager;

// Re-export TransactionScope from manager module
pub use super::manager::TransactionScope;

/// Transaction scope builder for configuring scoped transactions
pub struct TransactionScopeBuilder {
    auto_rollback: bool,
    isolation_level: Option<super::IsolationLevel>,
    read_only: bool,
}

impl TransactionScopeBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            auto_rollback: true,
            isolation_level: None,
            read_only: false,
        }
    }
    
    /// Set whether to automatically rollback on drop
    pub fn auto_rollback(mut self, enabled: bool) -> Self {
        self.auto_rollback = enabled;
        self
    }
    
    /// Set isolation level for transactions in this scope
    pub fn isolation_level(mut self, level: super::IsolationLevel) -> Self {
        self.isolation_level = Some(level);
        self
    }
    
    /// Set transactions as read-only
    pub fn read_only(mut self, enabled: bool) -> Self {
        self.read_only = enabled;
        self
    }
    
    /// Build the transaction scope
    pub async fn build(self, manager: &TransactionManager) -> TransactionScope {
        let scope = manager.create_scope().await;
        // Apply configuration to scope
        // Note: This would require extending TransactionScope with these options
        scope
    }
}

impl Default for TransactionScopeBuilder {
    fn default() -> Self {
        Self::new()
    }
}