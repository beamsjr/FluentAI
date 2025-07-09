//! Transaction manager for coordinating transactions

use super::{Transaction, TransactionOptions};
use crate::connection::DbConnection;
use crate::error::{DbError, DbResult};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Transaction manager for tracking and coordinating transactions
pub struct TransactionManager {
    /// Active transactions
    transactions: Arc<RwLock<HashMap<Uuid, Arc<Transaction>>>>,
    /// Connection pool
    connection: Arc<DbConnection>,
    /// Nested transaction support
    nested_enabled: bool,
}

impl TransactionManager {
    /// Create a new transaction manager
    pub fn new(connection: Arc<DbConnection>) -> Self {
        Self {
            transactions: Arc::new(RwLock::new(HashMap::new())),
            connection,
            nested_enabled: true,
        }
    }

    /// Enable or disable nested transactions
    pub fn set_nested_enabled(&mut self, enabled: bool) {
        self.nested_enabled = enabled;
    }

    /// Begin a new managed transaction
    pub async fn begin_transaction(&self) -> DbResult<Arc<Transaction>> {
        self.begin_transaction_with_options(TransactionOptions::default())
            .await
    }

    /// Begin a new managed transaction with options
    pub async fn begin_transaction_with_options(
        &self,
        options: TransactionOptions,
    ) -> DbResult<Arc<Transaction>> {
        let tx = Transaction::begin_with_options(self.connection.clone(), options).await?;
        let tx = Arc::new(tx);

        let id = tx.id();
        self.transactions.write().await.insert(id, tx.clone());

        Ok(tx)
    }

    /// Get a transaction by ID
    pub async fn get_transaction(&self, id: Uuid) -> Option<Arc<Transaction>> {
        self.transactions.read().await.get(&id).cloned()
    }

    /// Remove a transaction from tracking
    pub async fn remove_transaction(&self, id: Uuid) {
        self.transactions.write().await.remove(&id);
    }

    /// Get all active transactions
    pub async fn active_transactions(&self) -> Vec<Arc<Transaction>> {
        self.transactions.read().await.values().cloned().collect()
    }

    /// Get count of active transactions
    pub async fn active_count(&self) -> usize {
        self.transactions.read().await.len()
    }

    /// Rollback all active transactions (useful for cleanup)
    pub async fn rollback_all(&self) -> DbResult<()> {
        let transactions: Vec<_> = self
            .transactions
            .write()
            .await
            .drain()
            .map(|(_, tx)| tx)
            .collect();

        for tx in transactions {
            // Try to rollback, but don't fail if already completed
            if tx.is_active().await {
                match Arc::try_unwrap(tx) {
                    Ok(tx) => {
                        let _ = tx.rollback().await;
                    }
                    Err(_) => {
                        // Transaction is still referenced elsewhere
                        eprintln!("Warning: Could not rollback transaction - still referenced");
                    }
                }
            }
        }

        Ok(())
    }

    /// Execute a function with automatic transaction management
    pub async fn with_transaction<'a, T, F>(&'a self, f: F) -> DbResult<T>
    where
        F: FnOnce(Arc<Transaction>) -> futures::future::BoxFuture<'a, DbResult<T>> + Send,
        T: Send,
    {
        let tx = self.begin_transaction().await?;
        let id = tx.id();

        match f(tx.clone()).await {
            Ok(result) => {
                // Remove from tracking before commit
                self.remove_transaction(id).await;

                // Try to get exclusive ownership for commit
                match Arc::try_unwrap(tx) {
                    Ok(tx) => {
                        tx.commit().await?;
                    }
                    Err(_tx) => {
                        // Still referenced, can't commit
                        return Err(DbError::Transaction(
                            "Cannot commit transaction - still referenced".into(),
                        ));
                    }
                }
                Ok(result)
            }
            Err(e) => {
                // Remove from tracking before rollback
                self.remove_transaction(id).await;

                // Try to rollback
                match Arc::try_unwrap(tx) {
                    Ok(tx) => {
                        let _ = tx.rollback().await;
                    }
                    Err(_) => {
                        // Transaction still referenced, will rollback on drop
                    }
                }
                Err(e)
            }
        }
    }

    /// Create a transaction scope for automatic cleanup
    pub async fn create_scope(&self) -> TransactionScope {
        TransactionScope::new(self).await
    }
}

/// Transaction scope for automatic cleanup
pub struct TransactionScope {
    manager: Arc<TransactionManager>,
    transactions: Vec<Uuid>,
}

impl TransactionScope {
    pub(crate) async fn new(manager: &TransactionManager) -> Self {
        Self {
            manager: Arc::new(TransactionManager::new(manager.connection.clone())),
            transactions: Vec::new(),
        }
    }

    /// Begin a transaction in this scope
    pub async fn begin_transaction(&mut self) -> DbResult<Arc<Transaction>> {
        let tx = self.manager.begin_transaction().await?;
        self.transactions.push(tx.id());
        Ok(tx)
    }

    /// Rollback all transactions in this scope
    pub async fn rollback_all(&mut self) -> DbResult<()> {
        for id in self.transactions.drain(..) {
            if let Some(tx) = self.manager.get_transaction(id).await {
                if tx.is_active().await {
                    self.manager.remove_transaction(id).await;
                    match Arc::try_unwrap(tx) {
                        Ok(tx) => {
                            let _ = tx.rollback().await;
                        }
                        Err(_) => {
                            // Still referenced
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl Drop for TransactionScope {
    fn drop(&mut self) {
        if !self.transactions.is_empty() {
            let transactions = std::mem::take(&mut self.transactions);
            let manager = self.manager.clone();

            // Schedule cleanup
            tokio::spawn(async move {
                for id in transactions {
                    if let Some(tx) = manager.get_transaction(id).await {
                        if tx.is_active().await {
                            eprintln!(
                                "Warning: Transaction {} in scope not explicitly handled",
                                id
                            );
                        }
                    }
                }
            });
        }
    }
}
