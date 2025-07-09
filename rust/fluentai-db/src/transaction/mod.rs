//! Advanced transaction management for FluentAi

use crate::connection::DbConnection;
use crate::error::{DbError, DbResult};
use sqlx::Any;
use std::sync::Arc;
use tokio::sync::Mutex;
use uuid::Uuid;

pub mod manager;
pub mod savepoint;
pub mod scope;

#[cfg(test)]
mod tests;

pub use manager::TransactionManager;
pub use savepoint::SavepointManager;
pub use scope::TransactionScope;

/// Transaction isolation levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsolationLevel {
    ReadUncommitted,
    ReadCommitted,
    RepeatableRead,
    Serializable,
}

impl IsolationLevel {
    /// Convert to SQL string
    pub fn to_sql(&self) -> &'static str {
        match self {
            Self::ReadUncommitted => "READ UNCOMMITTED",
            Self::ReadCommitted => "READ COMMITTED",
            Self::RepeatableRead => "REPEATABLE READ",
            Self::Serializable => "SERIALIZABLE",
        }
    }
}

/// Transaction options
#[derive(Debug, Clone)]
pub struct TransactionOptions {
    pub isolation_level: Option<IsolationLevel>,
    pub read_only: bool,
    pub deferrable: bool,
    pub timeout_ms: Option<u64>,
}

impl Default for TransactionOptions {
    fn default() -> Self {
        Self {
            isolation_level: None,
            read_only: false,
            deferrable: false,
            timeout_ms: None,
        }
    }
}

/// Enhanced transaction handle with advanced features
pub struct Transaction {
    id: Uuid,
    connection: Arc<DbConnection>,
    inner: Arc<Mutex<TransactionInner>>,
    savepoint_manager: Arc<SavepointManager>,
    #[allow(dead_code)]
    options: TransactionOptions,
}

struct TransactionInner {
    tx: Option<sqlx::Transaction<'static, Any>>,
    committed: bool,
    depth: usize,
}

impl Transaction {
    /// Begin a new transaction with options
    pub async fn begin_with_options(
        connection: Arc<DbConnection>,
        options: TransactionOptions,
    ) -> DbResult<Self> {
        // Apply transaction options
        if let Some(level) = options.isolation_level {
            let query = format!("SET TRANSACTION ISOLATION LEVEL {}", level.to_sql());
            connection.execute_raw_unsafe(&query).await?;
        }

        if options.read_only {
            connection
                .execute_raw_unsafe("SET TRANSACTION READ ONLY")
                .await?;
        }

        // Begin transaction
        let pool = connection.pool();
        let tx = pool.begin().await?;

        // Convert to 'static lifetime using unsafe transmute
        // This is safe because we manage the transaction lifetime manually
        let tx: sqlx::Transaction<'static, Any> = unsafe { std::mem::transmute(tx) };

        let id = Uuid::new_v4();
        let savepoint_manager = Arc::new(SavepointManager::new());

        Ok(Self {
            id,
            connection,
            inner: Arc::new(Mutex::new(TransactionInner {
                tx: Some(tx),
                committed: false,
                depth: 0,
            })),
            savepoint_manager,
            options,
        })
    }

    /// Begin a new transaction with default options
    pub async fn begin(connection: Arc<DbConnection>) -> DbResult<Self> {
        Self::begin_with_options(connection, TransactionOptions::default()).await
    }

    /// Get transaction ID
    pub fn id(&self) -> Uuid {
        self.id
    }

    /// Get transaction depth (for nested transactions)
    pub async fn depth(&self) -> usize {
        self.inner.lock().await.depth
    }

    /// Create a savepoint
    pub async fn savepoint(&self, name: &str) -> DbResult<()> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = &mut inner.tx {
            let query = format!("SAVEPOINT {}", name);
            sqlx::query(&query).execute(&mut **tx).await?;
            self.savepoint_manager.add_savepoint(name).await;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Release a savepoint
    pub async fn release_savepoint(&self, name: &str) -> DbResult<()> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = &mut inner.tx {
            let query = format!("RELEASE SAVEPOINT {}", name);
            sqlx::query(&query).execute(&mut **tx).await?;
            self.savepoint_manager.remove_savepoint(name).await;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Rollback to a savepoint
    pub async fn rollback_to_savepoint(&self, name: &str) -> DbResult<()> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = &mut inner.tx {
            let query = format!("ROLLBACK TO SAVEPOINT {}", name);
            sqlx::query(&query).execute(&mut **tx).await?;
            self.savepoint_manager.rollback_to(name).await;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Execute a query within the transaction
    pub async fn execute(&self, query: &str, params: Vec<fluentai_vm::Value>) -> DbResult<u64> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = &mut inner.tx {
            let mut q = sqlx::query(query);

            // Bind parameters safely
            for param in params {
                q = match param {
                    fluentai_vm::Value::Nil => q.bind(None::<i32>),
                    fluentai_vm::Value::Boolean(b) => q.bind(b),
                    fluentai_vm::Value::Integer(i) => q.bind(i),
                    fluentai_vm::Value::Float(f) => q.bind(f),
                    fluentai_vm::Value::String(s) => q.bind(s),
                    fluentai_vm::Value::List(bytes) => {
                        let byte_vec: Result<Vec<u8>, _> = bytes
                            .iter()
                            .map(|v| match v {
                                fluentai_vm::Value::Integer(i) => Ok(*i as u8),
                                _ => Err(DbError::Transaction(
                                    "Binary data must be a list of integers".into(),
                                )),
                            })
                            .collect();
                        q.bind(byte_vec?)
                    }
                    _ => {
                        return Err(DbError::Transaction(format!(
                            "Cannot bind value type: {:?}",
                            param
                        )))
                    }
                };
            }

            let result = q.execute(&mut **tx).await?;
            Ok(result.rows_affected())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Fetch all rows
    pub async fn fetch_all(
        &self,
        query: &str,
        params: Vec<fluentai_vm::Value>,
    ) -> DbResult<Vec<sqlx::any::AnyRow>> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = &mut inner.tx {
            let mut q = sqlx::query(query);

            // Bind parameters
            for param in params {
                q = match param {
                    fluentai_vm::Value::Nil => q.bind(None::<i32>),
                    fluentai_vm::Value::Boolean(b) => q.bind(b),
                    fluentai_vm::Value::Integer(i) => q.bind(i),
                    fluentai_vm::Value::Float(f) => q.bind(f),
                    fluentai_vm::Value::String(s) => q.bind(s),
                    fluentai_vm::Value::List(bytes) => {
                        let byte_vec: Result<Vec<u8>, _> = bytes
                            .iter()
                            .map(|v| match v {
                                fluentai_vm::Value::Integer(i) => Ok(*i as u8),
                                _ => Err(DbError::Transaction(
                                    "Binary data must be a list of integers".into(),
                                )),
                            })
                            .collect();
                        q.bind(byte_vec?)
                    }
                    _ => {
                        return Err(DbError::Transaction(format!(
                            "Cannot bind value type: {:?}",
                            param
                        )))
                    }
                };
            }

            let rows = q.fetch_all(&mut **tx).await?;
            Ok(rows)
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Commit the transaction
    pub async fn commit(self) -> DbResult<()> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = inner.tx.take() {
            tx.commit().await?;
            inner.committed = true;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Rollback the transaction
    pub async fn rollback(self) -> DbResult<()> {
        let mut inner = self.inner.lock().await;
        if let Some(tx) = inner.tx.take() {
            tx.rollback().await?;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }

    /// Check if transaction is still active
    pub async fn is_active(&self) -> bool {
        let inner = self.inner.lock().await;
        inner.tx.is_some() && !inner.committed
    }

    /// Get connection
    pub fn connection(&self) -> &Arc<DbConnection> {
        &self.connection
    }
}

impl Drop for Transaction {
    fn drop(&mut self) {
        // Transaction will be automatically rolled back by SQLx if not committed
        let inner = self.inner.clone();
        tokio::spawn(async move {
            let inner = inner.lock().await;
            if inner.tx.is_some() && !inner.committed {
                eprintln!(
                    "Warning: Transaction {} dropped without explicit commit or rollback",
                    inner.depth
                );
            }
        });
    }
}

/// Transaction callback type
pub type TransactionCallback<T> =
    Box<dyn FnOnce(&Transaction) -> futures::future::BoxFuture<'_, DbResult<T>> + Send>;

/// Execute a function within a transaction
pub async fn with_transaction<T, F>(connection: Arc<DbConnection>, callback: F) -> DbResult<T>
where
    F: FnOnce(&Transaction) -> futures::future::BoxFuture<'_, DbResult<T>> + Send,
    T: Send,
{
    let tx = Transaction::begin(connection).await?;
    match callback(&tx).await {
        Ok(result) => {
            tx.commit().await?;
            Ok(result)
        }
        Err(e) => {
            let _ = tx.rollback().await;
            Err(e)
        }
    }
}

/// Execute a function within a transaction with retry logic
pub async fn with_transaction_retry<T, F>(
    connection: Arc<DbConnection>,
    max_retries: u32,
    callback: F,
) -> DbResult<T>
where
    F: Fn(&Transaction) -> futures::future::BoxFuture<'_, DbResult<T>> + Send,
    T: Send,
{
    let mut retries = 0;
    loop {
        let tx = Transaction::begin(connection.clone()).await?;
        match callback(&tx).await {
            Ok(result) => match tx.commit().await {
                Ok(()) => return Ok(result),
                Err(e) => {
                    if retries < max_retries && is_retryable_error(&e) {
                        retries += 1;
                        tokio::time::sleep(tokio::time::Duration::from_millis(
                            100 * retries as u64,
                        ))
                        .await;
                        continue;
                    }
                    return Err(e);
                }
            },
            Err(e) => {
                let _ = tx.rollback().await;
                if retries < max_retries && is_retryable_error(&e) {
                    retries += 1;
                    tokio::time::sleep(tokio::time::Duration::from_millis(100 * retries as u64))
                        .await;
                    continue;
                }
                return Err(e);
            }
        }
    }
}

/// Check if error is retryable (deadlock, serialization failure, etc.)
fn is_retryable_error(error: &DbError) -> bool {
    match error {
        DbError::Query(msg) | DbError::Transaction(msg) => {
            msg.contains("deadlock")
                || msg.contains("serialization")
                || msg.contains("could not serialize")
        }
        _ => false,
    }
}
