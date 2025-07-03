//! Database connection management with DI integration

use std::sync::Arc;
use tokio::sync::RwLock;
use sqlx::{AnyPool, Any};
use crate::error::{DbError, DbResult};
use crate::DbConfig;

/// Database connection wrapper
#[derive(Clone)]
pub struct DbConnection {
    pool: AnyPool,
}

impl DbConnection {
    /// Create a new connection from config
    pub async fn new(config: &DbConfig) -> DbResult<Self> {
        let pool = AnyPool::connect(&config.url).await?;
        Ok(Self { pool })
    }
    
    /// Get the underlying pool
    pub fn pool(&self) -> &AnyPool {
        &self.pool
    }
    
    /// Execute a parameterized query safely
    pub async fn execute(&self, query: &str, params: Vec<claudelang_vm::bytecode::Value>) -> DbResult<u64> {
        let mut q = sqlx::query(query);
        
        // Bind all parameters safely
        for param in params {
            q = match param {
                claudelang_vm::bytecode::Value::Nil => q.bind(None::<i32>),
                claudelang_vm::bytecode::Value::Bool(b) => q.bind(b),
                claudelang_vm::bytecode::Value::Int(i) => q.bind(i),
                claudelang_vm::bytecode::Value::Float(f) => q.bind(f),
                claudelang_vm::bytecode::Value::String(s) => q.bind(s),
                // For binary data, we can use a List of integers and convert
                claudelang_vm::bytecode::Value::List(bytes) => {
                    let byte_vec: Result<Vec<u8>, _> = bytes.iter()
                        .map(|v| match v {
                            claudelang_vm::bytecode::Value::Int(i) => Ok(*i as u8),
                            _ => Err(DbError::Query("Binary data must be a list of integers".into())),
                        })
                        .collect();
                    q.bind(byte_vec?)
                }
                _ => return Err(DbError::Query(format!("Cannot bind value type: {:?}", param))),
            };
        }
        
        let result = q.execute(&self.pool).await?;
        Ok(result.rows_affected())
    }
    
    /// Fetch one row with parameterized query
    pub async fn fetch_one(&self, query: &str, params: Vec<claudelang_vm::bytecode::Value>) -> DbResult<sqlx::any::AnyRow> {
        let mut q = sqlx::query(query);
        
        // Bind all parameters safely
        for param in params {
            q = match param {
                claudelang_vm::bytecode::Value::Nil => q.bind(None::<i32>),
                claudelang_vm::bytecode::Value::Bool(b) => q.bind(b),
                claudelang_vm::bytecode::Value::Int(i) => q.bind(i),
                claudelang_vm::bytecode::Value::Float(f) => q.bind(f),
                claudelang_vm::bytecode::Value::String(s) => q.bind(s),
                // For binary data, we can use a List of integers and convert
                claudelang_vm::bytecode::Value::List(bytes) => {
                    let byte_vec: Result<Vec<u8>, _> = bytes.iter()
                        .map(|v| match v {
                            claudelang_vm::bytecode::Value::Int(i) => Ok(*i as u8),
                            _ => Err(DbError::Query("Binary data must be a list of integers".into())),
                        })
                        .collect();
                    q.bind(byte_vec?)
                }
                _ => return Err(DbError::Query(format!("Cannot bind value type: {:?}", param))),
            };
        }
        
        let row = q.fetch_one(&self.pool).await?;
        Ok(row)
    }
    
    /// Fetch all rows with parameterized query
    pub async fn fetch_all(&self, query: &str, params: Vec<claudelang_vm::bytecode::Value>) -> DbResult<Vec<sqlx::any::AnyRow>> {
        let mut q = sqlx::query(query);
        
        // Bind all parameters safely
        for param in params {
            q = match param {
                claudelang_vm::bytecode::Value::Nil => q.bind(None::<i32>),
                claudelang_vm::bytecode::Value::Bool(b) => q.bind(b),
                claudelang_vm::bytecode::Value::Int(i) => q.bind(i),
                claudelang_vm::bytecode::Value::Float(f) => q.bind(f),
                claudelang_vm::bytecode::Value::String(s) => q.bind(s),
                // For binary data, we can use a List of integers and convert
                claudelang_vm::bytecode::Value::List(bytes) => {
                    let byte_vec: Result<Vec<u8>, _> = bytes.iter()
                        .map(|v| match v {
                            claudelang_vm::bytecode::Value::Int(i) => Ok(*i as u8),
                            _ => Err(DbError::Query("Binary data must be a list of integers".into())),
                        })
                        .collect();
                    q.bind(byte_vec?)
                }
                _ => return Err(DbError::Query(format!("Cannot bind value type: {:?}", param))),
            };
        }
        
        let rows = q.fetch_all(&self.pool).await?;
        Ok(rows)
    }
    
    /// Execute a raw query WITHOUT parameters - DANGEROUS!
    /// Only use this for DDL statements or when you're absolutely sure the query is safe
    pub async fn execute_raw_unsafe(&self, query: &str) -> DbResult<u64> {
        eprintln!("WARNING: Using raw SQL execution without parameters. Ensure this query is safe!");
        let result = sqlx::query(query)
            .execute(&self.pool)
            .await?;
        Ok(result.rows_affected())
    }
    
    /// Check if connected
    pub async fn is_connected(&self) -> bool {
        !self.pool.is_closed()
    }
    
    /// Close the connection pool
    pub async fn close(&self) {
        self.pool.close().await;
    }
}

/// Connection pool for managing database connections
#[derive(Clone)]
pub struct ConnectionPool {
    inner: Arc<RwLock<ConnectionPoolInner>>,
}

struct ConnectionPoolInner {
    config: DbConfig,
    connection: Option<DbConnection>,
}

impl ConnectionPool {
    /// Create a new connection pool
    pub fn new(config: DbConfig) -> Self {
        Self {
            inner: Arc::new(RwLock::new(ConnectionPoolInner {
                config,
                connection: None,
            })),
        }
    }
    
    /// Get or create a connection
    pub async fn get_connection(&self) -> DbResult<DbConnection> {
        let mut inner = self.inner.write().await;
        
        if let Some(conn) = &inner.connection {
            if conn.is_connected().await {
                return Ok(conn.clone());
            }
        }
        
        // Create new connection
        let conn = DbConnection::new(&inner.config).await?;
        inner.connection = Some(conn.clone());
        Ok(conn)
    }
    
    /// Close the pool
    pub async fn close(&self) {
        let mut inner = self.inner.write().await;
        if let Some(conn) = &inner.connection {
            conn.close().await;
        }
        inner.connection = None;
    }
    
    /// Update configuration
    pub async fn update_config(&self, config: DbConfig) {
        let mut inner = self.inner.write().await;
        inner.config = config;
        // Close existing connection to force reconnect with new config
        if let Some(conn) = &inner.connection {
            conn.close().await;
        }
        inner.connection = None;
    }
}

/// DI service for database connections
pub struct DbConnectionService {
    pool: ConnectionPool,
}

impl DbConnectionService {
    pub fn new(config: DbConfig) -> Self {
        Self {
            pool: ConnectionPool::new(config),
        }
    }
    
    pub async fn get_connection(&self) -> DbResult<DbConnection> {
        self.pool.get_connection().await
    }
    
    pub fn pool(&self) -> &ConnectionPool {
        &self.pool
    }
}

/// Register database services with DI container builder
pub fn register_db_services(builder: &mut claudelang_di::builder::ContainerBuilder, config: DbConfig) -> DbResult<()> {
    // Clone config for each closure
    let config1 = config.clone();
    let config2 = config;
    
    // Register connection pool as singleton
    builder.register_singleton(move || {
        ConnectionPool::new(config1.clone())
    });
    
    // Register connection service
    builder.register_singleton(move || {
        DbConnectionService::new(config2.clone())
    });
    
    Ok(())
}

/// Transaction handle with RAII semantics
pub struct Transaction<'a> {
    connection: &'a DbConnection,
    tx: Option<sqlx::Transaction<'a, Any>>,
    committed: bool,
}

impl<'a> Transaction<'a> {
    /// Begin a new transaction
    pub async fn begin(connection: &'a DbConnection) -> DbResult<Self> {
        let tx = connection.pool().begin().await?;
        Ok(Self {
            connection,
            tx: Some(tx),
            committed: false,
        })
    }
    
    /// Execute a parameterized query within the transaction
    pub async fn execute(&mut self, query: &str, params: Vec<claudelang_vm::bytecode::Value>) -> DbResult<u64> {
        if let Some(tx) = &mut self.tx {
            let mut q = sqlx::query(query);
            
            // Bind parameters safely
            for param in params {
                q = match param {
                    claudelang_vm::bytecode::Value::Nil => q.bind(None::<i32>),
                    claudelang_vm::bytecode::Value::Bool(b) => q.bind(b),
                    claudelang_vm::bytecode::Value::Int(i) => q.bind(i),
                    claudelang_vm::bytecode::Value::Float(f) => q.bind(f),
                    claudelang_vm::bytecode::Value::String(s) => q.bind(s),
                    // For binary data, we can use a List of integers and convert
                    claudelang_vm::bytecode::Value::List(bytes) => {
                        let byte_vec: Result<Vec<u8>, _> = bytes.iter()
                            .map(|v| match v {
                                claudelang_vm::bytecode::Value::Int(i) => Ok(*i as u8),
                                _ => Err(DbError::Transaction("Binary data must be a list of integers".into())),
                            })
                            .collect();
                        q.bind(byte_vec?)
                    }
                    _ => return Err(DbError::Transaction(format!("Cannot bind value type: {:?}", param))),
                };
            }
            
            let result = q.execute(&mut **tx).await?;
            Ok(result.rows_affected())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }
    
    /// Commit the transaction
    pub async fn commit(mut self) -> DbResult<()> {
        if let Some(tx) = self.tx.take() {
            tx.commit().await?;
            self.committed = true;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }
    
    /// Rollback the transaction
    pub async fn rollback(mut self) -> DbResult<()> {
        if let Some(tx) = self.tx.take() {
            tx.rollback().await?;
            Ok(())
        } else {
            Err(DbError::Transaction("Transaction already completed".into()))
        }
    }
    
    pub fn connection(&self) -> &DbConnection {
        self.connection
    }
}

impl<'a> Drop for Transaction<'a> {
    fn drop(&mut self) {
        if self.tx.is_some() && !self.committed {
            // Transaction was not explicitly committed or rolled back
            // The SQLX transaction will automatically rollback on drop
            eprintln!("Warning: Transaction dropped without explicit commit or rollback - rolling back");
        }
    }
}