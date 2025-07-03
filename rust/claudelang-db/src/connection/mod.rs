//! Database connection management with DI integration

use std::sync::Arc;
use tokio::sync::RwLock;
use sqlx::AnyPool;
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
    
    /// Execute a query
    pub async fn execute(&self, query: &str, _params: Vec<sqlx::any::AnyValue>) -> DbResult<u64> {
        // TODO: Bind parameters properly
        let result = sqlx::query(query)
            .execute(&self.pool)
            .await?;
        Ok(result.rows_affected())
    }
    
    /// Fetch one row
    pub async fn fetch_one(&self, query: &str, _params: Vec<sqlx::any::AnyValue>) -> DbResult<sqlx::any::AnyRow> {
        // TODO: Bind parameters properly
        let row = sqlx::query(query)
            .fetch_one(&self.pool)
            .await?;
        Ok(row)
    }
    
    /// Fetch all rows
    pub async fn fetch_all(&self, query: &str, _params: Vec<sqlx::any::AnyValue>) -> DbResult<Vec<sqlx::any::AnyRow>> {
        // TODO: Bind parameters properly
        let rows = sqlx::query(query)
            .fetch_all(&self.pool)
            .await?;
        Ok(rows)
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

/// Transaction handle
pub struct Transaction {
    // TODO: Implement actual transaction handling with SQLX
    connection: DbConnection,
    active: bool,
}

impl Transaction {
    pub async fn begin(connection: DbConnection) -> DbResult<Self> {
        // TODO: Begin actual transaction
        Ok(Self {
            connection,
            active: true,
        })
    }
    
    pub async fn commit(mut self) -> DbResult<()> {
        if !self.active {
            return Err(DbError::Transaction("Transaction already completed".into()));
        }
        // TODO: Commit actual transaction
        self.active = false;
        Ok(())
    }
    
    pub async fn rollback(mut self) -> DbResult<()> {
        if !self.active {
            return Err(DbError::Transaction("Transaction already completed".into()));
        }
        // TODO: Rollback actual transaction
        self.active = false;
        Ok(())
    }
    
    pub fn connection(&self) -> &DbConnection {
        &self.connection
    }
}

impl Drop for Transaction {
    fn drop(&mut self) {
        if self.active {
            // Transaction was not explicitly committed or rolled back
            // In production, this should rollback the transaction
            eprintln!("Warning: Transaction dropped without explicit commit or rollback");
        }
    }
}