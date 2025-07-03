//! Dependency injection integration for the database system

use std::sync::Arc;
use claudelang_di::prelude::*;
use crate::{
    DbConfig, DbConnection, ConnectionPool, DbHandler,
    TransactionManager, MigrationRunner,
    error::DbResult,
};

// Note: Service trait is automatically implemented for all types that are 
// Any + Send + Sync via blanket implementation in claudelang-di

/// Database services registration module
pub struct DatabaseModule {
    config: DbConfig,
}

impl DatabaseModule {
    /// Create a new database module with configuration
    pub fn new(config: DbConfig) -> Self {
        Self { config }
    }
    
    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self {
            config: DbConfig::default(),
        }
    }
    
    /// Register all database services
    pub fn register(&self, builder: &mut ContainerBuilder) -> DbResult<()> {
        let config = self.config.clone();
        
        // Register database configuration
        builder.register_instance(config.clone());
        
        // Register connection pool as singleton
        builder.register_singleton({
            let config = config.clone();
            move || {
                Arc::new(ConnectionPool::new(config.clone()))
            }
        });
        
        // Register database connection factory as transient
        // Note: We'll use the connection pool from the container after building
        let config2 = config.clone();
        builder.register_transient(move || {
            let pool = ConnectionPool::new(config2.clone());
            // Create async runtime for connection
            let rt = tokio::runtime::Handle::current();
            let conn = rt.block_on(async {
                pool.get_connection().await
                    .expect("Failed to get connection")
            });
            Arc::new(conn)
        });
        
        // Register transaction manager as transient
        let config3 = config.clone();
        builder.register_transient(move || {
            let pool = ConnectionPool::new(config3.clone());
            let rt = tokio::runtime::Handle::current();
            let conn = rt.block_on(async {
                pool.get_connection().await
                    .expect("Failed to get connection")
            });
            Arc::new(TransactionManager::new(Arc::new(conn)))
        });
        
        // Register migration runner as transient
        let config4 = config.clone();
        builder.register_transient(move || {
            let pool = ConnectionPool::new(config4.clone());
            let rt = tokio::runtime::Handle::current();
            let conn = rt.block_on(async {
                pool.get_connection().await
                    .expect("Failed to get connection")
            });
            Arc::new(MigrationRunner::new(Arc::new(conn)))
        });
        
        // Register database effect handler
        builder.register_singleton({
            let config = config.clone();
            move || {
                let pool = ConnectionPool::new(config.clone());
                Arc::new(DbHandler::with_pool(pool))
            }
        });
        
        Ok(())
    }
}

/// Database service provider trait
pub trait DatabaseServiceProvider {
    /// Get database connection
    fn get_connection(&self) -> DbResult<Arc<DbConnection>>;
    
    /// Get transaction manager
    fn get_transaction_manager(&self) -> DbResult<Arc<TransactionManager>>;
    
    /// Get migration runner
    fn get_migration_runner(&self) -> DbResult<Arc<MigrationRunner>>;
}

/// Default implementation of DatabaseServiceProvider using DI container
pub struct ContainerDatabaseProvider {
    container: Arc<Container>,
}

impl ContainerDatabaseProvider {
    pub fn new(container: Arc<Container>) -> Self {
        Self { container }
    }
}

impl DatabaseServiceProvider for ContainerDatabaseProvider {
    fn get_connection(&self) -> DbResult<Arc<DbConnection>> {
        self.container.resolve::<Arc<DbConnection>>()
            .map_err(|e| crate::error::DbError::Other(e.into()))
    }
    
    fn get_transaction_manager(&self) -> DbResult<Arc<TransactionManager>> {
        self.container.resolve::<Arc<TransactionManager>>()
            .map_err(|e| crate::error::DbError::Other(e.into()))
    }
    
    fn get_migration_runner(&self) -> DbResult<Arc<MigrationRunner>> {
        self.container.resolve::<Arc<MigrationRunner>>()
            .map_err(|e| crate::error::DbError::Other(e.into()))
    }
}

/// Extension trait for ContainerBuilder
pub trait DatabaseContainerBuilderExt {
    /// Register database services with custom configuration
    fn register_database(&mut self, config: DbConfig) -> DbResult<()>;
    
    /// Register database services with default configuration
    fn register_database_with_defaults(&mut self) -> DbResult<()>;
}

impl DatabaseContainerBuilderExt for ContainerBuilder {
    fn register_database(&mut self, config: DbConfig) -> DbResult<()> {
        let module = DatabaseModule::new(config);
        module.register(self)
    }
    
    fn register_database_with_defaults(&mut self) -> DbResult<()> {
        let module = DatabaseModule::with_defaults();
        module.register(self)
    }
}

/// Builder for database services with DI
pub struct DatabaseServicesBuilder {
    config: DbConfig,
    enable_migrations: bool,
    enable_transactions: bool,
    enable_effects: bool,
}

impl DatabaseServicesBuilder {
    pub fn new() -> Self {
        Self {
            config: DbConfig::default(),
            enable_migrations: true,
            enable_transactions: true,
            enable_effects: true,
        }
    }
    
    pub fn with_config(mut self, config: DbConfig) -> Self {
        self.config = config;
        self
    }
    
    pub fn with_url(mut self, url: impl Into<String>) -> Self {
        self.config.url = url.into();
        self
    }
    
    pub fn with_max_connections(mut self, max: u32) -> Self {
        self.config.max_connections = max;
        self
    }
    
    pub fn enable_migrations(mut self, enable: bool) -> Self {
        self.enable_migrations = enable;
        self
    }
    
    pub fn enable_transactions(mut self, enable: bool) -> Self {
        self.enable_transactions = enable;
        self
    }
    
    pub fn enable_effects(mut self, enable: bool) -> Self {
        self.enable_effects = enable;
        self
    }
    
    pub fn build(self, builder: &mut ContainerBuilder) -> DbResult<()> {
        // Register core database services
        builder.register_instance(self.config.clone());
        
        // Connection pool
        builder.register_singleton({
            let config = self.config.clone();
            move || Arc::new(ConnectionPool::new(config.clone()))
        });
        
        // Connection factory
        builder.register_transient({
            let config = self.config.clone();
            move || {
                let pool = ConnectionPool::new(config.clone());
                let rt = tokio::runtime::Handle::current();
                let conn = rt.block_on(async {
                    pool.get_connection().await
                        .expect("Failed to get connection")
                });
                Arc::new(conn)
            }
        });
        
        // Conditional service registration
        if self.enable_transactions {
            let config = self.config.clone();
            builder.register_transient(move || {
                let pool = ConnectionPool::new(config.clone());
                let rt = tokio::runtime::Handle::current();
                let conn = rt.block_on(async {
                    pool.get_connection().await
                        .expect("Failed to get connection")
                });
                Arc::new(TransactionManager::new(Arc::new(conn)))
            });
        }
        
        if self.enable_migrations {
            let config = self.config.clone();
            builder.register_transient(move || {
                let pool = ConnectionPool::new(config.clone());
                let rt = tokio::runtime::Handle::current();
                let conn = rt.block_on(async {
                    pool.get_connection().await
                        .expect("Failed to get connection")
                });
                Arc::new(MigrationRunner::new(Arc::new(conn)))
            });
        }
        
        if self.enable_effects {
            builder.register_singleton({
                let config = self.config.clone();
                move || {
                    let pool = ConnectionPool::new(config.clone());
                    Arc::new(DbHandler::with_pool(pool))
                }
            });
        }
        
        Ok(())
    }
}

impl Default for DatabaseServicesBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_database_module_creation() {
        let config = DbConfig {
            url: "sqlite::memory:".to_string(),
            ..Default::default()
        };
        
        let module = DatabaseModule::new(config);
        assert_eq!(module.config.url, "sqlite::memory:");
    }
    
    #[test]
    fn test_database_services_builder() {
        let builder = DatabaseServicesBuilder::new()
            .with_url("postgres://localhost/test")
            .with_max_connections(20)
            .enable_migrations(false);
        
        assert_eq!(builder.config.url, "postgres://localhost/test");
        assert_eq!(builder.config.max_connections, 20);
        assert!(!builder.enable_migrations);
    }
}