//! Functional Database Effect System for FluentAi
//! 
//! This crate provides a functional approach to database operations that integrates
//! with FluentAi's effect system and maintains functional purity.

pub mod effects;
pub mod query;
pub mod schema;
pub mod connection;
pub mod error;
pub mod transaction;
pub mod migration;
pub mod di;

pub use effects::{DbEffectType, DbHandler};
pub use query::{Query, QueryBuilder, QueryExpr};
pub use schema::{Schema, SchemaBuilder, FieldType};
pub use connection::{ConnectionPool, DbConnection};
pub use error::{DbError, DbResult};
pub use transaction::{
    Transaction, TransactionManager, TransactionOptions, 
    IsolationLevel, with_transaction, with_transaction_retry
};
pub use migration::{
    Migration, SqlMigration, MigrationBuilder, MigrationRunner,
    MigrationRepository, MigrationPlan, Direction
};
pub use di::{
    DatabaseModule, DatabaseServiceProvider, ContainerDatabaseProvider,
    DatabaseContainerBuilderExt, DatabaseServicesBuilder
};

/// Database configuration
#[derive(Debug, Clone)]
pub struct DbConfig {
    pub url: String,
    pub max_connections: u32,
    pub min_connections: u32,
    pub connect_timeout: std::time::Duration,
    pub idle_timeout: Option<std::time::Duration>,
}

impl Default for DbConfig {
    fn default() -> Self {
        Self {
            url: String::new(),
            max_connections: 10,
            min_connections: 1,
            connect_timeout: std::time::Duration::from_secs(30),
            idle_timeout: Some(std::time::Duration::from_secs(600)),
        }
    }
}