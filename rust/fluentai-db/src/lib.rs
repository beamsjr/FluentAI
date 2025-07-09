//! Functional Database Effect System for FluentAi
//!
//! This crate provides a functional approach to database operations that integrates
//! with FluentAi's effect system and maintains functional purity.
//!
//! The framework now includes AI-first graph-based schema representation that
//! integrates with FluentAi's AST structure for better analysis and optimization.

pub mod connection;
pub mod di;
pub mod effects;
pub mod error;
pub mod migration;
pub mod query;
pub mod schema;
pub mod transaction;

// AI-first graph-based schema representation
pub mod graph_schema;

pub use connection::{ConnectionPool, DbConnection};
pub use di::{
    ContainerDatabaseProvider, DatabaseContainerBuilderExt, DatabaseModule,
    DatabaseServiceProvider, DatabaseServicesBuilder,
};
pub use effects::{DbEffectType, DbHandler};
pub use error::{DbError, DbResult};
pub use migration::{
    Direction, Migration, MigrationBuilder, MigrationPlan, MigrationRepository, MigrationRunner,
    SqlMigration,
};
pub use query::{Query, QueryBuilder, QueryExpr};
pub use schema::{FieldType, Schema, SchemaBuilder};
pub use transaction::{
    with_transaction, with_transaction_retry, IsolationLevel, Transaction, TransactionManager,
    TransactionOptions,
};

// Graph-based schema exports
pub use graph_schema::{
    CacheStrategy, ColumnNode, ColumnStatistics, ConstraintNode, ConstraintType, IndexNode,
    IndexType, PartitioningStrategy, RelationshipEdge, RelationshipType, SchemaAnalysis,
    SchemaGraph, SchemaGraphBuilder, SchemaMetadata, TableHints, TableNode,
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
