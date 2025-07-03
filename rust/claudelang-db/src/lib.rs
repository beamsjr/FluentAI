//! Functional Database Effect System for ClaudeLang
//! 
//! This crate provides a functional approach to database operations that integrates
//! with ClaudeLang's effect system and maintains functional purity.

pub mod effects;
pub mod query;
pub mod schema;
pub mod connection;
pub mod error;

pub use effects::{DbEffectType, DbHandler};
pub use query::{Query, QueryBuilder, QueryExpr};
pub use schema::{Schema, SchemaBuilder, FieldType};
pub use connection::{ConnectionPool, DbConnection};
pub use error::{DbError, DbResult};

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