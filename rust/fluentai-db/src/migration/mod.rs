//! Database migration system for FluentAi

use crate::connection::DbConnection;
use crate::error::DbResult;
use crate::transaction::Transaction;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

pub mod builder;
pub mod runner;
pub mod versioning;

pub use builder::MigrationBuilder;
pub use runner::{MigrationRunner, MigrationStatus};
pub use versioning::{Version, VersionScheme};

/// Migration direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Up,
    Down,
}

/// Migration metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationMetadata {
    pub version: String,
    pub name: String,
    pub description: Option<String>,
    pub checksum: String,
    pub applied_at: Option<DateTime<Utc>>,
    pub execution_time_ms: Option<u64>,
}

/// Migration trait for custom migrations
#[async_trait::async_trait]
pub trait Migration: Send + Sync {
    /// Get migration version
    fn version(&self) -> &str;

    /// Get migration name
    fn name(&self) -> &str;

    /// Get migration description
    fn description(&self) -> Option<&str> {
        None
    }

    /// Execute the up migration
    async fn up(&self, tx: &Transaction) -> DbResult<()>;

    /// Execute the down migration
    async fn down(&self, tx: &Transaction) -> DbResult<()>;

    /// Get migration checksum
    fn checksum(&self) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.version().hash(&mut hasher);
        self.name().hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }
}

/// SQL-based migration
pub struct SqlMigration {
    version: String,
    name: String,
    description: Option<String>,
    up_sql: String,
    down_sql: String,
}

impl SqlMigration {
    pub fn new(
        version: impl Into<String>,
        name: impl Into<String>,
        up_sql: impl Into<String>,
        down_sql: impl Into<String>,
    ) -> Self {
        Self {
            version: version.into(),
            name: name.into(),
            description: None,
            up_sql: up_sql.into(),
            down_sql: down_sql.into(),
        }
    }

    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

#[async_trait::async_trait]
impl Migration for SqlMigration {
    fn version(&self) -> &str {
        &self.version
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    async fn up(&self, tx: &Transaction) -> DbResult<()> {
        // Split SQL by semicolons and execute each statement
        for statement in self.up_sql.split(';').filter(|s| !s.trim().is_empty()) {
            tx.execute(statement.trim(), vec![]).await?;
        }
        Ok(())
    }

    async fn down(&self, tx: &Transaction) -> DbResult<()> {
        // Split SQL by semicolons and execute each statement
        for statement in self.down_sql.split(';').filter(|s| !s.trim().is_empty()) {
            tx.execute(statement.trim(), vec![]).await?;
        }
        Ok(())
    }

    fn checksum(&self) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        self.version.hash(&mut hasher);
        self.name.hash(&mut hasher);
        self.up_sql.hash(&mut hasher);
        self.down_sql.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }
}

/// Migration repository for storing migration state
pub struct MigrationRepository {
    connection: Arc<DbConnection>,
    table_name: String,
}

impl MigrationRepository {
    /// Create a new migration repository
    pub fn new(connection: Arc<DbConnection>) -> Self {
        Self::with_table_name(connection, "_migrations")
    }

    /// Create with custom table name
    pub fn with_table_name(connection: Arc<DbConnection>, table_name: impl Into<String>) -> Self {
        Self {
            connection,
            table_name: table_name.into(),
        }
    }

    /// Initialize the migration table
    pub async fn init(&self) -> DbResult<()> {
        let query = format!(
            r#"
            CREATE TABLE IF NOT EXISTS {} (
                version VARCHAR(255) PRIMARY KEY,
                name VARCHAR(255) NOT NULL,
                description TEXT,
                checksum VARCHAR(64) NOT NULL,
                applied_at TIMESTAMP NOT NULL,
                execution_time_ms BIGINT
            )
            "#,
            self.table_name
        );

        self.connection.execute_raw_unsafe(&query).await?;
        Ok(())
    }

    /// Check if migration table exists
    pub async fn table_exists(&self) -> DbResult<bool> {
        // This is database-specific, using a generic approach
        let result = self
            .connection
            .fetch_all(
                "SELECT 1 FROM information_schema.tables WHERE table_name = ?",
                vec![fluentai_vm::Value::String(self.table_name.clone())],
            )
            .await;

        match result {
            Ok(rows) => Ok(!rows.is_empty()),
            Err(_) => {
                // Fallback: try to query the table
                let query = format!("SELECT 1 FROM {} LIMIT 1", self.table_name);
                match self.connection.execute_raw_unsafe(&query).await {
                    Ok(_) => Ok(true),
                    Err(_) => Ok(false),
                }
            }
        }
    }

    /// Get all applied migrations
    pub async fn get_applied_migrations(&self) -> DbResult<Vec<MigrationMetadata>> {
        let query = format!(
            "SELECT version, name, description, checksum, applied_at, execution_time_ms 
             FROM {} ORDER BY version",
            self.table_name
        );

        let rows = self.connection.fetch_all(&query, vec![]).await?;

        let mut migrations = Vec::new();
        for row in rows {
            use sqlx::Row;

            migrations.push(MigrationMetadata {
                version: row.try_get("version").unwrap_or_default(),
                name: row.try_get("name").unwrap_or_default(),
                description: row.try_get("description").ok(),
                checksum: row.try_get("checksum").unwrap_or_default(),
                applied_at: row
                    .try_get::<String, _>("applied_at")
                    .ok()
                    .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                    .map(|dt| dt.with_timezone(&Utc)),
                execution_time_ms: row
                    .try_get::<i64, _>("execution_time_ms")
                    .ok()
                    .map(|i| i as u64),
            });
        }

        Ok(migrations)
    }

    /// Check if a migration is applied
    pub async fn is_applied(&self, version: &str) -> DbResult<bool> {
        let query = format!("SELECT 1 FROM {} WHERE version = ?", self.table_name);

        let rows = self
            .connection
            .fetch_all(
                &query,
                vec![fluentai_vm::Value::String(version.to_string())],
            )
            .await?;

        Ok(!rows.is_empty())
    }

    /// Record a migration as applied
    pub async fn record_migration(
        &self,
        tx: &Transaction,
        metadata: &MigrationMetadata,
    ) -> DbResult<()> {
        let query = format!(
            r#"
            INSERT INTO {} (version, name, description, checksum, applied_at, execution_time_ms)
            VALUES (?, ?, ?, ?, ?, ?)
            "#,
            self.table_name
        );

        let params = vec![
            fluentai_vm::Value::String(metadata.version.clone()),
            fluentai_vm::Value::String(metadata.name.clone()),
            metadata
                .description
                .as_ref()
                .map(|d| fluentai_vm::Value::String(d.clone()))
                .unwrap_or(fluentai_vm::Value::Nil),
            fluentai_vm::Value::String(metadata.checksum.clone()),
            fluentai_vm::Value::String(metadata.applied_at.unwrap_or_else(Utc::now).to_rfc3339()),
            metadata
                .execution_time_ms
                .map(|ms| fluentai_vm::Value::Integer(ms as i64))
                .unwrap_or(fluentai_vm::Value::Nil),
        ];

        tx.execute(&query, params).await?;
        Ok(())
    }

    /// Remove a migration record
    pub async fn remove_migration(&self, tx: &Transaction, version: &str) -> DbResult<()> {
        let query = format!("DELETE FROM {} WHERE version = ?", self.table_name);

        tx.execute(
            &query,
            vec![fluentai_vm::Value::String(version.to_string())],
        )
        .await?;
        Ok(())
    }

    /// Get the latest applied version
    pub async fn get_latest_version(&self) -> DbResult<Option<String>> {
        let query = format!(
            "SELECT version FROM {} ORDER BY version DESC LIMIT 1",
            self.table_name
        );

        let rows = self.connection.fetch_all(&query, vec![]).await?;

        if let Some(row) = rows.first() {
            use sqlx::Row;
            Ok(row.try_get("version").ok())
        } else {
            Ok(None)
        }
    }
}

/// Migration plan
pub struct MigrationPlan {
    pub direction: Direction,
    pub migrations: Vec<Arc<dyn Migration>>,
}

impl MigrationPlan {
    /// Create a new migration plan
    pub fn new(direction: Direction) -> Self {
        Self {
            direction,
            migrations: Vec::new(),
        }
    }

    /// Add a migration to the plan
    pub fn add_migration(&mut self, migration: Arc<dyn Migration>) {
        self.migrations.push(migration);
    }

    /// Get the number of migrations in the plan
    pub fn len(&self) -> usize {
        self.migrations.len()
    }

    /// Check if the plan is empty
    pub fn is_empty(&self) -> bool {
        self.migrations.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sql_migration() {
        let migration = SqlMigration::new(
            "001",
            "create_users",
            "CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(255))",
            "DROP TABLE users",
        );

        assert_eq!(migration.version(), "001");
        assert_eq!(migration.name(), "create_users");
        assert!(migration.description().is_none());

        // Test with description
        let migration = migration.with_description("Create users table");
        assert_eq!(migration.description(), Some("Create users table"));
    }

    #[test]
    fn test_migration_checksum() {
        let migration1 = SqlMigration::new(
            "001",
            "create_users",
            "CREATE TABLE users (id INT)",
            "DROP TABLE users",
        );

        let migration2 = SqlMigration::new(
            "001",
            "create_users",
            "CREATE TABLE users (id INT)", // Same content
            "DROP TABLE users",
        );

        let migration3 = SqlMigration::new(
            "001",
            "create_users",
            "CREATE TABLE users (id INT, name VARCHAR(255))", // Different content
            "DROP TABLE users",
        );

        // Same migrations should have same checksum
        assert_eq!(migration1.checksum(), migration2.checksum());

        // Different migrations should have different checksums
        assert_ne!(migration1.checksum(), migration3.checksum());
    }

    #[test]
    fn test_migration_plan() {
        let mut plan = MigrationPlan::new(Direction::Up);
        assert!(plan.is_empty());

        let migration = Arc::new(SqlMigration::new("001", "test", "SELECT 1", "SELECT 0"));

        plan.add_migration(migration);
        assert_eq!(plan.len(), 1);
        assert!(!plan.is_empty());
    }
}
