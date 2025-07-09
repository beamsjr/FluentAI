//! Migration runner for executing migrations

use super::{Direction, Migration, MigrationMetadata, MigrationPlan, MigrationRepository};
use crate::connection::DbConnection;
use crate::error::{DbError, DbResult};
use crate::transaction::{Transaction, TransactionOptions};
use chrono::Utc;
use std::collections::HashMap;
use std::sync::Arc;

/// Migration runner configuration
#[derive(Debug, Clone)]
pub struct MigrationRunnerConfig {
    /// Whether to run in dry-run mode
    pub dry_run: bool,
    /// Whether to validate checksums
    pub validate_checksums: bool,
    /// Transaction options for migrations
    pub transaction_options: TransactionOptions,
    /// Whether to allow out-of-order migrations
    pub allow_out_of_order: bool,
}

impl Default for MigrationRunnerConfig {
    fn default() -> Self {
        Self {
            dry_run: false,
            validate_checksums: true,
            transaction_options: TransactionOptions::default(),
            allow_out_of_order: false,
        }
    }
}

/// Migration runner
pub struct MigrationRunner {
    connection: Arc<DbConnection>,
    repository: MigrationRepository,
    migrations: HashMap<String, Arc<dyn Migration>>,
    config: MigrationRunnerConfig,
}

impl MigrationRunner {
    /// Create a new migration runner
    pub fn new(connection: Arc<DbConnection>) -> Self {
        Self {
            repository: MigrationRepository::new(connection.clone()),
            connection,
            migrations: HashMap::new(),
            config: MigrationRunnerConfig::default(),
        }
    }

    /// Create with custom configuration
    pub fn with_config(connection: Arc<DbConnection>, config: MigrationRunnerConfig) -> Self {
        Self {
            repository: MigrationRepository::new(connection.clone()),
            connection,
            migrations: HashMap::new(),
            config,
        }
    }

    /// Add a migration
    pub fn add_migration(&mut self, migration: Arc<dyn Migration>) {
        self.migrations
            .insert(migration.version().to_string(), migration);
    }

    /// Add multiple migrations
    pub fn add_migrations(&mut self, migrations: Vec<Arc<dyn Migration>>) {
        for migration in migrations {
            self.add_migration(migration);
        }
    }

    /// Initialize the migration system
    pub async fn init(&self) -> DbResult<()> {
        if self.config.dry_run {
            println!("[DRY RUN] Would create migration table");
            return Ok(());
        }

        self.repository.init().await?;
        Ok(())
    }

    /// Get migration status
    pub async fn status(&self) -> DbResult<Vec<MigrationStatus>> {
        let applied = self.repository.get_applied_migrations().await?;
        let applied_map: HashMap<_, _> = applied
            .into_iter()
            .map(|m| (m.version.clone(), m))
            .collect();

        let mut statuses = Vec::new();

        // Get all migrations sorted by version
        let mut versions: Vec<_> = self.migrations.keys().cloned().collect();
        versions.sort();

        for version in versions {
            if let Some(migration) = self.migrations.get(&version) {
                let status = if let Some(metadata) = applied_map.get(&version) {
                    // Check checksum
                    let checksum_valid = !self.config.validate_checksums
                        || metadata.checksum == migration.checksum();

                    MigrationStatus::Applied {
                        metadata: metadata.clone(),
                        checksum_valid,
                    }
                } else {
                    MigrationStatus::Pending {
                        migration: migration.clone(),
                    }
                };

                statuses.push(status);
            }
        }

        // Check for applied migrations not in our list
        for (version, metadata) in applied_map {
            if !self.migrations.contains_key(&version) {
                statuses.push(MigrationStatus::Missing { metadata });
            }
        }

        // Sort by version
        statuses.sort_by(|a, b| a.version().cmp(&b.version()));

        Ok(statuses)
    }

    /// Plan migrations to run
    pub async fn plan(&self, target: Option<&str>) -> DbResult<MigrationPlan> {
        let status = self.status().await?;
        let mut plan = MigrationPlan::new(Direction::Up);

        for s in status {
            match s {
                MigrationStatus::Pending { migration } => {
                    // Check if we've reached the target
                    if let Some(target_version) = target {
                        if migration.version() > target_version {
                            break;
                        }
                    }

                    plan.add_migration(migration);
                }
                MigrationStatus::Applied {
                    metadata,
                    checksum_valid,
                } => {
                    if !checksum_valid && self.config.validate_checksums {
                        return Err(DbError::Migration(format!(
                            "Checksum mismatch for migration {}",
                            metadata.version
                        )));
                    }
                }
                MigrationStatus::Missing { metadata } => {
                    if !self.config.allow_out_of_order {
                        return Err(DbError::Migration(format!(
                            "Missing migration {} that was previously applied",
                            metadata.version
                        )));
                    }
                }
            }
        }

        Ok(plan)
    }

    /// Plan rollback
    pub async fn plan_rollback(&self, target: Option<&str>) -> DbResult<MigrationPlan> {
        let status = self.status().await?;
        let mut plan = MigrationPlan::new(Direction::Down);

        // Process in reverse order
        for s in status.into_iter().rev() {
            match s {
                MigrationStatus::Applied { metadata, .. } => {
                    // Check if we've reached the target
                    if let Some(target_version) = target {
                        if metadata.version.as_str() <= target_version {
                            break;
                        }
                    }

                    if let Some(migration) = self.migrations.get(&metadata.version) {
                        plan.add_migration(migration.clone());
                    } else {
                        return Err(DbError::Migration(format!(
                            "Cannot rollback migration {} - not found",
                            metadata.version
                        )));
                    }
                }
                _ => {}
            }
        }

        Ok(plan)
    }

    /// Run migrations up to a target version
    pub async fn migrate(&self, target: Option<&str>) -> DbResult<MigrationResult> {
        let plan = self.plan(target).await?;
        self.execute_plan(plan).await
    }

    /// Rollback to a target version
    pub async fn rollback(&self, target: Option<&str>) -> DbResult<MigrationResult> {
        let plan = self.plan_rollback(target).await?;
        self.execute_plan(plan).await
    }

    /// Execute a migration plan
    async fn execute_plan(&self, plan: MigrationPlan) -> DbResult<MigrationResult> {
        let mut result = MigrationResult {
            direction: plan.direction,
            migrations_run: Vec::new(),
            dry_run: self.config.dry_run,
        };

        for migration in &plan.migrations {
            let start_time = std::time::Instant::now();

            match plan.direction {
                Direction::Up => {
                    self.run_migration_up(migration.clone(), &mut result)
                        .await?;
                }
                Direction::Down => {
                    self.run_migration_down(migration.clone(), &mut result)
                        .await?;
                }
            }

            let execution_time_ms = start_time.elapsed().as_millis() as u64;

            if let Some(last) = result.migrations_run.last_mut() {
                last.execution_time_ms = Some(execution_time_ms);
            }
        }

        Ok(result)
    }

    /// Run a single migration up
    async fn run_migration_up(
        &self,
        migration: Arc<dyn Migration>,
        result: &mut MigrationResult,
    ) -> DbResult<()> {
        let version = migration.version().to_string();
        let name = migration.name().to_string();

        println!("Migrating up: {} - {}", version, name);

        if self.config.dry_run {
            println!("[DRY RUN] Would run migration");
            result.migrations_run.push(MigrationRunInfo {
                version,
                name,
                direction: Direction::Up,
                execution_time_ms: None,
            });
            return Ok(());
        }

        // Start transaction
        let tx = Transaction::begin_with_options(
            self.connection.clone(),
            self.config.transaction_options.clone(),
        )
        .await?;

        // Run migration
        migration.up(&tx).await?;

        // Record migration
        let metadata = MigrationMetadata {
            version: version.clone(),
            name: name.clone(),
            description: migration.description().map(|s| s.to_string()),
            checksum: migration.checksum(),
            applied_at: Some(Utc::now()),
            execution_time_ms: None,
        };

        self.repository.record_migration(&tx, &metadata).await?;

        // Commit
        tx.commit().await?;

        result.migrations_run.push(MigrationRunInfo {
            version,
            name,
            direction: Direction::Up,
            execution_time_ms: None,
        });

        Ok(())
    }

    /// Run a single migration down
    async fn run_migration_down(
        &self,
        migration: Arc<dyn Migration>,
        result: &mut MigrationResult,
    ) -> DbResult<()> {
        let version = migration.version().to_string();
        let name = migration.name().to_string();

        println!("Rolling back: {} - {}", version, name);

        if self.config.dry_run {
            println!("[DRY RUN] Would rollback migration");
            result.migrations_run.push(MigrationRunInfo {
                version,
                name,
                direction: Direction::Down,
                execution_time_ms: None,
            });
            return Ok(());
        }

        // Start transaction
        let tx = Transaction::begin_with_options(
            self.connection.clone(),
            self.config.transaction_options.clone(),
        )
        .await?;

        // Run migration
        migration.down(&tx).await?;

        // Remove migration record
        self.repository.remove_migration(&tx, &version).await?;

        // Commit
        tx.commit().await?;

        result.migrations_run.push(MigrationRunInfo {
            version,
            name,
            direction: Direction::Down,
            execution_time_ms: None,
        });

        Ok(())
    }
}

/// Migration status
pub enum MigrationStatus {
    /// Migration is pending
    Pending { migration: Arc<dyn Migration> },
    /// Migration is applied
    Applied {
        metadata: MigrationMetadata,
        checksum_valid: bool,
    },
    /// Migration was applied but is missing from current set
    Missing { metadata: MigrationMetadata },
}

impl MigrationStatus {
    /// Get the version
    pub fn version(&self) -> &str {
        match self {
            Self::Pending { migration } => migration.version(),
            Self::Applied { metadata, .. } => &metadata.version,
            Self::Missing { metadata } => &metadata.version,
        }
    }
}

/// Migration result
#[derive(Debug)]
pub struct MigrationResult {
    pub direction: Direction,
    pub migrations_run: Vec<MigrationRunInfo>,
    pub dry_run: bool,
}

/// Information about a migration run
#[derive(Debug)]
pub struct MigrationRunInfo {
    pub version: String,
    pub name: String,
    pub direction: Direction,
    pub execution_time_ms: Option<u64>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_migration_runner_config() {
        let config = MigrationRunnerConfig::default();
        assert!(!config.dry_run);
        assert!(config.validate_checksums);
        assert!(!config.allow_out_of_order);
    }
}
