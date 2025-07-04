//! Migration builder for creating migrations programmatically

use std::sync::Arc;
use super::{Migration, SqlMigration};
use crate::schema::{Schema, FieldType, SqlDialect};

/// Migration builder for creating migrations
pub struct MigrationBuilder {
    version: String,
    name: String,
    description: Option<String>,
    up_operations: Vec<MigrationOperation>,
    down_operations: Vec<MigrationOperation>,
}

/// Migration operations
#[derive(Debug, Clone)]
pub enum MigrationOperation {
    CreateTable {
        name: String,
        schema: Schema,
    },
    DropTable {
        name: String,
    },
    AddColumn {
        table: String,
        column: String,
        field_type: FieldType,
        nullable: bool,
        default: Option<String>,
    },
    DropColumn {
        table: String,
        column: String,
    },
    RenameColumn {
        table: String,
        old_name: String,
        new_name: String,
    },
    RenameTable {
        old_name: String,
        new_name: String,
    },
    CreateIndex {
        name: String,
        table: String,
        columns: Vec<String>,
        unique: bool,
    },
    DropIndex {
        name: String,
    },
    Raw {
        sql: String,
    },
}

impl MigrationBuilder {
    /// Create a new migration builder
    pub fn new(version: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            version: version.into(),
            name: name.into(),
            description: None,
            up_operations: Vec::new(),
            down_operations: Vec::new(),
        }
    }
    
    /// Set description
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
    
    /// Create a table
    pub fn create_table(mut self, name: impl Into<String>, schema: Schema) -> Self {
        let table_name = name.into();
        self.up_operations.push(MigrationOperation::CreateTable {
            name: table_name.clone(),
            schema,
        });
        self.down_operations.insert(0, MigrationOperation::DropTable {
            name: table_name,
        });
        self
    }
    
    /// Drop a table
    pub fn drop_table(mut self, name: impl Into<String>) -> Self {
        let table_name = name.into();
        self.up_operations.push(MigrationOperation::DropTable {
            name: table_name.clone(),
        });
        // Note: We can't automatically generate the reverse operation
        // User should manually specify the create table in down migration
        self
    }
    
    /// Add a column
    pub fn add_column(
        mut self,
        table: impl Into<String>,
        column: impl Into<String>,
        field_type: FieldType,
    ) -> Self {
        let table_name = table.into();
        let column_name = column.into();
        
        self.up_operations.push(MigrationOperation::AddColumn {
            table: table_name.clone(),
            column: column_name.clone(),
            field_type,
            nullable: true,
            default: None,
        });
        
        self.down_operations.insert(0, MigrationOperation::DropColumn {
            table: table_name,
            column: column_name,
        });
        
        self
    }
    
    /// Add a non-nullable column
    pub fn add_column_not_null(
        mut self,
        table: impl Into<String>,
        column: impl Into<String>,
        field_type: FieldType,
        default: impl Into<String>,
    ) -> Self {
        let table_name = table.into();
        let column_name = column.into();
        
        self.up_operations.push(MigrationOperation::AddColumn {
            table: table_name.clone(),
            column: column_name.clone(),
            field_type,
            nullable: false,
            default: Some(default.into()),
        });
        
        self.down_operations.insert(0, MigrationOperation::DropColumn {
            table: table_name,
            column: column_name,
        });
        
        self
    }
    
    /// Drop a column
    pub fn drop_column(
        mut self,
        table: impl Into<String>,
        column: impl Into<String>,
    ) -> Self {
        self.up_operations.push(MigrationOperation::DropColumn {
            table: table.into(),
            column: column.into(),
        });
        // Note: Can't auto-generate reverse
        self
    }
    
    /// Rename a column
    pub fn rename_column(
        mut self,
        table: impl Into<String>,
        old_name: impl Into<String>,
        new_name: impl Into<String>,
    ) -> Self {
        let table_name = table.into();
        let old = old_name.into();
        let new = new_name.into();
        
        self.up_operations.push(MigrationOperation::RenameColumn {
            table: table_name.clone(),
            old_name: old.clone(),
            new_name: new.clone(),
        });
        
        self.down_operations.insert(0, MigrationOperation::RenameColumn {
            table: table_name,
            old_name: new,
            new_name: old,
        });
        
        self
    }
    
    /// Rename a table
    pub fn rename_table(
        mut self,
        old_name: impl Into<String>,
        new_name: impl Into<String>,
    ) -> Self {
        let old = old_name.into();
        let new = new_name.into();
        
        self.up_operations.push(MigrationOperation::RenameTable {
            old_name: old.clone(),
            new_name: new.clone(),
        });
        
        self.down_operations.insert(0, MigrationOperation::RenameTable {
            old_name: new,
            new_name: old,
        });
        
        self
    }
    
    /// Create an index
    pub fn create_index(
        mut self,
        name: impl Into<String>,
        table: impl Into<String>,
        columns: Vec<String>,
    ) -> Self {
        let index_name = name.into();
        
        self.up_operations.push(MigrationOperation::CreateIndex {
            name: index_name.clone(),
            table: table.into(),
            columns,
            unique: false,
        });
        
        self.down_operations.insert(0, MigrationOperation::DropIndex {
            name: index_name,
        });
        
        self
    }
    
    /// Create a unique index
    pub fn create_unique_index(
        mut self,
        name: impl Into<String>,
        table: impl Into<String>,
        columns: Vec<String>,
    ) -> Self {
        let index_name = name.into();
        
        self.up_operations.push(MigrationOperation::CreateIndex {
            name: index_name.clone(),
            table: table.into(),
            columns,
            unique: true,
        });
        
        self.down_operations.insert(0, MigrationOperation::DropIndex {
            name: index_name,
        });
        
        self
    }
    
    /// Add raw SQL for up migration
    pub fn up_sql(mut self, sql: impl Into<String>) -> Self {
        self.up_operations.push(MigrationOperation::Raw {
            sql: sql.into(),
        });
        self
    }
    
    /// Add raw SQL for down migration
    pub fn down_sql(mut self, sql: impl Into<String>) -> Self {
        self.down_operations.push(MigrationOperation::Raw {
            sql: sql.into(),
        });
        self
    }
    
    /// Build the migration
    pub fn build(self) -> Arc<dyn Migration> {
        let up_sql = self.operations_to_sql(&self.up_operations);
        let down_sql = self.operations_to_sql(&self.down_operations);
        
        let mut migration = SqlMigration::new(
            self.version,
            self.name,
            up_sql,
            down_sql,
        );
        
        if let Some(desc) = self.description {
            migration = migration.with_description(desc);
        }
        
        Arc::new(migration)
    }
    
    /// Convert operations to SQL
    fn operations_to_sql(&self, operations: &[MigrationOperation]) -> String {
        operations.iter()
            .map(|op| self.operation_to_sql(op))
            .collect::<Vec<_>>()
            .join(";\n")
    }
    
    /// Convert a single operation to SQL
    fn operation_to_sql(&self, operation: &MigrationOperation) -> String {
        match operation {
            MigrationOperation::CreateTable { name, schema } => {
                let columns = schema.fields()
                    .map(|field| {
                        let mut sql = format!("{} {}", field.name, field.field_type.to_sql(SqlDialect::SQLite));
                        if !field.is_nullable() {
                            sql.push_str(" NOT NULL");
                        }
                        if field.is_primary_key() {
                            sql.push_str(" PRIMARY KEY");
                        }
                        sql
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                
                format!("CREATE TABLE {} ({})", name, columns)
            }
            
            MigrationOperation::DropTable { name } => {
                format!("DROP TABLE {}", name)
            }
            
            MigrationOperation::AddColumn { table, column, field_type, nullable, default } => {
                let mut sql = format!("ALTER TABLE {} ADD COLUMN {} {}", 
                    table, column, field_type.to_sql(SqlDialect::SQLite));
                
                if !nullable {
                    sql.push_str(" NOT NULL");
                }
                
                if let Some(default_value) = default {
                    sql.push_str(&format!(" DEFAULT {}", default_value));
                }
                
                sql
            }
            
            MigrationOperation::DropColumn { table, column } => {
                format!("ALTER TABLE {} DROP COLUMN {}", table, column)
            }
            
            MigrationOperation::RenameColumn { table, old_name, new_name } => {
                // Note: This is database-specific
                format!("ALTER TABLE {} RENAME COLUMN {} TO {}", table, old_name, new_name)
            }
            
            MigrationOperation::RenameTable { old_name, new_name } => {
                format!("ALTER TABLE {} RENAME TO {}", old_name, new_name)
            }
            
            MigrationOperation::CreateIndex { name, table, columns, unique } => {
                let unique_str = if *unique { "UNIQUE " } else { "" };
                format!("CREATE {}INDEX {} ON {} ({})", 
                    unique_str, name, table, columns.join(", "))
            }
            
            MigrationOperation::DropIndex { name } => {
                format!("DROP INDEX {}", name)
            }
            
            MigrationOperation::Raw { sql } => sql.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::SchemaBuilder;
    
    #[test]
    fn test_migration_builder_create_table() {
        let schema = SchemaBuilder::new("users")
            .add_field_with("id", FieldType::Int, |f| f.primary_key())
            .add_field_with("name", FieldType::Text, |f| f.not_null())
            .add_field_with("email", FieldType::Text, |f| f)
            .build();
        
        let migration = MigrationBuilder::new("001", "create_users")
            .description("Create users table")
            .create_table("users", schema)
            .build();
        
        assert_eq!(migration.version(), "001");
        assert_eq!(migration.name(), "create_users");
        assert_eq!(migration.description(), Some("Create users table"));
    }
    
    #[test]
    fn test_migration_builder_add_column() {
        let migration = MigrationBuilder::new("002", "add_user_age")
            .add_column("users", "age", FieldType::Int)
            .build();
        
        assert_eq!(migration.version(), "002");
        assert_eq!(migration.name(), "add_user_age");
    }
    
    #[test]
    fn test_migration_builder_complex() {
        let migration = MigrationBuilder::new("003", "complex_migration")
            .rename_column("users", "name", "full_name")
            .add_column_not_null("users", "created_at", FieldType::Text, "CURRENT_TIMESTAMP")
            .create_index("idx_users_email", "users", vec!["email".to_string()])
            .up_sql("UPDATE users SET active = true")
            .down_sql("UPDATE users SET active = false")
            .build();
        
        assert_eq!(migration.version(), "003");
        assert_eq!(migration.name(), "complex_migration");
    }
}