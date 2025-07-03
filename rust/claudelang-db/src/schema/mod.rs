//! Schema definition and management

use serde::{Serialize, Deserialize};
use crate::error::DbResult;

/// Field data types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FieldType {
    // Numeric types
    Int,
    BigInt,
    Float,
    Decimal(u8, u8), // precision, scale
    
    // String types
    String(Option<usize>), // max length
    Text,
    
    // Boolean
    Bool,
    
    // Date/Time types
    Date,
    Time,
    DateTime,
    Timestamp,
    
    // Binary
    Binary(Option<usize>), // max length
    
    // JSON
    Json,
    
    // UUID
    Uuid,
}

impl FieldType {
    /// Convert to SQL type string
    pub fn to_sql(&self, dialect: SqlDialect) -> String {
        match (self, dialect) {
            (FieldType::Int, _) => "INTEGER".to_string(),
            (FieldType::BigInt, _) => "BIGINT".to_string(),
            (FieldType::Float, SqlDialect::Postgres) => "REAL".to_string(),
            (FieldType::Float, _) => "FLOAT".to_string(),
            (FieldType::Decimal(p, s), _) => format!("DECIMAL({}, {})", p, s),
            (FieldType::String(Some(len)), _) => format!("VARCHAR({})", len),
            (FieldType::String(None), SqlDialect::Postgres) => "VARCHAR".to_string(),
            (FieldType::String(None), _) => "VARCHAR(255)".to_string(),
            (FieldType::Text, _) => "TEXT".to_string(),
            (FieldType::Bool, SqlDialect::Postgres) => "BOOLEAN".to_string(),
            (FieldType::Bool, SqlDialect::SQLite) => "INTEGER".to_string(),
            (FieldType::Bool, _) => "BOOLEAN".to_string(),
            (FieldType::Date, _) => "DATE".to_string(),
            (FieldType::Time, _) => "TIME".to_string(),
            (FieldType::DateTime, SqlDialect::SQLite) => "TEXT".to_string(),
            (FieldType::DateTime, _) => "DATETIME".to_string(),
            (FieldType::Timestamp, SqlDialect::Postgres) => "TIMESTAMP".to_string(),
            (FieldType::Timestamp, _) => "TIMESTAMP".to_string(),
            (FieldType::Binary(Some(len)), _) => format!("BINARY({})", len),
            (FieldType::Binary(None), SqlDialect::Postgres) => "BYTEA".to_string(),
            (FieldType::Binary(None), _) => "BLOB".to_string(),
            (FieldType::Json, SqlDialect::Postgres) => "JSONB".to_string(),
            (FieldType::Json, SqlDialect::MySQL) => "JSON".to_string(),
            (FieldType::Json, _) => "TEXT".to_string(),
            (FieldType::Uuid, SqlDialect::Postgres) => "UUID".to_string(),
            (FieldType::Uuid, _) => "VARCHAR(36)".to_string(),
        }
    }
}

/// SQL dialect
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlDialect {
    Postgres,
    MySQL,
    SQLite,
}

/// Field constraints
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Constraint {
    NotNull,
    Unique,
    PrimaryKey,
    ForeignKey {
        table: String,
        column: String,
        on_delete: ForeignKeyAction,
        on_update: ForeignKeyAction,
    },
    Check(String), // SQL expression
    Default(String), // SQL expression
}

/// Foreign key actions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ForeignKeyAction {
    Cascade,
    SetNull,
    SetDefault,
    Restrict,
    NoAction,
}

impl ForeignKeyAction {
    pub fn to_sql(&self) -> &'static str {
        match self {
            ForeignKeyAction::Cascade => "CASCADE",
            ForeignKeyAction::SetNull => "SET NULL",
            ForeignKeyAction::SetDefault => "SET DEFAULT",
            ForeignKeyAction::Restrict => "RESTRICT",
            ForeignKeyAction::NoAction => "NO ACTION",
        }
    }
}

/// Field definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub field_type: FieldType,
    pub constraints: Vec<Constraint>,
    pub comment: Option<String>,
}

impl Field {
    pub fn new(name: impl Into<String>, field_type: FieldType) -> Self {
        Self {
            name: name.into(),
            field_type,
            constraints: Vec::new(),
            comment: None,
        }
    }
    
    pub fn not_null(mut self) -> Self {
        self.constraints.push(Constraint::NotNull);
        self
    }
    
    pub fn unique(mut self) -> Self {
        self.constraints.push(Constraint::Unique);
        self
    }
    
    pub fn primary_key(mut self) -> Self {
        self.constraints.push(Constraint::PrimaryKey);
        self
    }
    
    pub fn foreign_key(
        mut self,
        table: impl Into<String>,
        column: impl Into<String>,
        on_delete: ForeignKeyAction,
        on_update: ForeignKeyAction,
    ) -> Self {
        self.constraints.push(Constraint::ForeignKey {
            table: table.into(),
            column: column.into(),
            on_delete,
            on_update,
        });
        self
    }
    
    pub fn default(mut self, expr: impl Into<String>) -> Self {
        self.constraints.push(Constraint::Default(expr.into()));
        self
    }
    
    pub fn check(mut self, expr: impl Into<String>) -> Self {
        self.constraints.push(Constraint::Check(expr.into()));
        self
    }
    
    pub fn comment(mut self, comment: impl Into<String>) -> Self {
        self.comment = Some(comment.into());
        self
    }
    
    /// Check if field is nullable
    pub fn is_nullable(&self) -> bool {
        !self.constraints.iter().any(|c| matches!(c, Constraint::NotNull))
    }
    
    /// Check if field is primary key
    pub fn is_primary_key(&self) -> bool {
        self.constraints.iter().any(|c| matches!(c, Constraint::PrimaryKey))
    }
}

/// Table index
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Index {
    pub name: String,
    pub columns: Vec<String>,
    pub unique: bool,
    pub where_clause: Option<String>,
}

/// Table schema definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Schema {
    pub name: String,
    pub fields: Vec<Field>,
    pub indexes: Vec<Index>,
    pub comment: Option<String>,
}

impl Schema {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            fields: Vec::new(),
            indexes: Vec::new(),
            comment: None,
        }
    }
    
    pub fn field(mut self, field: Field) -> Self {
        self.fields.push(field);
        self
    }
    
    pub fn index(mut self, index: Index) -> Self {
        self.indexes.push(index);
        self
    }
    
    pub fn comment(mut self, comment: impl Into<String>) -> Self {
        self.comment = Some(comment.into());
        self
    }
    
    /// Get an iterator over fields
    pub fn fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.iter()
    }
    
    /// Generate CREATE TABLE SQL
    pub fn to_create_sql(&self, dialect: SqlDialect) -> DbResult<String> {
        let mut sql = format!("CREATE TABLE {} (\n", self.name);
        
        // Fields
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                sql.push_str(",\n");
            }
            sql.push_str("    ");
            sql.push_str(&field.name);
            sql.push(' ');
            sql.push_str(&field.field_type.to_sql(dialect));
            
            // Constraints
            for constraint in &field.constraints {
                match constraint {
                    Constraint::NotNull => sql.push_str(" NOT NULL"),
                    Constraint::Unique => sql.push_str(" UNIQUE"),
                    Constraint::PrimaryKey => sql.push_str(" PRIMARY KEY"),
                    Constraint::Default(expr) => {
                        sql.push_str(" DEFAULT ");
                        sql.push_str(expr);
                    }
                    Constraint::Check(expr) => {
                        sql.push_str(" CHECK (");
                        sql.push_str(expr);
                        sql.push(')');
                    }
                    Constraint::ForeignKey { .. } => {
                        // Handle separately as table constraint
                    }
                }
            }
        }
        
        // Foreign key constraints
        for field in &self.fields {
            for constraint in &field.constraints {
                if let Constraint::ForeignKey { table, column, on_delete, on_update } = constraint {
                    sql.push_str(",\n    FOREIGN KEY (");
                    sql.push_str(&field.name);
                    sql.push_str(") REFERENCES ");
                    sql.push_str(table);
                    sql.push('(');
                    sql.push_str(column);
                    sql.push(')');
                    sql.push_str(" ON DELETE ");
                    sql.push_str(on_delete.to_sql());
                    sql.push_str(" ON UPDATE ");
                    sql.push_str(on_update.to_sql());
                }
            }
        }
        
        sql.push_str("\n)");
        
        // Table comment (if supported)
        if let Some(comment) = &self.comment {
            if dialect == SqlDialect::MySQL {
                sql.push_str(" COMMENT = '");
                sql.push_str(&comment.replace('\'', "''"));
                sql.push('\'');
            }
        }
        
        sql.push(';');
        
        // Create indexes
        let mut index_sql = String::new();
        for index in &self.indexes {
            index_sql.push_str("\nCREATE ");
            if index.unique {
                index_sql.push_str("UNIQUE ");
            }
            index_sql.push_str("INDEX ");
            index_sql.push_str(&index.name);
            index_sql.push_str(" ON ");
            index_sql.push_str(&self.name);
            index_sql.push_str(" (");
            index_sql.push_str(&index.columns.join(", "));
            index_sql.push(')');
            if let Some(where_clause) = &index.where_clause {
                index_sql.push_str(" WHERE ");
                index_sql.push_str(where_clause);
            }
            index_sql.push(';');
        }
        
        Ok(sql + &index_sql)
    }
}

/// Schema builder for fluent API
pub struct SchemaBuilder {
    schema: Schema,
}

impl SchemaBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            schema: Schema::new(name),
        }
    }
    
    pub fn add_field(mut self, field: Field) -> Self {
        self.schema.fields.push(field);
        self
    }
    
    /// Convenience method for adding a field with a builder function
    pub fn add_field_with<F>(mut self, name: impl Into<String>, field_type: FieldType, builder: F) -> Self 
    where
        F: FnOnce(Field) -> Field,
    {
        let field = Field::new(name, field_type);
        self.schema.fields.push(builder(field));
        self
    }
    
    pub fn add_index(mut self, name: impl Into<String>, columns: Vec<String>, unique: bool) -> Self {
        self.schema.indexes.push(Index {
            name: name.into(),
            columns,
            unique,
            where_clause: None,
        });
        self
    }
    
    pub fn comment(mut self, comment: impl Into<String>) -> Self {
        self.schema.comment = Some(comment.into());
        self
    }
    
    pub fn build(self) -> Schema {
        self.schema
    }
}

/// Migration representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Migration {
    pub version: String,
    pub description: String,
    pub up: Vec<String>, // SQL statements
    pub down: Vec<String>, // SQL statements
    pub checksum: Option<String>,
}

impl Migration {
    pub fn new(version: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            version: version.into(),
            description: description.into(),
            up: Vec::new(),
            down: Vec::new(),
            checksum: None,
        }
    }
    
    pub fn up(mut self, sql: impl Into<String>) -> Self {
        self.up.push(sql.into());
        self
    }
    
    pub fn down(mut self, sql: impl Into<String>) -> Self {
        self.down.push(sql.into());
        self
    }
}
#[cfg(test)]
mod tests;
