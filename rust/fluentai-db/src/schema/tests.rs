//! Tests for schema module

#[cfg(test)]
mod tests {
    use super::super::*;

    #[test]
    fn test_field_type_to_sql() {
        // Test basic types
        assert_eq!(FieldType::Int.to_sql(SqlDialect::SQLite), "INTEGER");
        assert_eq!(FieldType::Text.to_sql(SqlDialect::SQLite), "TEXT");
        assert_eq!(FieldType::Bool.to_sql(SqlDialect::SQLite), "INTEGER");
        assert_eq!(FieldType::Bool.to_sql(SqlDialect::Postgres), "BOOLEAN");

        // Test string with length
        assert_eq!(
            FieldType::String(Some(50)).to_sql(SqlDialect::Postgres),
            "VARCHAR(50)"
        );
        assert_eq!(
            FieldType::String(None).to_sql(SqlDialect::Postgres),
            "VARCHAR"
        );

        // Test decimal
        assert_eq!(
            FieldType::Decimal(10, 2).to_sql(SqlDialect::MySQL),
            "DECIMAL(10, 2)"
        );

        // Test JSON handling
        assert_eq!(FieldType::Json.to_sql(SqlDialect::Postgres), "JSONB");
        assert_eq!(FieldType::Json.to_sql(SqlDialect::MySQL), "JSON");
        assert_eq!(FieldType::Json.to_sql(SqlDialect::SQLite), "TEXT");

        // Test UUID
        assert_eq!(FieldType::Uuid.to_sql(SqlDialect::Postgres), "UUID");
        assert_eq!(FieldType::Uuid.to_sql(SqlDialect::SQLite), "VARCHAR(36)");
    }

    #[test]
    fn test_field_builder() {
        let field = Field::new("user_id", FieldType::Int)
            .primary_key()
            .not_null();

        assert_eq!(field.name, "user_id");
        assert!(field.is_primary_key());
        assert!(!field.is_nullable());

        let field_with_default = Field::new("created_at", FieldType::DateTime)
            .not_null()
            .default("CURRENT_TIMESTAMP");

        assert!(field_with_default
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Default(val) if val == "CURRENT_TIMESTAMP")));
    }

    #[test]
    fn test_foreign_key_constraint() {
        let field = Field::new("user_id", FieldType::Int).foreign_key(
            "users",
            "id",
            ForeignKeyAction::Cascade,
            ForeignKeyAction::Cascade,
        );

        let has_fk = field.constraints.iter().any(|c| {
            matches!(c, Constraint::ForeignKey { table, column, on_delete, on_update }
                if table == "users"
                && column == "id"
                && *on_delete == ForeignKeyAction::Cascade
                && *on_update == ForeignKeyAction::Cascade
            )
        });

        assert!(has_fk);
    }

    #[test]
    fn test_schema_builder() {
        let schema = SchemaBuilder::new("users")
            .add_field(Field::new("id", FieldType::Int).primary_key())
            .add_field_with("email", FieldType::String(Some(255)), |f| {
                f.unique().not_null()
            })
            .add_field_with("age", FieldType::Int, |f| f.check("age >= 0"))
            .add_index("idx_email", vec!["email".to_string()], true)
            .comment("User accounts table")
            .build();

        assert_eq!(schema.name, "users");
        assert_eq!(schema.fields.len(), 3);
        assert_eq!(schema.indexes.len(), 1);
        assert_eq!(schema.comment, Some("User accounts table".to_string()));

        // Check fields
        assert!(schema.fields[0].is_primary_key());
        assert!(schema.fields[1]
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Unique)));
        assert!(schema.fields[2]
            .constraints
            .iter()
            .any(|c| matches!(c, Constraint::Check(expr) if expr == "age >= 0")));
    }

    #[test]
    fn test_schema_to_create_sql_basic() {
        let schema = SchemaBuilder::new("products")
            .add_field(Field::new("id", FieldType::Int).primary_key())
            .add_field(Field::new("name", FieldType::Text).not_null())
            .add_field(Field::new("price", FieldType::Decimal(10, 2)))
            .build();

        let sql = schema.to_create_sql(SqlDialect::SQLite).unwrap();

        assert!(sql.contains("CREATE TABLE products"));
        assert!(sql.contains("id INTEGER PRIMARY KEY"));
        assert!(sql.contains("name TEXT NOT NULL"));
        assert!(sql.contains("price DECIMAL(10, 2)"));
    }

    #[test]
    fn test_schema_with_foreign_keys() {
        let schema = SchemaBuilder::new("orders")
            .add_field(Field::new("id", FieldType::Int).primary_key())
            .add_field(
                Field::new("user_id", FieldType::Int)
                    .not_null()
                    .foreign_key(
                        "users",
                        "id",
                        ForeignKeyAction::Restrict,
                        ForeignKeyAction::Cascade,
                    ),
            )
            .add_field(Field::new("total", FieldType::Float))
            .build();

        let sql = schema.to_create_sql(SqlDialect::Postgres).unwrap();

        assert!(sql.contains("FOREIGN KEY"));
        assert!(sql.contains("REFERENCES users(id)"));
        assert!(sql.contains("ON DELETE RESTRICT"));
        assert!(sql.contains("ON UPDATE CASCADE"));
    }

    #[test]
    fn test_schema_with_indexes() {
        let schema = SchemaBuilder::new("articles")
            .add_field(Field::new("id", FieldType::Int).primary_key())
            .add_field(Field::new("title", FieldType::String(Some(200))).not_null())
            .add_field(Field::new("published", FieldType::Bool).default("false"))
            .add_index("idx_title", vec!["title".to_string()], false)
            .add_index("idx_published", vec!["published".to_string()], false)
            .build();

        let sql = schema.to_create_sql(SqlDialect::MySQL).unwrap();

        assert!(sql.contains("CREATE INDEX idx_title"));
        assert!(sql.contains("CREATE INDEX idx_published"));
        assert!(!sql.contains("UNIQUE INDEX"));
    }

    #[test]
    fn test_index_with_where_clause() {
        let mut schema = Schema::new("users");
        schema.indexes.push(Index {
            name: "idx_active_users".to_string(),
            columns: vec!["email".to_string()],
            unique: true,
            where_clause: Some("active = true".to_string()),
        });

        let sql = schema.to_create_sql(SqlDialect::Postgres).unwrap();
        assert!(sql.contains("WHERE active = true"));
    }

    #[test]
    fn test_complex_schema() {
        let schema = SchemaBuilder::new("user_profiles")
            .add_field_with("id", FieldType::Uuid, |f| f.primary_key())
            .add_field_with("user_id", FieldType::Int, |f| {
                f.not_null().unique().foreign_key(
                    "users",
                    "id",
                    ForeignKeyAction::Cascade,
                    ForeignKeyAction::Cascade,
                )
            })
            .add_field_with("bio", FieldType::Text, |f| f.comment("User biography"))
            .add_field_with("avatar_url", FieldType::String(Some(500)), |f| f)
            .add_field_with("preferences", FieldType::Json, |f| f.default("'{}'::jsonb"))
            .add_field_with("verified", FieldType::Bool, |f| {
                f.not_null().default("false")
            })
            .add_field_with("created_at", FieldType::Timestamp, |f| {
                f.not_null().default("CURRENT_TIMESTAMP")
            })
            .add_field_with("updated_at", FieldType::Timestamp, |f| {
                f.not_null().default("CURRENT_TIMESTAMP")
            })
            .add_index("idx_user_id", vec!["user_id".to_string()], false)
            .comment("Extended user profile information")
            .build();

        assert_eq!(schema.fields.len(), 8);
        assert_eq!(schema.indexes.len(), 1);

        let sql = schema.to_create_sql(SqlDialect::Postgres).unwrap();
        assert!(sql.contains("UUID PRIMARY KEY"));
        assert!(sql.contains("preferences JSONB DEFAULT '{}'::jsonb"));
        assert!(sql.contains("CURRENT_TIMESTAMP"));
    }

    #[test]
    fn test_foreign_key_actions() {
        assert_eq!(ForeignKeyAction::Cascade.to_sql(), "CASCADE");
        assert_eq!(ForeignKeyAction::SetNull.to_sql(), "SET NULL");
        assert_eq!(ForeignKeyAction::SetDefault.to_sql(), "SET DEFAULT");
        assert_eq!(ForeignKeyAction::Restrict.to_sql(), "RESTRICT");
        assert_eq!(ForeignKeyAction::NoAction.to_sql(), "NO ACTION");
    }

    #[test]
    fn test_table_comment_mysql() {
        let schema = SchemaBuilder::new("logs")
            .add_field(Field::new("id", FieldType::BigInt).primary_key())
            .add_field(Field::new("message", FieldType::Text))
            .comment("Application logs")
            .build();

        let sql = schema.to_create_sql(SqlDialect::MySQL).unwrap();
        assert!(sql.contains("COMMENT = 'Application logs'"));

        // PostgreSQL doesn't support table comments in CREATE TABLE
        let sql_pg = schema.to_create_sql(SqlDialect::Postgres).unwrap();
        assert!(!sql_pg.contains("COMMENT"));
    }

    #[test]
    fn test_field_nullable_default() {
        let field = Field::new("optional_field", FieldType::String(None));
        assert!(field.is_nullable());

        let required_field = Field::new("required_field", FieldType::String(None)).not_null();
        assert!(!required_field.is_nullable());
    }

    #[test]
    fn test_escaped_quotes_in_comment() {
        let schema = SchemaBuilder::new("test")
            .add_field(Field::new("id", FieldType::Int))
            .comment("Table with 'quotes' in comment")
            .build();

        let sql = schema.to_create_sql(SqlDialect::MySQL).unwrap();
        // Should escape single quotes in comment
        assert!(sql.contains("''"));
    }

    #[test]
    fn test_binary_field_types() {
        assert_eq!(
            FieldType::Binary(Some(16)).to_sql(SqlDialect::MySQL),
            "BINARY(16)"
        );
        assert_eq!(
            FieldType::Binary(None).to_sql(SqlDialect::Postgres),
            "BYTEA"
        );
        assert_eq!(FieldType::Binary(None).to_sql(SqlDialect::SQLite), "BLOB");
    }

    #[test]
    fn test_datetime_field_types() {
        // SQLite stores datetime as TEXT
        assert_eq!(FieldType::DateTime.to_sql(SqlDialect::SQLite), "TEXT");
        assert_eq!(FieldType::DateTime.to_sql(SqlDialect::MySQL), "DATETIME");

        assert_eq!(FieldType::Date.to_sql(SqlDialect::Postgres), "DATE");
        assert_eq!(FieldType::Time.to_sql(SqlDialect::MySQL), "TIME");
        assert_eq!(
            FieldType::Timestamp.to_sql(SqlDialect::Postgres),
            "TIMESTAMP"
        );
    }
}
