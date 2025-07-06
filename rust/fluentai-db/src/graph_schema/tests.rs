//! Tests for graph-based schema representation

#[cfg(test)]
mod tests {
    use super::super::*;
    use fluentai_core::ast::NodeId;
    
    // Helper function to create NodeId
    fn node_id(id: u32) -> NodeId {
        NodeId::new(id).expect("NodeId should be non-zero")
    }
    
    #[test]
    fn test_schema_creation() {
        let mut builder = SchemaGraphBuilder::new();
        
        builder.schema("test_db")
            .table("users")
            .column("id", "uuid")
            .column("email", "varchar(255)")
            .column("created_at", "timestamp")
            .build();
        
        let schema = builder.build();
        
        assert!(schema.schemas.contains_key("test_db"));
        assert!(schema.tables.contains_key(&("test_db".to_string(), "users".to_string())));
        
        let table = &schema.tables[&("test_db".to_string(), "users".to_string())];
        assert_eq!(table.name, "users");
        assert_eq!(table.columns.len(), 3);
    }
    
    #[test]
    fn test_table_hints() {
        let mut builder = SchemaGraphBuilder::new();
        
        builder.schema("analytics")
            .table("events")
            .column("id", "bigint")
            .column("timestamp", "timestamp")
            .column("data", "jsonb")
            .with_hints(TableHints {
                row_count: Some(1_000_000_000),
                avg_row_size: Some(256),
                access_frequency: Some(10000.0),
                is_hot: true,
                cache_strategy: CacheStrategy::RefreshAhead,
                partitioning: Some(PartitioningStrategy::Range {
                    column: node_id(1),
                    boundaries: vec![node_id(2), node_id(3), node_id(4)],
                }),
            })
            .build();
        
        let schema = builder.build();
        let table = &schema.tables[&("analytics".to_string(), "events".to_string())];
        
        assert_eq!(table.hints.row_count, Some(1_000_000_000));
        assert!(table.hints.is_hot);
        assert_eq!(table.hints.cache_strategy, CacheStrategy::RefreshAhead);
        assert!(matches!(table.hints.partitioning, Some(PartitioningStrategy::Range { .. })));
    }
    
    #[test]
    fn test_relationships() {
        let mut builder = SchemaGraphBuilder::new();
        
        // Create tables
        builder.schema("app")
            .table("users")
            .column("id", "uuid")
            .column("name", "varchar(100)")
            .build();
        
        builder.schema("app")
            .table("posts")
            .column("id", "uuid")
            .column("user_id", "uuid")
            .column("content", "text")
            .build();
        
        // Create relationship
        builder.relationship(
            ("app", "posts"),
            ("app", "users"),
            RelationshipType::OneToMany,
        )
        .cardinality(0, None)
        .join_on("user_id", JoinOperator::Equals, "id")
        .build();
        
        let schema = builder.build();
        
        assert_eq!(schema.relationships.len(), 1);
        let rel = &schema.relationships[0];
        assert_eq!(rel.relationship_type, RelationshipType::OneToMany);
        assert_eq!(rel.cardinality.min, 0);
        assert!(rel.cardinality.max.is_none());
        assert_eq!(rel.join_conditions.len(), 1);
        assert_eq!(rel.join_conditions[0].operator, JoinOperator::Equals);
    }
    
    #[test]
    fn test_column_statistics() {
        let mut builder = SchemaGraphBuilder::new();
        
        builder.schema("stats")
            .table("products")
            .column("id", "bigint")
            .column("price", "decimal(10,2)")
            .column("category", "varchar(50)")
            .build();
        
        let schema = builder.build();
        let table = &schema.tables[&("stats".to_string(), "products".to_string())];
        
        // Verify columns have statistics structure
        for column in &table.columns {
            assert!(column.statistics.cardinality.is_none()); // Not set yet
            assert!(column.statistics.null_ratio.is_none());
            assert!(column.statistics.histogram.is_none());
            assert!(column.statistics.frequent_values.is_empty());
        }
    }
    
    #[test]
    fn test_index_metadata() {
        let mut builder = SchemaGraphBuilder::new();
        
        // In a full implementation, we'd have index building methods
        builder.schema("indexed")
            .table("orders")
            .column("id", "uuid")
            .column("customer_id", "uuid")
            .column("created_at", "timestamp")
            .column("status", "varchar(20)")
            .build();
        
        let schema = builder.build();
        let table = &schema.tables[&("indexed".to_string(), "orders".to_string())];
        
        // Verify index structure exists (even if empty)
        assert_eq!(table.indexes.len(), 0);
    }
    
    #[test]
    fn test_many_to_many_relationship() {
        let mut builder = SchemaGraphBuilder::new();
        
        // Create tables
        builder.schema("social")
            .table("users")
            .column("id", "uuid")
            .build();
        
        builder.schema("social")
            .table("groups")
            .column("id", "uuid")
            .build();
        
        builder.schema("social")
            .table("user_groups")
            .column("user_id", "uuid")
            .column("group_id", "uuid")
            .build();
        
        // Create many-to-many relationship through join table
        builder.relationship(
            ("social", "user_groups"),
            ("social", "users"),
            RelationshipType::OneToMany,
        )
        .join_on("user_id", JoinOperator::Equals, "id")
        .build();
        
        builder.relationship(
            ("social", "user_groups"),
            ("social", "groups"),
            RelationshipType::OneToMany,
        )
        .join_on("group_id", JoinOperator::Equals, "id")
        .build();
        
        let schema = builder.build();
        assert_eq!(schema.relationships.len(), 2);
    }
    
    #[test]
    fn test_schema_metadata() {
        let mut builder = SchemaGraphBuilder::new();
        
        builder.schema("test")
            .table("sample")
            .column("id", "int")
            .build();
        
        let mut schema = builder.build();
        
        // Set metadata
        schema.metadata = SchemaMetadata {
            embedding: Some(vec![0.1, 0.2, 0.3]),
            tags: vec!["test".to_string(), "sample".to_string()],
            version: SchemaVersion {
                version: 1,
                structure_hash: 0x12345678,
                compatibility: CompatibilityLevel::FullyCompatible,
                migration_complexity: Some(0.2),
            },
            performance_stats: PerformanceStats {
                total_queries: 1000,
                avg_query_time: 5.5,
                cache_hit_ratio: 0.85,
                index_usage_ratio: 0.92,
            },
            ..Default::default()
        };
        
        assert_eq!(schema.metadata.embedding.as_ref().unwrap().len(), 3);
        assert_eq!(schema.metadata.tags.len(), 2);
        assert_eq!(schema.metadata.version.version, 1);
        assert_eq!(schema.metadata.performance_stats.cache_hit_ratio, 0.85);
    }
    
    #[test]
    fn test_composite_partitioning() {
        let mut builder = SchemaGraphBuilder::new();
        
        builder.schema("partitioned")
            .table("logs")
            .column("id", "bigint")
            .column("timestamp", "timestamp")
            .column("region", "varchar(10)")
            .column("data", "jsonb")
            .with_hints(TableHints {
                partitioning: Some(PartitioningStrategy::Composite {
                    strategies: vec![
                        PartitioningStrategy::Range {
                            column: node_id(1),
                            boundaries: vec![node_id(2), node_id(3)],
                        },
                        PartitioningStrategy::List {
                            column: node_id(4),
                            values: vec![vec![node_id(5)], vec![node_id(6)]],
                        },
                    ],
                }),
                ..Default::default()
            })
            .build();
        
        let schema = builder.build();
        let table = &schema.tables[&("partitioned".to_string(), "logs".to_string())];
        
        match &table.hints.partitioning {
            Some(PartitioningStrategy::Composite { strategies }) => {
                assert_eq!(strategies.len(), 2);
            }
            _ => panic!("Expected composite partitioning"),
        }
    }
    
    #[test]
    fn test_constraint_types() {
        // This test verifies the constraint type enum structure
        let not_null = ConstraintType::NotNull;
        let unique = ConstraintType::Unique;
        let primary = ConstraintType::PrimaryKey;
        let foreign = ConstraintType::ForeignKey {
            target_table: node_id(1),
            target_column: node_id(2),
            on_delete: ForeignKeyAction::Cascade,
            on_update: ForeignKeyAction::Restrict,
        };
        
        assert_eq!(not_null, ConstraintType::NotNull);
        assert_eq!(unique, ConstraintType::Unique);
        assert_eq!(primary, ConstraintType::PrimaryKey);
        
        match foreign {
            ConstraintType::ForeignKey { on_delete, on_update, .. } => {
                assert_eq!(on_delete, ForeignKeyAction::Cascade);
                assert_eq!(on_update, ForeignKeyAction::Restrict);
            }
            _ => panic!("Expected foreign key constraint"),
        }
    }
    
    #[test]
    fn test_access_patterns() {
        let mut schema = SchemaGraphBuilder::new().build();
        
        // Add access patterns
        schema.metadata.access_patterns.push(AccessPattern {
            name: "user_dashboard".to_string(),
            tables: vec![node_id(1), node_id(2)],
            columns: vec![node_id(10), node_id(11), node_id(12)],
            frequency: 1000.0,
            avg_execution_time: 15.5,
        });
        
        schema.metadata.access_patterns.push(AccessPattern {
            name: "admin_reports".to_string(),
            tables: vec![node_id(1), node_id(2), node_id(3)],
            columns: vec![node_id(20), node_id(21)],
            frequency: 50.0,
            avg_execution_time: 250.0,
        });
        
        assert_eq!(schema.metadata.access_patterns.len(), 2);
        assert_eq!(schema.metadata.access_patterns[0].name, "user_dashboard");
        assert_eq!(schema.metadata.access_patterns[0].frequency, 1000.0);
        assert_eq!(schema.metadata.access_patterns[1].avg_execution_time, 250.0);
    }
}