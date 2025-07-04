//! Demonstration of FluentAI's AI-first design principles
//!
//! This example shows how FluentAI's graph-based architecture and
//! AI-friendly metadata enable powerful analysis and optimization.

use fluentai_core::ast::{Graph, Node, NodeId, NodeKind};
use fluentai_di::{ServiceGraphBuilder, ServiceMetadata, PerformanceHints, DependencyKind};
use fluentai_db::{SchemaGraphBuilder, TableHints, CacheStrategy, JoinOperator, RelationshipType};
use std::collections::HashMap;

fn main() {
    println!("FluentAI: AI-First Design Demo\n");
    
    // 1. Graph-based Dependency Injection
    demonstrate_graph_di();
    
    println!("\n" + &"=".repeat(60) + "\n");
    
    // 2. Graph-based Database Schema
    demonstrate_graph_schema();
    
    println!("\n" + &"=".repeat(60) + "\n");
    
    // 3. AST Analysis and Optimization
    demonstrate_ast_analysis();
}

fn demonstrate_graph_di() {
    println!("1. Graph-Based Dependency Injection");
    println!("   (Replacing string-based DI with AST nodes)\n");
    
    let mut builder = ServiceGraphBuilder::new();
    
    // Define services with rich metadata
    let logger_id = builder
        .service("ILogger")
        .implementation("FileLogger")
        .lifetime(fluentai_di::graph_based::ServiceLifetime::Singleton)
        .with_metadata(ServiceMetadata {
            performance_hints: PerformanceHints {
                instantiation_cost: Some(2.5),  // microseconds
                memory_footprint: Some(4096),   // bytes
                thread_safe: true,
                performs_io: true,
                cache_friendly: Some(0.3),      // IO-bound, not cache-friendly
            },
            tags: vec!["logging".to_string(), "io".to_string(), "diagnostics".to_string()],
            embedding: Some(vec![0.1, 0.8, 0.2, 0.9]), // Semantic embedding
            ..Default::default()
        })
        .register();
    
    let cache_id = builder
        .service("ICache")
        .implementation("RedisCache")
        .lifetime(fluentai_di::graph_based::ServiceLifetime::Singleton)
        .with_metadata(ServiceMetadata {
            performance_hints: PerformanceHints {
                instantiation_cost: Some(50.0), // Network connection setup
                memory_footprint: Some(1024),
                thread_safe: true,
                performs_io: true,
                cache_friendly: Some(0.9),      // Very cache-friendly
            },
            tags: vec!["caching".to_string(), "network".to_string(), "redis".to_string()],
            ..Default::default()
        })
        .register();
    
    let repository_id = builder
        .service("IUserRepository")
        .implementation("CachedUserRepository")
        .lifetime(fluentai_di::graph_based::ServiceLifetime::Scoped)
        .depends_on("ILogger", DependencyKind::Constructor)
        .depends_on("ICache", DependencyKind::Constructor)
        .depends_on("IDbConnection", DependencyKind::Property)
        .with_metadata(ServiceMetadata {
            performance_hints: PerformanceHints {
                instantiation_cost: Some(5.0),
                memory_footprint: Some(2048),
                thread_safe: false,  // Scoped, not thread-safe
                performs_io: true,
                cache_friendly: Some(0.7),
            },
            tags: vec!["repository".to_string(), "database".to_string(), "users".to_string()],
            ..Default::default()
        })
        .register();
    
    let container = builder.build();
    
    // Analyze dependencies
    println!("Analyzing IUserRepository dependencies:");
    let analysis = container.analyze_dependencies(repository_id);
    
    println!("  - Dependency depth: {}", analysis.dependency_depth);
    println!("  - Has circular dependencies: {}", analysis.has_cycles);
    println!("  - Total instantiation cost: {:.2}μs", analysis.total_cost);
    println!("  - Transitive dependencies: {} services", analysis.transitive_deps.len());
    
    // AI can now:
    // - Optimize instantiation order
    // - Predict performance bottlenecks
    // - Suggest caching strategies
    // - Find similar services by embedding distance
}

fn demonstrate_graph_schema() {
    println!("2. Graph-Based Database Schema");
    println!("   (Replacing string-based schemas with AST graphs)\n");
    
    let mut builder = SchemaGraphBuilder::new();
    
    // Define schema with AI-friendly hints
    builder.schema("ecommerce")
        .table("users")
        .column("id", "uuid")
        .column("email", "varchar(255)")
        .column("created_at", "timestamp")
        .with_hints(TableHints {
            row_count: Some(1_000_000),
            avg_row_size: Some(512),
            access_frequency: Some(5000.0), // queries per hour
            is_hot: true,
            cache_strategy: CacheStrategy::ReadThrough,
            ..Default::default()
        })
        .build();
    
    builder.schema("ecommerce")
        .table("orders")
        .column("id", "uuid")
        .column("user_id", "uuid")
        .column("total", "decimal(10,2)")
        .column("status", "varchar(50)")
        .with_hints(TableHints {
            row_count: Some(10_000_000),
            avg_row_size: Some(256),
            access_frequency: Some(10000.0),
            is_hot: true,
            cache_strategy: CacheStrategy::WriteBehind,
            ..Default::default()
        })
        .build();
    
    // Define relationships
    builder.relationship(
        ("ecommerce", "orders"),
        ("ecommerce", "users"),
        RelationshipType::ManyToOne,
    )
    .cardinality(0, None)  // 0 to many orders per user
    .join_on("user_id", JoinOperator::Equals, "id")
    .build();
    
    let schema = builder.build();
    
    // Analyze schema
    println!("Analyzing database schema:");
    let analysis = schema.analyze();
    
    println!("  - Average join complexity: {:.2}", analysis.join_complexity);
    println!("  - Denormalization candidates: {}", analysis.denormalization_candidates.len());
    println!("  - Missing indexes: {}", analysis.missing_indexes.len());
    println!("  - Redundant indexes: {}", analysis.redundant_indexes.len());
    
    // AI can now:
    // - Suggest optimal indexes based on access patterns
    // - Recommend denormalization for performance
    // - Predict query performance
    // - Optimize table partitioning
}

fn demonstrate_ast_analysis() {
    println!("3. AST Analysis and Optimization");
    println!("   (Showing how graph structure enables AI analysis)\n");
    
    // Create a simple AST graph
    let mut graph = Graph::new();
    
    // Function definition node
    let func_node = Node {
        id: NodeId(1),
        kind: NodeKind::Function {
            name: "calculate_total".to_string(),
            params: vec![NodeId(2), NodeId(3)],
            body: NodeId(4),
            is_pure: true,
        },
        context_memory: Some(fluentai_core::ast::ContextMemory {
            semantic_role: Some("Business logic - pricing calculation".to_string()),
            dependencies: vec!["tax_rate".to_string(), "discount_rules".to_string()],
            invariants: vec!["total >= 0".to_string()],
            usage_context: HashMap::from([
                ("frequency".to_string(), "high".to_string()),
                ("critical_path".to_string(), "true".to_string()),
            ]),
        }),
        performance_hints: Some(fluentai_core::ast::PerformanceHints {
            estimated_cost: Some(10.5),  // microseconds
            is_hot_path: true,
            can_parallelize: false,
            memory_usage: Some(256),      // bytes
            cache_hint: Some("memoize".to_string()),
        }),
        ..Default::default()
    };
    
    graph.add_node(func_node);
    
    // Parameter nodes
    graph.add_node(Node {
        id: NodeId(2),
        kind: NodeKind::Symbol("items".to_string()),
        ..Default::default()
    });
    
    graph.add_node(Node {
        id: NodeId(3),
        kind: NodeKind::Symbol("tax_rate".to_string()),
        ..Default::default()
    });
    
    // Body node (simplified)
    graph.add_node(Node {
        id: NodeId(4),
        kind: NodeKind::Let {
            bindings: vec![(NodeId(5), NodeId(6))],
            body: NodeId(7),
        },
        ..Default::default()
    });
    
    // Add edges
    graph.add_edge(NodeId(1), NodeId(4));  // Function to body
    graph.add_edge(NodeId(4), NodeId(7));  // Let to its body
    
    println!("Function Analysis:");
    println!("  - Semantic role: Business logic - pricing calculation");
    println!("  - Is pure: true (can be memoized)");
    println!("  - Is hot path: true (optimize aggressively)");
    println!("  - Estimated cost: 10.5μs");
    println!("  - Cache hint: memoize");
    println!("  - Dependencies: tax_rate, discount_rules");
    
    // Demonstrate usage tracking
    if let Some(ref usage_stats) = graph.nodes[&NodeId(1)].usage_statistics {
        println!("\nUsage Statistics:");
        println!("  - Execution count: {}", usage_stats.execution_count);
        println!("  - Average execution time: {}ns", usage_stats.avg_execution_time_ns);
        println!("  - Error rate: {}", usage_stats.error_count as f64 / usage_stats.execution_count as f64);
    }
    
    println!("\nAI Optimization Opportunities:");
    println!("  - Memoization (function is pure and hot)");
    println!("  - Inline expansion (small function, hot path)");
    println!("  - Specialization for common tax_rate values");
    println!("  - Vectorization for batch processing");
    
    // AI can now:
    // - Automatically apply optimizations based on metadata
    // - Learn patterns from usage statistics
    // - Suggest refactorings based on semantic roles
    // - Generate specialized versions for hot paths
}