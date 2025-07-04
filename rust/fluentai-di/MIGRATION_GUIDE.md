# Migration Guide: From Traditional DI to Graph-Based DI

This guide helps you migrate from the traditional TypeId-based dependency injection to the new AI-first, graph-based system.

## Why Migrate?

The new graph-based DI system provides:
- **AI-analyzable dependency graphs** instead of opaque TypeIds
- **Performance predictions** through metadata and usage statistics
- **Semantic versioning** based on behavior, not just code changes
- **Parallel resolution** opportunities detected automatically
- **Integration with FluentAi's AST** for better optimization

## Key Differences

### Old Approach (Traditional DI)
```rust
// String and TypeId based
let mut builder = ContainerBuilder::new();
builder.register_singleton::<ILogger, ConsoleLogger>();
builder.register_transient::<IRepository, SqlRepository>();
let container = builder.build();

// Type-based resolution
let logger = container.resolve::<ILogger>()?;
```

### New Approach (Graph-Based DI)
```rust
// Graph and AST based
let mut builder = ServiceGraphBuilder::new();

// Define services with explicit metadata
let logger_id = builder
    .service("ILogger")
    .implementation("ConsoleLogger")
    .lifetime(ServiceLifetime::Singleton)
    .with_metadata(ServiceMetadata {
        performance_hints: PerformanceHints {
            instantiation_cost: Some(0.5), // microseconds
            memory_footprint: Some(1024),   // bytes
            thread_safe: true,
            performs_io: true,
            ..Default::default()
        },
        tags: vec!["logging".to_string(), "io".to_string()],
        ..Default::default()
    })
    .register();

// Define dependencies explicitly
let repo_id = builder
    .service("IRepository")
    .implementation("SqlRepository")
    .lifetime(ServiceLifetime::Transient)
    .depends_on("ILogger", DependencyKind::Constructor)
    .depends_on("IDbConnection", DependencyKind::Property)
    .with_metadata(ServiceMetadata {
        performance_hints: PerformanceHints {
            instantiation_cost: Some(10.0), // microseconds
            performs_io: true,
            ..Default::default()
        },
        ..Default::default()
    })
    .register();

let container = builder.build();

// Analyze before resolution
let analysis = container.analyze_dependencies(repo_id);
println!("Dependency depth: {}", analysis.dependency_depth);
println!("Has cycles: {}", analysis.has_cycles);
println!("Total cost: {}μs", analysis.total_cost);

// Resolve with optimization
let repo = container.resolve::<SqlRepository>(repo_id)?;
```

## Migration Steps

### 1. Update Service Definitions

**Before:**
```rust
pub trait ILogger: Service {
    fn log(&self, message: &str);
}

impl Service for ConsoleLogger {}
```

**After:**
```rust
// Services are now defined in the graph
// The trait can remain for type safety, but registration is graph-based
pub trait ILogger: Send + Sync {
    fn log(&self, message: &str);
}

// No need for Service trait implementation
```

### 2. Update Registration

**Before:**
```rust
builder.register_singleton::<ILogger, ConsoleLogger>(|| ConsoleLogger::new());
```

**After:**
```rust
let logger_id = builder
    .service("ILogger")
    .implementation("ConsoleLogger")
    .lifetime(ServiceLifetime::Singleton)
    .register();

// Store factory separately
container.factories.insert(logger_id, Arc::new(|_| {
    Box::new(ConsoleLogger::new())
}));
```

### 3. Add Performance Metadata

The new system encourages adding performance hints:

```rust
.with_metadata(ServiceMetadata {
    performance_hints: PerformanceHints {
        instantiation_cost: Some(2.5), // Measure actual cost
        memory_footprint: Some(size_of::<YourService>()),
        thread_safe: true, // Important for parallel resolution
        performs_io: false,
        cache_friendly: Some(0.8), // 0-1 score
    },
    ..Default::default()
})
```

### 4. Define Dependencies Explicitly

**Before:** Dependencies were implicit through constructor parameters

**After:** Dependencies are explicit edges in the graph

```rust
.depends_on("ILogger", DependencyKind::Constructor)
.depends_on("ICache", DependencyKind::Property)
```

### 5. Leverage AI Features

Add semantic information for better AI analysis:

```rust
.with_metadata(ServiceMetadata {
    // Embedding for semantic similarity (optional)
    embedding: Some(vec![0.1, 0.2, ...]), // From your ML model
    
    // Semantic tags for categorization
    tags: vec!["database", "repository", "sql"],
    
    // Behavioral versioning
    semantic_version: SemanticVersion {
        major: 1,
        minor: 0,
        patch: 0,
        behavior_hash: Some(0x1234ABCD), // Hash of contract/behavior
    },
    ..Default::default()
})
```

## Benefits After Migration

1. **Dependency Analysis**
   ```rust
   let analysis = container.analyze_dependencies(service_id);
   // Now you can see cycles, depth, costs, and parallel opportunities
   ```

2. **Optimized Resolution**
   - The container can pre-compute resolution paths
   - Parallel resolution for independent services
   - Hot path optimization based on usage statistics

3. **AI Integration**
   - Service graphs can be analyzed by FluentAi's optimizer
   - Embeddings enable semantic search for services
   - Performance predictions based on metadata

4. **Better Debugging**
   - Visual dependency graphs
   - Clear resolution traces
   - Performance bottleneck identification

## Compatibility Layer

For gradual migration, you can use both systems:

```rust
pub struct HybridContainer {
    traditional: Container,
    graph_based: GraphContainer,
}

impl HybridContainer {
    pub fn resolve_legacy<T: Service>(&self) -> Result<Arc<T>> {
        self.traditional.resolve::<T>()
    }
    
    pub fn resolve_graph<T: 'static>(&mut self, id: NodeId) -> Option<Arc<T>> {
        self.graph_based.resolve::<T>(id)
    }
}
```

## Example: Complete Service Definition

Here's a complete example of defining a service with all AI-first features:

```rust
let service_id = builder
    .service("IUserService")
    .implementation("UserServiceImpl")
    .lifetime(ServiceLifetime::Scoped)
    
    // Define dependencies
    .depends_on("IUserRepository", DependencyKind::Constructor)
    .depends_on("ILogger", DependencyKind::Constructor)
    .depends_on("ICache", DependencyKind::Property)
    
    // Add rich metadata
    .with_metadata(ServiceMetadata {
        // Performance characteristics
        performance_hints: PerformanceHints {
            instantiation_cost: Some(5.0),
            memory_footprint: Some(2048),
            thread_safe: true,
            performs_io: false,
            cache_friendly: Some(0.9),
        },
        
        // Semantic information
        tags: vec!["user", "business-logic", "core"],
        
        // Version based on behavior
        semantic_version: SemanticVersion {
            major: 2,
            minor: 1,
            patch: 0,
            behavior_hash: Some(hash_of_service_contract()),
        },
        
        // Optional ML embedding
        embedding: user_service_embedding(),
        
        ..Default::default()
    })
    .register();
```

## Next Steps

1. Start by migrating your most critical services
2. Add performance metadata based on actual measurements
3. Use the dependency analysis to identify optimization opportunities
4. Integrate with FluentAi's optimization pipeline
5. Monitor usage statistics to improve performance predictions