# FluentAI: AI-First Design Guide

This guide documents the AI-first design principles used throughout FluentAI and provides guidelines for maintaining consistency across all modules.

## Core Principles

### 1. Graph-Based AST as the Foundation

All code representation should be based on the core AST graph structure defined in `fluentai-core`:

```rust
pub struct Graph {
    pub nodes: HashMap<NodeId, Node>,
    pub edges: HashMap<NodeId, Vec<NodeId>>,
    pub roots: Vec<NodeId>,
    pub context_memory: Option<ContextMemory>, // AI semantic information
}
```

**Why:** Graphs are naturally suited for AI analysis, optimization, and transformation. They make relationships explicit and enable powerful graph algorithms.

### 2. Semantic Information Over Syntax

Instead of string-based representations, use semantic nodes with rich metadata:

```rust
// ❌ Bad: String-based
pub struct Service {
    name: String,
    dependencies: Vec<String>,
}

// ✅ Good: Graph-based with semantics
pub struct ServiceNode {
    id: NodeId,
    interface: NodeId,  // Reference to interface node
    implementation: NodeId,  // Reference to implementation node
    dependencies: Vec<DependencyEdge>,
    metadata: ServiceMetadata,  // AI-friendly metadata
}
```

### 3. AI-Friendly Metadata

Every major concept should include metadata for AI optimization:

```rust
pub struct Metadata {
    // Semantic embedding for similarity search
    pub embedding: Option<Vec<f32>>,
    
    // Performance characteristics
    pub performance_hints: PerformanceHints,
    
    // Usage statistics for learning
    pub usage_stats: UsageStats,
    
    // Semantic versioning based on behavior
    pub semantic_version: SemanticVersion,
    
    // Tags for categorization
    pub tags: Vec<String>,
}
```

### 4. Explicit Relationships

Dependencies and relationships should be explicit edges in the graph:

```rust
pub struct RelationshipEdge {
    pub source: NodeId,
    pub target: NodeId,
    pub relationship_type: RelationType,
    pub metadata: EdgeMetadata,
}
```

## Module-Specific Guidelines

### Dependency Injection (fluentai-di)

**Current State:** Mixed - traditional DI exists alongside new graph-based DI

**Target State:** Full graph-based service definitions

```rust
// Services defined as nodes in the AST
let service_id = builder
    .service("IUserService")
    .implementation("UserServiceImpl")
    .depends_on("IRepository", DependencyKind::Constructor)
    .with_metadata(metadata)
    .register();

// Resolution through graph traversal
let analysis = container.analyze_dependencies(service_id);
let service = container.resolve::<UserService>(service_id);
```

### Database (fluentai-db)

**Current State:** Partially integrated - effects use AST, schemas still string-based

**Target State:** Graph-based schema representation

```rust
// Schemas as AST graphs
let mut builder = SchemaGraphBuilder::new();
builder.schema("public")
    .table("users")
    .column("id", "uuid")
    .column("email", "varchar")
    .with_hints(TableHints {
        row_count: Some(1000000),
        access_frequency: Some(1000.0),
        cache_strategy: CacheStrategy::ReadThrough,
        ..Default::default()
    })
    .build();

// Analyze schema for optimizations
let analysis = schema.analyze();
```

### Contracts (fluentai-contracts)

**Good Example:** Already well-integrated with AST

```rust
pub struct ContractCondition {
    pub expression: NodeId,  // AST node reference
    pub kind: ContractKind,
    pub message: Option<String>,
    pub span: Option<Span>,
    pub blame_label: Option<String>,
}
```

### Effects System (fluentai-effects)

**Good Example:** Properly uses AST effect types

```rust
pub enum EffectType {
    IO(IoEffect),
    State(StateEffect),
    Error(ErrorEffect),
    // ... all integrated with core AST
}
```

## Best Practices

### 1. Use NodeId References Instead of Strings

```rust
// ❌ Bad
pub struct Type {
    name: String,
    base_type: Option<String>,
}

// ✅ Good
pub struct TypeNode {
    id: NodeId,
    kind: TypeKind,
    base_type: Option<NodeId>,  // Reference to base type node
}
```

### 2. Include Performance Hints

```rust
pub struct PerformanceHints {
    pub estimated_cost: Option<f64>,  // microseconds
    pub memory_usage: Option<usize>,  // bytes
    pub cache_friendly: Option<f32>,  // 0-1 score
    pub parallelizable: bool,
}
```

### 3. Support AI Analysis

```rust
impl AnalyzableNode for YourNode {
    fn analyze(&self, graph: &Graph) -> Analysis {
        Analysis {
            complexity: self.calculate_complexity(),
            dependencies: self.get_dependencies(),
            optimization_opportunities: self.find_optimizations(),
        }
    }
}
```

### 4. Version Based on Behavior

```rust
pub struct SemanticVersion {
    pub major: u32,  // Breaking behavior changes
    pub minor: u32,  // New capabilities
    pub patch: u32,  // Bug fixes
    pub behavior_hash: Option<u64>,  // Hash of contract/interface
}
```

## Migration Strategy

For modules that need to be migrated:

1. **Phase 1: Add Graph Representation**
   - Keep existing API for compatibility
   - Add parallel graph-based implementation
   - Mark old API as deprecated

2. **Phase 2: Migrate Internal Usage**
   - Update internal code to use graph-based API
   - Maintain compatibility layer for external users

3. **Phase 3: Full Migration**
   - Remove deprecated string-based APIs
   - Update all documentation and examples

## Benefits of AI-First Design

1. **Better Optimization**
   - Graph algorithms can find optimization opportunities
   - Performance predictions based on metadata
   - Automatic parallelization detection

2. **Semantic Analysis**
   - Find similar code patterns using embeddings
   - Behavioral versioning instead of syntactic
   - Rich metadata for ML models

3. **Tool Integration**
   - LSP can provide better suggestions
   - Debuggers can show relationships visually
   - Profilers can use performance hints

4. **Future-Proofing**
   - Ready for AI-driven development tools
   - Supports advanced program synthesis
   - Enables intelligent code transformation

## Examples of AI-First Features

### Dependency Analysis
```rust
let analysis = container.analyze_dependencies(service_id);
println!("Circular dependencies: {}", analysis.has_cycles);
println!("Max depth: {}", analysis.dependency_depth);
println!("Parallel groups: {:?}", analysis.parallel_groups);
```

### Schema Optimization
```rust
let analysis = schema.analyze();
for suggestion in analysis.missing_indexes {
    println!("Suggest index on {} for {} improvement",
        suggestion.columns, suggestion.benefit_score);
}
```

### Contract Debugging
```rust
// Visual debugging with AI-generated suggestions
let debug_info = debugger.analyze_failure(violation);
println!("{}", debug_info.visual_diagram);
for suggestion in debug_info.suggestions {
    println!("Try: {}", suggestion);
}
```

## Checklist for New Modules

When creating a new module, ensure:

- [ ] Uses AST Graph structure from fluentai-core
- [ ] Represents concepts as nodes with NodeIds
- [ ] Includes AI-friendly metadata (embeddings, performance hints)
- [ ] Makes relationships explicit as edges
- [ ] Provides analysis capabilities
- [ ] Supports behavioral versioning
- [ ] Includes performance characteristics
- [ ] Documents optimization opportunities
- [ ] Integrates with effect system where appropriate
- [ ] Provides graph-based query/traversal APIs

## Anti-Patterns to Avoid

1. **String-based type systems** - Use NodeId references
2. **Implicit dependencies** - Make all relationships explicit edges
3. **Missing metadata** - Always include performance and semantic information
4. **Opaque operations** - Provide analysis and introspection capabilities
5. **Syntax-only representation** - Include semantic information

## Conclusion

FluentAI's AI-first design is what sets it apart from traditional programming languages. By consistently applying these principles across all modules, we create a system that is not just usable by AI, but optimized for AI analysis, transformation, and optimization from the ground up.