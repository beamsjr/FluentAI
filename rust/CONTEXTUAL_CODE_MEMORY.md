# Contextual Code Memory Implementation

This document describes the hybrid Documentation + Contextual Code Memory system implemented for FluentAi.

## Overview

We've implemented a hybrid approach that combines:
1. **Human-readable documentation** via the existing Documentation trait
2. **Machine-readable context** via the new ContextMemory system
3. **Semantic embeddings** for AI-powered features

## Architecture

### 1. Enhanced NodeMetadata

The `NodeMetadata` structure in `fluentai-core/src/ast.rs` now includes:

```rust
pub struct NodeMetadata {
    // Existing fields...
    pub documentation_id: Option<String>,        // Link to Documentation trait
    pub context_memory: Option<ContextMemory>,   // Machine-readable context
}

pub struct ContextMemory {
    pub embedding_id: Option<EmbeddingId>,       // Reference to semantic embedding
    pub usage_stats: UsageStatistics,            // Runtime performance data
    pub rationale: Option<String>,               // Why this code exists
    pub performance_hints: Vec<PerformanceHint>, // Optimization suggestions
    pub semantic_tags: Vec<String>,              // Categorization
    pub last_modified: Option<u64>,              // Timestamp
}
```

### 2. Documentation Service

Created `fluentai-lsp/src/documentation_service.rs` that:
- Bridges the DocumentationRegistry with LSP
- Generates documentation from ContextMemory for user-defined functions
- Provides unified access to all documentation sources

### 3. Embedding Infrastructure

Created `fluentai-embeddings/` crate with:

#### Core Components:
- **EmbeddingService**: Manages generation and retrieval of embeddings
- **EmbeddingGenerator**: Trait for different embedding strategies
- **EmbeddingStorage**: Trait for storage backends (in-memory, file-based)

#### Features:
- **Feature-based embeddings** using AST structure, ML hints, and documentation
- **256-dimensional vectors** capturing:
  - Node type features (32 dims)
  - Structural features from ML hints (32 dims)
  - Usage statistics (16 dims)
  - Documentation/rationale features (64 dims)
- **Similarity search** with cosine, Euclidean, and Manhattan distances
- **K-means clustering** for grouping similar code

## Benefits

### 1. For Developers
- Rich hover information combining docs + runtime stats
- Context-aware code completion
- Semantic code search ("find functions that process lists")

### 2. For Optimization
- Performance hints based on actual usage
- Hot path detection
- Similarity-based optimization decisions

### 3. For AI Integration
- Semantic embeddings enable ML-powered features
- Usage patterns inform optimization strategies
- Context preservation for better code understanding

## Implementation Status

### ✅ Completed
1. Enhanced AST with ContextMemory infrastructure
2. Documentation service for LSP integration
3. Embedding generation from multiple sources
4. Storage backends (in-memory and file-based)
5. Similarity computation utilities

### ✅ Completed
1. Enhanced AST with ContextMemory infrastructure
2. Documentation service for LSP integration
3. Embedding generation from multiple sources
4. Storage backends (in-memory and file-based)
5. Similarity computation utilities
6. REPL help commands using Documentation trait
7. Runtime usage tracking in VM
8. Integration with optimizer for context-aware decisions
9. Generate embeddings from existing documentation

## Usage Examples

### 1. Setting Context Memory
```rust
let mut graph = Graph::new();
let node_id = graph.add_node(node);

// Set documentation link
graph.set_documentation(node_id, "list-operations".to_string());

// Set context memory
let context = ContextMemory {
    embedding_id: None,
    usage_stats: UsageStatistics::default(),
    rationale: Some("Optimized list processing for packet filtering".to_string()),
    performance_hints: vec![
        PerformanceHint {
            hint_type: PerformanceHintType::CanVectorize,
            confidence: 0.9,
            context: Some("SIMD operations available".to_string()),
        }
    ],
    semantic_tags: vec!["list-processing", "performance-critical"],
    last_modified: Some(1234567890),
};
graph.set_context_memory(node_id, context);
```

### 2. Generating Embeddings
```rust
let service = EmbeddingService::default()?;

// Generate embedding for a node
let embedding_id = service.generate_embedding(&graph, node_id).await?;

// Find similar nodes
let similar = service.find_similar(embedding_id, 5, 0.8).await?;
```

### 3. LSP Integration
```rust
let doc_service = DocumentationService::new();

// Get documentation (combines registry + context)
if let Some(doc) = doc_service.get_node_documentation(&graph, node_id) {
    let hover_text = doc_service.format_hover(&doc);
    // Display in IDE...
}
```

## Performance Considerations

### Memory Overhead
- ~100 bytes per node for ContextMemory structure
- 1KB per embedding (256 * 4 bytes)
- Can be loaded on-demand for large programs

### Runtime Cost
- Zero when disabled (Option<ContextMemory>)
- Embedding generation: ~1ms per node (one-time)
- Similarity search: O(n) brute force, can add indexing

### Storage
- In-memory: Fast but limited by RAM
- File-based: Persistent with compression
- Can use external vector databases for scale

## Context-Aware Optimization

The system includes a context-aware optimization pass that uses ContextMemory to make better decisions:

### Features

1. **Hot Path Inlining**: Automatically inlines small functions that are frequently executed
2. **Vectorization**: Identifies operations that can benefit from SIMD instructions
3. **Error-Aware Optimization**: Avoids aggressive optimizations for error-prone code
4. **Performance Hint Integration**: Uses ML-generated hints to guide optimizations

### Example Usage

```rust
use fluentai_optimizer::passes::context_aware::ContextAwarePass;

// Add context memory to nodes
let context = ContextMemory {
    usage_stats: UsageStatistics {
        execution_count: 5000,
        is_hot_path: true,
        // ...
    },
    performance_hints: vec![
        PerformanceHint {
            hint_type: PerformanceHintType::ShouldInline,
            confidence: 0.9,
            // ...
        }
    ],
    // ...
};
graph.set_context_memory(node_id, context);

// Run optimization
let mut pass = ContextAwarePass::new();
let optimized = pass.run(&graph)?;
```

## Future Enhancements

1. **Pre-trained code models**: Use models like CodeBERT for better embeddings
2. **Distributed learning**: Learn from community usage patterns
3. **Automatic optimization**: AI suggests optimizations based on context
4. **Cross-language embeddings**: Share understanding between languages
5. **Version-aware embeddings**: Track how code evolves over time
6. **Adaptive optimization**: Learn from optimization outcomes to improve decisions

## Conclusion

The hybrid Documentation + Contextual Code Memory system provides:
- **Best of both worlds**: Human docs + machine understanding
- **Gradual adoption**: Can add context incrementally
- **Rich tooling**: Better IDE support and optimization
- **AI-ready**: Foundation for future ML-powered features

This aligns perfectly with FluentAi's AI-first philosophy while maintaining excellent developer experience.