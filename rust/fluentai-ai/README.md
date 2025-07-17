# FluentAI AI Analysis

Native AI-driven AST analysis for the FluentAI compiler, embedded directly into the AST without external dependencies.

## Features

- **Zero-Copy Analysis**: Direct feature extraction from AST nodes without serialization
- **Pattern Detection**: Identifies common code patterns (map-reduce, filter-map, recursion, etc.)
- **Optimization Suggestions**: Detects opportunities for constant folding, CSE, tail recursion, etc.
- **Node Embeddings**: Generates vector representations of AST nodes for ML models
- **Metadata Injection**: Enriches AST with AI analysis results
- **Multiple Backends**: Support for Burn, Candle, and ONNX (Burn is default)
- **Caching Layer**: Efficient caching of analysis results

## Usage

```rust
use fluentai_ai::{analyze_ast, AiAnalyzerBuilder};
use fluentai_core::ast::Graph;

// Basic analysis
let analysis = analyze_ast(&graph)?;
println!("Performance score: {}", analysis.performance_score);

// Custom analyzer with embeddings
let analyzer = AiAnalyzerBuilder::new()
    .generate_embeddings(true)
    .detect_patterns(true)
    .build();

let result = analyzer.analyze_graph(&graph)?;
```

## Architecture

The AI analysis system is composed of several key components:

1. **Feature Extraction** (`features.rs`): Extracts ML-ready features from AST nodes
2. **Tensor Conversion** (`tensor.rs`): Converts AST graphs to tensor format
3. **Pattern Detection** (`patterns.rs`): Identifies common code patterns
4. **Analysis Engine** (`analysis.rs`): Core analysis logic
5. **Metadata System** (`metadata.rs`): Injects/extracts AI metadata to/from AST
6. **ML Models** (`models.rs`): Backend-agnostic model interface

## Integration

The AI analysis integrates directly with the FluentAI compiler pipeline:

1. AST nodes implement `feature_vector()` method (when `ai-analysis` feature is enabled)
2. Analysis results are stored in `NodeMetadata.context_memory`
3. Zero runtime overhead when AI features are disabled

## Example

See `examples/analyze_ast.rs` for a complete example:

```bash
cargo run --example analyze_ast
```

## Performance

The AI analysis system is designed for performance:

- Feature extraction is O(n) where n is the number of nodes
- Caching reduces repeated analysis overhead
- Optional parallel processing with `rayon` feature
- Zero-copy tensor conversion

## Testing

Run tests with AI features enabled:

```bash
cargo test --features ai-analysis
```

Run benchmarks:

```bash
cargo bench
```