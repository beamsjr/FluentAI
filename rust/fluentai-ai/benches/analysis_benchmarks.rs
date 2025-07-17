//! Benchmarks for AI analysis performance

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fluentai_ai::{analyze_ast, create_default_analyzer, AiAnalyzerBuilder};
use fluentai_core::ast::{Graph, Node, Literal, NodeId};

/// Create a simple AST with n nodes
fn create_ast_of_size(n: usize) -> Graph {
    let mut graph = Graph::new();
    let mut nodes = Vec::new();
    
    // Create n variable nodes
    for i in 0..n {
        let node = graph.add_node(Node::Variable { 
            name: format!("var_{}", i) 
        }).unwrap();
        nodes.push(node);
    }
    
    // Create applications between nodes
    for i in 1..nodes.len() {
        graph.add_node(Node::Application {
            function: nodes[i-1],
            args: vec![nodes[i]],
        }).unwrap();
    }
    
    graph
}

/// Create a complex AST with nested structures
fn create_complex_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create a nested lambda structure
    let mut current = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    
    for i in 0..10 {
        let param = format!("x{}", i);
        let var = graph.add_node(Node::Variable { name: param.clone() }).unwrap();
        
        // Create a function body
        let body = graph.add_node(Node::Application {
            function: var,
            args: vec![current],
        }).unwrap();
        
        // Wrap in lambda
        current = graph.add_node(Node::Lambda {
            params: vec![param],
            body,
        }).unwrap();
    }
    
    // Add some map-reduce patterns
    let n1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    let n2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let n3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
    let list = graph.add_node(Node::List(vec![n1, n2, n3])).unwrap();
    
    let map_fn = graph.add_node(Node::Variable { name: "map".to_string() }).unwrap();
    let _mapped = graph.add_node(Node::Application {
        function: map_fn,
        args: vec![list, current],
    }).unwrap();
    
    graph
}

fn benchmark_basic_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("basic_analysis");
    
    for size in [10, 50, 100, 500].iter() {
        let graph = create_ast_of_size(*size);
        
        group.bench_with_input(
            BenchmarkId::new("analyze_ast", size),
            &graph,
            |b, graph| {
                b.iter(|| {
                    analyze_ast(black_box(graph))
                })
            },
        );
    }
    
    group.finish();
}

fn benchmark_analyzer_configurations(c: &mut Criterion) {
    let mut group = c.benchmark_group("analyzer_configs");
    let graph = create_complex_ast();
    
    // Full analysis
    let full_analyzer = create_default_analyzer();
    group.bench_function("full_analysis", |b| {
        b.iter(|| {
            full_analyzer.analyze_graph(black_box(&graph))
        })
    });
    
    // Minimal analysis (no embeddings)
    let minimal_analyzer = AiAnalyzerBuilder::new()
        .generate_embeddings(false)
        .build();
    group.bench_function("minimal_analysis", |b| {
        b.iter(|| {
            minimal_analyzer.analyze_graph(black_box(&graph))
        })
    });
    
    // Patterns only
    let patterns_analyzer = AiAnalyzerBuilder::new()
        .detect_optimizations(false)
        .generate_embeddings(false)
        .build();
    group.bench_function("patterns_only", |b| {
        b.iter(|| {
            patterns_analyzer.analyze_graph(black_box(&graph))
        })
    });
    
    group.finish();
}

fn benchmark_tensor_conversion(c: &mut Criterion) {
    use fluentai_ai::TensorBuilder;
    
    let mut group = c.benchmark_group("tensor_conversion");
    
    for size in [10, 50, 100, 500].iter() {
        let graph = create_ast_of_size(*size);
        let builder = TensorBuilder::new();
        
        group.bench_with_input(
            BenchmarkId::new("build_tensors", size),
            &graph,
            |b, graph| {
                b.iter(|| {
                    builder.build(black_box(graph))
                })
            },
        );
    }
    
    group.finish();
}

fn benchmark_cache_effectiveness(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache");
    let graph = create_complex_ast();
    let analyzer = create_default_analyzer();
    
    // Prime the cache
    analyzer.analyze_graph(&graph).unwrap();
    analyzer.clear_cache();
    
    // Benchmark cold vs warm cache
    group.bench_function("cold_cache", |b| {
        b.iter(|| {
            analyzer.clear_cache();
            analyzer.analyze_graph(black_box(&graph))
        })
    });
    
    // Prime cache again
    analyzer.analyze_graph(&graph).unwrap();
    
    group.bench_function("warm_cache", |b| {
        b.iter(|| {
            analyzer.analyze_graph(black_box(&graph))
        })
    });
    
    group.finish();
}

fn benchmark_metadata_operations(c: &mut Criterion) {
    use fluentai_ai::metadata::{MetadataInjector, MetadataExtractor, InjectorConfig};
    
    let mut group = c.benchmark_group("metadata");
    
    // Create graph and analysis result
    let mut graph = create_complex_ast();
    let analysis = analyze_ast(&graph).unwrap();
    let config = InjectorConfig::default();
    
    // Benchmark injection
    let injector = MetadataInjector::new(config.clone());
    group.bench_function("inject_metadata", |b| {
        b.iter(|| {
            let mut graph_copy = graph.clone();
            injector.inject(black_box(&mut graph_copy), black_box(&analysis))
        })
    });
    
    // Inject metadata for extraction benchmark
    injector.inject(&mut graph, &analysis).unwrap();
    
    // Benchmark extraction
    let extractor = MetadataExtractor::new(config);
    group.bench_function("extract_metadata", |b| {
        b.iter(|| {
            let node_ids: Vec<NodeId> = graph.node_ids().collect();
            for &node_id in &node_ids {
                extractor.extract_node_metadata(black_box(&graph), black_box(node_id));
            }
        })
    });
    
    group.finish();
}

criterion_group!(
    benches,
    benchmark_basic_analysis,
    benchmark_analyzer_configurations,
    benchmark_tensor_conversion,
    benchmark_cache_effectiveness,
    benchmark_metadata_operations
);
criterion_main!(benches);