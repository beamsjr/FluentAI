//! Tests for context-aware optimization pass

use fluentai_optimizer::passes::context_aware::ContextAwarePass;
use fluentai_optimizer::passes::OptimizationPass;
use fluentai_core::ast::{
    Graph, Node, ContextMemory, PerformanceHint, 
    PerformanceHintType, UsageStatistics
};
use fluentai_parser::parse;

/// Helper to create a context memory with specific attributes
fn create_context_memory(
    hot_path: bool,
    execution_count: u64,
    error_count: u64,
    hints: Vec<PerformanceHint>,
    tags: Vec<String>,
) -> ContextMemory {
    ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics {
            execution_count,
            error_count,
            is_hot_path: hot_path,
            avg_execution_time_ns: 0,
        },
        rationale: None,
        performance_hints: hints,
        semantic_tags: tags,
        last_modified: None,
    }
}

#[test]
fn test_context_aware_basic() {
    let mut pass = ContextAwarePass::new();
    let graph = parse("(+ 1 2)").unwrap();
    
    // Should run without errors
    let result = pass.run(&graph);
    assert!(result.is_ok());
    
    // Should preserve structure
    let optimized = result.unwrap();
    assert!(optimized.root_id.is_some());
    assert_eq!(graph.nodes.len(), optimized.nodes.len());
}

#[test]
fn test_should_inline_hot_small_function() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(lambda (x) (+ x 1))").unwrap();
    
    // Add context memory indicating this is a hot function
    if let Some(root) = graph.root_id {
        let context = create_context_memory(
            true,  // hot path
            2000,  // execution count
            0,     // error count
            vec![], // no hints yet
            vec![], // no tags
        );
        graph.set_context_memory(root, context);
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should add inline hint for hot small function
    if let Some(root) = optimized.root_id {
        // Check the root node (which should be the lambda)
        let context = optimized.get_context_memory(root);
        assert!(context.is_some());
        let context = context.unwrap();
        
        // Check for inline hint
        let has_inline_hint = context.performance_hints.iter()
            .any(|h| matches!(h.hint_type, PerformanceHintType::ShouldInline));
        assert!(has_inline_hint);
    }
}

#[test]
fn test_should_not_inline_error_prone_function() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(lambda (x) (/ 1 x))").unwrap();
    
    // Add context memory indicating high error rate
    if let Some(root) = graph.root_id {
        let context = create_context_memory(
            true,  // hot path
            1000,  // execution count
            200,   // 20% error rate
            vec![], // no hints
            vec![], // no tags
        );
        graph.set_context_memory(root, context);
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should NOT add inline hint for error-prone function
    if let Some(_root) = optimized.root_id {
        if let Some(mapped_id) = optimized.nodes.keys().find(|_| true) {
            if let Some(context) = optimized.get_context_memory(*mapped_id) {
                let has_inline_hint = context.performance_hints.iter()
                    .any(|h| matches!(h.hint_type, PerformanceHintType::ShouldInline));
                assert!(!has_inline_hint);
            }
        }
    }
}

#[test]
fn test_should_inline_with_existing_hint() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(lambda (x) x)").unwrap();
    
    // Add context memory with inline hint
    if let Some(root) = graph.root_id {
        let hints = vec![PerformanceHint {
            hint_type: PerformanceHintType::ShouldInline,
            confidence: 0.8,
            context: Some("User-provided hint".to_string()),
        }];
        let context = create_context_memory(false, 100, 0, hints, vec![]);
        graph.set_context_memory(root, context);
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should preserve existing hint
    if let Some(root) = optimized.root_id {
        let context = optimized.get_context_memory(root);
        assert!(context.is_some());
        let context = context.unwrap();
        
        let inline_hints: Vec<_> = context.performance_hints.iter()
            .filter(|h| matches!(h.hint_type, PerformanceHintType::ShouldInline))
            .collect();
        assert!(!inline_hints.is_empty());
    }
}

#[test]
fn test_should_unroll_with_hint() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(letrec ((loop (lambda (i) (if (< i 10) (loop (+ i 1)) i)))) (loop 0))").unwrap();
    
    // Add unroll hint to loop
    if let Some(root) = graph.root_id {
        let hints = vec![PerformanceHint {
            hint_type: PerformanceHintType::ShouldUnroll,
            confidence: 0.75,
            context: Some("Fixed iteration count".to_string()),
        }];
        let context = create_context_memory(true, 1000, 0, hints, vec!["fixed-iterations".to_string()]);
        graph.set_context_memory(root, context);
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
}

#[test]
fn test_can_vectorize_map_operation() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(map (lambda (x) (* x 2)) list)").unwrap();
    
    // Find the map application node and add context
    for (id, node) in &graph.nodes {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                if name == "map" {
                    let hints = vec![PerformanceHint {
                        hint_type: PerformanceHintType::CanVectorize,
                        confidence: 0.9,
                        context: Some("SIMD-compatible operation".to_string()),
                    }];
                    let context = create_context_memory(true, 5000, 0, hints, vec!["simd-compatible".to_string()]);
                    graph.set_context_memory(*id, context);
                    break;
                }
            }
        }
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should add vectorization hints and tags
    let mut found_vectorized_tag = false;
    for id in optimized.nodes.keys() {
        if let Some(context) = optimized.get_context_memory(*id) {
            if context.semantic_tags.contains(&"vectorized".to_string()) {
                found_vectorized_tag = true;
                break;
            }
        }
    }
    assert!(found_vectorized_tag);
}

#[test]
fn test_filter_vectorization() {
    let mut pass = ContextAwarePass::new();
    let code = "(filter (lambda (x) (> x 0)) numbers)";
    let mut graph = parse(code).unwrap();
    
    // Find filter application and mark as vectorizable
    for (id, node) in &graph.nodes {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                if name == "filter" {
                    let context = create_context_memory(
                        true, 1000, 0, vec![], vec!["simd-compatible".to_string()]
                    );
                    graph.set_context_memory(*id, context);
                    break;
                }
            }
        }
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should add vectorization hint for filter or have vectorized tag
    let mut found_vectorization = false;
    for id in optimized.nodes.keys() {
        if let Some(context) = optimized.get_context_memory(*id) {
            // Check for vectorize hint
            if context.performance_hints.iter()
                .any(|h| matches!(h.hint_type, PerformanceHintType::CanVectorize)) {
                found_vectorization = true;
                break;
            }
            // Or check for vectorized tag (added in third pass)
            if context.semantic_tags.contains(&"vectorized".to_string()) {
                found_vectorization = true;
                break;
            }
        }
    }
    assert!(found_vectorization, "Expected vectorization hint or tag");
}

#[test]
fn test_preserve_metadata() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(let ((f (lambda (x) x))) (f 5))").unwrap();
    
    // Add metadata to various nodes
    for (id, _) in graph.nodes.clone() {
        let metadata = graph.metadata_mut(id);
        metadata.span = Some((10, 15));
        metadata.annotations.push("test-annotation".to_string());
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should preserve all metadata
    for (_, _) in &optimized.nodes {
        for id in optimized.nodes.keys() {
            let metadata = optimized.get_metadata(*id);
            assert!(metadata.is_some());
            assert_eq!(metadata.unwrap().span, Some((10, 15)));
            assert!(metadata.unwrap().annotations.contains(&"test-annotation".to_string()));
        }
    }
}

#[test]
fn test_nested_lambdas() {
    let mut pass = ContextAwarePass::new();
    let code = "(lambda (x) (lambda (y) (+ x y)))";
    let mut graph = parse(code).unwrap();
    
    // Mark outer lambda as hot
    if let Some(root) = graph.root_id {
        let context = create_context_memory(true, 5000, 0, vec![], vec![]);
        graph.set_context_memory(root, context);
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
}

#[test]
fn test_large_function_not_inlined() {
    let mut pass = ContextAwarePass::new();
    
    // Create a large function with a simple nested structure that will generate many nodes
    let code = r#"
        (lambda (x)
            (let ((a (+ x 1))
                  (b (+ x 2))
                  (c (+ x 3))
                  (d (+ x 4))
                  (e (+ x 5))
                  (f (+ x 6))
                  (g (+ x 7))
                  (h (+ x 8))
                  (i (+ x 9))
                  (j (+ x 10)))
                (+ (+ (+ (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g) h) i) j)))
    "#;
    
    let mut graph = parse(code).unwrap();
    
    // Mark as hot path
    if let Some(root) = graph.root_id {
        let context = create_context_memory(true, 10000, 0, vec![], vec![]);
        graph.set_context_memory(root, context);
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should NOT inline large functions even if hot
    if let Some(root) = optimized.root_id {
        if let Some(context) = optimized.get_context_memory(root) {
            let has_inline_hint = context.performance_hints.iter()
                .any(|h| matches!(h.hint_type, PerformanceHintType::ShouldInline));
            assert!(!has_inline_hint, "Large function should not have inline hint");
        }
    }
}

#[test]
fn test_pass_name() {
    let pass = ContextAwarePass::new();
    assert_eq!(pass.name(), "context-aware");
}

#[test]
fn test_is_applicable() {
    let pass = ContextAwarePass::new();
    
    // Empty graph
    let empty_graph = Graph::new();
    assert!(!pass.is_applicable(&empty_graph));
    
    // Non-empty graph
    let graph = parse("(+ 1 2)").unwrap();
    assert!(pass.is_applicable(&graph));
}

#[test]
fn test_stats() {
    let pass = ContextAwarePass::new();
    let stats = pass.stats();
    assert!(stats.contains("context-aware"));
}

#[test]
fn test_multiple_hints_same_node() {
    let mut pass = ContextAwarePass::new();
    let mut graph = parse("(map (lambda (x) (* x x)) data)").unwrap();
    
    // Find map node and add multiple hints
    for (id, node) in &graph.nodes {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                if name == "map" {
                    let hints = vec![
                        PerformanceHint {
                            hint_type: PerformanceHintType::CanVectorize,
                            confidence: 0.9,
                            context: Some("SIMD operation".to_string()),
                        },
                        PerformanceHint {
                            hint_type: PerformanceHintType::ShouldUnroll,
                            confidence: 0.7,
                            context: Some("Known bounds".to_string()),
                        },
                    ];
                    let context = create_context_memory(
                        true, 2000, 0, hints, 
                        vec!["simd-compatible".to_string(), "fixed-iterations".to_string()]
                    );
                    graph.set_context_memory(*id, context);
                    break;
                }
            }
        }
    }
    
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should preserve all hints
    let mut found_multiple_hints = false;
    for id in optimized.nodes.keys() {
        if let Some(context) = optimized.get_context_memory(*id) {
            if context.performance_hints.len() >= 2 {
                found_multiple_hints = true;
                break;
            }
        }
    }
    assert!(found_multiple_hints);
}

#[test]
fn test_no_context_memory() {
    let mut pass = ContextAwarePass::new();
    let graph = parse("(lambda (x) (+ x 1))").unwrap();
    
    // Run without any context memory
    let result = pass.run(&graph);
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should not add any hints without context
    for id in optimized.nodes.keys() {
        if let Some(context) = optimized.get_context_memory(*id) {
            assert!(context.performance_hints.is_empty() || 
                   context.performance_hints.iter().all(|h| h.confidence < 0.5));
        }
    }
}