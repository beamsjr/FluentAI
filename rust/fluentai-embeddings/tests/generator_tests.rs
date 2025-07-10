//! Comprehensive tests for embedding generators

use fluentai_core::ast::{ContextMemory, Graph, Literal, Node, NodeMetadata, UsageStatistics};
use fluentai_core::ast::{PerformanceHint, PerformanceHintType};
use fluentai_embeddings::{
    generator::{EmbeddingGenerator, FeatureBasedGenerator},
    EmbeddingContext,
};
use fluentai_optimizer::ml_hints::ProgramFeatures;
use std::collections::HashMap;

#[tokio::test]
async fn test_feature_based_generator_new() {
    let generator = FeatureBasedGenerator::new();
    assert_eq!(generator.dimension(), 256);
}

#[tokio::test]
async fn test_feature_based_generator_with_dimension() {
    let generator = FeatureBasedGenerator::with_dimension(512);
    assert_eq!(generator.dimension(), 512);

    let generator = FeatureBasedGenerator::with_dimension(128);
    assert_eq!(generator.dimension(), 128);
}

#[tokio::test]
async fn test_generate_from_features() {
    let generator = FeatureBasedGenerator::new();

    // Test with empty features
    let features = HashMap::new();
    let embedding = generator.generate_from_features(&features).unwrap();
    assert_eq!(embedding.len(), 256);
    assert!(embedding.iter().all(|&x| x == 0.0));

    // Test with single feature
    let mut features = HashMap::new();
    features.insert("test_feature".to_string(), 1.0);
    let embedding = generator.generate_from_features(&features).unwrap();
    assert_eq!(embedding.len(), 256);

    // Should be L2 normalized
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!((norm - 1.0).abs() < 1e-6);

    // Test with multiple features
    let mut features = HashMap::new();
    features.insert("feature1".to_string(), 0.5);
    features.insert("feature2".to_string(), 0.8);
    features.insert("feature3".to_string(), 1.2);
    let embedding = generator.generate_from_features(&features).unwrap();
    assert_eq!(embedding.len(), 256);

    // Should be L2 normalized
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!((norm - 1.0).abs() < 1e-6);
}

#[tokio::test]
async fn test_generate_from_features_deterministic() {
    let generator = FeatureBasedGenerator::new();

    let mut features = HashMap::new();
    features.insert("test".to_string(), 1.0);

    // Generate embedding twice - should be identical
    let embedding1 = generator.generate_from_features(&features).unwrap();
    let embedding2 = generator.generate_from_features(&features).unwrap();

    assert_eq!(embedding1, embedding2);
}

#[tokio::test]
async fn test_node_features_all_types() {
    let generator = FeatureBasedGenerator::new();

    // Test each node type
    let test_cases = vec![
        (Node::Literal(Literal::Integer(42)), "Integer literal"),
        (Node::Literal(Literal::Float(3.14)), "Float literal"),
        (
            Node::Literal(Literal::String("test".to_string())),
            "String literal",
        ),
        (Node::Literal(Literal::Boolean(true)), "Boolean literal"),
        (Node::Literal(Literal::Nil), "Nil literal"),
        (
            Node::Variable {
                name: "x".to_string(),
            },
            "Variable",
        ),
        (Node::List(vec![]), "List"),
        (Node::Channel { capacity: None }, "Channel"),
    ];

    for (node, description) in test_cases {
        let context = create_test_context(node);
        let embedding = generator.generate(&context).await.unwrap();
        assert_eq!(embedding.len(), 256, "Failed for {}", description);

        // Should be normalized
        let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
        assert!(
            (norm - 1.0).abs() < 1e-6,
            "Not normalized for {}",
            description
        );
    }
}

#[tokio::test]
async fn test_generate_with_full_context() {
    let generator = FeatureBasedGenerator::new();

    // Create rich context with all features
    let mut context_memory = ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    };
    context_memory.usage_stats.execution_count = 1000;
    context_memory.usage_stats.avg_execution_time_ns = 5000;
    context_memory.usage_stats.error_count = 5;
    context_memory.usage_stats.is_hot_path = true;
    context_memory.rationale = Some("Optimization for performance".to_string());
    context_memory.performance_hints = vec![
        PerformanceHint {
            hint_type: PerformanceHintType::ShouldInline,
            confidence: 0.9,
            context: Some("Small function".to_string()),
        },
        PerformanceHint {
            hint_type: PerformanceHintType::CanVectorize {
                simd_width: Some(256),
            },
            confidence: 0.8,
            context: Some("Simple loop".to_string()),
        },
    ];
    context_memory.semantic_tags = vec!["hot_path".to_string(), "performance_critical".to_string()];

    let mut features = ProgramFeatures::default();
    features.node_count = 100;
    features.depth = 10;
    features.branching_factor = 2.5;
    features.has_recursion = true;
    features.has_loops = true;
    features.uses_lists = true;
    features.hotness_score = 0.9;

    let context = EmbeddingContext {
        node: Node::Literal(Literal::Integer(42)),
        metadata: Some(NodeMetadata {
            span: None,
            type_info: None,
            is_pure: Some(true),
            annotations: vec![],
            documentation_id: Some("test_doc".to_string()),
            context_memory: None,
        }),
        context_memory: Some(context_memory),
        features,
        documentation: Some("Test documentation".to_string()),
    };

    let embedding = generator.generate(&context).await.unwrap();
    assert_eq!(embedding.len(), 256);

    // Should be normalized
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!((norm - 1.0).abs() < 1e-6);

    // Should have non-zero values from all the context
    let non_zero_count = embedding.iter().filter(|&&x| x != 0.0).count();
    assert!(
        non_zero_count >= 40,
        "Expected at least 40 non-zero values, got {}",
        non_zero_count
    );
}

#[tokio::test]
async fn test_generate_with_minimal_context() {
    let generator = FeatureBasedGenerator::new();

    let context = create_test_context(Node::Literal(Literal::Integer(42)));
    let embedding = generator.generate(&context).await.unwrap();

    assert_eq!(embedding.len(), 256);

    // Should still be normalized
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!((norm - 1.0).abs() < 1e-6);
}

#[tokio::test]
async fn test_generate_different_dimensions() {
    for dim in [64, 128, 256, 512, 1024] {
        let generator = FeatureBasedGenerator::with_dimension(dim);
        let context = create_test_context(Node::Literal(Literal::Integer(42)));
        let embedding = generator.generate(&context).await.unwrap();

        assert_eq!(embedding.len(), dim);

        // Should be normalized
        let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
        assert!(
            (norm - 1.0).abs() < 1e-6,
            "Not normalized for dimension {}",
            dim
        );
    }
}

#[tokio::test]
async fn test_hash_to_features() {
    let generator = FeatureBasedGenerator::new();

    // Test through generate_from_features which uses hash internally
    let mut features1 = HashMap::new();
    features1.insert("test_string".to_string(), 1.0);
    let embedding1 = generator.generate_from_features(&features1).unwrap();

    let mut features2 = HashMap::new();
    features2.insert("different_string".to_string(), 1.0);
    let embedding2 = generator.generate_from_features(&features2).unwrap();

    // Different strings should produce different embeddings
    assert_ne!(embedding1, embedding2);

    // Same string should produce same embedding (deterministic)
    let embedding1_repeat = generator.generate_from_features(&features1).unwrap();
    assert_eq!(embedding1, embedding1_repeat);
}

#[tokio::test]
async fn test_performance_hints_influence() {
    let generator = FeatureBasedGenerator::new();

    // Context without performance hints
    let mut context1 = create_test_context(Node::Literal(Literal::Integer(42)));
    context1.context_memory = Some(ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    });
    let embedding1 = generator.generate(&context1).await.unwrap();

    // Context with performance hints
    let mut context2 = context1.clone();
    if let Some(ref mut mem) = context2.context_memory {
        mem.performance_hints = vec![
            PerformanceHint {
                hint_type: PerformanceHintType::ShouldInline,
                confidence: 0.9,
                context: None,
            },
            PerformanceHint {
                hint_type: PerformanceHintType::CanVectorize {
                    simd_width: Some(256),
                },
                confidence: 0.8,
                context: None,
            },
        ];
    }
    let embedding2 = generator.generate(&context2).await.unwrap();

    // Embeddings should be different
    assert_ne!(embedding1, embedding2);
}

#[tokio::test]
async fn test_usage_stats_influence() {
    let generator = FeatureBasedGenerator::new();

    // Context with zero usage stats
    let mut context1 = create_test_context(Node::Literal(Literal::Integer(42)));
    context1.context_memory = Some(ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    });
    let embedding1 = generator.generate(&context1).await.unwrap();

    // Context with high usage stats
    let mut context2 = context1.clone();
    if let Some(ref mut mem) = context2.context_memory {
        mem.usage_stats.execution_count = 10000;
        mem.usage_stats.avg_execution_time_ns = 50000;
        mem.usage_stats.is_hot_path = true;
    }
    let embedding2 = generator.generate(&context2).await.unwrap();

    // Embeddings should be different
    assert_ne!(embedding1, embedding2);
}

#[tokio::test]
async fn test_edge_cases() {
    let generator = FeatureBasedGenerator::new();

    // Test with very small dimension
    let small_gen = FeatureBasedGenerator::with_dimension(1);
    let context = create_test_context(Node::Literal(Literal::Integer(42)));
    let embedding = small_gen.generate(&context).await.unwrap();
    assert_eq!(embedding.len(), 1);

    // Test with large numbers in features
    let mut features = ProgramFeatures::default();
    features.node_count = usize::MAX;
    features.arithmetic_ops = usize::MAX;
    features.estimated_iterations = Some(usize::MAX);

    let context = EmbeddingContext {
        node: Node::Literal(Literal::Integer(42)),
        metadata: None,
        context_memory: None,
        features,
        documentation: None,
    };

    let embedding = generator.generate(&context).await.unwrap();
    assert_eq!(embedding.len(), 256);

    // Should still be normalized (no infinities)
    assert!(embedding.iter().all(|&x| x.is_finite()));
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!((norm - 1.0).abs() < 1e-6);
}

// Helper function to create test context
fn create_test_context(node: Node) -> EmbeddingContext {
    EmbeddingContext {
        node,
        metadata: None,
        context_memory: None,
        features: ProgramFeatures::default(),
        documentation: None,
    }
}
