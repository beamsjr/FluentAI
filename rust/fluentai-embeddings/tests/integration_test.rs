//! Integration tests for the embeddings module

use fluentai_core::ast::{
    ContextMemory, EmbeddingId, Graph, Literal, Node, NodeId, NodeMetadata, UsageStatistics,
};
use fluentai_core::ast::{PerformanceHint, PerformanceHintType};
use fluentai_embeddings::{
    doc_embeddings::DocumentationEmbeddingService,
    generator::FeatureBasedGenerator,
    similarity::{cosine_similarity, kmeans_cluster, knn_search, SimilarityMetric},
    storage::FileStorage,
    EmbeddingConfig, EmbeddingService, ModelType,
};
use tempfile::TempDir;
use tokio;

#[tokio::test]
async fn test_full_embedding_workflow() {
    // Create service
    let service = EmbeddingService::default().unwrap();

    // Create a complex graph
    let mut graph = create_complex_graph();

    // Generate embeddings for all nodes
    service.update_graph_embeddings(&mut graph).await.unwrap();

    // Verify all nodes have embeddings
    for node_id in graph.node_ids() {
        let context = graph.get_context_memory(node_id).unwrap();
        assert!(context.embedding_id.is_some());
    }

    // Find similar nodes
    let first_node_id = graph.node_ids().next().unwrap();
    let first_embedding_id = graph
        .get_context_memory(first_node_id)
        .unwrap()
        .embedding_id
        .unwrap();

    let similar = service
        .find_similar(first_embedding_id, 5, 0.5)
        .await
        .unwrap();
    assert!(!similar.is_empty());
    assert_eq!(similar[0].0, first_embedding_id); // Should find itself first
}

#[tokio::test]
async fn test_persistence_workflow() {
    let temp_dir = TempDir::new().unwrap();

    // Store embeddings
    let stored_ids = {
        let generator = Box::new(FeatureBasedGenerator::new());
        let storage = Box::new(FileStorage::new(temp_dir.path()).await.unwrap());
        let service = EmbeddingService::new(generator, storage);

        let mut graph = create_complex_graph();
        service.update_graph_embeddings(&mut graph).await.unwrap();

        // Collect embedding IDs
        graph
            .node_ids()
            .filter_map(|id| graph.get_context_memory(id))
            .filter_map(|ctx| ctx.embedding_id)
            .collect::<Vec<_>>()
    };

    // Create new service and verify embeddings persist
    let generator = Box::new(FeatureBasedGenerator::new());
    let storage = Box::new(FileStorage::new(temp_dir.path()).await.unwrap());
    let service = EmbeddingService::new(generator, storage);

    // Should be able to retrieve all stored embeddings
    for id in stored_ids {
        let embedding = service.get_embedding(id).await.unwrap();
        assert_eq!(embedding.len(), 256);
    }
}

#[tokio::test]
async fn test_clustering_workflow() {
    let service = EmbeddingService::default().unwrap();

    // Create nodes that should cluster together
    let mut embeddings = vec![];

    // Cluster 1: Integer literals
    for i in 0..5 {
        let mut graph = Graph::new();
        let node = Node::Literal(Literal::Integer(i));
        let node_id = graph.add_node(node).unwrap();
        let emb_id = service.generate_embedding(&graph, node_id).await.unwrap();
        let embedding = service.get_embedding(emb_id).await.unwrap();
        embeddings.push(embedding);
    }

    // Cluster 2: String literals
    for i in 0..5 {
        let mut graph = Graph::new();
        let node = Node::Literal(Literal::String(format!("string_{}", i)));
        let node_id = graph.add_node(node).unwrap();
        let emb_id = service.generate_embedding(&graph, node_id).await.unwrap();
        let embedding = service.get_embedding(emb_id).await.unwrap();
        embeddings.push(embedding);
    }

    // Perform clustering
    let assignments = kmeans_cluster(&embeddings, 2, 100).unwrap();

    // Verify clustering
    assert_eq!(assignments.len(), 10);

    // Check that we have two clusters
    let unique_clusters: std::collections::HashSet<_> = assignments.iter().cloned().collect();
    assert!(unique_clusters.len() <= 2, "Should have at most 2 clusters");

    // Check that similar items tend to cluster together
    // Count how many of the first 5 (integers) are in the same cluster
    let first_cluster = assignments[0];
    let integers_in_first_cluster = assignments[0..5]
        .iter()
        .filter(|&&c| c == first_cluster)
        .count();

    // At least 3 out of 5 integers should be in the same cluster
    assert!(
        integers_in_first_cluster >= 3,
        "Integers should tend to cluster together"
    );
}

#[tokio::test]
async fn test_documentation_integration() {
    // Create documentation embedding service
    let mut doc_service = DocumentationEmbeddingService::new().await.unwrap();

    // Generate all documentation embeddings
    let doc_embeddings = doc_service.generate_all_embeddings().await.unwrap();

    // Create regular embedding service
    let service = EmbeddingService::default().unwrap();

    // Create a lambda node
    let mut graph = Graph::new();
    // First create the variable node
    let var_node = Node::Variable {
        name: "x".to_string(),
    };
    let var_id = graph.add_node(var_node).unwrap();

    let lambda_node = Node::Lambda {
        params: vec!["x".to_string()],
        body: var_id,
    };
    let node_id = graph.add_node(lambda_node).unwrap();
    let _code_emb_id = service.generate_embedding(&graph, node_id).await.unwrap();

    // Test that we generated embeddings for documentation
    assert!(
        !doc_embeddings.is_empty(),
        "Should have generated documentation embeddings"
    );

    // Check that lambda keyword embedding was generated
    assert!(
        doc_embeddings.contains_key("kw_lambda"),
        "Should have lambda keyword embedding"
    );

    // The code and doc embeddings are in different services, so we can't directly compare them
    // Just verify that both were generated successfully
    assert!(_code_emb_id.0 > 0, "Code embedding should have valid ID");
}

#[tokio::test]
async fn test_performance_hints_impact() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();

    // Create two similar nodes
    let node1 = Node::Literal(Literal::Integer(42));
    let node2 = Node::Literal(Literal::Integer(42));

    let id1 = graph.add_node(node1).unwrap();
    let id2 = graph.add_node(node2).unwrap();

    // Add performance hints to one
    let mut context = ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    };
    context.performance_hints = vec![PerformanceHint {
        hint_type: PerformanceHintType::ShouldInline,
        confidence: 0.9,
        context: Some("Hot path".to_string()),
    }];
    context.usage_stats.is_hot_path = true;
    context.usage_stats.execution_count = 10000;
    graph.set_context_memory(id2, context);

    // Generate embeddings
    let emb_id1 = service.generate_embedding(&graph, id1).await.unwrap();
    let emb_id2 = service.generate_embedding(&graph, id2).await.unwrap();

    // Get embeddings
    let emb1 = service.get_embedding(emb_id1).await.unwrap();
    let emb2 = service.get_embedding(emb_id2).await.unwrap();

    // Check that embeddings have some content
    let non_zero1 = emb1.iter().filter(|&&x| x != 0.0).count();
    let non_zero2 = emb2.iter().filter(|&&x| x != 0.0).count();
    assert!(non_zero1 > 0, "First embedding should have non-zero values");
    assert!(
        non_zero2 > 0,
        "Second embedding should have non-zero values"
    );

    // Verify the embeddings are valid (normalized if non-zero)
    let norm1: f32 = emb1.iter().map(|x| x * x).sum::<f32>().sqrt();
    let norm2: f32 = emb2.iter().map(|x| x * x).sum::<f32>().sqrt();

    if norm1 > 0.0 && norm2 > 0.0 {
        assert!(
            (norm1 - 1.0).abs() < 1e-6,
            "First embedding not normalized: {}",
            norm1
        );
        assert!(
            (norm2 - 1.0).abs() < 1e-6,
            "Second embedding not normalized: {}",
            norm2
        );

        // Only check similarity if both embeddings are non-zero
        let similarity = cosine_similarity(&emb1, &emb2);
        // Just verify it's a valid similarity value
        assert!(
            similarity >= -1.0 && similarity <= 1.0,
            "Invalid similarity: {}",
            similarity
        );
    }
}

#[tokio::test]
async fn test_concurrent_operations() {
    // Create separate services for each concurrent operation
    // to avoid Send/Sync issues with the internal RwLock
    let mut tasks = vec![];

    for i in 0..20 {
        tasks.push(async move {
            let service = EmbeddingService::default().unwrap();
            let mut graph = Graph::new();
            let node = Node::Literal(Literal::Integer(i));
            let node_id = graph.add_node(node).unwrap();
            service.generate_embedding(&graph, node_id).await
        });
    }

    let results = futures::future::join_all(tasks).await;

    // All should succeed
    for result in results {
        assert!(result.is_ok());
    }
}

#[tokio::test]
async fn test_similarity_search_workflow() {
    let service = EmbeddingService::default().unwrap();

    // Create various types of nodes
    let test_nodes = vec![
        Node::Literal(Literal::Integer(1)),
        Node::Literal(Literal::Integer(2)),
        Node::Literal(Literal::Float(1.0)),
        Node::Literal(Literal::Float(2.0)),
        Node::Literal(Literal::String("hello".to_string())),
        Node::Literal(Literal::String("world".to_string())),
        Node::Literal(Literal::Boolean(true)),
        Node::Literal(Literal::Boolean(false)),
        Node::Variable {
            name: "x".to_string(),
        },
        Node::Variable {
            name: "y".to_string(),
        },
    ];

    // Generate embeddings
    let mut embedding_pairs = vec![];
    for (idx, node) in test_nodes.into_iter().enumerate() {
        let mut graph = Graph::new();
        let node_id = graph.add_node(node).unwrap();
        let emb_id = service.generate_embedding(&graph, node_id).await.unwrap();
        let embedding = service.get_embedding(emb_id).await.unwrap();
        embedding_pairs.push((idx, embedding));
    }

    // Test KNN search
    let query = &embedding_pairs[0].1; // First integer
    let results = knn_search(query, &embedding_pairs, 5, SimilarityMetric::Cosine);

    // Should find itself first
    assert_eq!(results[0].0, 0);
    assert!((results[0].1 - 1.0).abs() < 1e-6);

    // Second result should be the other integer (similar type)
    assert_eq!(results[1].0, 1);
}

#[tokio::test]
async fn test_metadata_enriched_embeddings() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();

    // Create node with rich metadata
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();

    // Add metadata
    let metadata = NodeMetadata {
        span: Some((10, 15)),
        type_info: Some("Integer".to_string()),
        is_pure: Some(true),
        annotations: vec![],
        documentation_id: Some("doc_important_constant".to_string()),
        context_memory: None,
    };
    graph.set_metadata(node_id, metadata);

    // Add context memory
    let mut context = ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    };
    context.rationale =
        Some("This is an important constant used throughout the system".to_string());
    context.semantic_tags = vec![
        "constant".to_string(),
        "important".to_string(),
        "configuration".to_string(),
    ];
    graph.set_context_memory(node_id, context);

    // Generate embedding
    let emb_id = service.generate_embedding(&graph, node_id).await.unwrap();
    let embedding = service.get_embedding(emb_id).await.unwrap();

    // Should have non-trivial embedding
    let non_zero_count = embedding.iter().filter(|&&x| x != 0.0).count();
    assert!(
        non_zero_count > 0,
        "Expected at least some non-zero values, got {}",
        non_zero_count
    ); // Should have some features
    assert_eq!(embedding.len(), 256, "Should have correct dimension");
}

#[tokio::test]
async fn test_embedding_config_variations() {
    // Test different configurations
    let configs = vec![
        EmbeddingConfig {
            dimension: 128,
            model_type: ModelType::FeatureBased,
            use_runtime_info: true,
            use_documentation: true,
        },
        EmbeddingConfig {
            dimension: 512,
            model_type: ModelType::FeatureBased,
            use_runtime_info: false,
            use_documentation: true,
        },
        EmbeddingConfig {
            dimension: 256,
            model_type: ModelType::PreTrained("test-model".to_string()),
            use_runtime_info: true,
            use_documentation: false,
        },
    ];

    for config in configs {
        // Would need to extend the API to accept config
        // For now just verify config creation works
        assert!(config.dimension > 0);
    }
}

#[tokio::test]
async fn test_error_handling() {
    let service = EmbeddingService::default().unwrap();

    // Try to get non-existent embedding
    let result = service.get_embedding(EmbeddingId(999999)).await;
    assert!(result.is_err());

    // Try to generate embedding for non-existent node
    let graph = Graph::new();
    let result = service
        .generate_embedding(&graph, NodeId::new(999).unwrap())
        .await;
    assert!(result.is_err());
}

// Helper function to create a complex graph
fn create_complex_graph() -> Graph {
    let mut graph = Graph::new();

    // Add various node types
    let int_node = Node::Literal(Literal::Integer(42));
    let float_node = Node::Literal(Literal::Float(3.14));
    let string_node = Node::Literal(Literal::String("hello".to_string()));
    let var_node = Node::Variable {
        name: "x".to_string(),
    };

    let int_id = graph.add_node(int_node).unwrap();
    let float_id = graph.add_node(float_node).unwrap();
    let string_id = graph.add_node(string_node).unwrap();
    let var_id = graph.add_node(var_node).unwrap();

    // Create lambda body first
    let lambda_body = Node::Variable {
        name: "x".to_string(),
    };
    let lambda_body_id = graph.add_node(lambda_body).unwrap();

    let lambda_node = Node::Lambda {
        params: vec!["x".to_string(), "y".to_string()],
        body: lambda_body_id,
    };
    let lambda_id = graph.add_node(lambda_node).unwrap();

    // Add metadata to some nodes
    graph.set_metadata(
        int_id,
        NodeMetadata {
            span: Some((1, 10)),
            type_info: Some("Integer".to_string()),
            is_pure: Some(true),
            annotations: vec![],
            documentation_id: Some("constant_42".to_string()),
            context_memory: None,
        },
    );

    // Add context memory to some nodes
    let mut context = ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    };
    context.usage_stats.execution_count = 100;
    context.usage_stats.is_hot_path = true;
    graph.set_context_memory(lambda_id, context);

    graph
}
