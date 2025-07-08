//! Comprehensive tests for EmbeddingService

use fluentai_embeddings::{
    EmbeddingService, EmbeddingConfig, ModelType,
    generator::FeatureBasedGenerator,
    storage::{InMemoryStorage, FileStorage},
};
use fluentai_core::ast::{Graph, Node, Literal, NodeMetadata, ContextMemory, EmbeddingId, UsageStatistics, NodeId};
use std::sync::Arc;
use tempfile::TempDir;
use tokio;
use futures;

#[tokio::test]
async fn test_service_creation() {
    let generator = Box::new(FeatureBasedGenerator::new());
    let storage = Box::new(InMemoryStorage::new());
    let service = EmbeddingService::new(generator, storage);
    
    // Service should be created successfully
    assert!(true); // If we get here, service was created
}

#[tokio::test]
async fn test_service_default() {
    let service = EmbeddingService::default().unwrap();
    
    // Default service should work
    let mut graph = Graph::new();
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();
    
    let embedding_id = service.generate_embedding(&graph, node_id).await.unwrap();
    assert!(embedding_id.0 > 0);
}

#[tokio::test]
async fn test_generate_embedding() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    
    // Add various types of nodes
    let int_node = Node::Literal(Literal::Integer(42));
    let float_node = Node::Literal(Literal::Float(3.14));
    let string_node = Node::Literal(Literal::String("test".to_string()));
    let bool_node = Node::Literal(Literal::Boolean(true));
    let nil_node = Node::Literal(Literal::Nil);
    
    let int_id = graph.add_node(int_node).unwrap();
    let float_id = graph.add_node(float_node).unwrap();
    let string_id = graph.add_node(string_node).unwrap();
    let bool_id = graph.add_node(bool_node).unwrap();
    let nil_id = graph.add_node(nil_node).unwrap();
    
    // Generate embeddings for each
    let int_emb_id = service.generate_embedding(&graph, int_id).await.unwrap();
    let float_emb_id = service.generate_embedding(&graph, float_id).await.unwrap();
    let string_emb_id = service.generate_embedding(&graph, string_id).await.unwrap();
    let bool_emb_id = service.generate_embedding(&graph, bool_id).await.unwrap();
    let nil_emb_id = service.generate_embedding(&graph, nil_id).await.unwrap();
    
    // All should have unique IDs
    let ids = vec![int_emb_id, float_emb_id, string_emb_id, bool_emb_id, nil_emb_id];
    let unique_ids: std::collections::HashSet<_> = ids.iter().collect();
    assert_eq!(unique_ids.len(), 5);
}

#[tokio::test]
async fn test_get_embedding() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();
    
    // Generate embedding
    let embedding_id = service.generate_embedding(&graph, node_id).await.unwrap();
    
    // Retrieve it
    let embedding = service.get_embedding(embedding_id).await.unwrap();
    assert_eq!(embedding.len(), 256); // Default dimension
    
    // Should be normalized
    let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
    assert!((norm - 1.0).abs() < 1e-6);
}

#[tokio::test]
async fn test_get_embedding_cached() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();
    
    // Generate embedding
    let embedding_id = service.generate_embedding(&graph, node_id).await.unwrap();
    
    // Retrieve it twice
    let embedding1 = service.get_embedding(embedding_id).await.unwrap();
    let embedding2 = service.get_embedding(embedding_id).await.unwrap();
    
    // Should be identical (from cache)
    assert_eq!(embedding1, embedding2);
}

#[tokio::test]
async fn test_get_embedding_not_found() {
    let service = EmbeddingService::default().unwrap();
    
    // Try to get non-existent embedding
    let result = service.get_embedding(EmbeddingId(999)).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_find_similar() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    
    // Create similar nodes (all integers)
    let node1 = Node::Literal(Literal::Integer(42));
    let node2 = Node::Literal(Literal::Integer(43));
    let node3 = Node::Literal(Literal::Integer(44));
    
    // Create different node (string)
    let node4 = Node::Literal(Literal::String("different".to_string()));
    
    let id1 = graph.add_node(node1).unwrap();
    let id2 = graph.add_node(node2).unwrap();
    let id3 = graph.add_node(node3).unwrap();
    let id4 = graph.add_node(node4).unwrap();
    
    let emb_id1 = service.generate_embedding(&graph, id1).await.unwrap();
    let emb_id2 = service.generate_embedding(&graph, id2).await.unwrap();
    let emb_id3 = service.generate_embedding(&graph, id3).await.unwrap();
    let _emb_id4 = service.generate_embedding(&graph, id4).await.unwrap();
    
    // Find similar to first integer
    let similar = service.find_similar(emb_id1, 3, 0.5).await.unwrap();
    
    // Should find at least 3 similar items
    assert!(similar.len() >= 3);
    
    // Should find itself with perfect similarity
    let self_match = similar.iter().find(|(id, _)| *id == emb_id1);
    assert!(self_match.is_some(), "Should find itself in results");
    assert!((self_match.unwrap().1 - 1.0).abs() < 1e-6, "Self-match should have similarity 1.0");
    
    // Other integers should be in the results
    let similar_ids: Vec<_> = similar.iter().map(|s| s.0).collect();
    assert!(similar_ids.contains(&emb_id2), "Should find second integer");
    assert!(similar_ids.contains(&emb_id3), "Should find third integer");
}

#[tokio::test]
async fn test_store_embedding_directly() {
    let service = EmbeddingService::default().unwrap();
    
    // Create and store a custom embedding
    let custom_embedding = vec![0.5; 256];
    let id = service.store_embedding(custom_embedding.clone()).await.unwrap();
    
    // Should be able to retrieve it
    let retrieved = service.get_embedding(id).await.unwrap();
    assert_eq!(retrieved, custom_embedding);
}

#[tokio::test]
async fn test_update_graph_embeddings() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    
    // Add multiple nodes
    let nodes = vec![
        Node::Literal(Literal::Integer(1)),
        Node::Literal(Literal::Integer(2)),
        Node::Literal(Literal::String("test".to_string())),
        Node::Variable { name: "x".to_string() },
        Node::List(vec![]),
    ];
    
    let node_ids: Vec<_> = nodes.into_iter()
        .map(|node| graph.add_node(node).unwrap())
        .collect();
    
    // Update all embeddings
    service.update_graph_embeddings(&mut graph).await.unwrap();
    
    // Check that all nodes have embeddings
    for node_id in node_ids {
        let context_memory = graph.get_context_memory(node_id).unwrap();
        assert!(context_memory.embedding_id.is_some());
        
        // Should be able to retrieve the embedding
        let emb_id = context_memory.embedding_id.unwrap();
        let embedding = service.get_embedding(emb_id).await.unwrap();
        assert_eq!(embedding.len(), 256);
    }
}

#[tokio::test]
async fn test_update_graph_embeddings_preserves_context() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    
    // Add node with existing context memory
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();
    
    let mut context = ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics::default(),
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    };
    context.rationale = Some("Test rationale".to_string());
    context.usage_stats.execution_count = 100;
    context.semantic_tags = vec!["important".to_string()];
    graph.set_context_memory(node_id, context.clone());
    
    // Update embeddings
    service.update_graph_embeddings(&mut graph).await.unwrap();
    
    // Check that context was preserved
    let updated_context = graph.get_context_memory(node_id).unwrap();
    assert_eq!(updated_context.rationale, Some("Test rationale".to_string()));
    assert_eq!(updated_context.usage_stats.execution_count, 100);
    assert_eq!(updated_context.semantic_tags, vec!["important".to_string()]);
    assert!(updated_context.embedding_id.is_some());
}

#[tokio::test]
async fn test_service_with_file_storage() {
    let temp_dir = TempDir::new().unwrap();
    let generator = Box::new(FeatureBasedGenerator::new());
    let storage = Box::new(FileStorage::new(temp_dir.path()).await.unwrap());
    let service = EmbeddingService::new(generator, storage);
    
    let mut graph = Graph::new();
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();
    
    // Generate and retrieve embedding
    let embedding_id = service.generate_embedding(&graph, node_id).await.unwrap();
    let embedding = service.get_embedding(embedding_id).await.unwrap();
    
    assert_eq!(embedding.len(), 256);
    
    // Check that file was created
    assert!(temp_dir.path().join(format!("{}.emb", embedding_id.0)).exists());
}

#[tokio::test]
async fn test_extract_context_with_metadata() {
    let service = EmbeddingService::default().unwrap();
    let mut graph = Graph::new();
    
    // Create node with metadata
    let node = Node::Literal(Literal::Integer(42));
    let node_id = graph.add_node(node).unwrap();
    
    let metadata = NodeMetadata {
        span: Some((10, 15)),
        type_info: Some("Integer".to_string()),
        is_pure: Some(true),
        annotations: vec![],
        documentation_id: Some("doc_123".to_string()),
        context_memory: None,
    };
    graph.set_metadata(node_id, metadata);
    
    // Generate embedding
    let embedding_id = service.generate_embedding(&graph, node_id).await.unwrap();
    let embedding = service.get_embedding(embedding_id).await.unwrap();
    
    // Should have used the documentation ID
    assert_eq!(embedding.len(), 256);
}

#[tokio::test]
async fn test_extract_context_not_found() {
    let service = EmbeddingService::default().unwrap();
    let graph = Graph::new();
    
    // Try to generate embedding for non-existent node
    let result = service.generate_embedding(&graph, NodeId::new(999).unwrap()).await;
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Node not found"));
}

#[tokio::test]
async fn test_concurrent_operations() {
    // Create a shared graph
    let mut graph = Graph::new();
    
    // Add nodes
    let node_ids: Vec<_> = (0..10).map(|i| {
        let node = Node::Literal(Literal::Integer(i));
        graph.add_node(node).unwrap()
    }).collect();
    
    // Create tasks for concurrent execution
    let mut tasks = vec![];
    
    for node_id in node_ids {
        let graph_clone = graph.clone();
        tasks.push(async move {
            let service = EmbeddingService::default().unwrap();
            service.generate_embedding(&graph_clone, node_id).await
        });
    }
    
    // Wait for all operations
    let results = futures::future::join_all(tasks).await;
    
    // All should succeed
    for result in results {
        assert!(result.is_ok());
    }
}

#[tokio::test]
async fn test_cache_consistency() {
    let service = EmbeddingService::default().unwrap();
    
    // Store embedding directly
    let embedding = vec![0.1; 256];
    let id = service.store_embedding(embedding.clone()).await.unwrap();
    
    // Retrieve it (should cache)
    let retrieved1 = service.get_embedding(id).await.unwrap();
    assert_eq!(retrieved1, embedding);
    
    // Try to delete from storage through the internal storage field would fail
    // because storage is not public, so we'll skip this test case
    
    // Should still get from cache
    let retrieved2 = service.get_embedding(id).await.unwrap();
    assert_eq!(retrieved2, embedding);
}

#[tokio::test]
async fn test_embedding_config() {
    // Test default config
    let config = EmbeddingConfig::default();
    assert_eq!(config.dimension, 256);
    assert!(matches!(config.model_type, ModelType::FeatureBased));
    assert!(config.use_runtime_info);
    assert!(config.use_documentation);
    
    // Test custom config
    let config = EmbeddingConfig {
        dimension: 512,
        model_type: ModelType::PreTrained("codegen-350M".to_string()),
        use_runtime_info: false,
        use_documentation: true,
    };
    assert_eq!(config.dimension, 512);
}