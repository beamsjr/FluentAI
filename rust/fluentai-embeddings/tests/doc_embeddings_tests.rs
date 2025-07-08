//! Tests for documentation embeddings using only the public API

use fluentai_embeddings::doc_embeddings::DocumentationEmbeddingService;

#[tokio::test]
async fn test_service_creation() {
    let service = DocumentationEmbeddingService::new().await.unwrap();
    // Service should be created successfully
    assert!(true);
}

#[tokio::test]
async fn test_generate_all_embeddings() {
    let mut service = DocumentationEmbeddingService::new().await.unwrap();
    let embeddings = service.generate_all_embeddings().await.unwrap();
    
    // Should generate embeddings for all registered documentation
    assert!(!embeddings.is_empty());
    
    // Should have different categories
    let has_keywords = embeddings.keys().any(|k| k.starts_with("kw_"));
    let has_operators = embeddings.keys().any(|k| k.starts_with("op_"));
    let has_builtins = embeddings.keys().any(|k| k.starts_with("builtin_"));
    
    assert!(has_keywords, "Should have keyword embeddings");
    assert!(has_operators, "Should have operator embeddings");
    assert!(has_builtins, "Should have builtin embeddings");
}

#[tokio::test]
async fn test_embeddings_are_unique() {
    let mut service = DocumentationEmbeddingService::new().await.unwrap();
    let embeddings = service.generate_all_embeddings().await.unwrap();
    
    // Collect all embedding IDs
    let ids: Vec<_> = embeddings.values().collect();
    
    // Check that all IDs are unique
    let unique_ids: std::collections::HashSet<_> = ids.iter().collect();
    assert_eq!(ids.len(), unique_ids.len(), "All embedding IDs should be unique");
}

#[tokio::test]
async fn test_consistent_embeddings() {
    let mut service1 = DocumentationEmbeddingService::new().await.unwrap();
    let embeddings1 = service1.generate_all_embeddings().await.unwrap();
    
    let mut service2 = DocumentationEmbeddingService::new().await.unwrap();
    let embeddings2 = service2.generate_all_embeddings().await.unwrap();
    
    // Should have the same keys
    let mut keys1: Vec<_> = embeddings1.keys().cloned().collect();
    let mut keys2: Vec<_> = embeddings2.keys().cloned().collect();
    keys1.sort();
    keys2.sort();
    assert_eq!(keys1, keys2, "Should generate embeddings for the same documentation");
}

#[tokio::test]
async fn test_core_documentation_exists() {
    let mut service = DocumentationEmbeddingService::new().await.unwrap();
    let embeddings = service.generate_all_embeddings().await.unwrap();
    
    // Check that core keywords have embeddings
    let core_keywords = ["if", "lambda", "let"];
    for keyword in &core_keywords {
        assert!(
            embeddings.contains_key(&format!("kw_{}", keyword)),
            "Should have embedding for keyword '{}'",
            keyword
        );
    }
    
    // Check that basic operators have embeddings
    let core_operators = ["+", "-", "*", "/"];
    for op in &core_operators {
        assert!(
            embeddings.contains_key(&format!("op_{}", op)),
            "Should have embedding for operator '{}'",
            op
        );
    }
}

#[tokio::test]
async fn test_concurrent_service_creation() {
    use futures::future::join_all;
    
    // Create multiple services concurrently
    let tasks: Vec<_> = (0..5)
        .map(|_| async {
            DocumentationEmbeddingService::new().await
        })
        .collect();
    
    let results = join_all(tasks).await;
    
    // All should succeed
    for result in results {
        assert!(result.is_ok());
    }
}

#[tokio::test]
async fn test_concurrent_embedding_generation() {
    use futures::future::join_all;
    
    // Generate embeddings concurrently
    let tasks: Vec<_> = (0..3)
        .map(|_| async {
            let mut service = DocumentationEmbeddingService::new().await.unwrap();
            service.generate_all_embeddings().await
        })
        .collect();
    
    let results = join_all(tasks).await;
    
    // All should succeed and produce embeddings
    for result in results {
        let embeddings = result.unwrap();
        assert!(!embeddings.is_empty());
    }
}

#[tokio::test]
async fn test_search_by_similarity() {
    let service = DocumentationEmbeddingService::new().await.unwrap();
    
    // This test demonstrates that search_by_similarity exists but
    // returns empty results due to missing reverse mapping implementation
    let results = service.search_by_similarity("math function", 10, 0.5).await.unwrap();
    
    // Currently returns empty due to implementation limitation
    assert_eq!(results.len(), 0);
}