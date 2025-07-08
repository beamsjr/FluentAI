//! Comprehensive tests for embedding storage

use fluentai_embeddings::storage::{EmbeddingStorage, InMemoryStorage, FileStorage};
use fluentai_core::ast::EmbeddingId;
use std::sync::Arc;
use std::thread;
use tempfile::TempDir;
use tokio;

// Helper function to create test embeddings
fn create_test_embedding(dim: usize, value: f32) -> Vec<f32> {
    vec![value; dim]
}

fn create_normalized_embedding(values: &[f32]) -> Vec<f32> {
    let norm: f32 = values.iter().map(|x| x * x).sum::<f32>().sqrt();
    if norm > 0.0 {
        values.iter().map(|x| x / norm).collect()
    } else {
        values.to_vec()
    }
}

#[tokio::test]
async fn test_in_memory_storage_basic() {
    let storage = InMemoryStorage::new();
    
    // Test store
    let embedding = create_test_embedding(128, 0.5);
    let id = storage.store(embedding.clone()).await.unwrap();
    assert_eq!(id.0, 1);
    
    // Test retrieve
    let retrieved = storage.retrieve(id).await.unwrap();
    assert_eq!(retrieved, embedding);
    
    // Test count
    let count = storage.count().await.unwrap();
    assert_eq!(count, 1);
}

#[tokio::test]
async fn test_in_memory_storage_multiple() {
    let storage = InMemoryStorage::new();
    
    // Store multiple embeddings
    let ids: Vec<_> = futures::future::join_all((0..10).map(|i| {
        let embedding = create_test_embedding(64, i as f32 * 0.1);
        storage.store(embedding)
    }))
    .await
    .into_iter()
    .collect::<Result<Vec<_>, _>>()
    .unwrap();
    
    // Check IDs are sequential
    for (i, id) in ids.iter().enumerate() {
        assert_eq!(id.0, (i + 1) as u64);
    }
    
    // Check count
    assert_eq!(storage.count().await.unwrap(), 10);
    
    // Retrieve all
    for (i, id) in ids.iter().enumerate() {
        let retrieved = storage.retrieve(*id).await.unwrap();
        let expected = create_test_embedding(64, i as f32 * 0.1);
        assert_eq!(retrieved, expected);
    }
}

#[tokio::test]
async fn test_in_memory_storage_delete() {
    let storage = InMemoryStorage::new();
    
    // Store embeddings
    let id1 = storage.store(create_test_embedding(32, 0.1)).await.unwrap();
    let id2 = storage.store(create_test_embedding(32, 0.2)).await.unwrap();
    let id3 = storage.store(create_test_embedding(32, 0.3)).await.unwrap();
    
    assert_eq!(storage.count().await.unwrap(), 3);
    
    // Delete middle one
    storage.delete(id2).await.unwrap();
    assert_eq!(storage.count().await.unwrap(), 2);
    
    // Should not be able to retrieve deleted
    assert!(storage.retrieve(id2).await.is_err());
    
    // Others should still exist
    assert!(storage.retrieve(id1).await.is_ok());
    assert!(storage.retrieve(id3).await.is_ok());
    
    // Delete non-existent should error
    assert!(storage.delete(EmbeddingId(999)).await.is_err());
}

#[tokio::test]
async fn test_in_memory_storage_search() {
    let storage = InMemoryStorage::new();
    
    // Store some normalized embeddings
    let emb1 = create_normalized_embedding(&[1.0, 0.0, 0.0]);
    let emb2 = create_normalized_embedding(&[0.9, 0.1, 0.0]);
    let emb3 = create_normalized_embedding(&[0.0, 1.0, 0.0]);
    let emb4 = create_normalized_embedding(&[0.0, 0.0, 1.0]);
    
    let id1 = storage.store(emb1.clone()).await.unwrap();
    let id2 = storage.store(emb2.clone()).await.unwrap();
    let id3 = storage.store(emb3.clone()).await.unwrap();
    let id4 = storage.store(emb4.clone()).await.unwrap();
    
    // Search for similar to emb1
    let results = storage.search(&emb1, 3, 0.5).await.unwrap();
    
    // Should find emb1 (similarity 1.0) and emb2 (high similarity)
    assert!(results.len() >= 2);
    assert_eq!(results[0].0, id1);
    assert!((results[0].1 - 1.0).abs() < 1e-6);
    assert_eq!(results[1].0, id2);
    assert!(results[1].1 > 0.8);
    
    // Search with high threshold
    let results = storage.search(&emb1, 10, 0.95).await.unwrap();
    // emb2 has cosine similarity ~0.994 with emb1, so both will be included
    assert!(results.len() >= 1 && results.len() <= 2);
    assert_eq!(results[0].0, id1); // emb1 should be first (exact match)
    
    // Search for k=1
    let results = storage.search(&emb3, 1, 0.0).await.unwrap();
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].0, id3);
}

#[tokio::test]
async fn test_in_memory_storage_search_empty() {
    let storage = InMemoryStorage::new();
    
    let query = create_normalized_embedding(&[1.0, 0.0, 0.0]);
    let results = storage.search(&query, 10, 0.0).await.unwrap();
    assert_eq!(results.len(), 0);
}

#[tokio::test]
async fn test_in_memory_storage_concurrent() {
    let storage = Arc::new(InMemoryStorage::new());
    
    // Spawn multiple threads to store embeddings concurrently
    let handles: Vec<_> = (0..10).map(|i| {
        let storage = Arc::clone(&storage);
        thread::spawn(move || {
            let rt = tokio::runtime::Runtime::new().unwrap();
            rt.block_on(async {
                let embedding = create_test_embedding(32, i as f32);
                storage.store(embedding).await
            })
        })
    }).collect();
    
    // Wait for all threads
    let ids: Vec<_> = handles.into_iter()
        .map(|h| h.join().unwrap())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    
    // All IDs should be unique
    let unique_ids: std::collections::HashSet<_> = ids.iter().collect();
    assert_eq!(unique_ids.len(), 10);
    
    // Count should be 10
    assert_eq!(storage.count().await.unwrap(), 10);
}

#[tokio::test]
async fn test_file_storage_basic() {
    let temp_dir = TempDir::new().unwrap();
    let storage = FileStorage::new(temp_dir.path()).await.unwrap();
    
    // Test store
    let embedding = create_test_embedding(128, 0.5);
    let id = storage.store(embedding.clone()).await.unwrap();
    assert_eq!(id.0, 1);
    
    // Test retrieve
    let retrieved = storage.retrieve(id).await.unwrap();
    assert_eq!(retrieved, embedding);
    
    // Test count
    let count = storage.count().await.unwrap();
    assert_eq!(count, 1);
    
    // Check index file exists
    assert!(temp_dir.path().join("index.json").exists());
}

#[tokio::test]
async fn test_file_storage_persistence() {
    let temp_dir = TempDir::new().unwrap();
    
    // Store some embeddings
    let ids = {
        let storage = FileStorage::new(temp_dir.path()).await.unwrap();
        
        let id1 = storage.store(create_test_embedding(64, 0.1)).await.unwrap();
        let id2 = storage.store(create_test_embedding(64, 0.2)).await.unwrap();
        let id3 = storage.store(create_test_embedding(64, 0.3)).await.unwrap();
        
        vec![id1, id2, id3]
    };
    
    // Create new storage instance (simulating restart)
    let storage = FileStorage::new(temp_dir.path()).await.unwrap();
    
    // Should be able to retrieve all embeddings
    assert_eq!(storage.count().await.unwrap(), 3);
    
    for (i, id) in ids.iter().enumerate() {
        let retrieved = storage.retrieve(*id).await.unwrap();
        let expected = create_test_embedding(64, (i + 1) as f32 * 0.1);
        assert_eq!(retrieved, expected);
    }
    
    // Next ID should continue from where it left off
    let new_id = storage.store(create_test_embedding(64, 0.4)).await.unwrap();
    assert_eq!(new_id.0, 4);
}

#[tokio::test]
async fn test_file_storage_delete() {
    let temp_dir = TempDir::new().unwrap();
    let storage = FileStorage::new(temp_dir.path()).await.unwrap();
    
    // Store embeddings
    let id1 = storage.store(create_test_embedding(32, 0.1)).await.unwrap();
    let id2 = storage.store(create_test_embedding(32, 0.2)).await.unwrap();
    let id3 = storage.store(create_test_embedding(32, 0.3)).await.unwrap();
    
    // Delete one
    storage.delete(id2).await.unwrap();
    
    // File should be removed
    assert!(!temp_dir.path().join(format!("{}.emb", id2.0)).exists());
    
    // Should not be retrievable
    assert!(storage.retrieve(id2).await.is_err());
    
    // Count should be updated
    assert_eq!(storage.count().await.unwrap(), 2);
}

#[tokio::test]
async fn test_file_storage_search() {
    let temp_dir = TempDir::new().unwrap();
    let storage = FileStorage::new(temp_dir.path()).await.unwrap();
    
    // Store normalized embeddings
    let emb1 = create_normalized_embedding(&[1.0, 0.0]);
    let emb2 = create_normalized_embedding(&[0.8, 0.6]);
    let emb3 = create_normalized_embedding(&[0.0, 1.0]);
    
    let id1 = storage.store(emb1.clone()).await.unwrap();
    let id2 = storage.store(emb2.clone()).await.unwrap();
    let id3 = storage.store(emb3.clone()).await.unwrap();
    
    // Search
    let results = storage.search(&emb1, 2, 0.5).await.unwrap();
    assert_eq!(results.len(), 2);
    assert_eq!(results[0].0, id1);
    assert_eq!(results[1].0, id2);
}

#[tokio::test]
async fn test_file_storage_corrupted_index() {
    let temp_dir = TempDir::new().unwrap();
    
    // Create corrupted index file
    tokio::fs::write(temp_dir.path().join("index.json"), "invalid json").await.unwrap();
    
    // Should fail to create storage
    assert!(FileStorage::new(temp_dir.path()).await.is_err());
}

#[tokio::test]
async fn test_file_storage_missing_file() {
    let temp_dir = TempDir::new().unwrap();
    let storage = FileStorage::new(temp_dir.path()).await.unwrap();
    
    // Store an embedding
    let id = storage.store(create_test_embedding(32, 0.5)).await.unwrap();
    
    // Delete the file manually
    tokio::fs::remove_file(temp_dir.path().join(format!("{}.emb", id.0))).await.unwrap();
    
    // Should fail to retrieve
    assert!(storage.retrieve(id).await.is_err());
}

#[tokio::test]
async fn test_cosine_similarity_edge_cases() {
    let storage = InMemoryStorage::new();
    
    // Store zero vector
    let zero_vec = vec![0.0; 10];
    let id1 = storage.store(zero_vec.clone()).await.unwrap();
    
    // Store normal vector
    let normal_vec = create_normalized_embedding(&[1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]);
    let id2 = storage.store(normal_vec.clone()).await.unwrap();
    
    // Search with zero vector - should return 0 similarity for all
    let results = storage.search(&zero_vec, 10, -1.0).await.unwrap();
    for (_, similarity) in results {
        assert_eq!(similarity, 0.0);
    }
}

#[tokio::test]
async fn test_mismatched_dimensions() {
    let storage = InMemoryStorage::new();
    
    // Store embedding of dimension 10
    let emb1 = create_test_embedding(10, 0.5);
    storage.store(emb1).await.unwrap();
    
    // Search with different dimension
    let query = create_test_embedding(5, 0.5);
    let results = storage.search(&query, 10, -1.0).await.unwrap();
    
    // Should return 0 similarity due to dimension mismatch
    for (_, similarity) in results {
        assert_eq!(similarity, 0.0);
    }
}

#[tokio::test]
async fn test_large_embeddings() {
    let storage = InMemoryStorage::new();
    
    // Store large embedding (1024 dimensions)
    let large_emb = create_test_embedding(1024, 0.1);
    let id = storage.store(large_emb.clone()).await.unwrap();
    
    // Should retrieve correctly
    let retrieved = storage.retrieve(id).await.unwrap();
    assert_eq!(retrieved.len(), 1024);
    assert_eq!(retrieved, large_emb);
}

#[tokio::test]
async fn test_file_storage_compression() {
    let temp_dir = TempDir::new().unwrap();
    let storage = FileStorage::new(temp_dir.path()).await.unwrap();
    
    // Store embedding with specific values
    let embedding = vec![1.0, -2.5, 3.14159, 0.0, f32::MIN, f32::MAX];
    let id = storage.store(embedding.clone()).await.unwrap();
    
    // Retrieve and check values are preserved
    let retrieved = storage.retrieve(id).await.unwrap();
    assert_eq!(retrieved.len(), embedding.len());
    
    for (original, retrieved) in embedding.iter().zip(retrieved.iter()) {
        assert!((original - retrieved).abs() < 1e-6 || 
                (original.is_infinite() && retrieved.is_infinite() && original.signum() == retrieved.signum()));
    }
}