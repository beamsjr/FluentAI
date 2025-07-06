//! Embedding storage backends

use crate::EmbeddingId;
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
use std::path::{Path, PathBuf};
use tokio::fs;
use tokio::io::AsyncReadExt;

/// Trait for embedding storage backends
#[async_trait]
pub trait EmbeddingStorage: Send + Sync {
    /// Store an embedding and return its ID
    async fn store(&self, embedding: Vec<f32>) -> Result<EmbeddingId>;
    
    /// Retrieve an embedding by ID
    async fn retrieve(&self, id: EmbeddingId) -> Result<Vec<f32>>;
    
    /// Delete an embedding
    async fn delete(&self, id: EmbeddingId) -> Result<()>;
    
    /// Search for similar embeddings
    async fn search(
        &self,
        query: &[f32],
        k: usize,
        threshold: f32,
    ) -> Result<Vec<(EmbeddingId, f32)>>;
    
    /// Get total number of stored embeddings
    async fn count(&self) -> Result<usize>;
}

/// In-memory embedding storage
pub struct InMemoryStorage {
    embeddings: Arc<RwLock<FxHashMap<EmbeddingId, Vec<f32>>>>,
    next_id: Arc<RwLock<u64>>,
}

impl InMemoryStorage {
    pub fn new() -> Self {
        Self {
            embeddings: Arc::new(RwLock::new(FxHashMap::default())),
            next_id: Arc::new(RwLock::new(1)),
        }
    }
}

#[async_trait]
impl EmbeddingStorage for InMemoryStorage {
    async fn store(&self, embedding: Vec<f32>) -> Result<EmbeddingId> {
        let id = {
            let mut next_id = self.next_id.write()
                .map_err(|_| anyhow!("Lock poisoned"))?;
            let id = EmbeddingId(*next_id);
            *next_id += 1;
            id
        };
        
        self.embeddings.write()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .insert(id, embedding);
        
        Ok(id)
    }
    
    async fn retrieve(&self, id: EmbeddingId) -> Result<Vec<f32>> {
        self.embeddings.read()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .get(&id)
            .cloned()
            .ok_or_else(|| anyhow!("Embedding not found"))
    }
    
    async fn delete(&self, id: EmbeddingId) -> Result<()> {
        self.embeddings.write()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .remove(&id)
            .ok_or_else(|| anyhow!("Embedding not found"))?;
        Ok(())
    }
    
    async fn search(
        &self,
        query: &[f32],
        k: usize,
        threshold: f32,
    ) -> Result<Vec<(EmbeddingId, f32)>> {
        let embeddings = self.embeddings.read()
            .map_err(|_| anyhow!("Lock poisoned"))?;
        
        let mut similarities: Vec<(EmbeddingId, f32)> = embeddings
            .iter()
            .map(|(id, emb)| {
                let similarity = cosine_similarity(query, emb);
                (*id, similarity)
            })
            .filter(|(_, sim)| *sim >= threshold)
            .collect();
        
        // Sort by similarity (descending)
        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        similarities.truncate(k);
        
        Ok(similarities)
    }
    
    async fn count(&self) -> Result<usize> {
        Ok(self.embeddings.read()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .len())
    }
}

/// File-based embedding storage with compression
pub struct FileStorage {
    base_path: PathBuf,
    index: Arc<RwLock<FxHashMap<EmbeddingId, PathBuf>>>,
    next_id: Arc<RwLock<u64>>,
}

impl FileStorage {
    pub async fn new(base_path: impl AsRef<Path>) -> Result<Self> {
        let base_path = base_path.as_ref().to_path_buf();
        fs::create_dir_all(&base_path).await?;
        
        let index_path = base_path.join("index.json");
        let (index, next_id) = if index_path.exists() {
            let data = fs::read_to_string(&index_path).await?;
            let index: FxHashMap<u64, String> = serde_json::from_str(&data)?;
            let max_id = index.keys().max().copied().unwrap_or(0);
            let index = index.into_iter()
                .map(|(id, path)| (EmbeddingId(id), PathBuf::from(path)))
                .collect();
            (index, max_id + 1)
        } else {
            (FxHashMap::default(), 1)
        };
        
        Ok(Self {
            base_path,
            index: Arc::new(RwLock::new(index)),
            next_id: Arc::new(RwLock::new(next_id)),
        })
    }
    
    async fn save_index(&self) -> Result<()> {
        let (json, index_path) = {
            let index = self.index.read()
                .map_err(|_| anyhow!("Lock poisoned"))?;
            
            let index_data: FxHashMap<u64, String> = index
                .iter()
                .map(|(id, path)| (id.0, path.to_string_lossy().to_string()))
                .collect();
            
            let json = serde_json::to_string_pretty(&index_data)?;
            let index_path = self.base_path.join("index.json");
            (json, index_path)
        };
        
        fs::write(index_path, json).await?;
        
        Ok(())
    }
}

#[async_trait]
impl EmbeddingStorage for FileStorage {
    async fn store(&self, embedding: Vec<f32>) -> Result<EmbeddingId> {
        let id = {
            let mut next_id = self.next_id.write()
                .map_err(|_| anyhow!("Lock poisoned"))?;
            let id = EmbeddingId(*next_id);
            *next_id += 1;
            id
        };
        
        // Convert to bytes with simple compression
        let bytes: Vec<u8> = embedding.iter()
            .flat_map(|&f| f.to_le_bytes())
            .collect();
        
        let filename = format!("{}.emb", id.0);
        let filepath = self.base_path.join(&filename);
        
        // Write compressed data
        fs::write(&filepath, &bytes).await?;
        
        // Update index
        self.index.write()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .insert(id, filepath);
        
        self.save_index().await?;
        
        Ok(id)
    }
    
    async fn retrieve(&self, id: EmbeddingId) -> Result<Vec<f32>> {
        let filepath = self.index.read()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .get(&id)
            .cloned()
            .ok_or_else(|| anyhow!("Embedding not found"))?;
        
        let bytes = fs::read(&filepath).await?;
        
        // Convert bytes back to floats
        let embedding: Vec<f32> = bytes
            .chunks_exact(4)
            .map(|chunk| {
                let arr: [u8; 4] = chunk.try_into().unwrap();
                f32::from_le_bytes(arr)
            })
            .collect();
        
        Ok(embedding)
    }
    
    async fn delete(&self, id: EmbeddingId) -> Result<()> {
        let filepath = self.index.write()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .remove(&id)
            .ok_or_else(|| anyhow!("Embedding not found"))?;
        
        fs::remove_file(&filepath).await?;
        self.save_index().await?;
        
        Ok(())
    }
    
    async fn search(
        &self,
        query: &[f32],
        k: usize,
        threshold: f32,
    ) -> Result<Vec<(EmbeddingId, f32)>> {
        let ids: Vec<_> = {
            let index = self.index.read()
                .map_err(|_| anyhow!("Lock poisoned"))?;
            index.keys().cloned().collect()
        };
        
        let mut similarities = Vec::new();
        
        for id in ids {
            let embedding = self.retrieve(id).await?;
            let similarity = cosine_similarity(query, &embedding);
            if similarity >= threshold {
                similarities.push((id, similarity));
            }
        }
        
        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        similarities.truncate(k);
        
        Ok(similarities)
    }
    
    async fn count(&self) -> Result<usize> {
        Ok(self.index.read()
            .map_err(|_| anyhow!("Lock poisoned"))?
            .len())
    }
}

/// Compute cosine similarity between two vectors
fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() {
        return 0.0;
    }
    
    let dot: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let norm_a: f32 = a.iter().map(|x| x * x).sum::<f32>().sqrt();
    let norm_b: f32 = b.iter().map(|x| x * x).sum::<f32>().sqrt();
    
    if norm_a > 0.0 && norm_b > 0.0 {
        dot / (norm_a * norm_b)
    } else {
        0.0
    }
}

impl Default for InMemoryStorage {
    fn default() -> Self {
        Self::new()
    }
}