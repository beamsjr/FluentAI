//! Embedding generation and management for FluentAi
//!
//! This crate provides infrastructure for generating, storing, and querying
//! semantic embeddings for code understanding and optimization.

use anyhow::{anyhow, Result};
use fluentai_core::ast::{Graph, Node, NodeId, EmbeddingId, ContextMemory};
use fluentai_optimizer::ml_hints::{ProgramFeatures, FeatureExtractor};
use rustc_hash::FxHashMap;
use serde::{Serialize, Deserialize};
use std::sync::{Arc, RwLock};
use std::path::Path;

pub mod generator;
pub mod storage;
pub mod similarity;
pub mod doc_embeddings;

use generator::EmbeddingGenerator;
use storage::EmbeddingStorage;

/// Embedding service that manages generation and retrieval
pub struct EmbeddingService {
    pub(crate) generator: Box<dyn EmbeddingGenerator>,
    pub(crate) storage: Box<dyn EmbeddingStorage>,
    cache: Arc<RwLock<FxHashMap<EmbeddingId, Vec<f32>>>>,
}

impl EmbeddingService {
    /// Create a new embedding service
    pub fn new(
        generator: Box<dyn EmbeddingGenerator>,
        storage: Box<dyn EmbeddingStorage>,
    ) -> Self {
        Self {
            generator,
            storage,
            cache: Arc::new(RwLock::new(FxHashMap::default())),
        }
    }
    
    /// Create default service with simple feature-based embeddings
    pub fn default() -> Result<Self> {
        let generator = Box::new(generator::FeatureBasedGenerator::new());
        let storage = Box::new(storage::InMemoryStorage::new());
        Ok(Self::new(generator, storage))
    }
    
    /// Generate embedding for a node
    pub async fn generate_embedding(
        &self,
        graph: &Graph,
        node_id: NodeId,
    ) -> Result<EmbeddingId> {
        // Extract context for embedding generation
        let context = self.extract_context(graph, node_id)?;
        
        // Generate embedding
        let embedding = self.generator.generate(&context).await?;
        
        // Store embedding
        let id = self.storage.store(embedding).await?;
        
        // Update cache
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(id, self.storage.retrieve(id).await?);
        }
        
        Ok(id)
    }
    
    /// Retrieve an embedding by ID
    pub async fn get_embedding(&self, id: EmbeddingId) -> Result<Vec<f32>> {
        // Check cache first
        if let Ok(cache) = self.cache.read() {
            if let Some(embedding) = cache.get(&id) {
                return Ok(embedding.clone());
            }
        }
        
        // Retrieve from storage
        let embedding = self.storage.retrieve(id).await?;
        
        // Update cache
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(id, embedding.clone());
        }
        
        Ok(embedding)
    }
    
    /// Find similar nodes by embedding similarity
    pub async fn find_similar(
        &self,
        id: EmbeddingId,
        k: usize,
        threshold: f32,
    ) -> Result<Vec<(EmbeddingId, f32)>> {
        let query_embedding = self.get_embedding(id).await?;
        self.storage.search(&query_embedding, k, threshold).await
    }
    
    /// Store an embedding directly
    pub async fn store_embedding(&self, embedding: Vec<f32>) -> Result<EmbeddingId> {
        let id = self.storage.store(embedding.clone()).await?;
        
        // Update cache
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(id, embedding);
        }
        
        Ok(id)
    }
    
    /// Update embeddings for all nodes in a graph
    pub async fn update_graph_embeddings(&self, graph: &mut Graph) -> Result<()> {
        let node_ids: Vec<_> = graph.node_ids().collect();
        
        for node_id in node_ids {
            // Generate embedding
            let embedding_id = self.generate_embedding(graph, node_id).await?;
            
            // Update context memory
            let context = graph.get_context_memory(node_id).cloned()
                .unwrap_or_else(|| ContextMemory {
                    embedding_id: None,
                    usage_stats: Default::default(),
                    rationale: None,
                    performance_hints: vec![],
                    semantic_tags: vec![],
                    last_modified: None,
                });
            
            let mut updated_context = context;
            updated_context.embedding_id = Some(embedding_id);
            graph.set_context_memory(node_id, updated_context);
        }
        
        Ok(())
    }
    
    /// Extract context for embedding generation
    fn extract_context(&self, graph: &Graph, node_id: NodeId) -> Result<EmbeddingContext> {
        let node = graph.get_node(node_id)
            .ok_or_else(|| anyhow!("Node not found"))?;
        
        let metadata = graph.get_metadata(node_id).cloned();
        let context_memory = graph.get_context_memory(node_id).cloned();
        
        // Extract structural features
        let extractor = FeatureExtractor::new();
        let features = extractor.extract_features(&graph);
        
        // Get documentation if available
        let documentation = metadata.as_ref()
            .and_then(|m| m.documentation_id.clone());
        
        Ok(EmbeddingContext {
            node: node.clone(),
            metadata,
            context_memory,
            features,
            documentation,
        })
    }
}

/// Context used for embedding generation
#[derive(Debug, Clone)]
pub struct EmbeddingContext {
    pub node: Node,
    pub metadata: Option<fluentai_core::ast::NodeMetadata>,
    pub context_memory: Option<ContextMemory>,
    pub features: ProgramFeatures,
    pub documentation: Option<String>,
}

/// Configuration for embedding generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbeddingConfig {
    /// Embedding dimension
    pub dimension: usize,
    
    /// Model type (feature-based, pretrained, etc.)
    pub model_type: ModelType,
    
    /// Whether to use runtime information
    pub use_runtime_info: bool,
    
    /// Whether to use documentation
    pub use_documentation: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ModelType {
    /// Simple feature-based embeddings
    FeatureBased,
    
    /// Pre-trained code model
    PreTrained(String),
    
    /// Custom trained model
    Custom(String),
}

impl Default for EmbeddingConfig {
    fn default() -> Self {
        Self {
            dimension: 256,
            model_type: ModelType::FeatureBased,
            use_runtime_info: true,
            use_documentation: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_embedding_service() {
        let service = EmbeddingService::default().unwrap();
        
        // Create a simple graph
        let mut graph = Graph::new();
        let node = Node::Literal(fluentai_core::ast::Literal::Integer(42));
        let node_id = graph.add_node(node);
        
        // Generate embedding
        let embedding_id = service.generate_embedding(&graph, node_id).await.unwrap();
        
        // Retrieve embedding
        let embedding = service.get_embedding(embedding_id).await.unwrap();
        assert!(!embedding.is_empty());
    }
}