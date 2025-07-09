//! Embedding generation strategies

use crate::EmbeddingContext;
use anyhow::Result;
use async_trait::async_trait;
use fluentai_core::ast::{Literal, Node};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Trait for embedding generation strategies
#[async_trait]
pub trait EmbeddingGenerator: Send + Sync {
    /// Generate embedding from context
    async fn generate(&self, context: &EmbeddingContext) -> Result<Vec<f32>>;

    /// Get embedding dimension
    fn dimension(&self) -> usize;
}

/// Simple feature-based embedding generator
pub struct FeatureBasedGenerator {
    dimension: usize,
}

impl FeatureBasedGenerator {
    pub fn new() -> Self {
        Self { dimension: 256 }
    }

    pub fn with_dimension(dimension: usize) -> Self {
        Self { dimension }
    }

    /// Generate embedding from raw features
    pub fn generate_from_features(
        &self,
        features: &std::collections::HashMap<String, f32>,
    ) -> Result<Vec<f32>> {
        let mut embedding = vec![0.0; self.dimension];

        // Hash each feature into the embedding space
        for (key, value) in features {
            let mut hasher = DefaultHasher::new();
            key.hash(&mut hasher);
            let hash = hasher.finish();

            // Use multiple hash positions for better distribution
            for i in 0..4 {
                let idx = ((hash.wrapping_add(i * 0x9e3779b9)) as usize) % self.dimension;
                embedding[idx] += value;
            }
        }

        // L2 normalize
        let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
        if norm > 0.0 {
            for x in &mut embedding {
                *x /= norm;
            }
        }

        Ok(embedding)
    }

    /// Extract features from AST node
    fn node_features(&self, node: &Node) -> Vec<f32> {
        let mut features = vec![0.0; 32]; // Node type features

        match node {
            Node::Literal(lit) => {
                features[0] = 1.0;
                match lit {
                    Literal::Integer(_) => features[1] = 1.0,
                    Literal::Float(_) => features[2] = 1.0,
                    Literal::String(_) => features[3] = 1.0,
                    Literal::Boolean(_) => features[4] = 1.0,
                    Literal::Nil => features[5] = 1.0,
                }
            }
            Node::Variable { .. } => features[6] = 1.0,
            Node::Lambda { .. } => features[7] = 1.0,
            Node::Application { .. } => features[8] = 1.0,
            Node::If { .. } => features[9] = 1.0,
            Node::Let { .. } => features[10] = 1.0,
            Node::Letrec { .. } => features[11] = 1.0,
            Node::Effect { .. } => features[12] = 1.0,
            Node::List(_) => features[13] = 1.0,
            Node::Match { .. } => features[14] = 1.0,
            Node::Module { .. } => features[15] = 1.0,
            Node::Import { .. } => features[16] = 1.0,
            Node::Async { .. } => features[17] = 1.0,
            Node::Await { .. } => features[18] = 1.0,
            Node::Spawn { .. } => features[19] = 1.0,
            Node::Channel => features[20] = 1.0,
            _ => features[31] = 1.0, // Other
        }

        features
    }

    /// Hash a string to a feature vector
    fn hash_to_features(&self, s: &str, size: usize) -> Vec<f32> {
        let mut features = vec![0.0; size];
        let mut hasher = DefaultHasher::new();

        // Hash each character position
        for (i, c) in s.chars().enumerate() {
            hasher.write_u8(i as u8);
            hasher.write_u32(c as u32);
            let hash = hasher.finish();
            let idx = (hash as usize) % size;
            features[idx] += 1.0;
        }

        // Normalize
        let sum: f32 = features.iter().sum();
        if sum > 0.0 {
            for f in &mut features {
                *f /= sum;
            }
        }

        features
    }
}

#[async_trait]
impl EmbeddingGenerator for FeatureBasedGenerator {
    async fn generate(&self, context: &EmbeddingContext) -> Result<Vec<f32>> {
        let mut embedding = Vec::with_capacity(self.dimension);

        // 1. Node type features (32 dims)
        embedding.extend(self.node_features(&context.node));

        // 2. Structural features from ML hints (32 dims)
        let features = &context.features;
        embedding.push(features.node_count as f32 / 1000.0);
        embedding.push(features.depth as f32 / 100.0);
        embedding.push(features.branching_factor);
        embedding.push(features.cycle_count as f32 / 10.0);
        embedding.push(features.arithmetic_ops as f32 / 100.0);
        embedding.push(features.memory_ops as f32 / 100.0);
        embedding.push(features.control_flow_ops as f32 / 100.0);
        embedding.push(features.function_calls as f32 / 100.0);
        embedding.push(features.data_dependencies as f32 / 100.0);
        embedding.push(features.live_variables as f32 / 100.0);
        embedding.push(features.register_pressure);
        embedding.push(if features.has_recursion { 1.0 } else { 0.0 });
        embedding.push(if features.has_loops { 1.0 } else { 0.0 });
        embedding.push(if features.has_map_pattern { 1.0 } else { 0.0 });
        embedding.push(if features.has_reduce_pattern {
            1.0
        } else {
            0.0
        });
        embedding.push(if features.uses_integers { 1.0 } else { 0.0 });
        embedding.push(if features.uses_floats { 1.0 } else { 0.0 });
        embedding.push(if features.uses_lists { 1.0 } else { 0.0 });
        embedding.push(if features.uses_higher_order { 1.0 } else { 0.0 });
        embedding.push(features.estimated_iterations.unwrap_or(0) as f32 / 1000.0);
        embedding.push(features.data_size_hint.unwrap_or(0) as f32 / 10000.0);
        embedding.push(features.hotness_score);

        // Pad to 32
        while embedding.len() < 64 {
            embedding.push(0.0);
        }

        // 3. Usage statistics (16 dims)
        if let Some(context_mem) = &context.context_memory {
            let stats = &context_mem.usage_stats;
            embedding.push((stats.execution_count as f32).log10() / 10.0);
            embedding.push((stats.avg_execution_time_ns as f32).log10() / 10.0);
            embedding.push((stats.error_count as f32).log10() / 10.0);
            embedding.push(if stats.is_hot_path { 1.0 } else { 0.0 });

            // Performance hints
            for hint_type in &[
                "ShouldInline",
                "ShouldUnroll",
                "CanVectorize",
                "CanParallelize",
                "ShouldMemoize",
            ] {
                let has_hint = context_mem
                    .performance_hints
                    .iter()
                    .any(|h| format!("{:?}", h.hint_type).contains(hint_type));
                embedding.push(if has_hint { 1.0 } else { 0.0 });
            }

            // Semantic tags
            embedding.push(context_mem.semantic_tags.len() as f32 / 10.0);
        } else {
            embedding.extend(vec![0.0; 10]);
        }

        // Pad to 80
        while embedding.len() < 80 {
            embedding.push(0.0);
        }

        // 4. Documentation/rationale features (64 dims)
        if let Some(doc_id) = &context.documentation {
            embedding.extend(self.hash_to_features(doc_id, 32));
        } else {
            embedding.extend(vec![0.0; 32]);
        }

        if let Some(context_mem) = &context.context_memory {
            if let Some(rationale) = &context_mem.rationale {
                embedding.extend(self.hash_to_features(rationale, 32));
            } else {
                embedding.extend(vec![0.0; 32]);
            }
        } else {
            embedding.extend(vec![0.0; 32]);
        }

        // 5. Fill remaining dimensions with noise reduction
        while embedding.len() < self.dimension {
            embedding.push(0.0);
        }

        // Truncate if needed
        embedding.truncate(self.dimension);

        // L2 normalize
        let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
        if norm > 0.0 {
            for x in &mut embedding {
                *x /= norm;
            }
        }

        Ok(embedding)
    }

    fn dimension(&self) -> usize {
        self.dimension
    }
}

impl Default for FeatureBasedGenerator {
    fn default() -> Self {
        Self::new()
    }
}
