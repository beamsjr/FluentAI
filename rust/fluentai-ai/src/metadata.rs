//! Metadata injection system for enriching AST with AI analysis results

use crate::analysis::{AiAnalysisResult, OptimizationType};
use crate::error::Result;
use fluentai_core::ast::{Graph, NodeId, NodeMetadata, ContextMemory, UsageStatistics};
use std::collections::HashMap;
use tracing::{debug, info};

/// Create a new ContextMemory instance
fn new_context_memory() -> ContextMemory {
    ContextMemory {
        embedding_id: None,
        usage_stats: UsageStatistics {
            execution_count: 0,
            avg_execution_time_ns: 0,
            error_count: 0,
            is_hot_path: false,
        },
        rationale: None,
        performance_hints: vec![],
        semantic_tags: vec![],
        last_modified: None,
    }
}

/// Metadata injector for enriching AST with AI analysis results
pub struct MetadataInjector {
    /// Configuration
    config: InjectorConfig,
}

/// Configuration for metadata injection
#[derive(Debug, Clone)]
pub struct InjectorConfig {
    /// Inject optimization suggestions
    pub inject_optimizations: bool,
    /// Inject pattern information
    pub inject_patterns: bool,
    /// Inject embeddings
    pub inject_embeddings: bool,
    /// Inject confidence scores
    pub inject_confidence: bool,
    /// Prefix for AI metadata keys
    pub metadata_prefix: String,
}

impl Default for InjectorConfig {
    fn default() -> Self {
        Self {
            inject_optimizations: true,
            inject_patterns: true,
            inject_embeddings: false, // Embeddings can be large
            inject_confidence: true,
            metadata_prefix: "ai_".to_string(),
        }
    }
}

impl MetadataInjector {
    /// Create a new metadata injector
    pub fn new(config: InjectorConfig) -> Self {
        Self { config }
    }
    
    /// Inject AI analysis results into the AST
    pub fn inject(&self, graph: &mut Graph, analysis: &AiAnalysisResult) -> Result<()> {
        info!("Injecting AI analysis metadata into AST");
        
        let mut injected_count = 0;
        
        // Inject optimization metadata
        if self.config.inject_optimizations {
            injected_count += self.inject_optimizations(graph, &analysis.optimizations)?;
        }
        
        // Inject pattern metadata
        if self.config.inject_patterns {
            injected_count += self.inject_patterns(graph, &analysis.patterns)?;
        }
        
        // Inject embeddings
        if self.config.inject_embeddings {
            injected_count += self.inject_embeddings(graph, &analysis.embeddings)?;
        }
        
        // Inject global analysis metadata
        self.inject_global_metadata(graph, analysis)?;
        
        debug!("Injected metadata for {} nodes", injected_count);
        Ok(())
    }
    
    /// Inject optimization suggestions
    fn inject_optimizations(
        &self,
        graph: &mut Graph,
        optimizations: &[crate::analysis::OptimizationSuggestion],
    ) -> Result<usize> {
        let mut count = 0;
        
        for opt in optimizations {
            for &node_id in &opt.target_nodes {
                let metadata = graph.metadata_mut(node_id);
                
                // Ensure context memory exists
                if metadata.context_memory.is_none() {
                    metadata.context_memory = Some(new_context_memory());
                }
                
                if let Some(ref mut context) = metadata.context_memory {
                    // Add optimization info as semantic tags
                    context.semantic_tags.push(
                        format!("{}opt:{:?}", self.config.metadata_prefix, opt.optimization_type)
                    );
                    context.semantic_tags.push(
                        format!("{}improvement:{:.2}", self.config.metadata_prefix, opt.expected_improvement)
                    );
                    
                    // Add description to rationale
                    if context.rationale.is_none() {
                        context.rationale = Some(opt.description.clone());
                    } else {
                        let existing = context.rationale.as_ref().unwrap();
                        context.rationale = Some(format!("{}\nAI: {}", existing, opt.description));
                    }
                }
                
                // Mark as optimizable
                metadata.is_pure = Some(matches!(
                    opt.optimization_type,
                    OptimizationType::ConstantFold | OptimizationType::CSE
                ));
                
                count += 1;
            }
        }
        
        Ok(count)
    }
    
    /// Inject pattern information
    fn inject_patterns(
        &self,
        graph: &mut Graph,
        patterns: &[crate::analysis::DetectedPattern],
    ) -> Result<usize> {
        let mut count = 0;
        
        for pattern in patterns {
            for &node_id in &pattern.nodes {
                let metadata = graph.metadata_mut(node_id);
                
                // Ensure context memory exists
                if metadata.context_memory.is_none() {
                    metadata.context_memory = Some(new_context_memory());
                }
                
                if let Some(ref mut context) = metadata.context_memory {
                    // Add pattern info as semantic tags
                    context.semantic_tags.push(
                        format!("{}pattern:{:?}", self.config.metadata_prefix, pattern.pattern_type)
                    );
                    
                    if self.config.inject_confidence {
                        context.semantic_tags.push(
                            format!("{}pattern_conf:{:.2}", self.config.metadata_prefix, pattern.confidence)
                        );
                    }
                }
                
                count += 1;
            }
        }
        
        Ok(count)
    }
    
    /// Inject embeddings
    fn inject_embeddings(
        &self,
        graph: &mut Graph,
        embeddings: &HashMap<NodeId, Vec<f32>>,
    ) -> Result<usize> {
        let mut count = 0;
        
        for (&node_id, embedding) in embeddings {
            let metadata = graph.metadata_mut(node_id);
            
            // Ensure context memory exists
            if metadata.context_memory.is_none() {
                metadata.context_memory = Some(new_context_memory());
            }
            
            if let Some(ref mut context) = metadata.context_memory {
                // Store embedding dimensions as a tag
                context.semantic_tags.push(
                    format!("{}embedding_dim:{}", self.config.metadata_prefix, embedding.len())
                );
                
                // Store first few embedding values as tags (to avoid bloat)
                for (i, &val) in embedding.iter().take(5).enumerate() {
                    context.semantic_tags.push(
                        format!("{}emb_{}:{:.3}", self.config.metadata_prefix, i, val)
                    );
                }
            }
            
            count += 1;
        }
        
        Ok(count)
    }
    
    /// Inject global analysis metadata
    fn inject_global_metadata(&self, graph: &mut Graph, analysis: &AiAnalysisResult) -> Result<()> {
        // Store global metadata in graph metadata
        let perf_key = format!("{}performance_score", self.config.metadata_prefix);
        graph.graph_metadata.insert(
            perf_key,
            analysis.performance_score.to_string(),
        );
        
        if self.config.inject_confidence {
            let conf_key = format!("{}analysis_confidence", self.config.metadata_prefix);
            graph.graph_metadata.insert(
                conf_key,
                analysis.confidence.to_string(),
            );
        }
        
        // Store analysis timestamp
        let timestamp_key = format!("{}analysis_timestamp", self.config.metadata_prefix);
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();
        graph.graph_metadata.insert(
            timestamp_key,
            timestamp.to_string(),
        );
        
        Ok(())
    }
}

/// Extract AI metadata from an AST
pub struct MetadataExtractor {
    /// Configuration
    config: InjectorConfig,
}

impl MetadataExtractor {
    /// Create a new metadata extractor
    pub fn new(config: InjectorConfig) -> Self {
        Self { config }
    }
    
    /// Extract AI metadata for a specific node
    pub fn extract_node_metadata(&self, graph: &Graph, node_id: NodeId) -> Option<AiNodeMetadata> {
        let metadata = graph.get_metadata(node_id)?;
        
        let mut ai_metadata = AiNodeMetadata::default();
        
        if let Some(ref context) = metadata.context_memory {
            // Extract from semantic tags
            for tag in &context.semantic_tags {
                if tag.starts_with(&self.config.metadata_prefix) {
                    let tag_content = &tag[self.config.metadata_prefix.len()..];
                    
                    if let Some(opt_type) = tag_content.strip_prefix("opt:") {
                        ai_metadata.optimization_type = Some(opt_type.to_string());
                    } else if let Some(improvement) = tag_content.strip_prefix("improvement:") {
                        ai_metadata.expected_improvement = improvement.parse().ok();
                    } else if let Some(pattern) = tag_content.strip_prefix("pattern:") {
                        ai_metadata.pattern_type = Some(pattern.to_string());
                    } else if let Some(conf) = tag_content.strip_prefix("pattern_conf:") {
                        ai_metadata.pattern_confidence = conf.parse().ok();
                    } else if let Some(dim) = tag_content.strip_prefix("embedding_dim:") {
                        if let Ok(d) = dim.parse::<usize>() {
                            // Initialize embedding vector
                            ai_metadata.embedding = Some(vec![0.0; d]);
                        }
                    } else if tag_content.starts_with("emb_") {
                        // Parse embedding values
                        if let Some((idx_str, val_str)) = tag_content[4..].split_once(':') {
                            if let (Ok(idx), Ok(val)) = (idx_str.parse::<usize>(), val_str.parse::<f32>()) {
                                if let Some(ref mut emb) = ai_metadata.embedding {
                                    if idx < emb.len() {
                                        emb[idx] = val;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            // Extract description from rationale
            if let Some(rationale) = &context.rationale {
                if rationale.contains("AI:") {
                    // Extract AI description
                    if let Some(ai_part) = rationale.split("AI:").nth(1) {
                        ai_metadata.optimization_description = Some(ai_part.trim().to_string());
                    }
                }
            }
        }
        
        Some(ai_metadata)
    }
    
    /// Extract global AI metadata
    pub fn extract_global_metadata(&self, graph: &Graph) -> AiGlobalMetadata {
        let mut global = AiGlobalMetadata::default();
        
        let perf_key = format!("{}performance_score", self.config.metadata_prefix);
        if let Some(score) = graph.graph_metadata.get(&perf_key) {
            global.performance_score = score.parse().ok();
        }
        
        let conf_key = format!("{}analysis_confidence", self.config.metadata_prefix);
        if let Some(conf) = graph.graph_metadata.get(&conf_key) {
            global.analysis_confidence = conf.parse().ok();
        }
        
        let timestamp_key = format!("{}analysis_timestamp", self.config.metadata_prefix);
        if let Some(timestamp) = graph.graph_metadata.get(&timestamp_key) {
            global.analysis_timestamp = timestamp.parse().ok();
        }
        
        global
    }
}

/// AI metadata for a single node
#[derive(Debug, Default, Clone)]
pub struct AiNodeMetadata {
    /// Optimization type if applicable
    pub optimization_type: Option<String>,
    /// Expected improvement from optimization
    pub expected_improvement: Option<f32>,
    /// Optimization description
    pub optimization_description: Option<String>,
    /// Pattern type if detected
    pub pattern_type: Option<String>,
    /// Pattern confidence
    pub pattern_confidence: Option<f32>,
    /// Node embedding
    pub embedding: Option<Vec<f32>>,
}

/// Global AI metadata for the entire graph
#[derive(Debug, Default, Clone)]
pub struct AiGlobalMetadata {
    /// Overall performance score
    pub performance_score: Option<f32>,
    /// Analysis confidence
    pub analysis_confidence: Option<f32>,
    /// Timestamp of analysis (Unix epoch)
    pub analysis_timestamp: Option<u64>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{OptimizationSuggestion, DetectedPattern};
    use crate::patterns::PatternType;
    use fluentai_core::ast::{Node, Literal};
    
    #[test]
    fn test_metadata_injection() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        let analysis = AiAnalysisResult {
            optimizations: vec![OptimizationSuggestion {
                optimization_type: OptimizationType::ConstantFold,
                target_nodes: vec![node],
                expected_improvement: 0.3,
                description: "Fold constant".to_string(),
            }],
            patterns: vec![],
            embeddings: HashMap::new(),
            performance_score: 0.8,
            confidence: 0.9,
        };
        
        let injector = MetadataInjector::new(InjectorConfig::default());
        injector.inject(&mut graph, &analysis).unwrap();
        
        // Check that metadata was injected
        let metadata = graph.get_metadata(node).unwrap();
        assert!(metadata.context_memory.is_some());
        
        let context = metadata.context_memory.as_ref().unwrap();
        assert!(context.semantic_tags.iter().any(|tag| tag.contains("ai_opt:")));
        assert_eq!(metadata.is_pure, Some(true)); // Should be marked as pure for constant folding
    }
    
    #[test]
    fn test_metadata_extraction() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        
        // Manually inject some metadata
        let metadata = graph.metadata_mut(node);
        metadata.context_memory = Some(new_context_memory());
        if let Some(ref mut context) = metadata.context_memory {
            context.semantic_tags.push("ai_pattern:MapReduce".to_string());
            context.semantic_tags.push("ai_pattern_conf:0.85".to_string());
        }
        
        let extractor = MetadataExtractor::new(InjectorConfig::default());
        let ai_metadata = extractor.extract_node_metadata(&graph, node).unwrap();
        
        assert_eq!(ai_metadata.pattern_type, Some("MapReduce".to_string()));
        assert_eq!(ai_metadata.pattern_confidence, Some(0.85));
    }
}