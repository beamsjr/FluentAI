//! High-level AI analyzer for FluentAI AST

use crate::analysis::{AiAnalysisResult, AstAnalyzer, BaseAnalyzer, AnalysisConfig};
use crate::cache::{AiCache, CacheConfig};
use crate::error::Result;
use crate::features::{FeatureExtractor, FeatureConfig};
use crate::patterns::{PatternDetector, DetectionConfig, PatternAnalysis};
use fluentai_core::ast::Graph;
use std::sync::Arc;
use tracing::{info, debug};

/// Main AI analyzer for FluentAI
pub struct AiAnalyzer {
    /// Base analyzer for core functionality
    base_analyzer: BaseAnalyzer,
    /// Pattern detector
    pattern_detector: PatternDetector,
    /// Feature extractor
    feature_extractor: FeatureExtractor,
    /// Cache for results
    cache: Arc<AiCache>,
    /// Configuration
    config: AiAnalyzerConfig,
}

/// Configuration for the AI analyzer
#[derive(Debug, Clone)]
pub struct AiAnalyzerConfig {
    /// Analysis configuration
    pub analysis: AnalysisConfig,
    /// Pattern detection configuration
    pub patterns: DetectionConfig,
    /// Feature extraction configuration
    pub features: FeatureConfig,
    /// Cache configuration
    pub cache: CacheConfig,
    /// Enable parallel processing
    pub parallel: bool,
}

impl Default for AiAnalyzerConfig {
    fn default() -> Self {
        Self {
            analysis: AnalysisConfig::default(),
            patterns: DetectionConfig::default(),
            features: FeatureConfig::default(),
            cache: CacheConfig::default(),
            parallel: true,
        }
    }
}

impl AiAnalyzer {
    /// Create a new AI analyzer
    pub fn new(config: AiAnalyzerConfig) -> Self {
        let base_analyzer = BaseAnalyzer::new(config.analysis.clone());
        let pattern_detector = PatternDetector::new(config.patterns.clone());
        let feature_extractor = FeatureExtractor::new(config.features.clone());
        let cache = Arc::new(AiCache::new(config.cache.clone()));
        
        Self {
            base_analyzer,
            pattern_detector,
            feature_extractor,
            cache,
            config,
        }
    }
    
    /// Analyze an AST graph
    pub fn analyze_graph(&self, graph: &Graph) -> Result<AiAnalysisResult> {
        info!("Starting AI analysis of AST graph");
        
        // Try to get from cache first
        self.cache.get_or_compute_analysis(graph, || {
            self.perform_analysis(graph)
        })
    }
    
    /// Perform pattern analysis
    pub fn analyze_patterns(&self, graph: &Graph) -> PatternAnalysis {
        self.pattern_detector.analyze_patterns(graph)
    }
    
    /// Extract features for all nodes
    pub fn extract_features(&mut self, graph: &Graph) -> Result<()> {
        let features = self.feature_extractor.extract_all(graph)?;
        debug!("Extracted features for {} nodes", features.len());
        Ok(())
    }
    
    /// Get embeddings for the graph
    pub fn get_embeddings(&self, graph: &Graph) -> Result<std::collections::HashMap<fluentai_core::ast::NodeId, Vec<f32>>> {
        self.cache.get_or_compute_embeddings(graph, || {
            self.base_analyzer.get_embeddings(graph)
        })
    }
    
    /// Clear the cache
    pub fn clear_cache(&self) {
        self.cache.clear();
    }
    
    /// Get cache statistics
    pub fn cache_stats(&self) -> crate::cache::CacheStats {
        self.cache.stats()
    }
    
    /// Perform the actual analysis
    fn perform_analysis(&self, graph: &Graph) -> Result<AiAnalysisResult> {
        debug!("Performing full AI analysis");
        
        // Use the base analyzer for now
        // In the future, this could combine results from multiple analyzers
        let mut result = self.base_analyzer.analyze(graph)?;
        
        // Enhance with pattern analysis
        if self.config.analysis.detect_patterns {
            let pattern_analysis = self.pattern_detector.analyze_patterns(graph);
            
            // Add refactoring suggestions based on patterns
            for refactoring in pattern_analysis.refactorings {
                result.optimizations.push(crate::analysis::OptimizationSuggestion {
                    optimization_type: match refactoring.refactoring_type {
                        crate::patterns::RefactoringType::TailRecursionConversion => {
                            crate::analysis::OptimizationType::TailCall
                        }
                        crate::patterns::RefactoringType::MapFusion => {
                            crate::analysis::OptimizationType::CSE
                        }
                        _ => crate::analysis::OptimizationType::CSE,
                    },
                    target_nodes: refactoring.pattern.nodes.clone(),
                    expected_improvement: refactoring.improvement,
                    description: refactoring.description,
                });
            }
        }
        
        Ok(result)
    }
}

/// Builder for AI analyzer
pub struct AiAnalyzerBuilder {
    config: AiAnalyzerConfig,
}

impl AiAnalyzerBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: AiAnalyzerConfig::default(),
        }
    }
    
    /// Enable or disable optimization detection
    pub fn detect_optimizations(mut self, enable: bool) -> Self {
        self.config.analysis.detect_optimizations = enable;
        self
    }
    
    /// Enable or disable pattern detection
    pub fn detect_patterns(mut self, enable: bool) -> Self {
        self.config.analysis.detect_patterns = enable;
        self
    }
    
    /// Enable or disable embedding generation
    pub fn generate_embeddings(mut self, enable: bool) -> Self {
        self.config.analysis.generate_embeddings = enable;
        self
    }
    
    /// Set minimum confidence threshold
    pub fn min_confidence(mut self, confidence: f32) -> Self {
        self.config.analysis.min_confidence = confidence;
        self.config.patterns.min_confidence = confidence;
        self
    }
    
    /// Set maximum graph size
    pub fn max_graph_size(mut self, size: usize) -> Self {
        self.config.analysis.max_graph_size = size;
        self
    }
    
    /// Enable or disable caching
    pub fn enable_cache(mut self, enable: bool) -> Self {
        if !enable {
            self.config.cache.max_analysis_entries = 0;
            self.config.cache.max_embedding_entries = 0;
        }
        self
    }
    
    /// Enable or disable parallel processing
    pub fn parallel(mut self, enable: bool) -> Self {
        self.config.parallel = enable;
        self
    }
    
    /// Build the analyzer
    pub fn build(self) -> AiAnalyzer {
        AiAnalyzer::new(self.config)
    }
}

/// Convenience function to create a default analyzer
pub fn create_default_analyzer() -> AiAnalyzer {
    AiAnalyzerBuilder::new().build()
}

/// Convenience function to analyze a graph with default settings
pub fn analyze_ast(graph: &Graph) -> Result<AiAnalysisResult> {
    let analyzer = create_default_analyzer();
    analyzer.analyze_graph(graph)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Node, Literal};
    
    #[test]
    fn test_analyzer_builder() {
        let analyzer = AiAnalyzerBuilder::new()
            .detect_optimizations(true)
            .detect_patterns(true)
            .min_confidence(0.8)
            .build();
        
        assert_eq!(analyzer.config.analysis.min_confidence, 0.8);
        assert!(analyzer.config.analysis.detect_optimizations);
    }
    
    #[test]
    fn test_analyze_ast() {
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        let result = analyze_ast(&graph).unwrap();
        assert!(result.confidence > 0.0);
    }
    
    #[test]
    fn test_cache_integration() {
        let analyzer = create_default_analyzer();
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        // First call should miss cache
        let result1 = analyzer.analyze_graph(&graph).unwrap();
        let stats1 = analyzer.cache_stats();
        assert_eq!(stats1.misses, 1);
        
        // Second call should hit cache
        let result2 = analyzer.analyze_graph(&graph).unwrap();
        let stats2 = analyzer.cache_stats();
        assert_eq!(stats2.hits, 1);
        
        // Results should be identical
        assert_eq!(result1.performance_score, result2.performance_score);
    }
}