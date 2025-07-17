//! Main analysis engine for AI-driven AST analysis

use crate::error::Result;
use crate::tensor::{GraphTensors, TensorBuilder};
use fluentai_core::ast::{Graph, NodeId};
use std::collections::HashMap;
use tracing::{debug, info};

/// Result of AI analysis on an AST
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AiAnalysisResult {
    /// Optimization suggestions
    pub optimizations: Vec<OptimizationSuggestion>,
    /// Detected patterns
    pub patterns: Vec<DetectedPattern>,
    /// Node embeddings
    pub embeddings: HashMap<NodeId, Vec<f32>>,
    /// Performance prediction
    pub performance_score: f32,
    /// Confidence in the analysis
    pub confidence: f32,
}

/// Suggested optimization
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct OptimizationSuggestion {
    /// Type of optimization
    pub optimization_type: OptimizationType,
    /// Target node(s)
    pub target_nodes: Vec<NodeId>,
    /// Expected improvement
    pub expected_improvement: f32,
    /// Human-readable description
    pub description: String,
}

/// Types of optimizations
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum OptimizationType {
    /// Inline function call
    Inline,
    /// Constant folding
    ConstantFold,
    /// Dead code elimination
    DeadCodeElimination,
    /// Loop unrolling
    LoopUnroll,
    /// Tail call optimization
    TailCall,
    /// Common subexpression elimination
    CSE,
    /// Memoization
    Memoize,
}

/// Detected code pattern
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DetectedPattern {
    /// Pattern type
    pub pattern_type: PatternType,
    /// Nodes involved in the pattern
    pub nodes: Vec<NodeId>,
    /// Confidence score
    pub confidence: f32,
}

/// Types of patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum PatternType {
    /// Map-reduce pattern
    MapReduce,
    /// Filter-map chain
    FilterMap,
    /// Recursive function
    Recursion,
    /// Higher-order function
    HigherOrder,
    /// Monadic chain
    MonadicChain,
    /// Actor message passing
    ActorPattern,
    /// Effect handling
    EffectPattern,
}

/// Trait for AST analyzers
pub trait AstAnalyzer: Send + Sync {
    /// Analyze an AST graph
    fn analyze(&self, graph: &Graph) -> Result<AiAnalysisResult>;
    
    /// Get node embeddings
    fn get_embeddings(&self, graph: &Graph) -> Result<HashMap<NodeId, Vec<f32>>>;
    
    /// Predict performance characteristics
    fn predict_performance(&self, graph: &Graph) -> Result<f32>;
    
    /// Check if the analyzer is ready
    fn is_ready(&self) -> bool;
}

/// Configuration for analysis
#[derive(Debug, Clone)]
pub struct AnalysisConfig {
    /// Enable optimization detection
    pub detect_optimizations: bool,
    /// Enable pattern detection
    pub detect_patterns: bool,
    /// Enable embedding generation
    pub generate_embeddings: bool,
    /// Minimum confidence threshold
    pub min_confidence: f32,
    /// Maximum graph size to analyze
    pub max_graph_size: usize,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        Self {
            detect_optimizations: true,
            detect_patterns: true,
            generate_embeddings: true,
            min_confidence: 0.7,
            max_graph_size: 10000,
        }
    }
}

/// Base analyzer implementation
pub struct BaseAnalyzer {
    config: AnalysisConfig,
    tensor_builder: TensorBuilder,
}

impl BaseAnalyzer {
    /// Create a new base analyzer
    pub fn new(config: AnalysisConfig) -> Self {
        let tensor_builder = TensorBuilder::new()
            .normalize(true)
            .max_nodes(config.max_graph_size);
            
        Self {
            config,
            tensor_builder,
        }
    }
    
    /// Analyze graph structure for patterns
    fn analyze_structure(&self, graph: &Graph) -> Vec<DetectedPattern> {
        let mut patterns = Vec::new();
        
        // Simple pattern detection based on graph structure
        let node_ids: Vec<NodeId> = graph.node_ids().collect();
        
        for &node_id in &node_ids {
            if let Some(_node) = graph.get_node(node_id) {
                // Detect recursive patterns
                if self.is_recursive_pattern(graph, node_id) {
                    patterns.push(DetectedPattern {
                        pattern_type: PatternType::Recursion,
                        nodes: vec![node_id],
                        confidence: 0.85,
                    });
                }
                
                // Detect map-reduce patterns
                if let Some(pattern) = self.detect_map_reduce(graph, node_id) {
                    patterns.push(pattern);
                }
            }
        }
        
        patterns
    }
    
    /// Check if a node is part of a recursive pattern
    fn is_recursive_pattern(&self, graph: &Graph, node_id: NodeId) -> bool {
        // Simple heuristic: check if node references itself through its children
        let mut visited = std::collections::HashSet::new();
        self.has_recursive_reference(graph, node_id, node_id, &mut visited)
    }
    
    /// Helper for recursive pattern detection
    fn has_recursive_reference(
        &self,
        graph: &Graph,
        start: NodeId,
        current: NodeId,
        visited: &mut std::collections::HashSet<NodeId>,
    ) -> bool {
        if !visited.insert(current) {
            return false;
        }
        
        for child in graph.children(current) {
            if child == start {
                return true;
            }
            if self.has_recursive_reference(graph, start, child, visited) {
                return true;
            }
        }
        
        false
    }
    
    /// Detect map-reduce patterns
    fn detect_map_reduce(&self, graph: &Graph, node_id: NodeId) -> Option<DetectedPattern> {
        // Simple heuristic: look for application nodes with "map" or "reduce" in metadata
        if let Some(node) = graph.get_node(node_id) {
            match node {
                fluentai_core::ast::Node::Application { .. } => {
                    // Check if this looks like a map or reduce operation
                    let children = graph.children(node_id);
                    if children.len() >= 2 {
                        // This could be a map-reduce pattern
                        return Some(DetectedPattern {
                            pattern_type: PatternType::MapReduce,
                            nodes: vec![node_id],
                            confidence: 0.75,
                        });
                    }
                }
                _ => {}
            }
        }
        None
    }
    
    /// Generate simple embeddings based on features
    fn generate_simple_embeddings(
        &self,
        _graph: &Graph,
        tensors: &GraphTensors,
    ) -> HashMap<NodeId, Vec<f32>> {
        let mut embeddings = HashMap::new();
        
        for (&node_id, &idx) in &tensors.node_mapping {
            // Use the feature vector as a simple embedding
            let features = tensors.features.row(idx).to_vec();
            embeddings.insert(node_id, features);
        }
        
        embeddings
    }
    
    /// Detect optimization opportunities
    fn detect_optimizations(&self, graph: &Graph) -> Vec<OptimizationSuggestion> {
        let mut suggestions = Vec::new();
        
        for node_id in graph.node_ids() {
            // Check for constant folding opportunities
            if self.can_constant_fold(graph, node_id) {
                suggestions.push(OptimizationSuggestion {
                    optimization_type: OptimizationType::ConstantFold,
                    target_nodes: vec![node_id],
                    expected_improvement: 0.2,
                    description: "This expression can be evaluated at compile time".to_string(),
                });
            }
            
            // Check for dead code
            if self.is_dead_code(graph, node_id) {
                suggestions.push(OptimizationSuggestion {
                    optimization_type: OptimizationType::DeadCodeElimination,
                    target_nodes: vec![node_id],
                    expected_improvement: 0.1,
                    description: "This code is unreachable and can be removed".to_string(),
                });
            }
        }
        
        suggestions
    }
    
    /// Check if a node can be constant folded
    fn can_constant_fold(&self, graph: &Graph, node_id: NodeId) -> bool {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                fluentai_core::ast::Node::Application { args, .. } => {
                    // Check if all arguments are literals (not the function itself)
                    args.iter().all(|&arg| {
                        matches!(
                            graph.get_node(arg),
                            Some(fluentai_core::ast::Node::Literal(_))
                        )
                    })
                }
                _ => false,
            }
        } else {
            false
        }
    }
    
    /// Check if a node is dead code
    fn is_dead_code(&self, _graph: &Graph, _node_id: NodeId) -> bool {
        // TODO: Implement proper dead code detection
        // For now, return false to avoid false positives
        false
    }
}

impl AstAnalyzer for BaseAnalyzer {
    fn analyze(&self, graph: &Graph) -> Result<AiAnalysisResult> {
        info!("Starting AI analysis of AST graph");
        
        // Build tensors from graph
        let tensors = self.tensor_builder.build(graph)?;
        debug!("Built tensors: {} nodes, {} edges", tensors.num_nodes, tensors.num_edges());
        
        // Detect patterns
        let patterns = if self.config.detect_patterns {
            self.analyze_structure(graph)
        } else {
            Vec::new()
        };
        
        // Detect optimizations
        let optimizations = if self.config.detect_optimizations {
            self.detect_optimizations(graph)
        } else {
            Vec::new()
        };
        
        // Generate embeddings
        let embeddings = if self.config.generate_embeddings {
            self.generate_simple_embeddings(graph, &tensors)
        } else {
            HashMap::new()
        };
        
        // Simple performance score based on graph size and patterns
        let performance_score = 1.0 - (tensors.num_nodes as f32 / 1000.0).min(1.0);
        
        Ok(AiAnalysisResult {
            optimizations,
            patterns,
            embeddings,
            performance_score,
            confidence: 0.8, // Fixed confidence for base analyzer
        })
    }
    
    fn get_embeddings(&self, graph: &Graph) -> Result<HashMap<NodeId, Vec<f32>>> {
        let tensors = self.tensor_builder.build(graph)?;
        Ok(self.generate_simple_embeddings(graph, &tensors))
    }
    
    fn predict_performance(&self, graph: &Graph) -> Result<f32> {
        let tensors = self.tensor_builder.build(graph)?;
        Ok(1.0 - (tensors.num_nodes as f32 / 1000.0).min(1.0))
    }
    
    fn is_ready(&self) -> bool {
        true // Base analyzer is always ready
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::AstAnalyzer;
    use fluentai_core::ast::{Node, Literal};
    
    #[test]
    fn test_base_analyzer() {
        let mut graph = Graph::new();
        let n1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let n2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let _n3 = graph.add_node(Node::Application {
            function: n1,
            args: vec![n2],
        }).unwrap();
        
        let analyzer = BaseAnalyzer::new(AnalysisConfig::default());
        let result = analyzer.analyze(&graph).unwrap();
        
        assert!(result.confidence > 0.0);
        assert!(result.performance_score > 0.0);
        assert_eq!(result.embeddings.len(), 3);
    }
    
    #[test]
    fn test_pattern_detection() {
        let mut graph = Graph::new();
        
        // Create a simple recursive pattern
        let var = graph.add_node(Node::Variable { name: "f".to_string() }).unwrap();
        let _app = graph.add_node(Node::Application {
            function: var,
            args: vec![var], // Self-reference
        }).unwrap();
        
        let analyzer = BaseAnalyzer::new(AnalysisConfig::default());
        let result = analyzer.analyze(&graph).unwrap();
        
        // Should detect at least one pattern
        assert!(!result.patterns.is_empty());
    }
    
    #[test]
    fn test_optimization_detection() {
        let mut graph = Graph::new();
        
        // Create a constant expression that can be folded
        let n1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let n2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        let _app = graph.add_node(Node::Application {
            function: plus,
            args: vec![n1, n2],
        }).unwrap();
        
        let config = AnalysisConfig {
            detect_optimizations: true,
            ..Default::default()
        };
        let analyzer = BaseAnalyzer::new(config);
        let result = analyzer.analyze(&graph).unwrap();
        
        // Should detect constant folding opportunity
        assert!(!result.optimizations.is_empty());
        assert_eq!(
            result.optimizations[0].optimization_type,
            OptimizationType::ConstantFold
        );
    }
}