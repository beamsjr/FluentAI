//! Machine Learning integration for optimization decisions
//!
//! This module bridges the AI analyzer with the new optimization passes,
//! allowing the ML system to learn when to apply each optimization.

use crate::passes::OptimizationPass;
use crate::passes::effect_reordering::{EffectReorderingPass, EffectReorderingConfig};
use crate::passes::subgraph_fusion::{SubgraphFusionPass, SubgraphFusionConfig};
use crate::passes::memory_aware::{MemoryAwarePass, MemoryAwareConfig};
use crate::passes::code_layout::{CodeLayoutPass, CodeLayoutConfig};
use crate::passes::memoization::{AdaptiveMemoizationPass, MemoizationConfig};
use crate::runtime_guided::{
    RuntimeGuidedConfig, HotPathInliningPass, ValueSpecializationPass, 
    AdaptiveLoopUnrollingPass
};
use fluentai_core::ast::{Graph, NodeId};
use fluentai_core::profiling::{FunctionProfileData, ValueProfileData, LoopProfileData};
use anyhow::Result;
use std::collections::HashMap;

#[cfg(feature = "ai-analysis")]
use fluentai_ai::{AiAnalyzer, AiAnalysisResult, OptimizationType as AiOptType};

/// Extended optimization types including new phases
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ExtendedOptimizationType {
    // Original AI-detected optimizations
    Inline,
    ConstantFold,
    DeadCodeElimination,
    LoopUnroll,
    TailCall,
    CSE,
    Memoize,
    
    // New Phase 2 optimizations
    HotPathInlining,
    ValueSpecialization,
    AdaptiveLoopUnrolling,
    
    // New Phase 4 optimizations
    EffectReordering,
    SubgraphFusion,
    
    // New Phase 5 optimizations
    MemoryAwareTransformation,
    CodeLayoutOptimization,
}

/// ML-driven optimization configuration
#[derive(Debug, Clone)]
pub struct MLOptimizationConfig {
    /// Confidence threshold for applying optimizations
    pub confidence_threshold: f64,
    /// Enable learning from optimization results
    pub enable_learning: bool,
    /// Path to ML model weights
    pub model_path: Option<String>,
    /// Runtime profiling data
    pub runtime_data: Option<RuntimeGuidedConfig>,
}

impl Default for MLOptimizationConfig {
    fn default() -> Self {
        Self {
            confidence_threshold: 0.7,
            enable_learning: true,
            model_path: None,
            runtime_data: None,
        }
    }
}

/// Machine learning optimization coordinator
pub struct MLOptimizationCoordinator {
    config: MLOptimizationConfig,
    #[cfg(feature = "ai-analysis")]
    ai_analyzer: AiAnalyzer,
    /// History of optimization decisions and their outcomes
    optimization_history: Vec<OptimizationDecision>,
}

/// Record of an optimization decision
#[derive(Debug, Clone)]
struct OptimizationDecision {
    graph_features: Vec<f32>,
    optimization_type: ExtendedOptimizationType,
    applied: bool,
    performance_impact: Option<f64>,
}

impl MLOptimizationCoordinator {
    /// Create a new ML optimization coordinator
    pub fn new(config: MLOptimizationConfig) -> Self {
        Self {
            config,
            #[cfg(feature = "ai-analysis")]
            ai_analyzer: AiAnalyzer::new(Default::default()),
            optimization_history: Vec::new(),
        }
    }
    
    /// Analyze a graph and determine which optimizations to apply
    pub fn analyze_and_optimize(&mut self, graph: &Graph) -> Result<Vec<Box<dyn OptimizationPass>>> {
        #[cfg(feature = "ai-analysis")]
        {
            // Use AI analyzer to get base analysis
            let ai_analysis = self.ai_analyzer.analyze_graph(graph)?;
            
            // Extract additional features for new optimization types
            let extended_features = self.extract_extended_features(graph);
            
            // Determine which optimizations to apply
            let optimization_decisions = self.make_optimization_decisions(
                &ai_analysis,
                &extended_features,
                graph
            );
            
            // Create optimization passes based on decisions
            self.create_optimization_passes(optimization_decisions)
        }
        
        #[cfg(not(feature = "ai-analysis"))]
        {
            // Fallback: use heuristics when AI analysis is not available
            self.create_heuristic_passes(graph)
        }
    }
    
    /// Extract features specific to new optimization types
    fn extract_extended_features(&self, graph: &Graph) -> ExtendedFeatures {
        ExtendedFeatures {
            effect_density: self.calculate_effect_density(graph),
            memory_pressure: self.estimate_memory_pressure(graph),
            loop_characteristics: self.analyze_loops(graph),
            function_call_graph: self.build_call_graph(graph),
            value_distribution: self.analyze_value_patterns(graph),
        }
    }
    
    /// Make optimization decisions based on AI analysis and extended features
    #[cfg(feature = "ai-analysis")]
    fn make_optimization_decisions(
        &mut self,
        ai_analysis: &AiAnalysisResult,
        extended_features: &ExtendedFeatures,
        graph: &Graph,
    ) -> Vec<OptimizationDecision> {
        let mut decisions = Vec::new();
        
        // Convert base AI optimization suggestions
        for suggestion in &ai_analysis.optimizations {
            if suggestion.confidence >= self.config.confidence_threshold as f32 {
                let opt_type = match suggestion.optimization_type {
                    AiOptType::Inline => ExtendedOptimizationType::Inline,
                    AiOptType::ConstantFold => ExtendedOptimizationType::ConstantFold,
                    AiOptType::DeadCodeElimination => ExtendedOptimizationType::DeadCodeElimination,
                    AiOptType::LoopUnroll => ExtendedOptimizationType::LoopUnroll,
                    AiOptType::TailCall => ExtendedOptimizationType::TailCall,
                    AiOptType::CSE => ExtendedOptimizationType::CSE,
                    AiOptType::Memoize => ExtendedOptimizationType::Memoize,
                };
                
                decisions.push(OptimizationDecision {
                    graph_features: vec![],
                    optimization_type: opt_type,
                    applied: true,
                    performance_impact: None,
                });
            }
        }
        
        // Add decisions for new optimization types based on extended features
        
        // Phase 2: Runtime-guided optimizations
        if let Some(runtime_data) = &self.config.runtime_data {
            if !runtime_data.hot_functions.is_empty() {
                decisions.push(OptimizationDecision {
                    graph_features: vec![runtime_data.hot_functions.len() as f32],
                    optimization_type: ExtendedOptimizationType::HotPathInlining,
                    applied: true,
                    performance_impact: None,
                });
            }
            
            if !runtime_data.skewed_values.is_empty() {
                decisions.push(OptimizationDecision {
                    graph_features: vec![runtime_data.skewed_values.len() as f32],
                    optimization_type: ExtendedOptimizationType::ValueSpecialization,
                    applied: true,
                    performance_impact: None,
                });
            }
            
            if !runtime_data.hot_loops.is_empty() {
                decisions.push(OptimizationDecision {
                    graph_features: vec![runtime_data.hot_loops.len() as f32],
                    optimization_type: ExtendedOptimizationType::AdaptiveLoopUnrolling,
                    applied: true,
                    performance_impact: None,
                });
            }
        }
        
        // Phase 4: AI-driven optimizations
        if extended_features.effect_density > 0.3 {
            decisions.push(OptimizationDecision {
                graph_features: vec![extended_features.effect_density],
                optimization_type: ExtendedOptimizationType::EffectReordering,
                applied: true,
                performance_impact: None,
            });
        }
        
        if self.detect_fusable_patterns(graph, &ai_analysis.patterns) {
            decisions.push(OptimizationDecision {
                graph_features: vec![],
                optimization_type: ExtendedOptimizationType::SubgraphFusion,
                applied: true,
                performance_impact: None,
            });
        }
        
        // Phase 5: Memory-aware optimizations
        if extended_features.memory_pressure > 0.5 {
            decisions.push(OptimizationDecision {
                graph_features: vec![extended_features.memory_pressure],
                optimization_type: ExtendedOptimizationType::MemoryAwareTransformation,
                applied: true,
                performance_impact: None,
            });
        }
        
        if extended_features.function_call_graph.len() > 10 {
            decisions.push(OptimizationDecision {
                graph_features: vec![extended_features.function_call_graph.len() as f32],
                optimization_type: ExtendedOptimizationType::CodeLayoutOptimization,
                applied: true,
                performance_impact: None,
            });
        }
        
        // Record decisions for learning
        self.optimization_history.extend(decisions.clone());
        
        decisions
    }
    
    /// Create optimization passes based on decisions
    fn create_optimization_passes(
        &self,
        decisions: Vec<OptimizationDecision>
    ) -> Result<Vec<Box<dyn OptimizationPass>>> {
        let mut passes: Vec<Box<dyn OptimizationPass>> = Vec::new();
        
        for decision in decisions {
            if !decision.applied {
                continue;
            }
            
            match decision.optimization_type {
                ExtendedOptimizationType::HotPathInlining => {
                    if let Some(runtime_data) = &self.config.runtime_data {
                        passes.push(Box::new(HotPathInliningPass::new(runtime_data.clone())));
                    }
                }
                ExtendedOptimizationType::ValueSpecialization => {
                    if let Some(runtime_data) = &self.config.runtime_data {
                        passes.push(Box::new(ValueSpecializationPass::new(runtime_data.clone())));
                    }
                }
                ExtendedOptimizationType::AdaptiveLoopUnrolling => {
                    if let Some(runtime_data) = &self.config.runtime_data {
                        passes.push(Box::new(AdaptiveLoopUnrollingPass::new(runtime_data.clone())));
                    }
                }
                ExtendedOptimizationType::Memoize => {
                    passes.push(Box::new(AdaptiveMemoizationPass::new(MemoizationConfig::default())));
                }
                ExtendedOptimizationType::EffectReordering => {
                    passes.push(Box::new(EffectReorderingPass::new(EffectReorderingConfig::default())));
                }
                ExtendedOptimizationType::SubgraphFusion => {
                    passes.push(Box::new(SubgraphFusionPass::new(SubgraphFusionConfig::default())));
                }
                ExtendedOptimizationType::MemoryAwareTransformation => {
                    passes.push(Box::new(MemoryAwarePass::new(MemoryAwareConfig::default())));
                }
                ExtendedOptimizationType::CodeLayoutOptimization => {
                    passes.push(Box::new(CodeLayoutPass::new(CodeLayoutConfig::default())));
                }
                _ => {
                    // Other optimization types handled by standard pipeline
                }
            }
        }
        
        Ok(passes)
    }
    
    /// Create passes using heuristics when AI is not available
    fn create_heuristic_passes(&self, _graph: &Graph) -> Result<Vec<Box<dyn OptimizationPass>>> {
        let mut passes: Vec<Box<dyn OptimizationPass>> = Vec::new();
        
        // Always apply basic optimizations
        if let Some(runtime_data) = &self.config.runtime_data {
            passes.push(Box::new(HotPathInliningPass::new(runtime_data.clone())));
            passes.push(Box::new(ValueSpecializationPass::new(runtime_data.clone())));
            passes.push(Box::new(AdaptiveLoopUnrollingPass::new(runtime_data.clone())));
        }
        
        passes.push(Box::new(AdaptiveMemoizationPass::new(MemoizationConfig::default())));
        passes.push(Box::new(EffectReorderingPass::new(EffectReorderingConfig::default())));
        passes.push(Box::new(SubgraphFusionPass::new(SubgraphFusionConfig::default())));
        passes.push(Box::new(MemoryAwarePass::new(MemoryAwareConfig::default())));
        passes.push(Box::new(CodeLayoutPass::new(CodeLayoutConfig::default())));
        
        Ok(passes)
    }
    
    /// Update the ML model with optimization results
    pub fn update_with_results(&mut self, performance_metrics: PerformanceMetrics) {
        if !self.config.enable_learning {
            return;
        }
        
        // Update the most recent decisions with performance impact
        for decision in self.optimization_history.iter_mut().rev() {
            if decision.performance_impact.is_none() {
                decision.performance_impact = Some(performance_metrics.improvement_percentage);
            }
        }
        
        // TODO: Train ML model with accumulated history
        // This would involve:
        // 1. Extracting features from historical decisions
        // 2. Using performance impact as labels
        // 3. Training a decision model
        // 4. Persisting the updated model
    }
    
    // Helper methods for feature extraction
    
    pub fn calculate_effect_density(&self, graph: &Graph) -> f32 {
        let total_nodes = graph.nodes.len() as f32;
        let effect_nodes = graph.nodes.values()
            .filter(|node| matches!(node, fluentai_core::ast::Node::Effect { .. }))
            .count() as f32;
        
        if total_nodes > 0.0 {
            effect_nodes / total_nodes
        } else {
            0.0
        }
    }
    
    pub fn estimate_memory_pressure(&self, graph: &Graph) -> f32 {
        let allocation_nodes = graph.nodes.values()
            .filter(|node| match node {
                fluentai_core::ast::Node::List(_) => true,
                fluentai_core::ast::Node::Map(_) => true,
                fluentai_core::ast::Node::Lambda { .. } => true,
                _ => false,
            })
            .count() as f32;
        
        let total_nodes = graph.nodes.len() as f32;
        if total_nodes > 0.0 {
            allocation_nodes / total_nodes
        } else {
            0.0
        }
    }
    
    fn analyze_loops(&self, _graph: &Graph) -> LoopCharacteristics {
        // Simplified loop analysis
        LoopCharacteristics {
            total_loops: 0,
            nested_loops: 0,
            average_depth: 0.0,
        }
    }
    
    fn build_call_graph(&self, graph: &Graph) -> HashMap<NodeId, Vec<NodeId>> {
        let mut call_graph = HashMap::new();
        
        for (node_id, node) in &graph.nodes {
            if let fluentai_core::ast::Node::Application { function, .. } = node {
                call_graph.entry(*node_id).or_insert_with(Vec::new).push(*function);
            }
        }
        
        call_graph
    }
    
    fn analyze_value_patterns(&self, _graph: &Graph) -> ValueDistribution {
        ValueDistribution {
            constant_ratio: 0.0,
            variable_ratio: 1.0,
        }
    }
    
    #[cfg(feature = "ai-analysis")]
    fn detect_fusable_patterns(
        &self,
        _graph: &Graph,
        patterns: &[fluentai_ai::DetectedPattern]
    ) -> bool {
        patterns.iter().any(|p| {
            matches!(
                p.pattern_type,
                fluentai_ai::PatternType::MapReduce |
                fluentai_ai::PatternType::FilterMap
            )
        })
    }
    
    #[cfg(not(feature = "ai-analysis"))]
    fn detect_fusable_patterns(&self, _graph: &Graph, _patterns: &[()]) -> bool {
        false
    }
}

/// Extended features for new optimization types
#[derive(Debug)]
struct ExtendedFeatures {
    effect_density: f32,
    memory_pressure: f32,
    loop_characteristics: LoopCharacteristics,
    function_call_graph: HashMap<NodeId, Vec<NodeId>>,
    value_distribution: ValueDistribution,
}

#[derive(Debug)]
struct LoopCharacteristics {
    total_loops: usize,
    nested_loops: usize,
    average_depth: f32,
}

#[derive(Debug)]
struct ValueDistribution {
    constant_ratio: f32,
    variable_ratio: f32,
}

/// Performance metrics for learning
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub execution_time_before: f64,
    pub execution_time_after: f64,
    pub improvement_percentage: f64,
    pub memory_usage_before: usize,
    pub memory_usage_after: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_ml_coordinator_creation() {
        let config = MLOptimizationConfig::default();
        let coordinator = MLOptimizationCoordinator::new(config);
        assert_eq!(coordinator.optimization_history.len(), 0);
    }
    
    #[test]
    fn test_effect_density_calculation() {
        let config = MLOptimizationConfig::default();
        let coordinator = MLOptimizationCoordinator::new(config);
        
        let mut graph = Graph::new();
        
        // Add some effect nodes
        for _ in 0..3 {
            graph.add_node(fluentai_core::ast::Node::Effect {
                effect_type: fluentai_core::ast::EffectType::IO,
                operation: "print".to_string(),
                args: vec![],
            }).unwrap();
        }
        
        // Add some non-effect nodes
        for i in 0..7 {
            graph.add_node(fluentai_core::ast::Node::Literal(
                fluentai_core::ast::Literal::Integer(i)
            )).unwrap();
        }
        
        let density = coordinator.calculate_effect_density(&graph);
        assert!((density - 0.3).abs() < 0.01); // 3/10 = 0.3
    }
}