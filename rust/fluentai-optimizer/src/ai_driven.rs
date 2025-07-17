//! AI-driven optimization configuration
//!
//! This module bridges AI analysis results with the optimization pipeline,
//! allowing the compiler to dynamically configure optimization passes based
//! on machine learning insights.

use crate::pipeline::{OptimizationConfig, OptimizationLevel};
use crate::passes::OptimizationPass;
use crate::passes::effect_reordering::{EffectReorderingPass, EffectReorderingConfig};
use crate::passes::subgraph_fusion::{SubgraphFusionPass, SubgraphFusionConfig};
use crate::passes::memory_aware::{MemoryAwarePass, MemoryAwareConfig};
use crate::passes::code_layout::{CodeLayoutPass, CodeLayoutConfig};
use anyhow::Result;
use fluentai_core::ast::Graph;
use std::collections::HashSet;


/// Source of optimization configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationSource {
    /// Manual optimization level
    Manual(OptimizationLevel),
    /// AI-driven optimization hints
    #[cfg(feature = "ai-analysis")]
    AIHints,
    /// Hybrid mode: base level enhanced by AI
    #[cfg(feature = "ai-analysis")]
    Hybrid(OptimizationLevel),
    /// Reinforcement Learning mode
    #[cfg(all(feature = "ai-analysis", feature = "rl"))]
    ReinforcementLearning,
}

/// Extract optimization configuration from AI analysis results
#[cfg(feature = "ai-analysis")]
pub fn ai_hints_to_optimization_config(
    graph: &Graph,
    analysis: &serde_json::Value,
) -> OptimizationConfig {
    use crate::ml_integration::{MLOptimizationCoordinator, MLOptimizationConfig};
    
    // Create ML coordinator
    let ml_config = MLOptimizationConfig {
        confidence_threshold: analysis["confidence"].as_f64().unwrap_or(0.7),
        enable_learning: true,
        model_path: None,
        runtime_data: None, // Would be populated from profiling data
    };
    
    let mut coordinator = MLOptimizationCoordinator::new(ml_config);
    
    // Use ML system to determine optimizations
    match coordinator.analyze_and_optimize(graph) {
        Ok(passes) => {
            // Create config with ML-determined passes
            let mut config = OptimizationConfig::for_level(OptimizationLevel::Standard);
            // The passes are already created by the coordinator
            // In a full implementation, we'd integrate these into the config
            config
        }
        Err(_) => {
            // Fallback to default
            OptimizationConfig::for_level(OptimizationLevel::Standard)
        }
    }
}

/// Create a hybrid configuration combining base level with AI hints
#[cfg(feature = "ai-analysis")]
pub fn hybrid_optimization_config(
    base: OptimizationLevel,
    _graph: &Graph,
    _analysis: &serde_json::Value,
) -> OptimizationConfig {
    // TODO: Implement hybrid configuration once circular dependency is resolved
    // For now, just return the base configuration
    OptimizationConfig::for_level(base)
}

/// Create an AI-driven optimization pipeline
pub fn create_ai_driven_pipeline() -> Vec<Box<dyn OptimizationPass>> {
    vec![
        // Phase 4: AI-driven optimizations
        // Effect reordering for better performance
        Box::new(EffectReorderingPass::new(EffectReorderingConfig {
            enable_batching: true,
            enable_pure_hoisting: true,
            max_reorder_distance: 15,
        })),
        // Subgraph fusion to eliminate redundant traversals
        Box::new(SubgraphFusionPass::new(SubgraphFusionConfig {
            enable_map_fusion: true,
            enable_filter_map_fusion: true,
            enable_general_fusion: true,
            max_subgraph_size: 30,
        })),
        
        // Phase 5: Memory-aware transformations
        // Memory-aware optimizations for better cache performance
        Box::new(MemoryAwarePass::new(MemoryAwareConfig {
            enable_field_reordering: true,
            enable_allocation_opt: true,
            enable_cache_layout: true,
            cache_line_size: 64,
            max_struct_size: 256,
        })),
        // Code layout optimization for instruction cache
        Box::new(CodeLayoutPass::new(CodeLayoutConfig {
            enable_function_reordering: true,
            enable_branch_optimization: true,
            enable_loop_alignment: true,
            hot_threshold: 1000,
            cold_threshold: 10,
        })),
    ]
}

/// Configuration for AI-driven optimizations
#[derive(Debug, Clone)]
pub struct AIDrivenConfig {
    /// Enable effect reordering
    pub enable_effect_reordering: bool,
    /// Enable subgraph fusion
    pub enable_subgraph_fusion: bool,
    /// Enable pattern-based optimizations
    pub enable_pattern_optimization: bool,
    /// Confidence threshold for applying optimizations
    pub confidence_threshold: f64,
}

impl Default for AIDrivenConfig {
    fn default() -> Self {
        Self {
            enable_effect_reordering: true,
            enable_subgraph_fusion: true,
            enable_pattern_optimization: true,
            confidence_threshold: 0.8,
        }
    }
}

/// Analyze which nodes should be targeted for specific optimizations
#[cfg(feature = "ai-analysis")]
pub struct TargetedOptimizationHints {
    /// Nodes that should be inlined
    pub inline_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Nodes that are candidates for constant folding
    pub constant_fold_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Nodes that should be eliminated
    pub dead_code_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Nodes that are candidates for CSE
    pub cse_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Nodes that can be tail-call optimized
    pub tail_call_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Loops that can be unrolled
    pub loop_unroll_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Functions that should be memoized
    pub memoize_targets: HashSet<fluentai_core::ast::NodeId>,
    
    // New optimization targets
    /// Hot functions for runtime-guided inlining
    pub hot_function_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Values for specialization
    pub value_specialization_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Effects that can be reordered
    pub effect_reorder_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Subgraphs that can be fused
    pub subgraph_fusion_targets: HashSet<fluentai_core::ast::NodeId>,
    /// Allocations for memory optimization
    pub memory_opt_targets: HashSet<fluentai_core::ast::NodeId>,
}

#[cfg(feature = "ai-analysis")]
impl TargetedOptimizationHints {
    /// Create targeted hints from AI analysis
    pub fn from_analysis(_analysis: &serde_json::Value) -> Self {
        // TODO: Implement targeted hints once circular dependency is resolved
        Self {
            inline_targets: HashSet::new(),
            constant_fold_targets: HashSet::new(),
            dead_code_targets: HashSet::new(),
            cse_targets: HashSet::new(),
            tail_call_targets: HashSet::new(),
            loop_unroll_targets: HashSet::new(),
            memoize_targets: HashSet::new(),
            hot_function_targets: HashSet::new(),
            value_specialization_targets: HashSet::new(),
            effect_reorder_targets: HashSet::new(),
            subgraph_fusion_targets: HashSet::new(),
            memory_opt_targets: HashSet::new(),
        }
    }
}

/// Extract features from AST for RL
#[cfg(feature = "ai-analysis")]
pub fn extract_ast_features(graph: &Graph) -> Vec<f32> {
    // TODO: Implement feature extraction once circular dependency is resolved
    // For now, return basic features
    vec![
        graph.nodes.len() as f32,
        0.0, // edge count placeholder
        0.0, // max depth placeholder
        0.0, // branching factor placeholder
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    #[cfg(feature = "ai-analysis")]
    fn test_ai_hints_to_config() {
        let analysis = serde_json::json!({
            "optimizations": [],
            "patterns": [],
            "confidence": 0.85
        });
        
        let graph = Graph::new();
        let config = ai_hints_to_optimization_config(&graph, &analysis);
        
        // For now, just check that it returns a valid config
        assert_eq!(config.level, OptimizationLevel::Standard);
    }
    
    #[test]
    #[cfg(feature = "ai-analysis")]
    fn test_hybrid_config() {
        let analysis = serde_json::json!({
            "optimizations": [],
            "patterns": [],
            "confidence": 0.6
        });
        
        let graph = Graph::new();
        let config = hybrid_optimization_config(OptimizationLevel::Basic, &graph, &analysis);
        
        // For now, just check that it returns the base config
        assert_eq!(config.level, OptimizationLevel::Basic);
        assert!(config.constant_folding);
        assert!(config.dead_code_elimination);
    }
}