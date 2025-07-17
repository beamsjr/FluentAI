//! FluentAi optimization framework
//!
//! This crate provides various program transformations to improve performance.

#![warn(missing_docs)]

pub mod advanced_optimizer;
pub mod ai_driven;
pub mod analysis;
pub mod di;
pub mod graph_optimizer;
pub mod ml_hints;
pub mod ml_integration;
pub mod passes;
pub mod pipeline;
pub mod runtime_guided;
pub mod stats;
pub mod visitor;

pub use advanced_optimizer::AdvancedOptimizer;
pub use ai_driven::OptimizationSource;
#[cfg(feature = "ai-analysis")]
pub use ai_driven::{
    ai_hints_to_optimization_config, hybrid_optimization_config, TargetedOptimizationHints,
};
pub use di::{
    ContainerOptimizationProvider, DynamicOptimizationPipeline, OptimizationPipelineBuilder,
    OptimizationServiceProvider, OptimizerContainerBuilderExt,
};
pub use graph_optimizer::GraphOptimizer;
pub use pipeline::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
pub use stats::OptimizationStats;
