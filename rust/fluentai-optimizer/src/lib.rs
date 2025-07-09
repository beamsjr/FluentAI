//! FluentAi optimization framework
//!
//! This crate provides various program transformations to improve performance.

#![warn(missing_docs)]

pub mod advanced_optimizer;
pub mod analysis;
pub mod di;
pub mod graph_optimizer;
pub mod ml_hints;
pub mod passes;
pub mod pipeline;
pub mod stats;
pub mod visitor;

pub use advanced_optimizer::AdvancedOptimizer;
pub use di::{
    ContainerOptimizationProvider, DynamicOptimizationPipeline, OptimizationPipelineBuilder,
    OptimizationServiceProvider, OptimizerContainerBuilderExt,
};
pub use graph_optimizer::GraphOptimizer;
pub use pipeline::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
pub use stats::OptimizationStats;
