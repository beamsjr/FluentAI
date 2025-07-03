//! ClaudeLang optimization framework
//! 
//! This crate provides various program transformations to improve performance.

#![warn(missing_docs)]

pub mod analysis;
pub mod graph_optimizer;
pub mod advanced_optimizer;
pub mod ml_hints;
pub mod passes;
pub mod pipeline;
pub mod stats;

pub use graph_optimizer::GraphOptimizer;
pub use advanced_optimizer::AdvancedOptimizer;
pub use pipeline::{OptimizationPipeline, OptimizationConfig};
pub use stats::OptimizationStats;