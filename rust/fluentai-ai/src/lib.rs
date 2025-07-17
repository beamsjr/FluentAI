//! AI-driven analysis for FluentAI AST
//!
//! This crate provides machine learning capabilities for analyzing and optimizing
//! FluentAI programs. It includes:
//! 
//! - Tensor conversion utilities for AST graphs
//! - Graph neural network models for code analysis
//! - Pattern detection and optimization prediction
//! - Integration with various ML backends (Burn, Candle, ONNX)

#![warn(missing_docs)]

pub mod analysis;
pub mod analyzer;
pub mod cache;
pub mod cli;
pub mod error;
pub mod features;
pub mod metadata;
pub mod models;
pub mod models_simple;
pub mod patterns;
pub mod rl;
pub mod tensor;

// Re-export core types
pub use analysis::{AiAnalysisResult, AstAnalyzer, OptimizationType, PatternType, DetectedPattern, OptimizationSuggestion};
pub use analyzer::{AiAnalyzer, AiAnalyzerBuilder, analyze_ast, create_default_analyzer};
pub use error::{AiError, Result};
pub use tensor::{TensorBuilder, TensorFormat};

// Re-export RL types
#[cfg(feature = "rl")]
pub use rl::{
    OptimizationAgent, AgentConfig,
    OptimizationEnvironment, EnvironmentState,
    ExperienceReplay, Experience,
    RewardCalculator, PerformanceMetrics,
    TrainingConfig, Trainer,
};

// Re-export feature flags
#[cfg(feature = "burn-backend")]
pub use models_simple::burn_simple;

#[cfg(feature = "candle-backend")]
pub use models::candle as candle_models;

#[cfg(feature = "onnx-backend")]
pub use models::onnx as onnx_models;