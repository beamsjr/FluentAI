//! Reinforcement Learning for optimization strategy learning
//!
//! This module implements an RL agent that learns which optimizations
//! to apply based on runtime performance feedback.

pub mod agent;
pub mod environment;
pub mod experience;
pub mod reward;
pub mod training;
#[cfg(feature = "rl")]
pub mod vm_adapter;

pub use agent::{OptimizationAgent, AgentConfig};
pub use environment::{OptimizationEnvironment, EnvironmentState, PerformanceMetrics, CompiledProgram};
pub use experience::{Experience, ExperienceReplay};
pub use reward::{RewardCalculator, RewardDetails, PerformanceTargets};
pub use training::{TrainingConfig, Trainer};
#[cfg(feature = "rl")]
pub use vm_adapter::{VMRLAdapter, create_vm_adapter};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Action space for the RL agent
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OptimizationAction {
    /// No optimization
    NoOp,
    /// Enable constant folding
    ConstantFolding,
    /// Enable dead code elimination
    DeadCodeElimination,
    /// Enable common subexpression elimination
    CSE,
    /// Enable function inlining
    Inline(InlineLevel),
    /// Enable tail call optimization
    TailCall,
    /// Enable loop optimization
    LoopOpt,
    /// Enable beta reduction
    BetaReduction,
    /// Enable partial evaluation
    PartialEval,
    /// Enable strength reduction
    StrengthReduction,
    /// Enable algebraic simplification
    AlgebraicSimplification,
    /// Enable loop invariant code motion
    LoopInvariantMotion,
    /// Enable function specialization
    FunctionSpecialization,
    /// Composite action: multiple optimizations
    Composite(u32), // Bitmask of optimizations
}

/// Inline threshold levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InlineLevel {
    Conservative, // threshold = 5
    Standard,     // threshold = 10
    Aggressive,   // threshold = 20
}

/// State representation for the RL agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizationState {
    /// AST features (from feature_vector)
    pub ast_features: Vec<f32>,
    /// Current optimization configuration
    pub current_config: OptimizationConfig,
    /// Performance history
    pub performance_history: Vec<f32>,
    /// Resource usage history
    pub resource_history: Vec<ResourceMetrics>,
}

/// Resource usage metrics
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ResourceMetrics {
    /// Memory usage in bytes
    pub memory_bytes: u64,
    /// Compilation time in microseconds
    pub compilation_time_us: u64,
    /// Binary size in bytes
    pub binary_size: u64,
}

/// Configuration for optimization actions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizationConfig {
    pub constant_folding: bool,
    pub dead_code_elimination: bool,
    pub cse: bool,
    pub inline: bool,
    pub inline_threshold: usize,
    pub tail_call_optimization: bool,
    pub loop_optimization: bool,
    pub beta_reduction: bool,
    pub partial_evaluation: bool,
    pub strength_reduction: bool,
    pub algebraic_simplification: bool,
    pub loop_invariant_code_motion: bool,
    pub function_specialization: bool,
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self {
            constant_folding: false,
            dead_code_elimination: false,
            cse: false,
            inline: false,
            inline_threshold: 0,
            tail_call_optimization: false,
            loop_optimization: false,
            beta_reduction: false,
            partial_evaluation: false,
            strength_reduction: false,
            algebraic_simplification: false,
            loop_invariant_code_motion: false,
            function_specialization: false,
        }
    }
}

impl OptimizationConfig {
    /// Apply an action to the configuration
    pub fn apply_action(&mut self, action: OptimizationAction) {
        match action {
            OptimizationAction::NoOp => {}
            OptimizationAction::ConstantFolding => self.constant_folding = true,
            OptimizationAction::DeadCodeElimination => self.dead_code_elimination = true,
            OptimizationAction::CSE => self.cse = true,
            OptimizationAction::Inline(level) => {
                self.inline = true;
                self.inline_threshold = match level {
                    InlineLevel::Conservative => 5,
                    InlineLevel::Standard => 10,
                    InlineLevel::Aggressive => 20,
                };
            }
            OptimizationAction::TailCall => self.tail_call_optimization = true,
            OptimizationAction::LoopOpt => self.loop_optimization = true,
            OptimizationAction::BetaReduction => self.beta_reduction = true,
            OptimizationAction::PartialEval => self.partial_evaluation = true,
            OptimizationAction::StrengthReduction => self.strength_reduction = true,
            OptimizationAction::AlgebraicSimplification => self.algebraic_simplification = true,
            OptimizationAction::LoopInvariantMotion => self.loop_invariant_code_motion = true,
            OptimizationAction::FunctionSpecialization => self.function_specialization = true,
            OptimizationAction::Composite(mask) => {
                // Apply multiple optimizations based on bitmask
                if mask & 0x001 != 0 { self.constant_folding = true; }
                if mask & 0x002 != 0 { self.dead_code_elimination = true; }
                if mask & 0x004 != 0 { self.cse = true; }
                if mask & 0x008 != 0 { self.inline = true; self.inline_threshold = 10; }
                if mask & 0x010 != 0 { self.tail_call_optimization = true; }
                if mask & 0x020 != 0 { self.loop_optimization = true; }
                if mask & 0x040 != 0 { self.beta_reduction = true; }
                if mask & 0x080 != 0 { self.partial_evaluation = true; }
                if mask & 0x100 != 0 { self.strength_reduction = true; }
                if mask & 0x200 != 0 { self.algebraic_simplification = true; }
                if mask & 0x400 != 0 { self.loop_invariant_code_motion = true; }
                if mask & 0x800 != 0 { self.function_specialization = true; }
            }
        }
    }
    
    /// Convert to a dictionary representation for optimization pipeline
    pub fn to_dict(&self) -> HashMap<String, serde_json::Value> {
        let mut config = HashMap::new();
        config.insert("constant_folding".to_string(), self.constant_folding.into());
        config.insert("dead_code_elimination".to_string(), self.dead_code_elimination.into());
        config.insert("cse".to_string(), self.cse.into());
        config.insert("inline".to_string(), self.inline.into());
        config.insert("inline_threshold".to_string(), (self.inline_threshold as i64).into());
        config.insert("tail_call_optimization".to_string(), self.tail_call_optimization.into());
        config.insert("loop_optimization".to_string(), self.loop_optimization.into());
        config.insert("beta_reduction".to_string(), self.beta_reduction.into());
        config.insert("partial_evaluation".to_string(), self.partial_evaluation.into());
        config.insert("strength_reduction".to_string(), self.strength_reduction.into());
        config.insert("algebraic_simplification".to_string(), self.algebraic_simplification.into());
        config.insert("loop_invariant_code_motion".to_string(), self.loop_invariant_code_motion.into());
        config.insert("function_specialization".to_string(), self.function_specialization.into());
        config.insert("max_iterations".to_string(), 2i64.into());
        config.insert("debug_mode".to_string(), false.into());
        config
    }
}