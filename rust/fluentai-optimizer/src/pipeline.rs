//! Optimization pipeline management

use fluentai_core::ast::Graph;
use anyhow::Result;
use crate::stats::OptimizationStats;
use crate::passes::OptimizationPass;
use crate::graph_optimizer::GraphOptimizer;
use crate::advanced_optimizer::AdvancedOptimizer;
use std::time::Instant;

/// Optimization level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    /// No optimizations
    None,
    /// Basic optimizations (O1)
    Basic,
    /// Standard optimizations (O2)
    Standard,
    /// Aggressive optimizations (O3)
    Aggressive,
}

/// Optimization configuration
#[derive(Debug, Clone)]
pub struct OptimizationConfig {
    /// Optimization level
    pub level: OptimizationLevel,
    /// Enable constant folding
    pub constant_folding: bool,
    /// Enable dead code elimination
    pub dead_code_elimination: bool,
    /// Enable common subexpression elimination
    pub cse: bool,
    /// Enable function inlining
    pub inline: bool,
    /// Inline threshold (node count)
    pub inline_threshold: usize,
    /// Enable tail call optimization
    pub tail_call_optimization: bool,
    /// Enable loop optimizations
    pub loop_optimization: bool,
    /// Enable beta reduction
    pub beta_reduction: bool,
    /// Enable partial evaluation
    pub partial_evaluation: bool,
    /// Maximum optimization iterations
    pub max_iterations: usize,
    /// Debug mode (preserve more information)
    pub debug_mode: bool,
}

impl OptimizationConfig {
    /// Create config for optimization level
    pub fn for_level(level: OptimizationLevel) -> Self {
        match level {
            OptimizationLevel::None => Self {
                level,
                constant_folding: false,
                dead_code_elimination: false,
                cse: false,
                inline: false,
                inline_threshold: 0,
                tail_call_optimization: false,
                loop_optimization: false,
                beta_reduction: false,
                partial_evaluation: false,
                max_iterations: 0,
                debug_mode: true,
            },
            OptimizationLevel::Basic => Self {
                level,
                constant_folding: true,
                dead_code_elimination: true,
                cse: false,
                inline: false,
                inline_threshold: 5,
                tail_call_optimization: false,
                loop_optimization: false,
                beta_reduction: false,
                partial_evaluation: false,
                max_iterations: 1,
                debug_mode: false,
            },
            OptimizationLevel::Standard => Self {
                level,
                constant_folding: true,
                dead_code_elimination: true,
                cse: true,
                inline: true,
                inline_threshold: 10,
                tail_call_optimization: true,
                loop_optimization: false,
                beta_reduction: true,
                partial_evaluation: false,
                max_iterations: 2,
                debug_mode: false,
            },
            OptimizationLevel::Aggressive => Self {
                level,
                constant_folding: true,
                dead_code_elimination: true,
                cse: true,
                inline: true,
                inline_threshold: 20,
                tail_call_optimization: true,
                loop_optimization: true,
                beta_reduction: true,
                partial_evaluation: true,
                max_iterations: 3,
                debug_mode: false,
            },
        }
    }
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self::for_level(OptimizationLevel::Standard)
    }
}

/// Optimization pipeline
pub struct OptimizationPipeline {
    config: OptimizationConfig,
    stats: OptimizationStats,
    passes: Vec<Box<dyn OptimizationPass>>,
}

impl OptimizationPipeline {
    /// Create new optimization pipeline
    pub fn new(config: OptimizationConfig) -> Self {
        let mut pipeline = Self {
            config,
            stats: OptimizationStats::new(),
            passes: Vec::new(),
        };

        // Add passes based on configuration
        pipeline.configure_passes();
        
        pipeline
    }

    /// Configure passes based on config
    fn configure_passes(&mut self) {
        use crate::passes::*;

        self.passes.clear();

        if self.config.constant_folding {
            self.passes.push(Box::new(constant_folding::ConstantFoldingPass::new()));
        }

        if self.config.dead_code_elimination {
            self.passes.push(Box::new(dead_code::DeadCodeEliminationPass::new()));
        }

        if self.config.cse {
            self.passes.push(Box::new(cse::CommonSubexpressionEliminationPass::new()));
        }

        if self.config.inline {
            self.passes.push(Box::new(inline::InlinePass::new(self.config.inline_threshold)));
        }

        if self.config.tail_call_optimization {
            self.passes.push(Box::new(tail_call::TailCallOptimizationPass::new()));
        }

        if self.config.loop_optimization {
            self.passes.push(Box::new(loop_opts::LoopOptimizationPass::new()));
        }

        if self.config.beta_reduction {
            self.passes.push(Box::new(beta_reduction::BetaReductionPass::new()));
        }

        if self.config.partial_evaluation {
            self.passes.push(Box::new(partial_eval::PartialEvaluationPass::new()));
        }

        // Add effect-aware optimization for Standard and Aggressive levels
        if self.config.level == OptimizationLevel::Standard || self.config.level == OptimizationLevel::Aggressive {
            self.passes.push(Box::new(effect_aware::EffectAwarePass::new()));
        }
    }

    /// Run optimization pipeline
    pub fn optimize(&mut self, graph: &Graph) -> Result<Graph> {
        let start = Instant::now();
        self.stats = OptimizationStats::new();
        self.stats.nodes_before = graph.nodes.len();

        let mut optimized = graph.clone();

        match self.config.level {
            OptimizationLevel::None => {
                // No optimization
            }
            OptimizationLevel::Basic => {
                // Use basic graph optimizer
                let mut optimizer = GraphOptimizer::new();
                optimized = optimizer.optimize(&optimized)?;
                self.stats.merge(&optimizer.stats());
            }
            OptimizationLevel::Standard | OptimizationLevel::Aggressive => {
                // Use advanced optimizer
                let mut optimizer = AdvancedOptimizer::new()
                    .with_inline_threshold(self.config.inline_threshold);
                optimized = optimizer.optimize(&optimized)?;
                self.stats.merge(&optimizer.stats());

                // Run additional passes for aggressive mode
                if self.config.level == OptimizationLevel::Aggressive {
                    for _iteration in 0..self.config.max_iterations {
                        let before = optimized.nodes.len();
                        
                        // Run individual passes
                        for pass in &mut self.passes {
                            if pass.is_applicable(&optimized) {
                                optimized = pass.run(&optimized)?;
                            }
                        }

                        let after = optimized.nodes.len();
                        
                        // Stop if no changes
                        if before == after {
                            break;
                        }
                    }
                }
            }
        }

        self.stats.nodes_after = optimized.nodes.len();
        self.stats.optimization_time_us = start.elapsed().as_micros() as u64;

        Ok(optimized)
    }

    /// Get optimization statistics
    pub fn stats(&self) -> &OptimizationStats {
        &self.stats
    }

    /// Add custom pass
    pub fn add_pass(&mut self, pass: Box<dyn OptimizationPass>) {
        self.passes.push(pass);
    }

    /// Remove all passes
    pub fn clear_passes(&mut self) {
        self.passes.clear();
    }
}

impl Default for OptimizationPipeline {
    fn default() -> Self {
        Self::new(OptimizationConfig::default())
    }
}