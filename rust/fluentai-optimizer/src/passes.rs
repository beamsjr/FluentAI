//! Individual optimization passes

pub mod beta_reduction;
pub mod constant_folding;
pub mod context_aware;
pub mod continuum_lowering;
pub mod cse;
pub mod dead_code;
pub mod effect_aware;
pub mod inline;
pub mod loop_opts;
pub mod partial_eval;
pub mod tail_call;

use anyhow::Result;
use fluentai_core::ast::Graph;

/// Trait for optimization passes
pub trait OptimizationPass: Send + Sync {
    /// Name of the optimization pass
    fn name(&self) -> &str;

    /// Run the optimization pass
    fn run(&mut self, graph: &Graph) -> Result<Graph>;

    /// Check if the pass is applicable
    fn is_applicable(&self, graph: &Graph) -> bool {
        !graph.nodes.is_empty()
    }

    /// Get statistics about the pass
    fn stats(&self) -> String {
        format!("{} pass completed", self.name())
    }
}

#[cfg(test)]
#[path = "passes_tests.rs"]
mod passes_tests;
