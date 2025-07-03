//! Individual optimization passes

pub mod constant_folding;
pub mod dead_code;
pub mod cse;
pub mod inline;
pub mod tail_call;
pub mod loop_opts;
pub mod beta_reduction;
pub mod partial_eval;

use claudelang_core::ast::Graph;
use anyhow::Result;

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