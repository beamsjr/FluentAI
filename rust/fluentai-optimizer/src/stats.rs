//! Optimization statistics tracking

use serde::{Serialize, Deserialize};
use std::fmt;

/// Statistics about optimizations performed
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct OptimizationStats {
    /// Number of constant folding operations
    pub constant_folded: usize,
    /// Number of dead code nodes eliminated
    pub dead_code_eliminated: usize,
    /// Number of pure expressions evaluated
    pub pure_expressions_evaluated: usize,
    /// Number of branches eliminated
    pub branches_eliminated: usize,
    /// Number of expressions inlined
    pub inlined_expressions: usize,
    /// Number of tail calls optimized
    pub tail_calls_optimized: usize,
    /// Number of common subexpressions eliminated
    pub cse_eliminated: usize,
    /// Number of loops unrolled
    pub loops_unrolled: usize,
    /// Number of operations fused
    pub operations_fused: usize,
    /// Number of nodes before optimization
    pub nodes_before: usize,
    /// Number of nodes after optimization
    pub nodes_after: usize,
    /// Time spent optimizing (microseconds)
    pub optimization_time_us: u64,
}

impl OptimizationStats {
    /// Create new statistics tracker
    pub fn new() -> Self {
        Self::default()
    }

    /// Calculate reduction percentage
    pub fn reduction_percentage(&self) -> f64 {
        if self.nodes_before == 0 {
            0.0
        } else {
            ((self.nodes_before - self.nodes_after) as f64 / self.nodes_before as f64) * 100.0
        }
    }

    /// Get total optimizations performed
    pub fn total_optimizations(&self) -> usize {
        self.constant_folded +
        self.dead_code_eliminated +
        self.pure_expressions_evaluated +
        self.branches_eliminated +
        self.inlined_expressions +
        self.tail_calls_optimized +
        self.cse_eliminated +
        self.loops_unrolled +
        self.operations_fused
    }

    /// Merge stats from another instance
    pub fn merge(&mut self, other: &OptimizationStats) {
        self.constant_folded += other.constant_folded;
        self.dead_code_eliminated += other.dead_code_eliminated;
        self.pure_expressions_evaluated += other.pure_expressions_evaluated;
        self.branches_eliminated += other.branches_eliminated;
        self.inlined_expressions += other.inlined_expressions;
        self.tail_calls_optimized += other.tail_calls_optimized;
        self.cse_eliminated += other.cse_eliminated;
        self.loops_unrolled += other.loops_unrolled;
        self.operations_fused += other.operations_fused;
        self.optimization_time_us += other.optimization_time_us;
    }
}

impl fmt::Display for OptimizationStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Optimization Statistics:")?;
        writeln!(f, "  Nodes: {} -> {} ({:.1}% reduction)", 
            self.nodes_before, self.nodes_after, self.reduction_percentage())?;
        writeln!(f, "  Constant folding: {}", self.constant_folded)?;
        writeln!(f, "  Dead code eliminated: {}", self.dead_code_eliminated)?;
        writeln!(f, "  Pure expressions evaluated: {}", self.pure_expressions_evaluated)?;
        writeln!(f, "  Branches eliminated: {}", self.branches_eliminated)?;
        writeln!(f, "  Expressions inlined: {}", self.inlined_expressions)?;
        writeln!(f, "  Tail calls optimized: {}", self.tail_calls_optimized)?;
        writeln!(f, "  CSE eliminated: {}", self.cse_eliminated)?;
        writeln!(f, "  Loops unrolled: {}", self.loops_unrolled)?;
        writeln!(f, "  Operations fused: {}", self.operations_fused)?;
        writeln!(f, "  Total optimizations: {}", self.total_optimizations())?;
        writeln!(f, "  Time: {:.3}ms", self.optimization_time_us as f64 / 1000.0)?;
        Ok(())
    }
}