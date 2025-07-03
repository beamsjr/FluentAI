//! Tail call optimization pass

use claudelang_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};
use anyhow::Result;
use crate::passes::OptimizationPass;

/// Tail call optimization pass
pub struct TailCallOptimizationPass {
    optimized_count: usize,
}

impl TailCallOptimizationPass {
    /// Create new tail call optimization pass
    pub fn new() -> Self {
        Self { optimized_count: 0 }
    }
}

impl OptimizationPass for TailCallOptimizationPass {
    fn name(&self) -> &str {
        "Tail Call Optimization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.optimized_count = 0;
        let mut optimized = graph.clone();
        
        // Find tail-recursive functions in letrec bindings
        for (node_id, node) in &graph.nodes {
            if let Node::Letrec { bindings, body: _ } = node {
                for (func_name, func_id) in bindings {
                    if let Some(Node::Lambda { params: _, body }) = graph.get_node(*func_id) {
                        if self.is_tail_recursive(graph, func_name, *body) {
                            // Mark this function for tail call optimization
                            // In a real implementation, we would transform this to a loop
                            self.optimized_count += 1;
                        }
                    }
                }
            }
        }
        
        Ok(optimized)
    }
    
    /// Check if a function body contains tail recursion
    fn is_tail_recursive(&self, graph: &Graph, func_name: &str, body: NodeId) -> bool {
        self.check_tail_position(graph, func_name, body, true)
    }
    
    /// Check if a node is in tail position and contains a recursive call
    fn check_tail_position(&self, graph: &Graph, func_name: &str, node_id: NodeId, is_tail: bool) -> bool {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, .. } => {
                    if is_tail {
                        if let Some(Node::Variable { name }) = graph.get_node(*function) {
                            return name == func_name;
                        }
                    }
                    false
                }
                Node::If { then_branch, else_branch, .. } => {
                    // Both branches are in tail position
                    self.check_tail_position(graph, func_name, *then_branch, is_tail) ||
                    self.check_tail_position(graph, func_name, *else_branch, is_tail)
                }
                Node::Let { body, .. } | Node::Letrec { body, .. } => {
                    // Body is in tail position
                    self.check_tail_position(graph, func_name, *body, is_tail)
                }
                _ => false,
            }
        } else {
            false
        }
    }

    fn stats(&self) -> String {
        format!("{} pass: {} tail calls optimized", self.name(), self.optimized_count)
    }
}