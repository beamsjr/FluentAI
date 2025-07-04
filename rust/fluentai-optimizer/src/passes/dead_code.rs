//! Dead code elimination pass

use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashSet;
use anyhow::Result;
use crate::passes::OptimizationPass;

/// Dead code elimination pass
pub struct DeadCodeEliminationPass {
    eliminated_count: usize,
}

impl DeadCodeEliminationPass {
    /// Create new dead code elimination pass
    pub fn new() -> Self {
        Self { eliminated_count: 0 }
    }

    /// Mark node and dependencies as reachable
    fn mark_reachable(&self, graph: &Graph, node_id: NodeId, reachable: &mut FxHashSet<NodeId>) {
        if !reachable.insert(node_id) {
            return; // Already visited
        }

        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    self.mark_reachable(graph, *function, reachable);
                    for arg in args {
                        self.mark_reachable(graph, *arg, reachable);
                    }
                }
                Node::Lambda { body, .. } => {
                    self.mark_reachable(graph, *body, reachable);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value_id) in bindings {
                        self.mark_reachable(graph, *value_id, reachable);
                    }
                    self.mark_reachable(graph, *body, reachable);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.mark_reachable(graph, *condition, reachable);
                    self.mark_reachable(graph, *then_branch, reachable);
                    self.mark_reachable(graph, *else_branch, reachable);
                }
                Node::List(items) => {
                    for item in items {
                        self.mark_reachable(graph, *item, reachable);
                    }
                }
                Node::Match { expr, branches } => {
                    self.mark_reachable(graph, *expr, reachable);
                    for (_, body) in branches {
                        self.mark_reachable(graph, *body, reachable);
                    }
                }
                Node::Effect { args, .. } => {
                    for arg in args {
                        self.mark_reachable(graph, *arg, reachable);
                    }
                }
                Node::Async { body } | Node::Await { expr: body } | Node::Spawn { expr: body } => {
                    self.mark_reachable(graph, *body, reachable);
                }
                Node::Send { channel, value } => {
                    self.mark_reachable(graph, *channel, reachable);
                    self.mark_reachable(graph, *value, reachable);
                }
                Node::Receive { channel } => {
                    self.mark_reachable(graph, *channel, reachable);
                }
                _ => {}
            }
        }
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &str {
        "Dead Code Elimination"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.eliminated_count = 0;
        let mut reachable = FxHashSet::default();

        // Mark all reachable nodes from root
        if let Some(root) = graph.root_id {
            self.mark_reachable(graph, root, &mut reachable);
        }

        // Create new graph with only reachable nodes
        let mut optimized = Graph::new();
        
        for node_id in &reachable {
            if let Some(node) = graph.get_node(*node_id) {
                optimized.nodes.insert(*node_id, node.clone());
            }
        }

        optimized.root_id = graph.root_id;
        self.eliminated_count = graph.nodes.len() - optimized.nodes.len();

        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!("{} pass: {} nodes eliminated", self.name(), self.eliminated_count)
    }
}