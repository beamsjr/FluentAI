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
                Node::Let { bindings, body } => {
                    // First mark the body as reachable
                    self.mark_reachable(graph, *body, reachable);
                    
                    // Then only mark bindings that are used in the reachable set
                    let used_vars = self.find_used_variables(graph, reachable);
                    for (name, value_id) in bindings {
                        // Always mark bindings with side effects
                        if used_vars.contains(name) || self.has_side_effects(graph, *value_id) {
                            self.mark_reachable(graph, *value_id, reachable);
                        }
                    }
                }
                Node::Letrec { bindings, body } => {
                    // For letrec, all bindings are potentially mutually recursive
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
                Node::Handler { handlers, body } => {
                    self.mark_reachable(graph, *body, reachable);
                    for (_, _, handler_fn) in handlers {
                        self.mark_reachable(graph, *handler_fn, reachable);
                    }
                }
                _ => {}
            }
        }
    }
    
    /// Find variables used in the reachable nodes
    fn find_used_variables(&self, graph: &Graph, reachable: &FxHashSet<NodeId>) -> FxHashSet<String> {
        let mut used = FxHashSet::default();
        
        for node_id in reachable {
            if let Some(node) = graph.get_node(*node_id) {
                self.collect_used_vars(node, &mut used);
            }
        }
        
        used
    }
    
    /// Collect variable names used in a node
    fn collect_used_vars(&self, node: &Node, used: &mut FxHashSet<String>) {
        match node {
            Node::Variable { name } => {
                used.insert(name.clone());
            }
            _ => {}
        }
    }
    
    /// Check if a node has side effects
    fn has_side_effects(&self, graph: &Graph, node_id: NodeId) -> bool {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                // Effect nodes always have side effects
                Node::Effect { .. } => true,
                // Spawn/Await have implicit effects
                Node::Spawn { .. } | Node::Await { .. } => true,
                // Send/Receive have side effects
                Node::Send { .. } | Node::Receive { .. } => true,
                // Applications might have side effects if they call effectful functions
                Node::Application { function, args } => {
                    // Check if it's a known effectful function
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        // Known effectful functions
                        if matches!(name.as_str(), "print" | "println" | "error" | "panic" | "debug" | "log") {
                            return true;
                        }
                    }
                    // Recursively check if any arguments have side effects
                    args.iter().any(|arg| self.has_side_effects(graph, *arg))
                }
                // Let/Letrec might have side effects in their bindings
                Node::Let { bindings, body } => {
                    bindings.iter().any(|(_, value_id)| self.has_side_effects(graph, *value_id)) ||
                    self.has_side_effects(graph, *body)
                }
                Node::Letrec { bindings, body } => {
                    bindings.iter().any(|(_, value_id)| self.has_side_effects(graph, *value_id)) ||
                    self.has_side_effects(graph, *body)
                }
                // If/Match might have side effects in their branches
                Node::If { condition, then_branch, else_branch } => {
                    self.has_side_effects(graph, *condition) ||
                    self.has_side_effects(graph, *then_branch) ||
                    self.has_side_effects(graph, *else_branch)
                }
                Node::Match { expr, branches } => {
                    self.has_side_effects(graph, *expr) ||
                    branches.iter().any(|(_, branch)| self.has_side_effects(graph, *branch))
                }
                // Lists might have side effects in their elements
                Node::List(items) => {
                    items.iter().any(|item| self.has_side_effects(graph, *item))
                }
                // Async might have side effects
                Node::Async { body } => self.has_side_effects(graph, *body),
                // Handler nodes might have side effects in their body
                Node::Handler { handlers, body } => {
                    // Check if the body has side effects
                    if self.has_side_effects(graph, *body) {
                        return true;
                    }
                    // Check if any handler function has side effects
                    handlers.iter().any(|(_, _, handler_fn)| self.has_side_effects(graph, *handler_fn))
                }
                // Lambda bodies are not evaluated until called
                Node::Lambda { .. } => false,
                // Pure nodes
                Node::Literal(_) | Node::Variable { .. } => false,
                // Module-related nodes - check their contents
                Node::Module { body, .. } => self.has_side_effects(graph, *body),
                Node::Import { .. } => false, // Imports themselves don't have side effects
                Node::Export { .. } => false, // Exports themselves don't have side effects
                Node::QualifiedVariable { .. } => false, // Just a reference
                Node::Channel => false, // Channel creation is considered pure
                Node::Contract { .. } => false, // Contracts themselves don't have side effects
                Node::Define { value, .. } => self.has_side_effects(graph, *value), // Define has side effects if its value does
                Node::Begin { exprs } => {
                    // Begin has side effects if any of its expressions do
                    exprs.iter().any(|expr| self.has_side_effects(graph, *expr))
                }
            }
        } else {
            false
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