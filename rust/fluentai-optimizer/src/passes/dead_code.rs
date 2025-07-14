//! Dead code elimination pass

use crate::analysis::EffectAnalysis;
use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashSet;

/// Dead code elimination pass
pub struct DeadCodeEliminationPass {
    eliminated_count: usize,
    effect_analysis: Option<EffectAnalysis>,
}

impl DeadCodeEliminationPass {
    /// Create new dead code elimination pass
    pub fn new() -> Self {
        Self {
            eliminated_count: 0,
            effect_analysis: None,
        }
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
                        if used_vars.contains(name) || self.has_side_effects(*value_id) {
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
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
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
                Node::Assignment { target, value } => {
                    self.mark_reachable(graph, *target, reachable);
                    self.mark_reachable(graph, *value, reachable);
                }
                Node::Begin { exprs } => {
                    for expr in exprs {
                        self.mark_reachable(graph, *expr, reachable);
                    }
                }
                Node::Module { body, .. } => {
                    self.mark_reachable(graph, *body, reachable);
                }
                Node::Define { value, .. } => {
                    self.mark_reachable(graph, *value, reachable);
                }
                Node::Import { .. } | Node::Export { .. } | Node::QualifiedVariable { .. } => {
                    // These nodes have no child nodes to traverse
                }
                Node::Channel { capacity } => {
                    if let Some(cap_expr) = capacity {
                        self.mark_reachable(graph, *cap_expr, reachable);
                    }
                }
                Node::TrySend { channel, value } => {
                    self.mark_reachable(graph, *channel, reachable);
                    self.mark_reachable(graph, *value, reachable);
                }
                Node::TryReceive { channel } => {
                    self.mark_reachable(graph, *channel, reachable);
                }
                Node::Select { branches, default } => {
                    for (channel_op, handler) in branches {
                        self.mark_reachable(graph, *channel_op, reachable);
                        self.mark_reachable(graph, *handler, reachable);
                    }
                    if let Some(default_handler) = default {
                        self.mark_reachable(graph, *default_handler, reachable);
                    }
                }
                Node::Actor { initial_state, handler } => {
                    self.mark_reachable(graph, *initial_state, reachable);
                    self.mark_reachable(graph, *handler, reachable);
                }
                Node::ActorSend { actor, message } => {
                    self.mark_reachable(graph, *actor, reachable);
                    self.mark_reachable(graph, *message, reachable);
                }
                Node::ActorReceive { patterns, timeout } => {
                    for (_, handler) in patterns {
                        self.mark_reachable(graph, *handler, reachable);
                    }
                    if let Some((duration, timeout_handler)) = timeout {
                        self.mark_reachable(graph, *duration, reachable);
                        self.mark_reachable(graph, *timeout_handler, reachable);
                    }
                }
                Node::Become { new_state } => {
                    self.mark_reachable(graph, *new_state, reachable);
                }
                Node::Try { body, catch_branches, finally } => {
                    self.mark_reachable(graph, *body, reachable);
                    for (_, handler) in catch_branches {
                        self.mark_reachable(graph, *handler, reachable);
                    }
                    if let Some(finally_block) = finally {
                        self.mark_reachable(graph, *finally_block, reachable);
                    }
                }
                Node::Throw { error } => {
                    self.mark_reachable(graph, *error, reachable);
                }
                Node::Promise { body } => {
                    self.mark_reachable(graph, *body, reachable);
                }
                Node::PromiseAll { promises } | Node::PromiseRace { promises } => {
                    for promise in promises {
                        self.mark_reachable(graph, *promise, reachable);
                    }
                }
                Node::Timeout { duration, promise, default } => {
                    self.mark_reachable(graph, *duration, reachable);
                    self.mark_reachable(graph, *promise, reachable);
                    if let Some(default_value) = default {
                        self.mark_reachable(graph, *default_value, reachable);
                    }
                }
                Node::Contract { .. } => {
                    // Contract nodes have no child nodes to traverse
                }
                // Continuum UI nodes - these will be compiled away
                Node::Surface { children, .. } | Node::Space { children, .. } => {
                    for child in children {
                        self.mark_reachable(graph, *child, reachable);
                    }
                }
                Node::Element { handlers, conditionals, .. } => {
                    for (_, handler) in handlers {
                        self.mark_reachable(graph, *handler, reachable);
                    }
                    for conditional in conditionals {
                        self.mark_reachable(graph, *conditional, reachable);
                    }
                }
                Node::StateField { initial, .. } => {
                    if let Some(init) = initial {
                        self.mark_reachable(graph, *init, reachable);
                    }
                }
                Node::When { condition, .. } => {
                    self.mark_reachable(graph, *condition, reachable);
                }
                Node::Disturb { value, .. } => {
                    if let Some(val) = value {
                        self.mark_reachable(graph, *val, reachable);
                    }
                }
                // Leaf nodes - no children to traverse
                Node::Literal(_) | Node::Variable { .. } => {}
            }
        }
    }

    /// Find variables used in the reachable nodes
    fn find_used_variables(
        &self,
        graph: &Graph,
        reachable: &FxHashSet<NodeId>,
    ) -> FxHashSet<String> {
        let mut used = FxHashSet::default();

        // We need to traverse the graph to collect variable names
        for node_id in reachable {
            self.collect_vars_from_node(graph, *node_id, &mut used);
        }

        used
    }

    /// Recursively collect variable names from a node and its children
    fn collect_vars_from_node(
        &self,
        graph: &Graph,
        node_id: NodeId,
        used: &mut FxHashSet<String>,
    ) {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => {
                    used.insert(name.clone());
                }
                Node::Application { function, args } => {
                    self.collect_vars_from_node(graph, *function, used);
                    for arg in args {
                        self.collect_vars_from_node(graph, *arg, used);
                    }
                }
                Node::Lambda { body, .. } => {
                    self.collect_vars_from_node(graph, *body, used);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    // Note: We collect variables from binding values too,
                    // as they might reference other variables
                    for (_, value) in bindings {
                        self.collect_vars_from_node(graph, *value, used);
                    }
                    self.collect_vars_from_node(graph, *body, used);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.collect_vars_from_node(graph, *condition, used);
                    self.collect_vars_from_node(graph, *then_branch, used);
                    self.collect_vars_from_node(graph, *else_branch, used);
                }
                Node::Match { expr, branches } => {
                    self.collect_vars_from_node(graph, *expr, used);
                    for (_, branch) in branches {
                        self.collect_vars_from_node(graph, *branch, used);
                    }
                }
                Node::List(items) => {
                    for item in items {
                        self.collect_vars_from_node(graph, *item, used);
                    }
                }
                Node::Effect { args, .. } => {
                    for arg in args {
                        self.collect_vars_from_node(graph, *arg, used);
                    }
                }
                Node::Handler { handlers, body } => {
                    self.collect_vars_from_node(graph, *body, used);
                    for (_, _, handler) in handlers {
                        self.collect_vars_from_node(graph, *handler, used);
                    }
                }
                Node::Assignment { target, value } => {
                    self.collect_vars_from_node(graph, *target, used);
                    self.collect_vars_from_node(graph, *value, used);
                }
                Node::Begin { exprs } => {
                    for expr in exprs {
                        self.collect_vars_from_node(graph, *expr, used);
                    }
                }
                Node::Async { body } | Node::Await { expr: body } | Node::Spawn { expr: body } => {
                    self.collect_vars_from_node(graph, *body, used);
                }
                Node::Send { channel, value } => {
                    self.collect_vars_from_node(graph, *channel, used);
                    self.collect_vars_from_node(graph, *value, used);
                }
                Node::Receive { channel } => {
                    self.collect_vars_from_node(graph, *channel, used);
                }
                Node::Module { body, .. } => {
                    self.collect_vars_from_node(graph, *body, used);
                }
                Node::Define { value, .. } => {
                    self.collect_vars_from_node(graph, *value, used);
                }
                Node::Channel { capacity } => {
                    if let Some(cap_expr) = capacity {
                        self.collect_vars_from_node(graph, *cap_expr, used);
                    }
                }
                Node::TrySend { channel, value } => {
                    self.collect_vars_from_node(graph, *channel, used);
                    self.collect_vars_from_node(graph, *value, used);
                }
                Node::TryReceive { channel } => {
                    self.collect_vars_from_node(graph, *channel, used);
                }
                Node::Select { branches, default } => {
                    for (channel_op, handler) in branches {
                        self.collect_vars_from_node(graph, *channel_op, used);
                        self.collect_vars_from_node(graph, *handler, used);
                    }
                    if let Some(default_handler) = default {
                        self.collect_vars_from_node(graph, *default_handler, used);
                    }
                }
                Node::Actor { initial_state, handler } => {
                    self.collect_vars_from_node(graph, *initial_state, used);
                    self.collect_vars_from_node(graph, *handler, used);
                }
                Node::ActorSend { actor, message } => {
                    self.collect_vars_from_node(graph, *actor, used);
                    self.collect_vars_from_node(graph, *message, used);
                }
                Node::ActorReceive { patterns, timeout } => {
                    for (_, handler) in patterns {
                        self.collect_vars_from_node(graph, *handler, used);
                    }
                    if let Some((duration, timeout_handler)) = timeout {
                        self.collect_vars_from_node(graph, *duration, used);
                        self.collect_vars_from_node(graph, *timeout_handler, used);
                    }
                }
                Node::Become { new_state } => {
                    self.collect_vars_from_node(graph, *new_state, used);
                }
                Node::Try { body, catch_branches, finally } => {
                    self.collect_vars_from_node(graph, *body, used);
                    for (_, handler) in catch_branches {
                        self.collect_vars_from_node(graph, *handler, used);
                    }
                    if let Some(finally_block) = finally {
                        self.collect_vars_from_node(graph, *finally_block, used);
                    }
                }
                Node::Throw { error } => {
                    self.collect_vars_from_node(graph, *error, used);
                }
                Node::Promise { body } => {
                    self.collect_vars_from_node(graph, *body, used);
                }
                Node::PromiseAll { promises } | Node::PromiseRace { promises } => {
                    for promise in promises {
                        self.collect_vars_from_node(graph, *promise, used);
                    }
                }
                Node::Timeout { duration, promise, default } => {
                    self.collect_vars_from_node(graph, *duration, used);
                    self.collect_vars_from_node(graph, *promise, used);
                    if let Some(default_value) = default {
                        self.collect_vars_from_node(graph, *default_value, used);
                    }
                }
                Node::QualifiedVariable { variable_name, .. } => {
                    // For qualified variables, we track the variable name
                    used.insert(variable_name.clone());
                }
                // Continuum UI nodes - these will be compiled away
                Node::Surface { children, .. } | Node::Space { children, .. } => {
                    for child in children {
                        self.collect_vars_from_node(graph, *child, used);
                    }
                }
                Node::Element { handlers, conditionals, .. } => {
                    for (_, handler) in handlers {
                        self.collect_vars_from_node(graph, *handler, used);
                    }
                    for conditional in conditionals {
                        self.collect_vars_from_node(graph, *conditional, used);
                    }
                }
                Node::StateField { initial, .. } => {
                    if let Some(init) = initial {
                        self.collect_vars_from_node(graph, *init, used);
                    }
                }
                Node::When { condition, .. } => {
                    self.collect_vars_from_node(graph, *condition, used);
                }
                Node::Disturb { value, .. } => {
                    if let Some(val) = value {
                        self.collect_vars_from_node(graph, *val, used);
                    }
                }
                // Leaf nodes - no variables to collect
                Node::Literal(_) | Node::Import { .. } | Node::Export { .. } | Node::Contract { .. } => {}
            }
        }
    }

    /// Collect variable names used in a node
    fn collect_used_vars(&self, node: &Node, used: &mut FxHashSet<String>) {
        match node {
            Node::Variable { name } => {
                used.insert(name.clone());
            }
            Node::Application { .. } => {
                // Application nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Lambda { .. } => {
                // Lambda nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Let { .. } | Node::Letrec { .. } => {
                // Let/Letrec nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::If { .. } => {
                // If nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Match { .. } => {
                // Match nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::List(_) => {
                // List nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Effect { .. } => {
                // Effect nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Handler { .. } => {
                // Handler nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Assignment { .. } => {
                // Assignment nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Begin { .. } => {
                // Begin nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Async { .. } | Node::Await { .. } | Node::Spawn { .. } => {
                // Async nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Send { .. } | Node::Receive { .. } => {
                // Channel operation nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Module { .. } => {
                // Module nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Define { .. } => {
                // Define nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Channel { .. } => {
                // Channel nodes may contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::TrySend { .. } | Node::TryReceive { .. } => {
                // Try channel operations contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Select { .. } => {
                // Select nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Actor { .. } | Node::ActorSend { .. } | Node::ActorReceive { .. } => {
                // Actor nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Become { .. } => {
                // Become nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Try { .. } | Node::Throw { .. } => {
                // Exception handling nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Promise { .. } | Node::PromiseAll { .. } | Node::PromiseRace { .. } => {
                // Promise nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::Timeout { .. } => {
                // Timeout nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::QualifiedVariable { .. } => {
                // QualifiedVariable has module and name, we should collect the name
                // But we need to handle it properly based on the actual structure
            }
            // Continuum UI nodes - these will be compiled away
            Node::Surface { .. } | Node::Space { .. } | Node::Element { .. } => {
                // Continuum nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            Node::StateField { .. } | Node::When { .. } | Node::Disturb { .. } => {
                // Continuum nodes contain NodeIds, not inline nodes
                // This will be handled by the graph traversal in find_used_variables
            }
            // Leaf nodes - these don't contain variable references
            Node::Literal(_) | Node::Import { .. } | Node::Export { .. } | Node::Contract { .. } => {
                // These nodes don't contain variable references we need to track
            }
        }
    }

    /// Check if a node has side effects
    fn has_side_effects(&self, node_id: NodeId) -> bool {
        // Use the effect analysis if available
        if let Some(ref effect_analysis) = self.effect_analysis {
            // A node has side effects if it's not pure
            !effect_analysis.is_pure(node_id)
        } else {
            // Conservative: assume everything has side effects if we don't have analysis
            true
        }
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &str {
        "Dead Code Elimination"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.eliminated_count = 0;
        
        // Perform effect analysis
        self.effect_analysis = Some(EffectAnalysis::analyze(graph));
        
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
        format!(
            "{} pass: {} nodes eliminated",
            self.name(),
            self.eliminated_count
        )
    }
}
