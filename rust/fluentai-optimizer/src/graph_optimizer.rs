//! Basic graph-based optimizations

use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::{FxHashMap, FxHashSet};
use crate::stats::OptimizationStats;
use crate::analysis::EffectAnalysis;
use anyhow::{Result, anyhow};
use std::time::Instant;

/// Basic graph optimizer with fundamental optimizations
pub struct GraphOptimizer {
    stats: OptimizationStats,
    effect_analysis: Option<EffectAnalysis>,
}

impl GraphOptimizer {
    /// Create a new graph optimizer
    pub fn new() -> Self {
        Self {
            stats: OptimizationStats::new(),
            effect_analysis: None,
        }
    }

    /// Optimize a graph with multiple passes
    pub fn optimize(&mut self, graph: &Graph) -> Result<Graph> {
        let start = Instant::now();
        self.stats = OptimizationStats::new();
        self.stats.nodes_before = graph.nodes.len();

        // Perform effect analysis
        self.effect_analysis = Some(EffectAnalysis::analyze(graph));

        // Apply optimization passes
        let mut optimized = graph.clone();

        // Pass 1: Constant folding
        optimized = self.constant_folding_pass(&optimized)?;

        // Pass 2: Dead code elimination
        optimized = self.dead_code_elimination_pass(&optimized)?;

        // Pass 3: Pure expression evaluation
        optimized = self.pure_evaluation_pass(&optimized)?;

        // Pass 4: Common subexpression elimination
        optimized = self.common_subexpression_elimination(&optimized)?;

        self.stats.nodes_after = optimized.nodes.len();
        self.stats.optimization_time_us = start.elapsed().as_micros() as u64;

        Ok(optimized)
    }

    /// Get optimization statistics
    pub fn stats(&self) -> &OptimizationStats {
        &self.stats
    }

    /// Constant folding pass
    fn constant_folding_pass(&mut self, graph: &Graph) -> Result<Graph> {
        let mut current = graph.clone();
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 10;
        
        // Iterate until no more folding is possible
        loop {
            let mut optimized = Graph::new();
            let mut node_mapping = FxHashMap::default();
            let mut folded_count = 0;

            // Process nodes in deterministic order
            let mut nodes: Vec<_> = current.nodes.keys().copied().collect();
            nodes.sort_by_key(|n| n.0);
            
            for node_id in nodes {
                if let Some(node) = current.get_node(node_id) {
                    if let Some(folded) = self.try_fold_node_in_optimized(&current, &optimized, &node_mapping, node_id, node) {
                        let new_id = optimized.add_node(folded)?;
                        node_mapping.insert(node_id, new_id);
                        folded_count += 1;
                    } else {
                        let new_node = self.copy_with_mapping(node, &node_mapping);
                        let new_id = optimized.add_node(new_node)?;
                        node_mapping.insert(node_id, new_id);
                    }
                }
            }

            // Update root
            if let Some(root) = current.root_id {
                optimized.root_id = node_mapping.get(&root).copied();
            }

            self.stats.constant_folded += folded_count;
            
            // Check if we made progress
            if folded_count == 0 || iterations >= MAX_ITERATIONS {
                return Ok(optimized);
            }
            
            current = optimized;
            iterations += 1;
        }
    }

    /// Try to fold a node into a constant (with access to optimized graph)
    fn try_fold_node_in_optimized(&mut self, current: &Graph, optimized: &Graph, 
                                  mapping: &FxHashMap<NodeId, NodeId>, 
                                  _node_id: NodeId, node: &Node) -> Option<Node> {
        match node {
            Node::Application { function, args } => {
                // Check if it's a foldable function
                let func_name = if let Some(Node::Variable { name }) = current.get_node(*function) {
                    name.clone()
                } else {
                    return None;
                };
                
                // Only try to fold pure primitives
                if !is_pure_primitive(&func_name) {
                    return None;
                }
                
                // Try to get literal values from current graph or optimized graph
                let mut arg_values = Vec::new();
                for arg_id in args {
                    // Look up the argument in the optimized graph if mapped, otherwise in current
                    let arg_node = if let Some(new_id) = mapping.get(arg_id) {
                        optimized.get_node(*new_id)
                    } else {
                        current.get_node(*arg_id)
                    };
                    
                    match arg_node {
                        Some(Node::Literal(lit)) => arg_values.push(lit.clone()),
                        _ => return None, // Can't fold if any arg is not a literal
                    }
                }
                
                if arg_values.len() == args.len() {
                    return evaluate_primitive(&func_name, &arg_values);
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                // Check if condition is constant
                let cond_value = if let Some(new_id) = mapping.get(condition) {
                    optimized.get_node(*new_id)
                } else {
                    current.get_node(*condition)
                };
                
                if let Some(Node::Literal(Literal::Boolean(value))) = cond_value {
                    // Return the appropriate branch
                    self.stats.branches_eliminated += 1;
                    let branch_id = if *value { then_branch } else { else_branch };
                    return current.get_node(*branch_id).cloned();
                }
            }
            _ => {}
        }
        None
    }


    /// Try to fold a primitive function application
    fn try_fold_primitive(&self, graph: &Graph, mapping: &FxHashMap<NodeId, NodeId>,
                         func_name: &str, args: &[NodeId]) -> Option<Node> {
        // Only fold pure functions
        if !is_pure_primitive(func_name) {
            return None;
        }

        // Get argument values - need to check both original and mapped nodes
        let mut arg_values = Vec::new();
        for arg_id in args {
            // First try the mapped node
            let node = if let Some(&mapped_id) = mapping.get(arg_id) {
                graph.get_node(mapped_id)
            } else {
                graph.get_node(*arg_id)
            };
            
            match node {
                Some(Node::Literal(lit)) => arg_values.push(lit.clone()),
                _ => return None,
            }
        }

        // Evaluate the primitive
        evaluate_primitive(func_name, &arg_values)
    }

    /// Dead code elimination pass
    fn dead_code_elimination_pass(&mut self, graph: &Graph) -> Result<Graph> {
        let mut reachable = FxHashSet::default();

        // Mark all reachable nodes from root
        if let Some(root) = graph.root_id {
            self.mark_reachable(graph, root, &mut reachable);
        }
        

        // Build new graph with only reachable nodes
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();

        // Process nodes in two passes to handle forward references
        // First pass: Create all nodes without updating references
        let mut temp_nodes = Vec::new();
        for node_id in &reachable {
            if let Some(node) = graph.get_node(*node_id) {
                let new_id = optimized.add_node(node.clone())?;
                node_mapping.insert(*node_id, new_id);
                temp_nodes.push((*node_id, new_id));
            }
        }

        // Second pass: Update all references now that all nodes are mapped
        for (old_id, new_id) in temp_nodes {
            if let Some(node) = graph.get_node(old_id) {
                let updated_node = self.copy_with_mapping_safe(node, &node_mapping, &reachable)?;
                optimized.nodes.insert(new_id, updated_node);
            }
        }

        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }

        self.stats.dead_code_eliminated = graph.nodes.len() - optimized.nodes.len();

        Ok(optimized)
    }

    /// Check if a node or its children have any effects
    fn has_effects(&self, graph: &Graph, node_id: NodeId) -> bool {
        // Use the effect analysis if available
        if let Some(ref ea) = self.effect_analysis {
            !ea.pure_nodes.contains(&node_id)
        } else {
            // Fallback: check for effect nodes directly
            self.contains_effect_node(graph, node_id)
        }
    }
    
    /// Check if a subtree contains any effect nodes
    fn contains_effect_node(&self, graph: &Graph, node_id: NodeId) -> bool {
        let mut visited = FxHashSet::default();
        self.check_for_effects(graph, node_id, &mut visited)
    }
    
    /// Recursively check for effect nodes
    fn check_for_effects(&self, graph: &Graph, node_id: NodeId, visited: &mut FxHashSet<NodeId>) -> bool {
        if !visited.insert(node_id) {
            return false;
        }
        
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Effect { .. } => return true,
                Node::Application { function, args } => {
                    if self.check_for_effects(graph, *function, visited) {
                        return true;
                    }
                    for arg in args {
                        if self.check_for_effects(graph, *arg, visited) {
                            return true;
                        }
                    }
                }
                Node::Lambda { body, .. } => {
                    return self.check_for_effects(graph, *body, visited);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value_id) in bindings {
                        if self.check_for_effects(graph, *value_id, visited) {
                            return true;
                        }
                    }
                    return self.check_for_effects(graph, *body, visited);
                }
                Node::If { condition, then_branch, else_branch } => {
                    return self.check_for_effects(graph, *condition, visited) ||
                           self.check_for_effects(graph, *then_branch, visited) ||
                           self.check_for_effects(graph, *else_branch, visited);
                }
                Node::List(items) => {
                    for item in items {
                        if self.check_for_effects(graph, *item, visited) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Find all variables used in an expression
    fn find_used_variables(&self, graph: &Graph, node_id: NodeId) -> FxHashSet<String> {
        let mut used = FxHashSet::default();
        let mut visited = FxHashSet::default();
        self.collect_used_variables(graph, node_id, &mut used, &mut visited);
        used
    }
    
    /// Recursively collect used variables
    fn collect_used_variables(&self, graph: &Graph, node_id: NodeId, used: &mut FxHashSet<String>, visited: &mut FxHashSet<NodeId>) {
        if !visited.insert(node_id) {
            return; // Already visited
        }
        
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => {
                    used.insert(name.clone());
                }
                Node::Application { function, args } => {
                    self.collect_used_variables(graph, *function, used, visited);
                    for arg in args {
                        self.collect_used_variables(graph, *arg, used, visited);
                    }
                }
                Node::Lambda { body, .. } => {
                    self.collect_used_variables(graph, *body, used, visited);
                }
                Node::Let { bindings, body } => {
                    // First collect variables from the body
                    self.collect_used_variables(graph, *body, used, visited);
                    
                    // Then check binding values for variables (they might reference outer scope)
                    for (_, value_id) in bindings {
                        self.collect_used_variables(graph, *value_id, used, visited);
                    }
                }
                Node::Letrec { bindings, body } => {
                    self.collect_used_variables(graph, *body, used, visited);
                    for (_, value_id) in bindings {
                        self.collect_used_variables(graph, *value_id, used, visited);
                    }
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.collect_used_variables(graph, *condition, used, visited);
                    self.collect_used_variables(graph, *then_branch, used, visited);
                    self.collect_used_variables(graph, *else_branch, used, visited);
                }
                Node::List(items) => {
                    for item in items {
                        self.collect_used_variables(graph, *item, used, visited);
                    }
                }
                Node::Match { expr, branches } => {
                    self.collect_used_variables(graph, *expr, used, visited);
                    for (_, branch_body) in branches {
                        self.collect_used_variables(graph, *branch_body, used, visited);
                    }
                }
                Node::Effect { args, .. } => {
                    for arg in args {
                        self.collect_used_variables(graph, *arg, used, visited);
                    }
                }
                Node::Async { body } | Node::Await { expr: body } | Node::Spawn { expr: body } => {
                    self.collect_used_variables(graph, *body, used, visited);
                }
                Node::Send { channel, value } => {
                    self.collect_used_variables(graph, *channel, used, visited);
                    self.collect_used_variables(graph, *value, used, visited);
                }
                Node::Receive { channel } => {
                    self.collect_used_variables(graph, *channel, used, visited);
                }
                Node::Contract { preconditions, postconditions, invariants, .. } => {
                    for pre in preconditions {
                        self.collect_used_variables(graph, *pre, used, visited);
                    }
                    for post in postconditions {
                        self.collect_used_variables(graph, *post, used, visited);
                    }
                    for inv in invariants {
                        self.collect_used_variables(graph, *inv, used, visited);
                    }
                }
                _ => {}
            }
        }
    }

    /// Mark node and its dependencies as reachable
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
                    // For let bindings, only mark used bindings as reachable
                    self.mark_reachable(graph, *body, reachable);
                    
                    // Find which variables are used in the body
                    let used_vars = self.find_used_variables(graph, *body);
                    
                    // Mark bindings that are used OR have effects
                    for (name, value_id) in bindings {
                        // Always preserve bindings with effects (e.g., from 'do' expressions)
                        // The "_" binding is used by the parser for sequencing effects
                        if name == "_" || used_vars.contains(name) {
                            self.mark_reachable(graph, *value_id, reachable);
                        } else {
                            // Check if the binding has effects before discarding
                            if self.has_effects(graph, *value_id) {
                                self.mark_reachable(graph, *value_id, reachable);
                            }
                        }
                    }
                }
                Node::Letrec { bindings, body } => {
                    // For letrec, all bindings must be kept (mutual recursion)
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
                    for (_, branch_body) in branches {
                        self.mark_reachable(graph, *branch_body, reachable);
                    }
                }
                Node::Effect { args, .. } => {
                    // Effects must always be preserved
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
                Node::Contract { preconditions, postconditions, invariants, .. } => {
                    for pre in preconditions {
                        self.mark_reachable(graph, *pre, reachable);
                    }
                    for post in postconditions {
                        self.mark_reachable(graph, *post, reachable);
                    }
                    for inv in invariants {
                        self.mark_reachable(graph, *inv, reachable);
                    }
                }
                _ => {}
            }
        }
    }

    /// Pure expression evaluation pass
    fn pure_evaluation_pass(&mut self, graph: &Graph) -> Result<Graph> {
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        let value_cache = FxHashMap::default();

        // First pass: Create placeholder nodes to establish mappings
        let nodes: Vec<_> = graph.nodes.keys().copied().collect();
        for node_id in &nodes {
            if graph.get_node(*node_id).is_some() {
                let placeholder = Node::Literal(Literal::Nil);
                let new_id = optimized.add_node(placeholder)?;
                node_mapping.insert(*node_id, new_id);
            }
        }

        // Second pass: Process nodes with all mappings available
        for node_id in nodes {
            if let Some(node) = graph.get_node(node_id) {
                let new_node = if self.effect_analysis.as_ref()
                    .map_or(false, |ea| ea.pure_nodes.contains(&node_id)) {
                    if let Some(value) = self.evaluate_pure_node(graph, &node_mapping, &value_cache, node_id, node) {
                        self.stats.pure_expressions_evaluated += 1;
                        value
                    } else {
                        self.copy_with_mapping(node, &node_mapping)
                    }
                } else {
                    self.copy_with_mapping(node, &node_mapping)
                };
                
                // Update the node in the optimized graph
                if let Some(new_id) = node_mapping.get(&node_id) {
                    optimized.nodes.insert(*new_id, new_node);
                }
            }
        }

        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }

        Ok(optimized)
    }

    /// Evaluate a pure node
    fn evaluate_pure_node(&self, graph: &Graph, mapping: &FxHashMap<NodeId, NodeId>,
                         _cache: &FxHashMap<NodeId, Literal>, _node_id: NodeId, node: &Node) -> Option<Node> {
        match node {
            Node::Literal(_) => Some(node.clone()),
            Node::Application { function, args } => {
                // Get the function name from the original graph
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    self.try_fold_primitive(graph, mapping, name, args)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Common subexpression elimination
    fn common_subexpression_elimination(&mut self, graph: &Graph) -> Result<Graph> {
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        let mut expr_cache: FxHashMap<String, NodeId> = FxHashMap::default();

        // First pass: Create all nodes to establish mappings
        let nodes: Vec<_> = graph.nodes.keys().copied().collect();
        for node_id in &nodes {
            if graph.get_node(*node_id).is_some() {
                let placeholder = Node::Literal(Literal::Nil);
                let new_id = optimized.add_node(placeholder)?;
                node_mapping.insert(*node_id, new_id);
            }
        }

        // Second pass: Process with CSE
        for node_id in nodes {
            if let Some(node) = graph.get_node(node_id) {
                if self.effect_analysis.as_ref()
                    .map_or(false, |ea| ea.pure_nodes.contains(&node_id)) {
                    let expr_key = self.node_to_key(node);
                    
                    if let Some(existing_id) = expr_cache.get(&expr_key) {
                        // Reuse existing node - update mapping
                        if let Some(old_mapping) = node_mapping.get(&node_id) {
                            // Remove the placeholder node we created
                            optimized.nodes.remove(old_mapping);
                        }
                        node_mapping.insert(node_id, *existing_id);
                        self.stats.cse_eliminated += 1;
                    } else {
                        // Update the node with proper references
                        let new_node = self.copy_with_mapping(node, &node_mapping);
                        if let Some(new_id) = node_mapping.get(&node_id) {
                            optimized.nodes.insert(*new_id, new_node);
                            expr_cache.insert(expr_key, *new_id);
                        }
                    }
                } else {
                    // Non-pure nodes can't be eliminated
                    let new_node = self.copy_with_mapping(node, &node_mapping);
                    if let Some(new_id) = node_mapping.get(&node_id) {
                        optimized.nodes.insert(*new_id, new_node);
                    }
                }
            }
        }

        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }

        Ok(optimized)
    }

    /// Convert node to a key for CSE
    fn node_to_key(&self, node: &Node) -> String {
        match node {
            Node::Literal(lit) => format!("lit:{:?}", lit),
            Node::Variable { name } => format!("var:{}", name),
            Node::Application { function, args } => {
                format!("app:{}:{:?}", function.0, args)
            }
            _ => format!("node:{:?}", node),
        }
    }

    /// Copy a node with updated references (safe version that validates references)
    fn copy_with_mapping_safe(&self, node: &Node, mapping: &FxHashMap<NodeId, NodeId>, _reachable: &FxHashSet<NodeId>) -> Result<Node> {
        let map_node_id = |id: &NodeId| -> Result<NodeId> {
            mapping.get(id).copied().ok_or_else(|| {
                anyhow!("Invalid node reference: NodeId({}) is not in the reachable set", id.0)
            })
        };

        let node = match node {
            Node::Application { function, args } => {
                Node::Application {
                    function: map_node_id(function)?,
                    args: args.iter()
                        .map(map_node_id)
                        .collect::<Result<Vec<_>>>()?,
                }
            }
            Node::Lambda { params, body } => {
                Node::Lambda {
                    params: params.clone(),
                    body: map_node_id(body)?,
                }
            }
            Node::Let { bindings, body } => {
                // Only include bindings that are reachable
                let filtered_bindings = bindings.iter()
                    .filter(|(_, value_id)| mapping.contains_key(value_id))
                    .map(|(name, value_id)| {
                        Ok((name.clone(), map_node_id(value_id)?))
                    })
                    .collect::<Result<Vec<_>>>()?;
                    
                Node::Let {
                    bindings: filtered_bindings,
                    body: map_node_id(body)?,
                }
            }
            Node::Letrec { bindings, body } => {
                Node::Letrec {
                    bindings: bindings.iter()
                        .map(|(name, value_id)| {
                            Ok((name.clone(), map_node_id(value_id)?))
                        })
                        .collect::<Result<Vec<_>>>()?,
                    body: map_node_id(body)?,
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                Node::If {
                    condition: map_node_id(condition)?,
                    then_branch: map_node_id(then_branch)?,
                    else_branch: map_node_id(else_branch)?,
                }
            }
            Node::List(items) => {
                Node::List(
                    items.iter()
                        .map(map_node_id)
                        .collect::<Result<Vec<_>>>()?
                )
            }
            Node::Match { expr, branches } => {
                Node::Match {
                    expr: map_node_id(expr)?,
                    branches: branches.iter()
                        .map(|(pattern, body)| {
                            Ok((pattern.clone(), map_node_id(body)?))
                        })
                        .collect::<Result<Vec<_>>>()?,
                }
            }
            Node::Effect { effect_type, operation, args } => {
                Node::Effect {
                    effect_type: *effect_type,
                    operation: operation.clone(),
                    args: args.iter()
                        .map(map_node_id)
                        .collect::<Result<Vec<_>>>()?,
                }
            }
            Node::Async { body } => {
                Node::Async {
                    body: map_node_id(body)?,
                }
            }
            Node::Await { expr } => {
                Node::Await {
                    expr: map_node_id(expr)?,
                }
            }
            Node::Spawn { expr } => {
                Node::Spawn {
                    expr: map_node_id(expr)?,
                }
            }
            Node::Send { channel, value } => {
                Node::Send {
                    channel: map_node_id(channel)?,
                    value: map_node_id(value)?,
                }
            }
            Node::Receive { channel } => {
                Node::Receive {
                    channel: map_node_id(channel)?,
                }
            }
            Node::Contract { function_name, preconditions, postconditions, invariants, complexity, pure } => {
                Node::Contract {
                    function_name: function_name.clone(),
                    preconditions: preconditions.iter()
                        .map(map_node_id)
                        .collect::<Result<Vec<_>>>()?,
                    postconditions: postconditions.iter()
                        .map(map_node_id)
                        .collect::<Result<Vec<_>>>()?,
                    invariants: invariants.iter()
                        .map(map_node_id)
                        .collect::<Result<Vec<_>>>()?,
                    complexity: complexity.clone(),
                    pure: *pure,
                }
            }
            _ => node.clone(),
        };
        
        Ok(node)
    }

    /// Copy a node with updated references
    fn copy_with_mapping(&self, node: &Node, mapping: &FxHashMap<NodeId, NodeId>) -> Node {
        match node {
            Node::Application { function, args } => {
                Node::Application {
                    function: mapping.get(function).copied().unwrap_or(*function),
                    args: args.iter()
                        .map(|id| mapping.get(id).copied().unwrap_or(*id))
                        .collect(),
                }
            }
            Node::Lambda { params, body } => {
                Node::Lambda {
                    params: params.clone(),
                    body: mapping.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Let { bindings, body } => {
                Node::Let {
                    bindings: bindings.iter()
                        .map(|(name, value_id)| {
                            (name.clone(), mapping.get(value_id).copied().unwrap_or(*value_id))
                        })
                        .collect(),
                    body: mapping.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Letrec { bindings, body } => {
                Node::Letrec {
                    bindings: bindings.iter()
                        .map(|(name, value_id)| {
                            (name.clone(), mapping.get(value_id).copied().unwrap_or(*value_id))
                        })
                        .collect(),
                    body: mapping.get(body).copied().unwrap_or(*body),
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                Node::If {
                    condition: mapping.get(condition).copied().unwrap_or(*condition),
                    then_branch: mapping.get(then_branch).copied().unwrap_or(*then_branch),
                    else_branch: mapping.get(else_branch).copied().unwrap_or(*else_branch),
                }
            }
            Node::List(items) => {
                Node::List(
                    items.iter()
                        .map(|id| mapping.get(id).copied().unwrap_or(*id))
                        .collect()
                )
            }
            Node::Match { expr, branches } => {
                Node::Match {
                    expr: mapping.get(expr).copied().unwrap_or(*expr),
                    branches: branches.iter()
                        .map(|(pattern, body)| {
                            (pattern.clone(), mapping.get(body).copied().unwrap_or(*body))
                        })
                        .collect(),
                }
            }
            Node::Effect { effect_type, operation, args } => {
                Node::Effect {
                    effect_type: *effect_type,
                    operation: operation.clone(),
                    args: args.iter()
                        .map(|id| mapping.get(id).copied().unwrap_or(*id))
                        .collect(),
                }
            }
            Node::Async { body } => {
                Node::Async {
                    body: mapping.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Await { expr } => {
                Node::Await {
                    expr: mapping.get(expr).copied().unwrap_or(*expr),
                }
            }
            Node::Spawn { expr } => {
                Node::Spawn {
                    expr: mapping.get(expr).copied().unwrap_or(*expr),
                }
            }
            Node::Send { channel, value } => {
                Node::Send {
                    channel: mapping.get(channel).copied().unwrap_or(*channel),
                    value: mapping.get(value).copied().unwrap_or(*value),
                }
            }
            Node::Receive { channel } => {
                Node::Receive {
                    channel: mapping.get(channel).copied().unwrap_or(*channel),
                }
            }
            Node::Contract { function_name, preconditions, postconditions, invariants, complexity, pure } => {
                Node::Contract {
                    function_name: function_name.clone(),
                    preconditions: preconditions.iter()
                        .map(|id| mapping.get(id).copied().unwrap_or(*id))
                        .collect(),
                    postconditions: postconditions.iter()
                        .map(|id| mapping.get(id).copied().unwrap_or(*id))
                        .collect(),
                    invariants: invariants.iter()
                        .map(|id| mapping.get(id).copied().unwrap_or(*id))
                        .collect(),
                    complexity: complexity.clone(),
                    pure: *pure,
                }
            }
            _ => node.clone(),
        }
    }
}

impl Default for GraphOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if a function is a pure primitive
fn is_pure_primitive(name: &str) -> bool {
    matches!(name,
        "+" | "-" | "*" | "/" | "mod" |
        "<" | ">" | "<=" | ">=" | "=" | "==" | "!=" | "<>" |
        "and" | "or" | "not" |
        "car" | "cdr" | "cons" | "list" | "head" | "tail" | "first" | "rest" |
        "list-len" | "list-empty?" | "length" | "empty?" |
        "str-len" | "str-concat" | "str-upper" | "str-lower" |
        "string-length" | "string-append" | "string-upcase" | "string-downcase"
    )
}

/// Evaluate a primitive function with literal arguments
fn evaluate_primitive(func_name: &str, args: &[Literal]) -> Option<Node> {
    use Literal::*;

    let result = match (func_name, args) {
        // Arithmetic
        ("+", [Integer(a), Integer(b)]) => Integer(a + b),
        ("-", [Integer(a), Integer(b)]) => Integer(a - b),
        ("*", [Integer(a), Integer(b)]) => Integer(a * b),
        ("/", [Integer(a), Integer(b)]) if *b != 0 => Integer(a / b),
        ("mod", [Integer(a), Integer(b)]) if *b != 0 => Integer(a % b),
        
        // Floating point
        ("+", [Float(a), Float(b)]) => Float(a + b),
        ("-", [Float(a), Float(b)]) => Float(a - b),
        ("*", [Float(a), Float(b)]) => Float(a * b),
        ("/", [Float(a), Float(b)]) if *b != 0.0 => Float(a / b),
        
        // Comparison
        ("<", [Integer(a), Integer(b)]) => Boolean(a < b),
        (">", [Integer(a), Integer(b)]) => Boolean(a > b),
        ("<=", [Integer(a), Integer(b)]) => Boolean(a <= b),
        (">=", [Integer(a), Integer(b)]) => Boolean(a >= b),
        ("=" | "==", [Integer(a), Integer(b)]) => Boolean(a == b),
        ("!=" | "<>", [Integer(a), Integer(b)]) => Boolean(a != b),
        
        // Boolean operations
        ("and", [Boolean(a), Boolean(b)]) => Boolean(*a && *b),
        ("or", [Boolean(a), Boolean(b)]) => Boolean(*a || *b),
        ("not", [Boolean(a)]) => Boolean(!a),
        
        // String operations
        ("str-concat" | "string-append", [String(a), String(b)]) => String(format!("{}{}", a, b)),
        ("str-len" | "string-length", [String(s)]) => Integer(s.len() as i64),
        ("str-upper" | "string-upcase", [String(s)]) => String(s.to_uppercase()),
        ("str-lower" | "string-downcase", [String(s)]) => String(s.to_lowercase()),
        
        _ => return None,
    };

    Some(Node::Literal(result))
}

#[cfg(test)]
#[path = "optimizer_tests.rs"]
mod optimizer_tests;