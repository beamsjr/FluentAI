//! Effect-aware optimization pass

use fluentai_core::ast::{Graph, Node, NodeId, EffectType};
use rustc_hash::{FxHashMap, FxHashSet};
use anyhow::Result;
use crate::passes::OptimizationPass;
use crate::analysis::{EffectAnalysis, TypeAnalysis};

/// Effect-aware optimization pass
pub struct EffectAwarePass {
    pure_hoisted: usize,
    effects_reordered: usize,
    duplicates_removed: usize,
}

impl EffectAwarePass {
    /// Create new effect-aware optimization pass
    pub fn new() -> Self {
        Self {
            pure_hoisted: 0,
            effects_reordered: 0,
            duplicates_removed: 0,
        }
    }

    /// Check if two effects can be reordered
    fn can_reorder_effects(effect1: &EffectType, effect2: &EffectType) -> bool {
        use EffectType::*;
        
        match (effect1, effect2) {
            // Pure operations can always be reordered
            (Pure, _) | (_, Pure) => true,
            
            // IO operations generally cannot be reordered
            (IO, IO) => false,
            
            // State operations on different states can be reordered
            (State, State) => false, // Conservative: assume same state
            
            // Async operations can sometimes be reordered
            (Async, Async) => true,
            
            // Network operations might be reorderable depending on endpoints
            (Network, Network) => false, // Conservative
            
            // Different effect types might be reorderable
            _ => false, // Conservative default
        }
    }


    /// Create a canonical string representation of an expression
    fn canonicalize_expr(&self, node: &Node, mapping: &FxHashMap<NodeId, NodeId>) -> String {
        match node {
            Node::Literal(lit) => format!("lit:{:?}", lit),
            Node::Variable { name } => format!("var:{}", name),
            Node::Application { function, args } => {
                let func_id = mapping.get(function).copied().unwrap_or(*function);
                let arg_ids: Vec<_> = args.iter()
                    .map(|id| mapping.get(id).copied().unwrap_or(*id))
                    .collect();
                format!("app:{}:{:?}", func_id.0, arg_ids.iter().map(|id| id.0).collect::<Vec<_>>())
            }
            _ => format!("node:{:?}", node),
        }
    }
}

impl OptimizationPass for EffectAwarePass {
    fn name(&self) -> &str {
        "Effect-Aware Optimization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.pure_hoisted = 0;
        self.effects_reordered = 0;
        self.duplicates_removed = 0;
        
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        
        // Perform effect analysis
        let effect_analysis = EffectAnalysis::analyze(graph);
        
        // Single pass: Process all nodes, hoisting pure computations and removing duplicates
        let mut expr_cache: FxHashMap<String, NodeId> = FxHashMap::default();
        
        // Process nodes in topological order starting from root
        let mut visited = FxHashSet::default();
        let mut work_stack = Vec::new();
        
        if let Some(root) = graph.root_id {
            work_stack.push(root);
        }
        
        // First, collect all nodes in dependency order
        let mut ordered_nodes = Vec::new();
        while let Some(node_id) = work_stack.pop() {
            if !visited.insert(node_id) {
                continue;
            }
            
            ordered_nodes.push(node_id);
            
            if let Some(node) = graph.get_node(node_id) {
                // Add dependencies to work stack
                match node {
                    Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                        work_stack.push(*body);
                        for (_, value_id) in bindings.iter().rev() {
                            work_stack.push(*value_id);
                        }
                    }
                    Node::Application { function, args } => {
                        work_stack.push(*function);
                        for arg in args.iter().rev() {
                            work_stack.push(*arg);
                        }
                    }
                    Node::If { condition, then_branch, else_branch } => {
                        work_stack.push(*else_branch);
                        work_stack.push(*then_branch);
                        work_stack.push(*condition);
                    }
                    Node::Lambda { body, .. } => {
                        work_stack.push(*body);
                    }
                    Node::Effect { args, .. } => {
                        for arg in args.iter().rev() {
                            work_stack.push(*arg);
                        }
                    }
                    _ => {}
                }
            }
        }
        
        // Process nodes in reverse order (dependencies first)
        ordered_nodes.reverse();
        
        // Also add any nodes that weren't reachable from root
        for (node_id, _) in &graph.nodes {
            if !visited.contains(node_id) {
                ordered_nodes.push(*node_id);
            }
        }
        
        for node_id in ordered_nodes {
            let node = match graph.get_node(node_id) {
                Some(n) => n,
                None => continue,
            };
            match node {
                Node::Let { bindings, body } => {
                    // Check if we can hoist pure computations
                    let mut pure_bindings = Vec::new();
                    let mut effectful_bindings = Vec::new();
                    
                    for (name, value_id) in bindings {
                        if effect_analysis.pure_nodes.contains(value_id) {
                            pure_bindings.push((name.clone(), *value_id));
                        } else {
                            effectful_bindings.push((name.clone(), *value_id));
                        }
                    }
                    
                    // If we have both pure and effectful bindings, hoist the pure ones
                    if !pure_bindings.is_empty() && !effectful_bindings.is_empty() {
                        // Map the bindings through node_mapping
                        let mapped_effectful: Vec<_> = effectful_bindings.iter()
                            .map(|(name, id)| (name.clone(), node_mapping.get(id).copied().unwrap_or(*id)))
                            .collect();
                        let mapped_pure: Vec<_> = pure_bindings.iter()
                            .map(|(name, id)| (name.clone(), node_mapping.get(id).copied().unwrap_or(*id)))
                            .collect();
                        let mapped_body = node_mapping.get(body).copied().unwrap_or(*body);
                        
                        // Create inner let with effectful bindings
                        let inner = Node::Let {
                            bindings: mapped_effectful,
                            body: mapped_body,
                        };
                        let inner_id = optimized.add_node(inner);
                        
                        // Create outer let with pure bindings
                        let outer = Node::Let {
                            bindings: mapped_pure,
                            body: inner_id,
                        };
                        let new_id = optimized.add_node(outer);
                        node_mapping.insert(node_id, new_id);
                        self.pure_hoisted += 1;
                        continue;
                    }
                    
                    // Otherwise, just map the node normally
                    let mapped_node = map_node_refs(node, &node_mapping);
                    let new_id = optimized.add_node(mapped_node);
                    node_mapping.insert(node_id, new_id);
                }
                _ => {
                    // For pure nodes, check if we've seen this expression before
                    if effect_analysis.pure_nodes.contains(&node_id) {
                        let expr_key = self.canonicalize_expr(node, &node_mapping);
                        
                        if let Some(&existing_id) = expr_cache.get(&expr_key) {
                            // Reuse existing computation
                            node_mapping.insert(node_id, existing_id);
                            self.duplicates_removed += 1;
                        } else {
                            // New expression, add to cache
                            let mapped_node = map_node_refs(node, &node_mapping);
                            let new_id = optimized.add_node(mapped_node);
                            node_mapping.insert(node_id, new_id);
                            expr_cache.insert(expr_key, new_id);
                        }
                    } else {
                        // Effectful node, just copy it
                        let mapped_node = map_node_refs(node, &node_mapping);
                        let new_id = optimized.add_node(mapped_node);
                        node_mapping.insert(node_id, new_id);
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

    fn stats(&self) -> String {
        format!("{} pass: {} pure computations hoisted, {} duplicates removed, {} effects reordered",
                self.name(), self.pure_hoisted, self.duplicates_removed, self.effects_reordered)
    }
}

/// Map node references using the mapping table
fn map_node_refs(node: &Node, mapping: &FxHashMap<NodeId, NodeId>) -> Node {
    match node {
        Node::Application { function, args } => {
            let new_func = mapping.get(function).copied().unwrap_or(*function);
            let new_args: Vec<_> = args.iter()
                .map(|&arg| mapping.get(&arg).copied().unwrap_or(arg))
                .collect();
            Node::Application {
                function: new_func,
                args: new_args,
            }
        }
        Node::Lambda { params, body } => {
            let new_body = mapping.get(body).copied().unwrap_or(*body);
            Node::Lambda {
                params: params.clone(),
                body: new_body,
            }
        }
        Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
            let new_bindings: Vec<_> = bindings.iter()
                .map(|(name, value)| {
                    let new_value = mapping.get(value).copied().unwrap_or(*value);
                    (name.clone(), new_value)
                })
                .collect();
            let new_body = mapping.get(body).copied().unwrap_or(*body);
            
            if matches!(node, Node::Letrec { .. }) {
                Node::Letrec {
                    bindings: new_bindings,
                    body: new_body,
                }
            } else {
                Node::Let {
                    bindings: new_bindings,
                    body: new_body,
                }
            }
        }
        Node::If { condition, then_branch, else_branch } => {
            let new_cond = mapping.get(condition).copied().unwrap_or(*condition);
            let new_then = mapping.get(then_branch).copied().unwrap_or(*then_branch);
            let new_else = mapping.get(else_branch).copied().unwrap_or(*else_branch);
            Node::If {
                condition: new_cond,
                then_branch: new_then,
                else_branch: new_else,
            }
        }
        Node::Effect { effect_type, operation, args } => {
            let new_args: Vec<_> = args.iter()
                .map(|&arg| mapping.get(&arg).copied().unwrap_or(arg))
                .collect();
            Node::Effect {
                effect_type: *effect_type,
                operation: operation.clone(),
                args: new_args,
            }
        }
        _ => node.clone(),
    }
}