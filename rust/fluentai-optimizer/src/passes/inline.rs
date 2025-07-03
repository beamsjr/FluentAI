//! Function inlining pass

use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};
use anyhow::Result;
use crate::passes::OptimizationPass;
use crate::analysis::{calculate_node_size, is_recursive_function};

/// Function inlining pass
pub struct InlinePass {
    threshold: usize,
    inlined_count: usize,
}

impl InlinePass {
    /// Create new inline pass
    pub fn new(threshold: usize) -> Self {
        Self { 
            threshold,
            inlined_count: 0,
        }
    }

    /// Check if a function should be inlined
    fn should_inline(&self, graph: &Graph, func_id: NodeId) -> bool {
        if let Some(Node::Lambda { body, params, .. }) = graph.get_node(func_id) {
            // Don't inline recursive functions
            if is_recursive_function(graph, func_id) {
                return false;
            }
            
            // Check size threshold
            let size = calculate_node_size(graph, *body);
            if size > self.threshold {
                return false;
            }
            
            // Don't inline functions with too many parameters (complexity)
            if params.len() > 5 {
                return false;
            }
            
            true
        } else {
            false
        }
    }

    /// Perform beta reduction (substitute arguments in function body)
    fn beta_reduce(&self, 
                   graph: &Graph, 
                   body: NodeId, 
                   params: &[String], 
                   args: &[NodeId],
                   node_mapping: &mut FxHashMap<NodeId, NodeId>,
                   optimized: &mut Graph) -> NodeId {
        // Create substitution map
        let mut substitutions = FxHashMap::default();
        for (param, arg) in params.iter().zip(args.iter()) {
            substitutions.insert(param.clone(), *arg);
        }
        
        // Recursively copy and substitute
        self.copy_with_substitution(graph, body, &substitutions, node_mapping, optimized)
    }

    /// Copy a node with variable substitution
    fn copy_with_substitution(&self,
                              graph: &Graph,
                              node_id: NodeId,
                              substitutions: &FxHashMap<String, NodeId>,
                              node_mapping: &mut FxHashMap<NodeId, NodeId>,
                              optimized: &mut Graph) -> NodeId {
        // Check if already mapped
        if let Some(&mapped_id) = node_mapping.get(&node_id) {
            return mapped_id;
        }

        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return node_id,
        };

        let new_node = match node {
            Node::Variable { name } => {
                // Check if this variable should be substituted
                if let Some(&arg_id) = substitutions.get(name) {
                    // Return the argument node id (already mapped)
                    return node_mapping.get(&arg_id).copied().unwrap_or(arg_id);
                }
                Node::Variable { name: name.clone() }
            }
            Node::Lambda { params, body } => {
                // Create new substitution map without shadowed params
                let mut new_subs = substitutions.clone();
                for param in params {
                    new_subs.remove(param);
                }
                
                let new_body = self.copy_with_substitution(graph, *body, &new_subs, node_mapping, optimized);
                Node::Lambda {
                    params: params.clone(),
                    body: new_body,
                }
            }
            Node::Application { function, args } => {
                let new_func = self.copy_with_substitution(graph, *function, substitutions, node_mapping, optimized);
                let new_args: Vec<_> = args.iter()
                    .map(|&arg| self.copy_with_substitution(graph, arg, substitutions, node_mapping, optimized))
                    .collect();
                Node::Application {
                    function: new_func,
                    args: new_args,
                }
            }
            Node::Let { bindings, body } => {
                // Process bindings
                let mut new_subs = substitutions.clone();
                let mut new_bindings = Vec::new();
                
                for (name, value) in bindings {
                    let new_value = self.copy_with_substitution(graph, *value, &new_subs, node_mapping, optimized);
                    new_bindings.push((name.clone(), new_value));
                    // Shadow the binding
                    new_subs.remove(name);
                }
                
                let new_body = self.copy_with_substitution(graph, *body, &new_subs, node_mapping, optimized);
                Node::Let {
                    bindings: new_bindings,
                    body: new_body,
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                let new_cond = self.copy_with_substitution(graph, *condition, substitutions, node_mapping, optimized);
                let new_then = self.copy_with_substitution(graph, *then_branch, substitutions, node_mapping, optimized);
                let new_else = self.copy_with_substitution(graph, *else_branch, substitutions, node_mapping, optimized);
                Node::If {
                    condition: new_cond,
                    then_branch: new_then,
                    else_branch: new_else,
                }
            }
            _ => node.clone(),
        };

        let new_id = optimized.add_node(new_node);
        node_mapping.insert(node_id, new_id);
        new_id
    }
}

impl OptimizationPass for InlinePass {
    fn name(&self) -> &str {
        "Function Inlining"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.inlined_count = 0;
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        
        // First pass: identify inline candidates
        let mut inline_candidates = FxHashSet::default();
        for (node_id, node) in &graph.nodes {
            if matches!(node, Node::Lambda { .. }) && self.should_inline(graph, *node_id) {
                inline_candidates.insert(*node_id);
            }
        }

        // Process all nodes
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Application { function, args } => {
                    // Check if we're applying an inline candidate
                    let func_id = node_mapping.get(function).copied().unwrap_or(*function);
                    
                    if inline_candidates.contains(&func_id) {
                        // Inline the function
                        if let Some(Node::Lambda { body, params, .. }) = graph.get_node(func_id) {
                            // Map arguments first
                            let mapped_args: Vec<_> = args.iter()
                                .map(|&arg| {
                                    if let Some(&mapped) = node_mapping.get(&arg) {
                                        mapped
                                    } else {
                                        // Map the argument node
                                        self.copy_node(graph, arg, &node_mapping, &mut optimized)
                                    }
                                })
                                .collect();
                            
                            // Perform beta reduction
                            let inlined = self.beta_reduce(
                                graph, 
                                *body, 
                                params, 
                                &mapped_args,
                                &mut node_mapping,
                                &mut optimized
                            );
                            
                            node_mapping.insert(*node_id, inlined);
                            self.inlined_count += 1;
                            continue;
                        }
                    }
                    
                    // Regular application - copy with mapping
                    let mapped_node = self.copy_node(graph, *node_id, &node_mapping, &mut optimized);
                    node_mapping.insert(*node_id, mapped_node);
                }
                _ => {
                    // Copy other nodes normally
                    let mapped_node = self.copy_node(graph, *node_id, &node_mapping, &mut optimized);
                    node_mapping.insert(*node_id, mapped_node);
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
        format!("{} pass: {} functions inlined", self.name(), self.inlined_count)
    }
}

impl InlinePass {
    /// Helper to copy a node with mapping
    fn copy_node(&self, 
                 graph: &Graph, 
                 node_id: NodeId, 
                 mapping: &FxHashMap<NodeId, NodeId>,
                 optimized: &mut Graph) -> NodeId {
        if let Some(&mapped) = mapping.get(&node_id) {
            return mapped;
        }

        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return node_id,
        };

        let mapped_node = match node {
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
            Node::Let { bindings, body } => {
                let new_bindings: Vec<_> = bindings.iter()
                    .map(|(name, value)| {
                        let new_value = mapping.get(value).copied().unwrap_or(*value);
                        (name.clone(), new_value)
                    })
                    .collect();
                let new_body = mapping.get(body).copied().unwrap_or(*body);
                Node::Let {
                    bindings: new_bindings,
                    body: new_body,
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
            _ => node.clone(),
        };

        optimized.add_node(mapped_node)
    }
}