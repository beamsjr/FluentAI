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

    /// Hoist pure computations out of effectful contexts
    fn hoist_pure_computations(&mut self, 
                                graph: &Graph, 
                                effect_analysis: &EffectAnalysis,
                                node_mapping: &mut FxHashMap<NodeId, NodeId>,
                                optimized: &mut Graph) -> bool {
        let mut hoisted = false;
        
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Let { bindings, body } => {
                    // Check if any bindings are pure and can be hoisted
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
                        // Create outer let with pure bindings
                        let inner_let = if effectful_bindings.is_empty() {
                            *body
                        } else {
                            let inner = Node::Let {
                                bindings: effectful_bindings,
                                body: *body,
                            };
                            optimized.add_node(inner)
                        };
                        
                        let outer = Node::Let {
                            bindings: pure_bindings,
                            body: inner_let,
                        };
                        
                        let new_id = optimized.add_node(outer);
                        node_mapping.insert(*node_id, new_id);
                        self.pure_hoisted += 1;
                        hoisted = true;
                        continue;
                    }
                }
                _ => {}
            }
            
            // Default: copy node
            let mapped_node = map_node_refs(node, node_mapping);
            let new_id = optimized.add_node(mapped_node);
            node_mapping.insert(*node_id, new_id);
        }
        
        hoisted
    }

    /// Remove duplicate pure computations
    fn remove_duplicate_pure(&mut self,
                            graph: &Graph,
                            effect_analysis: &EffectAnalysis,
                            node_mapping: &mut FxHashMap<NodeId, NodeId>,
                            optimized: &mut Graph) -> bool {
        let mut removed = false;
        let mut expr_cache: FxHashMap<String, NodeId> = FxHashMap::default();
        
        for (node_id, node) in &graph.nodes {
            // Only consider pure nodes
            if !effect_analysis.pure_nodes.contains(node_id) {
                let mapped_node = map_node_refs(node, node_mapping);
                let new_id = optimized.add_node(mapped_node);
                node_mapping.insert(*node_id, new_id);
                continue;
            }
            
            // Create a canonical representation of the expression
            let expr_key = self.canonicalize_expr(node, node_mapping);
            
            if let Some(&existing_id) = expr_cache.get(&expr_key) {
                // Reuse existing computation
                node_mapping.insert(*node_id, existing_id);
                self.duplicates_removed += 1;
                removed = true;
            } else {
                // New expression, add to cache
                let mapped_node = map_node_refs(node, node_mapping);
                let new_id = optimized.add_node(mapped_node);
                node_mapping.insert(*node_id, new_id);
                expr_cache.insert(expr_key, new_id);
            }
        }
        
        removed
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
        
        // Pass 1: Hoist pure computations
        self.hoist_pure_computations(graph, &effect_analysis, &mut node_mapping, &mut optimized);
        
        // Pass 2: Remove duplicate pure computations
        if !node_mapping.is_empty() {
            let temp_graph = optimized;
            optimized = Graph::new();
            let mut new_mapping = FxHashMap::default();
            self.remove_duplicate_pure(&temp_graph, &effect_analysis, &mut new_mapping, &mut optimized);
            
            // Update the original mapping
            let updates: Vec<_> = node_mapping.iter()
                .filter_map(|(old_id, temp_id)| {
                    new_mapping.get(temp_id).map(|&new_id| (*old_id, new_id))
                })
                .collect();
            
            for (old_id, new_id) in updates {
                node_mapping.insert(old_id, new_id);
            }
        } else {
            self.remove_duplicate_pure(graph, &effect_analysis, &mut node_mapping, &mut optimized);
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