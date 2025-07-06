//! Common subexpression elimination pass

use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::FxHashMap;
use anyhow::Result;
use crate::passes::OptimizationPass;
use crate::analysis::EffectAnalysis;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

/// Common subexpression elimination pass
pub struct CommonSubexpressionEliminationPass {
    eliminated_count: usize,
}

impl CommonSubexpressionEliminationPass {
    /// Create new CSE pass
    pub fn new() -> Self {
        Self { eliminated_count: 0 }
    }

    /// Generate a structural hash for a node
    fn node_hash(&self, node: &Node, graph: &Graph, node_mapping: &FxHashMap<NodeId, NodeId>) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash_node(node, graph, node_mapping, &mut hasher);
        hasher.finish()
    }

    /// Hash a node structurally
    fn hash_node<H: Hasher>(&self, node: &Node, graph: &Graph, node_mapping: &FxHashMap<NodeId, NodeId>, hasher: &mut H) {
        match node {
            Node::Literal(lit) => {
                "literal".hash(hasher);
                match lit {
                    Literal::Integer(i) => {
                        "int".hash(hasher);
                        i.hash(hasher);
                    }
                    Literal::Float(f) => {
                        "float".hash(hasher);
                        f.to_bits().hash(hasher);
                    }
                    Literal::String(s) => {
                        "string".hash(hasher);
                        s.hash(hasher);
                    }
                    Literal::Boolean(b) => {
                        "bool".hash(hasher);
                        b.hash(hasher);
                    }
                    Literal::Nil => {
                        "nil".hash(hasher);
                    }
                }
            }
            Node::Variable { name } => {
                "var".hash(hasher);
                name.hash(hasher);
            }
            Node::Application { function, args } => {
                "app".hash(hasher);
                // Hash the function structurally
                let mapped_func = node_mapping.get(function).copied().unwrap_or(*function);
                if let Some(func_node) = graph.get_node(mapped_func) {
                    self.hash_node(func_node, graph, node_mapping, hasher);
                } else {
                    mapped_func.hash(hasher);
                }
                // Hash each argument structurally
                args.len().hash(hasher);
                for arg in args {
                    let mapped_arg = node_mapping.get(arg).copied().unwrap_or(*arg);
                    if let Some(arg_node) = graph.get_node(mapped_arg) {
                        self.hash_node(arg_node, graph, node_mapping, hasher);
                    } else {
                        mapped_arg.hash(hasher);
                    }
                }
            }
            Node::List(items) => {
                "list".hash(hasher);
                items.len().hash(hasher);
                for item in items {
                    let mapped_item = node_mapping.get(item).copied().unwrap_or(*item);
                    if let Some(item_node) = graph.get_node(mapped_item) {
                        self.hash_node(item_node, graph, node_mapping, hasher);
                    } else {
                        mapped_item.hash(hasher);
                    }
                }
            }
            _ => {
                // For other node types, just use a unique identifier
                format!("{:?}", node).hash(hasher);
            }
        }
    }

    /// Check if nodes are structurally equal
    fn nodes_equal(&self, node1: &Node, node2: &Node, graph1: &Graph, graph2: &Graph, 
                   mapping1: &FxHashMap<NodeId, NodeId>, mapping2: &FxHashMap<NodeId, NodeId>) -> bool {
        match (node1, node2) {
            (Node::Literal(l1), Node::Literal(l2)) => l1 == l2,
            (Node::Variable { name: n1 }, Node::Variable { name: n2 }) => n1 == n2,
            (Node::Application { function: f1, args: a1 }, 
             Node::Application { function: f2, args: a2 }) => {
                if a1.len() != a2.len() {
                    return false;
                }
                // Compare functions structurally
                let mapped_f1 = mapping1.get(f1).copied().unwrap_or(*f1);
                let mapped_f2 = mapping2.get(f2).copied().unwrap_or(*f2);
                if let (Some(func1), Some(func2)) = (graph1.get_node(mapped_f1), graph2.get_node(mapped_f2)) {
                    if !self.nodes_equal(func1, func2, graph1, graph2, mapping1, mapping2) {
                        return false;
                    }
                } else if mapped_f1 != mapped_f2 {
                    return false;
                }
                // Compare arguments structurally
                for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                    let mapped_a1 = mapping1.get(arg1).copied().unwrap_or(*arg1);
                    let mapped_a2 = mapping2.get(arg2).copied().unwrap_or(*arg2);
                    if let (Some(arg_node1), Some(arg_node2)) = (graph1.get_node(mapped_a1), graph2.get_node(mapped_a2)) {
                        if !self.nodes_equal(arg_node1, arg_node2, graph1, graph2, mapping1, mapping2) {
                            return false;
                        }
                    } else if mapped_a1 != mapped_a2 {
                        return false;
                    }
                }
                true
            }
            (Node::List(i1), Node::List(i2)) => {
                if i1.len() != i2.len() {
                    return false;
                }
                for (item1, item2) in i1.iter().zip(i2.iter()) {
                    let mapped_i1 = mapping1.get(item1).copied().unwrap_or(*item1);
                    let mapped_i2 = mapping2.get(item2).copied().unwrap_or(*item2);
                    if let (Some(item_node1), Some(item_node2)) = (graph1.get_node(mapped_i1), graph2.get_node(mapped_i2)) {
                        if !self.nodes_equal(item_node1, item_node2, graph1, graph2, mapping1, mapping2) {
                            return false;
                        }
                    } else if mapped_i1 != mapped_i2 {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &str {
        "Common Subexpression Elimination"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.eliminated_count = 0;
        
        // Perform effect analysis
        let effect_analysis = EffectAnalysis::analyze(graph);
        
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        let mut expr_cache: FxHashMap<u64, Vec<NodeId>> = FxHashMap::default();

        // Process nodes in topological order to ensure dependencies are processed first
        let mut processed = std::collections::HashSet::new();
        let mut to_process: Vec<NodeId> = graph.nodes.keys().copied().collect();
        
        while !to_process.is_empty() {
            let mut made_progress = false;
            let mut remaining = Vec::new();
            
            for node_id in to_process {
                // Check if all dependencies are processed
                let node = graph.get_node(node_id).unwrap();
                let deps_ready = match node {
                    Node::Application { function, args } => {
                        node_mapping.contains_key(function) && 
                        args.iter().all(|arg| node_mapping.contains_key(arg))
                    }
                    Node::List(items) => {
                        items.iter().all(|item| node_mapping.contains_key(item))
                    }
                    Node::Lambda { body, .. } => node_mapping.contains_key(body),
                    Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                        bindings.iter().all(|(_, val)| node_mapping.contains_key(val)) &&
                        node_mapping.contains_key(body)
                    }
                    Node::If { condition, then_branch, else_branch } => {
                        node_mapping.contains_key(condition) &&
                        node_mapping.contains_key(then_branch) &&
                        node_mapping.contains_key(else_branch)
                    }
                    _ => true, // Literals and variables have no dependencies
                };
                
                if !deps_ready {
                    remaining.push(node_id);
                    continue;
                }
                
                processed.insert(node_id);
                made_progress = true;
                
                // Only eliminate pure expressions
                if effect_analysis.pure_nodes.contains(&node_id) {
                    // Generate structural hash
                    let hash = self.node_hash(node, graph, &node_mapping);
                    
                    // Check if we've seen a structurally equal expression
                    let mut found_match = false;
                    if let Some(candidates) = expr_cache.get(&hash) {
                        for &existing_id in candidates {
                            if let Some(existing_node) = optimized.get_node(existing_id) {
                                if self.nodes_equal(node, existing_node, graph, &optimized, &node_mapping, &node_mapping) {
                                    // Reuse existing node
                                    node_mapping.insert(node_id, existing_id);
                                    self.eliminated_count += 1;
                                    found_match = true;
                                    break;
                                }
                            }
                        }
                    }
                    
                    if !found_match {
                        // Add new expression
                        let mapped_node = map_node_refs(node, &node_mapping);
                        let new_id = optimized.add_node(mapped_node);
                        node_mapping.insert(node_id, new_id);
                        expr_cache.entry(hash).or_default().push(new_id);
                    }
                } else {
                    // Non-pure expressions can't be eliminated
                    let mapped_node = map_node_refs(node, &node_mapping);
                    let new_id = optimized.add_node(mapped_node);
                    node_mapping.insert(node_id, new_id);
                }
            }
            
            to_process = remaining;
            
            // If we didn't make progress, there might be a cycle
            if !made_progress && !to_process.is_empty() {
                // Process remaining nodes without CSE
                for node_id in to_process {
                    let node = graph.get_node(node_id).unwrap();
                    let mapped_node = map_node_refs(node, &node_mapping);
                    let new_id = optimized.add_node(mapped_node);
                    node_mapping.insert(node_id, new_id);
                }
                break;
            }
        }

        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }

        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!("{} pass: {} expressions eliminated", self.name(), self.eliminated_count)
    }
}

/// Map node references through the mapping
fn map_node_refs(node: &Node, mapping: &FxHashMap<NodeId, NodeId>) -> Node {
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
                    .map(|(name, id)| {
                        (name.clone(), mapping.get(id).copied().unwrap_or(*id))
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
        _ => node.clone(),
    }
}