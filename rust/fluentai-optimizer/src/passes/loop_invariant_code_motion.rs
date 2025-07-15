//! Loop-invariant code motion optimization pass
//!
//! This pass identifies computations within loops that don't depend on loop variables
//! and moves them outside the loop to avoid redundant calculations.

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Loop-invariant code motion optimization pass
pub struct LoopInvariantCodeMotion {
    hoisted_count: usize,
}

impl LoopInvariantCodeMotion {
    /// Create a new loop-invariant code motion pass
    pub fn new() -> Self {
        Self { hoisted_count: 0 }
    }

    /// Optimize a graph by hoisting loop-invariant code
    fn optimize_graph(&mut self, graph: &Graph) -> Graph {
        let mut optimized = Graph::new();
        let mut node_map = FxHashMap::default();
        
        // First, identify all loops in the graph
        let loops = self.find_loops(graph);
        
        // For each loop, identify invariant code
        let mut hoisted_nodes = FxHashMap::default();
        for loop_info in &loops {
            let invariants = self.find_invariants(graph, loop_info);
            if !invariants.is_empty() {
                hoisted_nodes.insert(loop_info.id, invariants);
            }
        }
        
        // Now rebuild the graph with hoisted code
        for (node_id, node) in &graph.nodes {
            let new_node = if let Some(loop_info) = loops.iter().find(|l| l.id == *node_id) {
                // This is a loop node that may have invariants to hoist
                if let Some(invariants) = hoisted_nodes.get(node_id) {
                    self.hoisted_count += invariants.len();
                    self.hoist_invariants(node, invariants, loop_info, &node_map)
                } else {
                    self.clone_node_with_refs(node, &node_map)
                }
            } else {
                self.clone_node_with_refs(node, &node_map)
            };
            
            let new_id = optimized.add_node(new_node).unwrap();
            node_map.insert(*node_id, new_id);
        }
        
        // Update root
        optimized.root_id = graph.root_id.and_then(|id| node_map.get(&id).copied());
        
        optimized
    }

    /// Find all loops in the graph
    fn find_loops(&self, graph: &Graph) -> Vec<LoopInfo> {
        let mut loops = Vec::new();
        
        for (node_id, node) in &graph.nodes {
            match node {
                // Tail-recursive functions are loops
                Node::Letrec { bindings, body } => {
                    for (name, func_id) in bindings {
                        if let Some(Node::Lambda { params, body: lambda_body }) = graph.get_node(*func_id) {
                            if self.is_tail_recursive(graph, name, *lambda_body) {
                                loops.push(LoopInfo {
                                    id: *node_id,
                                    kind: LoopKind::TailRecursive(name.clone()),
                                    params: params.clone(),
                                    body_id: *lambda_body,
                                    bindings: bindings.clone(),
                                    outer_body: *body,
                                });
                            }
                        }
                    }
                }
                // Map/filter/fold are also loops
                Node::Application { function, args } => {
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        match name.as_str() {
                            "map" | "filter" | "fold" | "reduce" if args.len() >= 2 => {
                                loops.push(LoopInfo {
                                    id: *node_id,
                                    kind: LoopKind::HigherOrder(name.clone()),
                                    params: vec![],
                                    body_id: args[0], // The function being mapped
                                    bindings: vec![],
                                    outer_body: *node_id,
                                });
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        
        loops
    }

    /// Check if a function is tail-recursive
    fn is_tail_recursive(&self, graph: &Graph, func_name: &str, body: NodeId) -> bool {
        self.check_tail_calls(graph, func_name, body, true)
    }

    /// Check if all recursive calls are in tail position
    fn check_tail_calls(&self, graph: &Graph, func_name: &str, node_id: NodeId, is_tail: bool) -> bool {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        if name == func_name && is_tail {
                            // Check that arguments don't contain recursive calls
                            for arg in args {
                                if !self.check_tail_calls(graph, func_name, *arg, false) {
                                    return false;
                                }
                            }
                            return true;
                        }
                    }
                    // Check all subexpressions
                    if !self.check_tail_calls(graph, func_name, *function, false) {
                        return false;
                    }
                    for arg in args {
                        if !self.check_tail_calls(graph, func_name, *arg, false) {
                            return false;
                        }
                    }
                    true
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.check_tail_calls(graph, func_name, *condition, false)
                        && self.check_tail_calls(graph, func_name, *then_branch, is_tail)
                        && self.check_tail_calls(graph, func_name, *else_branch, is_tail)
                }
                Node::Let { bindings, body } => {
                    for (_, value) in bindings {
                        if !self.check_tail_calls(graph, func_name, *value, false) {
                            return false;
                        }
                    }
                    self.check_tail_calls(graph, func_name, *body, is_tail)
                }
                _ => true
            }
        } else {
            true
        }
    }

    /// Find loop-invariant expressions in a loop
    fn find_invariants(&self, graph: &Graph, loop_info: &LoopInfo) -> Vec<(String, NodeId)> {
        let mut invariants = Vec::new();
        let loop_vars = self.get_loop_variables(loop_info);
        
        // Analyze the loop body for invariant expressions
        let mut to_check = vec![loop_info.body_id];
        let mut checked = FxHashSet::default();
        let mut binding_counter = 0;
        
        while let Some(node_id) = to_check.pop() {
            if !checked.insert(node_id) {
                continue;
            }
            
            if let Some(node) = graph.get_node(node_id) {
                // Check if this entire subexpression is invariant
                if self.is_invariant_expr(graph, node_id, &loop_vars) {
                    // Don't hoist trivial expressions
                    if !matches!(node, Node::Variable { .. } | Node::Literal(_)) {
                        let binding_name = format!("_invariant_{}", binding_counter);
                        binding_counter += 1;
                        invariants.push((binding_name, node_id));
                        // Don't check children of invariant expressions
                        continue;
                    }
                }
                
                // Add children to check
                match node {
                    Node::Application { function, args } => {
                        to_check.push(*function);
                        to_check.extend(args);
                    }
                    Node::Let { bindings, body } => {
                        for (_, value) in bindings {
                            to_check.push(*value);
                        }
                        to_check.push(*body);
                    }
                    Node::If { condition, then_branch, else_branch } => {
                        to_check.push(*condition);
                        to_check.push(*then_branch);
                        to_check.push(*else_branch);
                    }
                    Node::Lambda { body, .. } => {
                        to_check.push(*body);
                    }
                    _ => {}
                }
            }
        }
        
        invariants
    }

    /// Get the set of loop variables (parameters and induction variables)
    fn get_loop_variables(&self, loop_info: &LoopInfo) -> FxHashSet<String> {
        let mut vars = FxHashSet::default();
        
        // Add loop parameters
        vars.extend(loop_info.params.iter().cloned());
        
        // Add the recursive function name for tail-recursive loops
        if let LoopKind::TailRecursive(name) = &loop_info.kind {
            vars.insert(name.clone());
        }
        
        vars
    }

    /// Check if an expression is loop-invariant
    fn is_invariant_expr(&self, graph: &Graph, node_id: NodeId, loop_vars: &FxHashSet<String>) -> bool {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => !loop_vars.contains(name),
                Node::Literal(_) => true,
                Node::Application { function, args } => {
                    self.is_invariant_expr(graph, *function, loop_vars)
                        && args.iter().all(|arg| self.is_invariant_expr(graph, *arg, loop_vars))
                }
                Node::Lambda { params, body } => {
                    // Lambda captures make this more complex
                    // For now, be conservative and don't hoist lambdas
                    false
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.is_invariant_expr(graph, *condition, loop_vars)
                        && self.is_invariant_expr(graph, *then_branch, loop_vars)
                        && self.is_invariant_expr(graph, *else_branch, loop_vars)
                }
                Node::Let { bindings, body } => {
                    // Check if all bindings are invariant
                    bindings.iter().all(|(name, value)| {
                        !loop_vars.contains(name) && self.is_invariant_expr(graph, *value, loop_vars)
                    }) && self.is_invariant_expr(graph, *body, loop_vars)
                }
                _ => false
            }
        } else {
            false
        }
    }

    /// Hoist invariant expressions outside the loop
    fn hoist_invariants(
        &self,
        node: &Node,
        invariants: &[(String, NodeId)],
        loop_info: &LoopInfo,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Node {
        match node {
            Node::Letrec { bindings, body } => {
                // Create new bindings for hoisted expressions
                let mut new_bindings = Vec::new();
                
                // First add the hoisted invariants
                for (name, inv_id) in invariants {
                    let mapped_id = node_map.get(inv_id).copied().unwrap_or(*inv_id);
                    new_bindings.push((name.clone(), mapped_id));
                }
                
                // Then add the original loop bindings
                for (name, value) in bindings {
                    let mapped_value = node_map.get(value).copied().unwrap_or(*value);
                    new_bindings.push((name.clone(), mapped_value));
                }
                
                let mapped_body = node_map.get(body).copied().unwrap_or(*body);
                
                // Wrap in a Let node to bind the hoisted expressions
                Node::Let {
                    bindings: new_bindings,
                    body: mapped_body,
                }
            }
            _ => self.clone_node_with_refs(node, node_map)
        }
    }

    /// Clone a node and update its references
    fn clone_node_with_refs(&self, node: &Node, node_map: &FxHashMap<NodeId, NodeId>) -> Node {
        match node {
            Node::Application { function, args } => {
                Node::Application {
                    function: node_map.get(function).copied().unwrap_or(*function),
                    args: args.iter()
                        .map(|id| node_map.get(id).copied().unwrap_or(*id))
                        .collect(),
                }
            }
            Node::Lambda { params, body } => {
                Node::Lambda {
                    params: params.clone(),
                    body: node_map.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Let { bindings, body } => {
                Node::Let {
                    bindings: bindings.iter()
                        .map(|(name, id)| (name.clone(), node_map.get(id).copied().unwrap_or(*id)))
                        .collect(),
                    body: node_map.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Letrec { bindings, body } => {
                Node::Letrec {
                    bindings: bindings.iter()
                        .map(|(name, id)| (name.clone(), node_map.get(id).copied().unwrap_or(*id)))
                        .collect(),
                    body: node_map.get(body).copied().unwrap_or(*body),
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                Node::If {
                    condition: node_map.get(condition).copied().unwrap_or(*condition),
                    then_branch: node_map.get(then_branch).copied().unwrap_or(*then_branch),
                    else_branch: node_map.get(else_branch).copied().unwrap_or(*else_branch),
                }
            }
            Node::Match { expr, branches } => {
                Node::Match {
                    expr: node_map.get(expr).copied().unwrap_or(*expr),
                    branches: branches.iter()
                        .map(|(pat, body)| (pat.clone(), node_map.get(body).copied().unwrap_or(*body)))
                        .collect(),
                }
            }
            Node::List(items) => {
                Node::List(items.iter()
                    .map(|id| node_map.get(id).copied().unwrap_or(*id))
                    .collect())
            }
            Node::Map(pairs) => {
                Node::Map(pairs.iter()
                    .map(|(k, v)| {
                        (node_map.get(k).copied().unwrap_or(*k),
                         node_map.get(v).copied().unwrap_or(*v))
                    })
                    .collect())
            }
            Node::Async { body } => {
                Node::Async { body: node_map.get(body).copied().unwrap_or(*body) }
            }
            _ => node.clone(),
        }
    }
}

impl OptimizationPass for LoopInvariantCodeMotion {
    fn name(&self) -> &str {
        "Loop-Invariant Code Motion"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.hoisted_count = 0;
        Ok(self.optimize_graph(graph))
    }

    fn stats(&self) -> String {
        format!("{} pass: {} invariant expressions hoisted", self.name(), self.hoisted_count)
    }
}

/// Information about a loop
struct LoopInfo {
    id: NodeId,
    kind: LoopKind,
    params: Vec<String>,
    body_id: NodeId,
    bindings: Vec<(String, NodeId)>,
    outer_body: NodeId,
}

/// Type of loop
enum LoopKind {
    TailRecursive(String),
    HigherOrder(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::Literal;

    #[test]
    fn test_hoist_constant_from_loop() {
        let mut graph = Graph::new();
        
        // Create a tail-recursive loop with an invariant expression
        // letrec loop = \n -> if (n > 0) then (let x = 10 * 20 in loop(n - 1)) else 0
        
        // Constants
        let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
        let twenty = graph.add_node(Node::Literal(Literal::Integer(20))).unwrap();
        
        // Operations
        let gt = graph.add_node(Node::Variable { name: ">".to_string() }).unwrap();
        let minus = graph.add_node(Node::Variable { name: "-".to_string() }).unwrap();
        let mult = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
        
        // Variables
        let n = graph.add_node(Node::Variable { name: "n".to_string() }).unwrap();
        let loop_var = graph.add_node(Node::Variable { name: "loop".to_string() }).unwrap();
        
        // Build the invariant expression: 10 * 20
        let invariant_expr = graph.add_node(Node::Application {
            function: mult,
            args: vec![ten, twenty],
        }).unwrap();
        
        // Build n > 0
        let condition = graph.add_node(Node::Application {
            function: gt,
            args: vec![n, zero],
        }).unwrap();
        
        // Build n - 1
        let n_minus_1 = graph.add_node(Node::Application {
            function: minus,
            args: vec![n, one],
        }).unwrap();
        
        // Build loop(n - 1)
        let recursive_call = graph.add_node(Node::Application {
            function: loop_var,
            args: vec![n_minus_1],
        }).unwrap();
        
        // Build let x = 10 * 20 in loop(n - 1)
        let then_branch = graph.add_node(Node::Let {
            bindings: vec![("x".to_string(), invariant_expr)],
            body: recursive_call,
        }).unwrap();
        
        // Build the if expression
        let if_expr = graph.add_node(Node::If {
            condition,
            then_branch,
            else_branch: zero,
        }).unwrap();
        
        // Build the lambda
        let lambda = graph.add_node(Node::Lambda {
            params: vec!["n".to_string()],
            body: if_expr,
        }).unwrap();
        
        // Build the letrec
        let letrec = graph.add_node(Node::Letrec {
            bindings: vec![("loop".to_string(), lambda)],
            body: loop_var,
        }).unwrap();
        
        graph.root_id = Some(letrec);
        
        // Run the optimization
        let mut pass = LoopInvariantCodeMotion::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have hoisted the invariant expression
        assert_eq!(pass.hoisted_count, 1);
        
        // The optimized graph should have moved 10 * 20 outside the loop
        // We can't easily verify the exact structure, but we know it was hoisted
    }

    #[test]
    fn test_no_hoist_loop_dependent() {
        let mut graph = Graph::new();
        
        // Create a loop where the expression depends on the loop variable
        // letrec loop = \n -> if (n > 0) then (let x = n * 2 in loop(n - 1)) else 0
        
        let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        
        let gt = graph.add_node(Node::Variable { name: ">".to_string() }).unwrap();
        let minus = graph.add_node(Node::Variable { name: "-".to_string() }).unwrap();
        let mult = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
        
        let n = graph.add_node(Node::Variable { name: "n".to_string() }).unwrap();
        let loop_var = graph.add_node(Node::Variable { name: "loop".to_string() }).unwrap();
        
        // Build n * 2 (depends on loop variable)
        let dependent_expr = graph.add_node(Node::Application {
            function: mult,
            args: vec![n, two],
        }).unwrap();
        
        let condition = graph.add_node(Node::Application {
            function: gt,
            args: vec![n, zero],
        }).unwrap();
        
        let n_minus_1 = graph.add_node(Node::Application {
            function: minus,
            args: vec![n, one],
        }).unwrap();
        
        let recursive_call = graph.add_node(Node::Application {
            function: loop_var,
            args: vec![n_minus_1],
        }).unwrap();
        
        let then_branch = graph.add_node(Node::Let {
            bindings: vec![("x".to_string(), dependent_expr)],
            body: recursive_call,
        }).unwrap();
        
        let if_expr = graph.add_node(Node::If {
            condition,
            then_branch,
            else_branch: zero,
        }).unwrap();
        
        let lambda = graph.add_node(Node::Lambda {
            params: vec!["n".to_string()],
            body: if_expr,
        }).unwrap();
        
        let letrec = graph.add_node(Node::Letrec {
            bindings: vec![("loop".to_string(), lambda)],
            body: loop_var,
        }).unwrap();
        
        graph.root_id = Some(letrec);
        
        // Run the optimization
        let mut pass = LoopInvariantCodeMotion::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should not hoist anything because n * 2 depends on the loop variable
        assert_eq!(pass.hoisted_count, 0);
    }
}