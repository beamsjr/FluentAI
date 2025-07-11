//! Loop optimization passes

use crate::analysis::{ControlFlowGraph, DataFlowAnalysis};
use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Loop optimization pass
pub struct LoopOptimizationPass {
    unrolled_count: usize,
    fused_count: usize,
    hoisted_count: usize,
}

impl LoopOptimizationPass {
    /// Create new loop optimization pass
    pub fn new() -> Self {
        Self {
            unrolled_count: 0,
            fused_count: 0,
            hoisted_count: 0,
        }
    }

    /// Detect if a node represents a loop construct
    fn detect_loop(&self, graph: &Graph, node: &Node) -> Option<LoopInfo> {
        match node {
            // Detect tail-recursive functions (common loop pattern in functional languages)
            Node::Letrec { bindings, body: _ } => {
                // Look for recursive functions that could be loops
                for (name, func_id) in bindings {
                    if let Some(Node::Lambda {
                        params,
                        body: lambda_body,
                    }) = graph.get_node(*func_id)
                    {
                        if self.is_tail_recursive(graph, name, *lambda_body) {
                            return Some(LoopInfo {
                                kind: LoopKind::TailRecursive,
                                _func_name: name.clone(),
                                params: params.clone(),
                                body: *lambda_body,
                                _binding_id: *func_id,
                            });
                        }
                    }
                }
                None
            }
            // Detect map/fold patterns
            Node::Application { function, args } => {
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    match name.as_str() {
                        "map" | "filter" | "fold" | "reduce" => {
                            if args.len() >= 2 {
                                return Some(LoopInfo {
                                    kind: LoopKind::HigherOrder(name.clone()),
                                    _func_name: name.clone(),
                                    params: vec![],
                                    body: args[0], // The function being mapped
                                    _binding_id: *function,
                                });
                            }
                        }
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Check if a function is tail-recursive
    fn is_tail_recursive(&self, graph: &Graph, func_name: &str, body: NodeId) -> bool {
        self.check_tail_position(graph, func_name, body, true)
    }

    /// Check if recursive calls are in tail position
    fn check_tail_position(
        &self,
        graph: &Graph,
        func_name: &str,
        node_id: NodeId,
        is_tail: bool,
    ) -> bool {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, .. } => {
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        if name == func_name {
                            return is_tail;
                        }
                    }
                    true
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    self.check_tail_position(graph, func_name, *condition, false)
                        && self.check_tail_position(graph, func_name, *then_branch, is_tail)
                        && self.check_tail_position(graph, func_name, *else_branch, is_tail)
                }
                Node::Let { bindings, body } => {
                    for (_, value) in bindings {
                        if !self.check_tail_position(graph, func_name, *value, false) {
                            return false;
                        }
                    }
                    self.check_tail_position(graph, func_name, *body, is_tail)
                }
                _ => true,
            }
        } else {
            true
        }
    }

    /// Try to unroll a small loop
    fn try_unroll_loop(
        &self,
        graph: &Graph,
        loop_info: &LoopInfo,
        unroll_factor: usize,
    ) -> Option<Node> {
        // Only unroll small loops with constant bounds
        if let Some(bound) = self.get_loop_bound(graph, loop_info) {
            if bound <= 10 && bound <= unroll_factor {
                // Unroll completely for small constant loops
                return self.unroll_completely(graph, loop_info, bound);
            }
        }
        None
    }

    /// Get the loop bound if it's constant
    fn get_loop_bound(&self, _graph: &Graph, loop_info: &LoopInfo) -> Option<usize> {
        // Simple heuristic: look for patterns like (loop 0 10 ...)
        // This is a simplified version - real implementation would need more analysis
        match &loop_info.kind {
            LoopKind::TailRecursive => {
                // Analyze the termination condition
                None // TODO: Implement constant bound detection
            }
            LoopKind::HigherOrder(name) => {
                if name == "map" || name == "filter" {
                    // These preserve list length, so check the list argument
                    None // TODO: Implement list length detection
                } else {
                    None
                }
            }
        }
    }

    /// Completely unroll a loop with known bounds
    fn unroll_completely(
        &self,
        _graph: &Graph,
        _loop_info: &LoopInfo,
        _bound: usize,
    ) -> Option<Node> {
        // TODO: Implement complete unrolling
        None
    }

    /// Find loop-invariant code
    fn find_invariant_code(
        &self,
        graph: &Graph,
        loop_info: &LoopInfo,
        _cfg: &ControlFlowGraph,
        dfa: &DataFlowAnalysis,
    ) -> Vec<NodeId> {
        let mut invariant = Vec::new();
        let loop_vars = self.get_loop_variables(loop_info);

        // Check nodes in the loop body
        let mut to_check = vec![loop_info.body];
        let mut checked = FxHashSet::default();

        while let Some(node_id) = to_check.pop() {
            if !checked.insert(node_id) {
                continue;
            }

            if let Some(node) = graph.get_node(node_id) {
                // Check if this node is invariant
                if self.is_invariant(node, &loop_vars, dfa) {
                    invariant.push(node_id);
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
                    Node::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        to_check.push(*condition);
                        to_check.push(*then_branch);
                        to_check.push(*else_branch);
                    }
                    _ => {}
                }
            }
        }

        invariant
    }

    /// Get loop variables (induction variables and parameters)
    fn get_loop_variables(&self, loop_info: &LoopInfo) -> FxHashSet<String> {
        let mut vars = FxHashSet::default();
        vars.extend(loop_info.params.iter().cloned());
        vars
    }

    /// Check if a node is loop-invariant
    fn is_invariant(
        &self,
        node: &Node,
        loop_vars: &FxHashSet<String>,
        _dfa: &DataFlowAnalysis,
    ) -> bool {
        match node {
            Node::Variable { name } => !loop_vars.contains(name),
            Node::Literal(_) => true,
            _ => false, // Conservative - need more analysis
        }
    }
}

impl OptimizationPass for LoopOptimizationPass {
    fn name(&self) -> &str {
        "Loop Optimization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.unrolled_count = 0;
        self.fused_count = 0;
        self.hoisted_count = 0;

        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();

        // Build control flow and data flow analysis
        let cfg = ControlFlowGraph::build(graph);
        let dfa = DataFlowAnalysis::analyze(graph, &cfg);

        // Process nodes looking for optimization opportunities
        for (node_id, node) in &graph.nodes {
            let mut optimized_node = None;

            // Try loop optimizations
            if let Some(loop_info) = self.detect_loop(graph, node) {
                // Try unrolling
                if let Some(unrolled) = self.try_unroll_loop(graph, &loop_info, 4) {
                    optimized_node = Some(unrolled);
                    self.unrolled_count += 1;
                } else {
                    // Try hoisting invariant code
                    let invariants = self.find_invariant_code(graph, &loop_info, &cfg, &dfa);
                    if !invariants.is_empty() {
                        self.hoisted_count += invariants.len();
                        // TODO: Actually hoist the code
                    }
                }
            }

            // Copy node (optimized or original)
            let new_node = optimized_node.unwrap_or_else(|| map_node_refs(node, &node_mapping));
            let new_id = optimized.add_node(new_node)?;
            node_mapping.insert(*node_id, new_id);
        }

        // Look for fusion opportunities (simplified - would need more sophisticated analysis)
        // This is a second pass that could fuse adjacent operations

        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }

        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!(
            "{} pass: {} loops unrolled, {} loops fused, {} invariants hoisted",
            self.name(),
            self.unrolled_count,
            self.fused_count,
            self.hoisted_count
        )
    }
}

/// Information about a detected loop
struct LoopInfo {
    kind: LoopKind,
    _func_name: String,
    params: Vec<String>,
    body: NodeId,
    _binding_id: NodeId,
}

/// Kind of loop detected
enum LoopKind {
    TailRecursive,
    HigherOrder(String), // map, filter, fold, etc.
}

/// Map node references using the mapping table
fn map_node_refs(node: &Node, mapping: &FxHashMap<NodeId, NodeId>) -> Node {
    match node {
        Node::Application { function, args } => {
            let new_func = mapping.get(function).copied().unwrap_or(*function);
            let new_args: Vec<_> = args
                .iter()
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
            let new_bindings: Vec<_> = bindings
                .iter()
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
        Node::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let new_cond = mapping.get(condition).copied().unwrap_or(*condition);
            let new_then = mapping.get(then_branch).copied().unwrap_or(*then_branch);
            let new_else = mapping.get(else_branch).copied().unwrap_or(*else_branch);
            Node::If {
                condition: new_cond,
                then_branch: new_then,
                else_branch: new_else,
            }
        }
        Node::Match { expr, branches } => {
            let new_expr = mapping.get(expr).copied().unwrap_or(*expr);
            let new_branches: Vec<_> = branches
                .iter()
                .map(|(pattern, branch)| {
                    let new_branch = mapping.get(branch).copied().unwrap_or(*branch);
                    (pattern.clone(), new_branch)
                })
                .collect();
            Node::Match {
                expr: new_expr,
                branches: new_branches,
            }
        }
        Node::Assignment { target, value } => Node::Assignment {
            target: mapping.get(target).copied().unwrap_or(*target),
            value: mapping.get(value).copied().unwrap_or(*value),
        },
        _ => node.clone(),
    }
}
