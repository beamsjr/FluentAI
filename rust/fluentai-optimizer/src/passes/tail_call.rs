//! Tail call optimization pass

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Information about a tail call
#[derive(Debug, Clone)]
struct TailCallInfo {
    _node_id: NodeId,
    _args: Vec<NodeId>,
    _is_self_recursive: bool,
    _target_function: String,
}

/// Tail call optimization pass
pub struct TailCallOptimizationPass {
    optimized_count: usize,
    tail_calls_count: usize,
    /// Track which functions have been optimized
    optimized_functions: FxHashSet<String>,
    /// Map from function names to their parameter lists
    function_params: FxHashMap<String, Vec<String>>,
}

impl TailCallOptimizationPass {
    /// Create new tail call optimization pass
    pub fn new() -> Self {
        Self {
            optimized_count: 0,
            tail_calls_count: 0,
            optimized_functions: FxHashSet::default(),
            function_params: FxHashMap::default(),
        }
    }
}

impl TailCallOptimizationPass {
    /// Check if a function body contains tail recursion and collect tail call info
    fn analyze_tail_recursion(
        &self,
        graph: &Graph,
        func_name: &str,
        body: NodeId,
    ) -> Option<Vec<TailCallInfo>> {
        let mut tail_calls = Vec::new();
        self.collect_tail_calls(graph, func_name, body, true, &mut tail_calls);
        if !tail_calls.is_empty() {
            Some(tail_calls)
        } else {
            None
        }
    }

    /// Collect all tail calls in a function body
    fn collect_tail_calls(
        &self,
        graph: &Graph,
        func_name: &str,
        node_id: NodeId,
        is_tail: bool,
        tail_calls: &mut Vec<TailCallInfo>,
    ) {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    if is_tail {
                        if let Some(Node::Variable { name }) = graph.get_node(*function) {
                            if name == func_name {
                                tail_calls.push(TailCallInfo {
                                    _node_id: node_id,
                                    _args: args.clone(),
                                    _is_self_recursive: true,
                                    _target_function: name.clone(),
                                });
                            }
                        }
                    }
                }
                Node::If {
                    condition: _,
                    then_branch,
                    else_branch,
                } => {
                    // Both branches are in tail position
                    self.collect_tail_calls(graph, func_name, *then_branch, is_tail, tail_calls);
                    self.collect_tail_calls(graph, func_name, *else_branch, is_tail, tail_calls);
                }
                Node::Let { body, .. } | Node::Letrec { body, .. } => {
                    // Body is in tail position
                    self.collect_tail_calls(graph, func_name, *body, is_tail, tail_calls);
                }
                Node::Match { branches, .. } => {
                    // All branches are in tail position
                    for (_, branch) in branches {
                        self.collect_tail_calls(graph, func_name, *branch, is_tail, tail_calls);
                    }
                }
                _ => {}
            }
        }
    }

    /// Mark a function as tail-call optimized
    fn mark_as_optimized(
        &mut self,
        graph: &mut Graph,
        func_name: &str,
        body: NodeId,
        tail_calls: Vec<TailCallInfo>,
    ) -> NodeId {
        let tail_call_count = tail_calls.len();

        // Add metadata to indicate optimization
        let metadata = graph.metadata_mut(body);
        metadata
            .annotations
            .push("tail-recursive-optimized".to_string());
        metadata
            .annotations
            .push(format!("tail-calls-eliminated:{}", tail_call_count));

        // Update statistics
        self.optimized_count += 1;
        self.tail_calls_count += tail_call_count;
        self.optimized_functions.insert(func_name.to_string());

        body
    }
}

impl OptimizationPass for TailCallOptimizationPass {
    fn name(&self) -> &str {
        "Tail Call Optimization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.optimized_count = 0;
        self.tail_calls_count = 0;
        self.optimized_functions.clear();
        self.function_params.clear();

        let mut optimized = graph.clone();

        // Collect all function definitions first
        for (_node_id, node) in &graph.nodes {
            match node {
                Node::Letrec { bindings, .. } => {
                    for (func_name, func_id) in bindings {
                        if let Some(Node::Lambda { params, .. }) = graph.get_node(*func_id) {
                            self.function_params
                                .insert(func_name.clone(), params.clone());
                        }
                    }
                }
                Node::Let { bindings, .. } => {
                    for (func_name, func_id) in bindings {
                        if let Some(Node::Lambda { params, .. }) = graph.get_node(*func_id) {
                            self.function_params
                                .insert(func_name.clone(), params.clone());
                        }
                    }
                }
                _ => {}
            }
        }

        // Find and optimize tail-recursive functions
        let mut functions_to_optimize = Vec::new();

        for (node_id, node) in &graph.nodes {
            if let Node::Letrec { bindings, body: _ } = node {
                for (i, (func_name, func_id)) in bindings.iter().enumerate() {
                    if let Some(Node::Lambda {
                        params,
                        body: lambda_body,
                    }) = graph.get_node(*func_id)
                    {
                        if let Some(tail_calls) =
                            self.analyze_tail_recursion(graph, func_name, *lambda_body)
                        {
                            functions_to_optimize.push((
                                *node_id,
                                i,
                                func_name.clone(),
                                *func_id,
                                params.clone(),
                                *lambda_body,
                                tail_calls,
                            ));
                        }
                    }
                }
            }
        }

        // Apply optimizations (for now, just mark with metadata)
        for (letrec_id, binding_idx, func_name, _lambda_id, params, body, tail_calls) in
            functions_to_optimize
        {
            // Mark the lambda body as optimized
            let marked_body = self.mark_as_optimized(&mut optimized, &func_name, body, tail_calls);

            // Create new lambda with marked body
            let new_lambda = optimized.add_node(Node::Lambda {
                params: params.clone(),
                body: marked_body,
            })?;

            // Update the letrec binding
            if let Some(Node::Letrec { bindings, body }) = optimized.get_node(letrec_id).cloned() {
                let mut new_bindings = bindings;
                if binding_idx < new_bindings.len() {
                    new_bindings[binding_idx].1 = new_lambda;
                }
                // Replace the letrec node
                if let Some(node) = optimized.get_node_mut(letrec_id) {
                    *node = Node::Letrec {
                        bindings: new_bindings,
                        body,
                    };
                }
            }
        }

        Ok(optimized)
    }

    fn stats(&self) -> String {
        if self.tail_calls_count > 0 {
            format!(
                "{} pass: {} tail calls optimized",
                self.name(),
                self.tail_calls_count
            )
        } else {
            format!("{} pass: no optimizations performed", self.name())
        }
    }
}
