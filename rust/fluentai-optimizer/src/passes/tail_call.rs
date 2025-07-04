//! Tail call optimization pass

use fluentai_core::ast::{Graph, Node, NodeId, Literal, Pattern};
use anyhow::Result;
use crate::passes::OptimizationPass;
use rustc_hash::{FxHashMap, FxHashSet};

/// Information about a tail call
#[derive(Debug, Clone)]
struct TailCallInfo {
    node_id: NodeId,
    args: Vec<NodeId>,
    is_self_recursive: bool,
    target_function: String,
}

/// Tail call optimization pass
pub struct TailCallOptimizationPass {
    optimized_count: usize,
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
            optimized_functions: FxHashSet::default(),
            function_params: FxHashMap::default(),
        }
    }
}

impl TailCallOptimizationPass {
    /// Check if a function body contains tail recursion and collect tail call info
    fn analyze_tail_recursion(&self, graph: &Graph, func_name: &str, body: NodeId) -> Option<Vec<TailCallInfo>> {
        let mut tail_calls = Vec::new();
        self.collect_tail_calls(graph, func_name, body, true, &mut tail_calls);
        if !tail_calls.is_empty() {
            Some(tail_calls)
        } else {
            None
        }
    }
    
    /// Collect all tail calls in a function body
    fn collect_tail_calls(&self, graph: &Graph, func_name: &str, node_id: NodeId, is_tail: bool, tail_calls: &mut Vec<TailCallInfo>) {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    if is_tail {
                        if let Some(Node::Variable { name }) = graph.get_node(*function) {
                            if name == func_name {
                                tail_calls.push(TailCallInfo {
                                    node_id,
                                    args: args.clone(),
                                    is_self_recursive: true,
                                    target_function: name.clone(),
                                });
                            }
                        }
                    }
                }
                Node::If { condition: _, then_branch, else_branch } => {
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
    
    /// Transform a tail-recursive function into a loop
    fn transform_to_loop(&mut self, graph: &mut Graph, func_name: &str, params: &[String], body: NodeId, tail_calls: Vec<TailCallInfo>) -> NodeId {
        // For now, we'll mark the function with metadata indicating it's tail-recursive
        // In a real implementation, we would:
        // 1. Create a new node type for tail-recursive loops
        // 2. Transform the body to use loop constructs
        // 3. Replace tail calls with parameter updates and continue
        
        // Add metadata to indicate this function should be compiled as a loop
        let metadata = graph.metadata_mut(body);
        metadata.annotations.push("tail-recursive".to_string());
        metadata.annotations.push(format!("tail-calls:{}", tail_calls.len()));
        
        // Transform each tail call site
        for tail_call in tail_calls {
            let call_metadata = graph.metadata_mut(tail_call.node_id);
            call_metadata.annotations.push("tail-call".to_string());
        }
        
        self.optimized_count += 1;
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
        self.optimized_functions.clear();
        self.function_params.clear();
        
        let mut optimized = graph.clone();
        
        // Collect all function definitions first
        for (_node_id, node) in &graph.nodes {
            match node {
                Node::Letrec { bindings, .. } => {
                    for (func_name, func_id) in bindings {
                        if let Some(Node::Lambda { params, .. }) = graph.get_node(*func_id) {
                            self.function_params.insert(func_name.clone(), params.clone());
                        }
                    }
                }
                Node::Let { bindings, .. } => {
                    for (func_name, func_id) in bindings {
                        if let Some(Node::Lambda { params, .. }) = graph.get_node(*func_id) {
                            self.function_params.insert(func_name.clone(), params.clone());
                        }
                    }
                }
                _ => {}
            }
        }
        
        // Find and optimize tail-recursive functions
        let mut functions_to_optimize = Vec::new();
        
        for (node_id, node) in &graph.nodes {
            if let Node::Letrec { bindings, body } = node {
                for (func_name, func_id) in bindings {
                    if let Some(Node::Lambda { params, body: lambda_body }) = graph.get_node(*func_id) {
                        if let Some(tail_calls) = self.analyze_tail_recursion(graph, func_name, *lambda_body) {
                            functions_to_optimize.push((*node_id, func_name.clone(), params.clone(), *lambda_body, tail_calls));
                        }
                    }
                }
            }
        }
        
        // Apply transformations
        for (letrec_id, func_name, params, body, tail_calls) in functions_to_optimize {
            self.transform_to_loop(&mut optimized, &func_name, &params, body, tail_calls);
        }
        
        Ok(optimized)
    }

    fn stats(&self) -> String {
        if self.optimized_count > 0 {
            let funcs = self.optimized_functions.iter().cloned().collect::<Vec<_>>().join(", ");
            format!("{} pass: {} functions optimized ({})", self.name(), self.optimized_count, funcs)
        } else {
            format!("{} pass: no optimizations performed", self.name())
        }
    }
}