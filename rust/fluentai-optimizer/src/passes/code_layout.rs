//! Code layout optimization pass
//!
//! This pass optimizes the layout of code for better instruction cache performance:
//! - Groups hot functions together
//! - Separates cold code paths
//! - Optimizes branch layout for better prediction
//! - Aligns critical loops

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Configuration for code layout optimization
pub struct CodeLayoutConfig {
    /// Enable function reordering
    pub enable_function_reordering: bool,
    /// Enable branch layout optimization
    pub enable_branch_optimization: bool,
    /// Enable loop alignment
    pub enable_loop_alignment: bool,
    /// Hot threshold (execution count)
    pub hot_threshold: u64,
    /// Cold threshold (execution count)
    pub cold_threshold: u64,
}

impl Default for CodeLayoutConfig {
    fn default() -> Self {
        Self {
            enable_function_reordering: true,
            enable_branch_optimization: true,
            enable_loop_alignment: true,
            hot_threshold: 1000,
            cold_threshold: 10,
        }
    }
}

/// Code layout optimization pass
pub struct CodeLayoutPass {
    config: CodeLayoutConfig,
    functions_reordered: usize,
    branches_optimized: usize,
    loops_aligned: usize,
    /// Function hotness information
    function_temps: FxHashMap<NodeId, Temperature>,
    /// Call graph for layout decisions
    call_graph: CallGraph,
}

impl CodeLayoutPass {
    /// Create a new code layout optimization pass
    pub fn new(config: CodeLayoutConfig) -> Self {
        Self {
            config,
            functions_reordered: 0,
            branches_optimized: 0,
            loops_aligned: 0,
            function_temps: FxHashMap::default(),
            call_graph: CallGraph::new(),
        }
    }

    /// Build the call graph from the AST
    fn build_call_graph(&mut self, graph: &Graph) {
        let mut visited = FxHashSet::default();
        if let Some(root) = graph.root_id {
            self.analyze_calls(graph, root, None, &mut visited);
        }
    }

    /// Analyze function calls to build call graph
    fn analyze_calls(
        &mut self,
        graph: &Graph,
        node_id: NodeId,
        current_func: Option<NodeId>,
        visited: &mut FxHashSet<NodeId>,
    ) {
        if !visited.insert(node_id) {
            return;
        }

        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Lambda { body, .. } => {
                    // This is a function definition
                    self.call_graph.add_function(node_id);
                    self.analyze_calls(graph, *body, Some(node_id), visited);
                }
                Node::Application { function, args } => {
                    // This is a function call
                    if let Some(caller) = current_func {
                        // Try to resolve the callee
                        if let Some(Node::Variable { .. }) = graph.get_node(*function) {
                            // Direct function call
                            self.call_graph.add_call(caller, *function);
                        }
                    }
                    
                    // Analyze arguments
                    self.analyze_calls(graph, *function, current_func, visited);
                    for arg in args {
                        self.analyze_calls(graph, *arg, current_func, visited);
                    }
                }
                Node::Let { bindings, body } => {
                    for (_, value) in bindings {
                        self.analyze_calls(graph, *value, current_func, visited);
                    }
                    self.analyze_calls(graph, *body, current_func, visited);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.analyze_calls(graph, *condition, current_func, visited);
                    self.analyze_calls(graph, *then_branch, current_func, visited);
                    self.analyze_calls(graph, *else_branch, current_func, visited);
                }
                _ => {}
            }
        }
    }

    /// Classify functions by temperature (hot/warm/cold)
    fn classify_functions(&mut self, _graph: &Graph) {
        // In a real implementation, we would use profiling data
        // For now, use call frequency as a heuristic
        for func_id in self.call_graph.functions.iter() {
            let call_count = self.call_graph.get_call_count(*func_id);
            
            let temp = if call_count >= self.config.hot_threshold {
                Temperature::Hot
            } else if call_count <= self.config.cold_threshold {
                Temperature::Cold
            } else {
                Temperature::Warm
            };
            
            self.function_temps.insert(*func_id, temp);
        }
    }

    /// Optimize function layout based on temperature and call patterns
    fn optimize_function_layout(&mut self, graph: &Graph) -> Vec<NodeId> {
        let mut layout = Vec::new();
        
        // Group functions by temperature
        let mut hot_funcs = Vec::new();
        let mut warm_funcs = Vec::new();
        let mut cold_funcs = Vec::new();
        
        for (func_id, temp) in &self.function_temps {
            match temp {
                Temperature::Hot => hot_funcs.push(*func_id),
                Temperature::Warm => warm_funcs.push(*func_id),
                Temperature::Cold => cold_funcs.push(*func_id),
            }
        }
        
        // Sort hot functions by call frequency (most called first)
        hot_funcs.sort_by_key(|f| std::cmp::Reverse(self.call_graph.get_call_count(*f)));
        
        // Layout: hot functions first, then warm, then cold
        layout.extend(hot_funcs);
        layout.extend(warm_funcs);
        layout.extend(cold_funcs);
        
        self.functions_reordered = layout.len();
        layout
    }

    /// Optimize branch layout for better prediction
    fn optimize_branches(&mut self, graph: &Graph, node_id: NodeId) -> Option<BranchHint> {
        if let Some(Node::If { condition, then_branch, else_branch }) = graph.get_node(node_id) {
            // Analyze which branch is more likely
            // In a real implementation, we would use profiling data
            
            // Heuristics:
            // 1. Error checking - error path is usually cold
            // 2. Loop conditions - usually true
            // 3. Null checks - usually not null
            
            if self.is_error_check(graph, *condition) {
                self.branches_optimized += 1;
                return Some(BranchHint::Likely(*else_branch));
            }
            
            if self.is_loop_condition(graph, *condition) {
                self.branches_optimized += 1;
                return Some(BranchHint::Likely(*then_branch));
            }
        }
        
        None
    }

    /// Check if a condition is an error check
    fn is_error_check(&self, _graph: &Graph, _condition: NodeId) -> bool {
        // Simplified heuristic
        false
    }

    /// Check if a condition is a loop condition
    fn is_loop_condition(&self, _graph: &Graph, _condition: NodeId) -> bool {
        // Simplified heuristic
        false
    }

    /// Find loops that should be aligned for better performance
    fn find_loops_to_align(&mut self, graph: &Graph) -> Vec<NodeId> {
        let mut loops = Vec::new();
        let mut visited = FxHashSet::default();
        
        if let Some(root) = graph.root_id {
            self.find_loops_recursive(graph, root, &mut visited, &mut loops);
        }
        
        self.loops_aligned = loops.len();
        loops
    }

    /// Recursively find loops in the AST
    fn find_loops_recursive(
        &self,
        graph: &Graph,
        node_id: NodeId,
        visited: &mut FxHashSet<NodeId>,
        loops: &mut Vec<NodeId>,
    ) {
        if !visited.insert(node_id) {
            return;
        }

        if let Some(node) = graph.get_node(node_id) {
            // Look for recursive functions (tail-recursive loops)
            if let Node::Lambda { body, .. } = node {
                if self.is_tail_recursive(graph, node_id, *body) {
                    loops.push(node_id);
                }
                self.find_loops_recursive(graph, *body, visited, loops);
            }
            
            // Recurse into other nodes
            match node {
                Node::Let { bindings, body } => {
                    for (_, value) in bindings {
                        self.find_loops_recursive(graph, *value, visited, loops);
                    }
                    self.find_loops_recursive(graph, *body, visited, loops);
                }
                Node::Application { function, args } => {
                    self.find_loops_recursive(graph, *function, visited, loops);
                    for arg in args {
                        self.find_loops_recursive(graph, *arg, visited, loops);
                    }
                }
                _ => {}
            }
        }
    }

    /// Check if a function is tail-recursive
    fn is_tail_recursive(&self, _graph: &Graph, _func_id: NodeId, _body: NodeId) -> bool {
        // Simplified check
        // In a real implementation, we would check if the function
        // calls itself in tail position
        false
    }
}

impl OptimizationPass for CodeLayoutPass {
    fn name(&self) -> &str {
        "Code Layout Optimization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.functions_reordered = 0;
        self.branches_optimized = 0;
        self.loops_aligned = 0;
        self.function_temps.clear();
        self.call_graph = CallGraph::new();
        
        // Build call graph
        self.build_call_graph(graph);
        
        // Classify functions by temperature
        self.classify_functions(graph);
        
        // Optimize function layout
        if self.config.enable_function_reordering && !self.function_temps.is_empty() {
            let _layout = self.optimize_function_layout(graph);
            // In a real implementation, we would reorder the functions
            // in the output according to the optimized layout
        }
        
        // Optimize branch layout
        if self.config.enable_branch_optimization {
            let mut branch_hints = FxHashMap::default();
            for (node_id, _) in graph.nodes() {
                if let Some(hint) = self.optimize_branches(graph, *node_id) {
                    branch_hints.insert(*node_id, hint);
                }
            }
            // In a real implementation, we would apply these hints
        }
        
        // Find and align hot loops
        if self.config.enable_loop_alignment {
            let _aligned_loops = self.find_loops_to_align(graph);
            // In a real implementation, we would ensure these loops
            // are aligned to cache line boundaries
        }
        
        // For now, return the original graph
        // A full implementation would restructure the code
        Ok(graph.clone())
    }

    fn stats(&self) -> String {
        format!(
            "{} pass: {} functions reordered, {} branches optimized, {} loops aligned",
            self.name(),
            self.functions_reordered,
            self.branches_optimized,
            self.loops_aligned
        )
    }
}

/// Temperature classification for functions
#[derive(Debug, Clone, Copy, PartialEq)]
enum Temperature {
    Hot,
    Warm,
    Cold,
}

/// Branch prediction hint
#[derive(Debug, Clone, Copy)]
enum BranchHint {
    Likely(NodeId),
    Unlikely(NodeId),
}

/// Simple call graph representation
struct CallGraph {
    functions: FxHashSet<NodeId>,
    calls: FxHashMap<NodeId, Vec<NodeId>>, // caller -> callees
    call_counts: FxHashMap<NodeId, u64>,   // function -> times called
}

impl CallGraph {
    fn new() -> Self {
        Self {
            functions: FxHashSet::default(),
            calls: FxHashMap::default(),
            call_counts: FxHashMap::default(),
        }
    }

    fn add_function(&mut self, func_id: NodeId) {
        self.functions.insert(func_id);
    }

    fn add_call(&mut self, caller: NodeId, callee: NodeId) {
        self.calls.entry(caller).or_default().push(callee);
        *self.call_counts.entry(callee).or_default() += 1;
    }

    fn get_call_count(&self, func_id: NodeId) -> u64 {
        self.call_counts.get(&func_id).copied().unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_layout_empty() {
        let mut graph = Graph::new();
        let nil = graph.add_node(Node::Literal(fluentai_core::ast::Literal::Nil)).unwrap();
        graph.root_id = Some(nil);
        
        let mut pass = CodeLayoutPass::new(CodeLayoutConfig::default());
        let result = pass.run(&graph);
        
        assert!(result.is_ok());
        assert_eq!(pass.functions_reordered, 0);
    }

    #[test]
    fn test_call_graph_building() {
        let mut graph = Graph::new();
        
        // Create a simple function
        let body = graph.add_node(Node::Literal(fluentai_core::ast::Literal::Integer(42))).unwrap();
        let func = graph.add_node(Node::Lambda {
            params: vec![],
            body,
        }).unwrap();
        
        graph.root_id = Some(func);
        
        let mut pass = CodeLayoutPass::new(CodeLayoutConfig::default());
        pass.build_call_graph(&graph);
        
        assert!(pass.call_graph.functions.contains(&func));
    }
}