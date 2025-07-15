//! Function specialization optimization pass
//!
//! This pass creates specialized versions of functions when they are frequently
//! called with constant arguments or specific patterns. This enables better
//! optimization opportunities by allowing constant folding and other optimizations
//! within the specialized function body.

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Function specialization optimization pass
pub struct FunctionSpecializationPass {
    specialized_count: usize,
}

impl FunctionSpecializationPass {
    /// Create a new function specialization pass
    pub fn new() -> Self {
        Self {
            specialized_count: 0,
        }
    }

    /// Optimize by specializing functions
    fn specialize_functions(&mut self, graph: &Graph) -> Graph {
        // First, analyze call patterns to find specialization opportunities
        let call_patterns = self.analyze_call_patterns(graph);
        
        // Find functions worth specializing
        let specialization_candidates = self.find_specialization_candidates(graph, &call_patterns);
        
        if specialization_candidates.is_empty() {
            return graph.clone();
        }
        
        // Create a new graph with specialized functions
        let mut new_graph = Graph::new();
        let mut node_map = FxHashMap::default();
        
        // First pass: copy all nodes and track specializations to add
        let mut specializations_to_add = Vec::new();
        
        for (node_id, node) in &graph.nodes {
            let new_node = match node {
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    // Check if any of the bindings should be specialized
                    let mut new_bindings = Vec::new();
                    
                    for (name, func_id) in bindings {
                        new_bindings.push((name.clone(), *func_id));
                        
                        if let Some(patterns) = specialization_candidates.get(name) {
                            for pattern in patterns {
                                if self.should_specialize(graph, name, pattern) {
                                    specializations_to_add.push((
                                        *node_id,
                                        name.clone(),
                                        *func_id,
                                        pattern.clone(),
                                    ));
                                    self.specialized_count += 1;
                                }
                            }
                        }
                    }
                    
                    if matches!(node, Node::Let { .. }) {
                        Node::Let {
                            bindings: new_bindings,
                            body: *body,
                        }
                    } else {
                        Node::Letrec {
                            bindings: new_bindings,
                            body: *body,
                        }
                    }
                }
                _ => node.clone(),
            };
            
            let new_id = new_graph.add_node(new_node).unwrap();
            node_map.insert(*node_id, new_id);
        }
        
        // Add specialized functions
        for (container_id, func_name, func_id, pattern) in specializations_to_add {
            self.add_specialized_function(
                &mut new_graph,
                &node_map,
                graph,
                container_id,
                &func_name,
                func_id,
                &pattern,
            );
        }
        
        // Update all references
        self.update_references(&mut new_graph, &node_map);
        
        // Rewrite calls to use specialized versions
        self.rewrite_specialized_calls(&mut new_graph, graph, &specialization_candidates);
        
        // Update root
        new_graph.root_id = graph.root_id.and_then(|id| node_map.get(&id).copied());
        
        new_graph
    }

    /// Add a specialized function to the graph
    fn add_specialized_function(
        &self,
        new_graph: &mut Graph,
        node_map: &FxHashMap<NodeId, NodeId>,
        old_graph: &Graph,
        container_id: NodeId,
        func_name: &str,
        func_id: NodeId,
        pattern: &[ArgumentPattern],
    ) {
        let specialized_name = self.generate_specialized_name(func_name, pattern);
        
        if let Some(Node::Lambda { params, body }) = old_graph.get_node(func_id) {
            // Create specialized parameters (excluding constants)
            let specialized_params: Vec<_> = params.iter()
                .zip(pattern.iter())
                .filter_map(|(param, pat)| {
                    match pat {
                        ArgumentPattern::Constant(_) => None,
                        _ => Some(param.clone()),
                    }
                })
                .collect();
            
            // Create substitution map for constant arguments
            let mut substitutions = FxHashMap::default();
            for (param, arg_pattern) in params.iter().zip(pattern.iter()) {
                if let ArgumentPattern::Constant(lit) = arg_pattern {
                    // Create a literal node for this constant
                    let lit_node = Node::Literal(lit.clone());
                    let lit_id = new_graph.add_node(lit_node).unwrap();
                    substitutions.insert(param.clone(), lit_id);
                }
            }
            
            // Copy the body with substitutions
            let specialized_body = self.copy_with_substitutions(
                old_graph,
                new_graph,
                *body,
                &substitutions,
                node_map,
            );
            
            let specialized_func = Node::Lambda {
                params: specialized_params,
                body: specialized_body,
            };
            
            let specialized_id = new_graph.add_node(specialized_func).unwrap();
            
            // Add to container's bindings
            if let Some(new_container_id) = node_map.get(&container_id) {
                match new_graph.get_node_mut(*new_container_id) {
                    Some(Node::Let { bindings, .. }) | Some(Node::Letrec { bindings, .. }) => {
                        bindings.push((specialized_name, specialized_id));
                    }
                    _ => {}
                }
            }
        }
    }

    /// Copy a node tree with substitutions
    fn copy_with_substitutions(
        &self,
        old_graph: &Graph,
        new_graph: &mut Graph,
        node_id: NodeId,
        substitutions: &FxHashMap<String, NodeId>,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> NodeId {
        if let Some(node) = old_graph.get_node(node_id) {
            match node {
                Node::Variable { name } => {
                    if let Some(&sub_id) = substitutions.get(name) {
                        sub_id
                    } else {
                        node_map.get(&node_id).copied().unwrap_or(node_id)
                    }
                }
                Node::Application { function, args } => {
                    let new_func = self.copy_with_substitutions(
                        old_graph,
                        new_graph,
                        *function,
                        substitutions,
                        node_map,
                    );
                    let new_args: Vec<_> = args.iter()
                        .map(|&arg| self.copy_with_substitutions(
                            old_graph,
                            new_graph,
                            arg,
                            substitutions,
                            node_map,
                        ))
                        .collect();
                    
                    let new_app = Node::Application {
                        function: new_func,
                        args: new_args,
                    };
                    new_graph.add_node(new_app).unwrap()
                }
                Node::Lambda { params, body } => {
                    // Don't substitute bound variables
                    let mut new_subs = substitutions.clone();
                    for param in params {
                        new_subs.remove(param);
                    }
                    
                    let new_body = self.copy_with_substitutions(
                        old_graph,
                        new_graph,
                        *body,
                        &new_subs,
                        node_map,
                    );
                    let new_lambda = Node::Lambda {
                        params: params.clone(),
                        body: new_body,
                    };
                    new_graph.add_node(new_lambda).unwrap()
                }
                _ => node_map.get(&node_id).copied().unwrap_or(node_id),
            }
        } else {
            node_id
        }
    }

    /// Update all node references in the new graph
    fn update_references(&self, graph: &mut Graph, node_map: &FxHashMap<NodeId, NodeId>) {
        let nodes: Vec<_> = graph.nodes.keys().cloned().collect();
        
        for node_id in nodes {
            if let Some(node) = graph.get_node_mut(node_id) {
                match node {
                    Node::Application { function, args } => {
                        *function = node_map.get(function).copied().unwrap_or(*function);
                        for arg in args {
                            *arg = node_map.get(arg).copied().unwrap_or(*arg);
                        }
                    }
                    Node::Lambda { body, .. } => {
                        *body = node_map.get(body).copied().unwrap_or(*body);
                    }
                    Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                        for (_, value) in bindings {
                            *value = node_map.get(value).copied().unwrap_or(*value);
                        }
                        *body = node_map.get(body).copied().unwrap_or(*body);
                    }
                    Node::If { condition, then_branch, else_branch } => {
                        *condition = node_map.get(condition).copied().unwrap_or(*condition);
                        *then_branch = node_map.get(then_branch).copied().unwrap_or(*then_branch);
                        *else_branch = node_map.get(else_branch).copied().unwrap_or(*else_branch);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Rewrite calls to use specialized versions
    fn rewrite_specialized_calls(
        &self,
        graph: &mut Graph,
        old_graph: &Graph,
        candidates: &FxHashMap<String, Vec<Vec<ArgumentPattern>>>,
    ) {
        let nodes: Vec<_> = graph.nodes.keys().cloned().collect();
        
        for node_id in nodes {
            if let Some(Node::Application { function, args }) = graph.get_node(node_id).cloned() {
                // Check if this is a call to a specialized function
                let func_name = match graph.get_node(function) {
                    Some(Node::Variable { name }) => name.clone(),
                    _ => continue,
                };
                
                if let Some(patterns) = candidates.get(&func_name) {
                    // Check if arguments match any pattern
                    for pattern in patterns {
                        if self.matches_pattern_in_graph(graph, &args, pattern) {
                            let specialized_name = self.generate_specialized_name(&func_name, pattern);
                            
                            // Create new function reference
                            let new_func = Node::Variable { name: specialized_name };
                            let new_func_id = graph.add_node(new_func).unwrap();
                            
                            // Filter out constant arguments
                            let new_args: Vec<_> = args.iter().zip(pattern.iter())
                                .filter_map(|(arg, pat)| {
                                    match pat {
                                        ArgumentPattern::Constant(_) => None,
                                        _ => Some(*arg),
                                    }
                                })
                                .collect();
                            
                            // Update the application
                            if let Some(node) = graph.get_node_mut(node_id) {
                                *node = Node::Application {
                                    function: new_func_id,
                                    args: new_args,
                                };
                            }
                            break;
                        }
                    }
                }
            }
        }
    }

    /// Analyze call patterns in the graph
    fn analyze_call_patterns(&self, graph: &Graph) -> CallPatternAnalysis {
        let mut analysis = CallPatternAnalysis::new();
        
        // Walk the graph looking for function calls - need to traverse the entire graph
        if let Some(root) = graph.root_id {
            let mut visited = FxHashSet::default();
            self.analyze_calls_recursive(graph, root, &mut analysis, &mut visited);
        }
        
        analysis
    }

    /// Recursively analyze calls in the graph
    fn analyze_calls_recursive(
        &self,
        graph: &Graph,
        node_id: NodeId,
        analysis: &mut CallPatternAnalysis,
        visited: &mut FxHashSet<NodeId>,
    ) {
        if !visited.insert(node_id) {
            return;
        }
        
        if let Some(node) = graph.get_node(node_id) {
            // First analyze this node for calls
            self.analyze_node_calls(graph, node, analysis);
            
            // Then recursively analyze children
            match node {
                Node::Application { function, args } => {
                    self.analyze_calls_recursive(graph, *function, analysis, visited);
                    for arg in args {
                        self.analyze_calls_recursive(graph, *arg, analysis, visited);
                    }
                }
                Node::Lambda { body, .. } => {
                    self.analyze_calls_recursive(graph, *body, analysis, visited);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value) in bindings {
                        self.analyze_calls_recursive(graph, *value, analysis, visited);
                    }
                    self.analyze_calls_recursive(graph, *body, analysis, visited);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.analyze_calls_recursive(graph, *condition, analysis, visited);
                    self.analyze_calls_recursive(graph, *then_branch, analysis, visited);
                    self.analyze_calls_recursive(graph, *else_branch, analysis, visited);
                }
                Node::List(items) => {
                    for item in items {
                        self.analyze_calls_recursive(graph, *item, analysis, visited);
                    }
                }
                _ => {}
            }
        }
    }

    /// Analyze calls in a node
    fn analyze_node_calls(&self, graph: &Graph, node: &Node, analysis: &mut CallPatternAnalysis) {
        match node {
            Node::Application { function, args } => {
                // Check if this is a direct function call
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    // Analyze the arguments
                    let mut arg_pattern = Vec::new();
                    for arg_id in args {
                        if let Some(arg_node) = graph.get_node(*arg_id) {
                            arg_pattern.push(self.classify_argument(arg_node));
                        }
                    }
                    
                    analysis.record_call(name.clone(), arg_pattern);
                }
            }
            _ => {}
        }
    }

    /// Classify an argument for pattern analysis
    fn classify_argument(&self, node: &Node) -> ArgumentPattern {
        match node {
            Node::Literal(lit) => ArgumentPattern::Constant(lit.clone()),
            Node::Variable { name } => ArgumentPattern::Variable(name.clone()),
            Node::Lambda { .. } => ArgumentPattern::Function,
            _ => ArgumentPattern::Complex,
        }
    }

    /// Find functions that are good candidates for specialization
    fn find_specialization_candidates(
        &self,
        graph: &Graph,
        call_patterns: &CallPatternAnalysis,
    ) -> FxHashMap<String, Vec<Vec<ArgumentPattern>>> {
        let mut candidates = FxHashMap::default();
        
        // Look for functions with repeated patterns
        for (func_name, patterns) in &call_patterns.patterns {
            let mut pattern_counts = FxHashMap::default();
            
            for pattern in patterns {
                // Normalize the pattern for counting - replace variable names with generic placeholder
                let normalized = self.normalize_pattern(pattern);
                *pattern_counts.entry(normalized).or_insert(0) += 1;
            }
            
            // Find patterns that appear frequently
            let frequent_patterns: Vec<_> = pattern_counts
                .into_iter()
                .filter(|(pattern, count)| {
                    *count >= 2 && self.is_specializable_pattern(pattern)
                })
                .map(|(pattern, _)| pattern)
                .collect();
            
            if !frequent_patterns.is_empty() {
                candidates.insert(func_name.clone(), frequent_patterns);
            }
        }
        
        candidates
    }

    /// Normalize a pattern by replacing variable names with generic placeholders
    fn normalize_pattern(&self, pattern: &[ArgumentPattern]) -> Vec<ArgumentPattern> {
        pattern.iter().map(|arg| match arg {
            ArgumentPattern::Variable(_) => ArgumentPattern::Variable("_var".to_string()),
            other => other.clone(),
        }).collect()
    }

    /// Check if a pattern is worth specializing
    fn is_specializable_pattern(&self, pattern: &[ArgumentPattern]) -> bool {
        // At least one constant argument makes specialization worthwhile
        pattern.iter().any(|arg| matches!(arg, ArgumentPattern::Constant(_)))
    }

    /// Determine if we should create a specialized version
    fn should_specialize(
        &self,
        graph: &Graph,
        func_name: &str,
        pattern: &[ArgumentPattern],
    ) -> bool {
        // Find the function definition
        for (_, node) in &graph.nodes {
            match node {
                Node::Let { bindings, .. } | Node::Letrec { bindings, .. } => {
                    for (name, func_id) in bindings {
                        if name == func_name {
                            if let Some(Node::Lambda { params, body }) = graph.get_node(*func_id) {
                                // Check if the function is complex enough to benefit
                                let node_count = self.count_nodes(graph, *body);
                                return params.len() == pattern.len() 
                                    && node_count >= 3;
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Count nodes in a subgraph
    fn count_nodes(&self, graph: &Graph, root: NodeId) -> usize {
        let mut count = 0;
        let mut visited = FxHashSet::default();
        let mut to_visit = vec![root];
        
        while let Some(node_id) = to_visit.pop() {
            if !visited.insert(node_id) {
                continue;
            }
            
            count += 1;
            
            if let Some(node) = graph.get_node(node_id) {
                match node {
                    Node::Application { function, args } => {
                        to_visit.push(*function);
                        to_visit.extend(args);
                    }
                    Node::Lambda { body, .. } => {
                        to_visit.push(*body);
                    }
                    _ => {}
                }
            }
        }
        
        count
    }

    /// Generate a name for the specialized function
    fn generate_specialized_name(&self, base_name: &str, pattern: &[ArgumentPattern]) -> String {
        let mut name = format!("{}_spec", base_name);
        for (i, arg) in pattern.iter().enumerate() {
            match arg {
                ArgumentPattern::Constant(Literal::Integer(n)) => {
                    name.push_str(&format!("_{}_{}", i, n));
                }
                ArgumentPattern::Constant(Literal::Boolean(b)) => {
                    name.push_str(&format!("_{}_{}", i, if *b { "true" } else { "false" }));
                }
                _ => {}
            }
        }
        name
    }

    /// Check if arguments match a pattern
    fn matches_pattern_in_graph(
        &self,
        graph: &Graph,
        args: &[NodeId],
        pattern: &[ArgumentPattern],
    ) -> bool {
        if args.len() != pattern.len() {
            return false;
        }
        
        for (arg_id, expected) in args.iter().zip(pattern.iter()) {
            if let Some(arg_node) = graph.get_node(*arg_id) {
                let actual = self.classify_argument(arg_node);
                if !actual.matches(expected) {
                    return false;
                }
            } else {
                return false;
            }
        }
        
        true
    }
}

impl OptimizationPass for FunctionSpecializationPass {
    fn name(&self) -> &str {
        "Function Specialization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.specialized_count = 0;
        Ok(self.specialize_functions(graph))
    }

    fn stats(&self) -> String {
        format!("{} pass: {} functions specialized", self.name(), self.specialized_count)
    }
}

/// Analysis of call patterns in the program
#[derive(Debug)]
struct CallPatternAnalysis {
    patterns: FxHashMap<String, Vec<Vec<ArgumentPattern>>>,
}

impl CallPatternAnalysis {
    fn new() -> Self {
        Self {
            patterns: FxHashMap::default(),
        }
    }

    fn record_call(&mut self, func_name: String, pattern: Vec<ArgumentPattern>) {
        self.patterns.entry(func_name).or_default().push(pattern);
    }
}

/// Pattern for function arguments
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ArgumentPattern {
    Constant(Literal),
    Variable(String),
    Function,
    Complex,
}

impl ArgumentPattern {
    fn matches(&self, other: &ArgumentPattern) -> bool {
        match (self, other) {
            (ArgumentPattern::Constant(a), ArgumentPattern::Constant(b)) => a == b,
            (ArgumentPattern::Variable(_), ArgumentPattern::Variable(_)) => true,
            (ArgumentPattern::Function, ArgumentPattern::Function) => true,
            (ArgumentPattern::Complex, ArgumentPattern::Complex) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_specialize_constant_argument() {
        let mut graph = Graph::new();
        
        // Create a function: let add_n = (x, n) => x + n
        let x_param = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let n_param = graph.add_node(Node::Variable { name: "n".to_string() }).unwrap();
        let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        let add_op = graph.add_node(Node::Application {
            function: plus,
            args: vec![x_param, n_param],
        }).unwrap();
        
        let add_n_func = graph.add_node(Node::Lambda {
            params: vec!["x".to_string(), "n".to_string()],
            body: add_op,
        }).unwrap();
        
        // Variable for function calls
        let add_n_var = graph.add_node(Node::Variable { name: "add_n".to_string() }).unwrap();
        
        // Create calls with constant second argument
        let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let five = graph.add_node(Node::Literal(Literal::Integer(5))).unwrap();
        
        let call1 = graph.add_node(Node::Application {
            function: add_n_var,
            args: vec![x_var, five],
        }).unwrap();
        
        // Add another call with same pattern
        let y_var = graph.add_node(Node::Variable { name: "y".to_string() }).unwrap();
        let add_n_var2 = graph.add_node(Node::Variable { name: "add_n".to_string() }).unwrap();
        let call2 = graph.add_node(Node::Application {
            function: add_n_var2,
            args: vec![y_var, five],
        }).unwrap();
        
        // Create a block that uses both calls
        let block = graph.add_node(Node::List(vec![call1, call2])).unwrap();
        
        // Create the let binding
        let let_node = graph.add_node(Node::Let {
            bindings: vec![("add_n".to_string(), add_n_func)],
            body: block,
        }).unwrap();
        
        graph.root_id = Some(let_node);
        
        // Run optimization
        let mut pass = FunctionSpecializationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have created a specialized version
        assert_eq!(pass.specialized_count, 1);
        
        // The optimized graph should have a specialized function
        let mut found_specialized = false;
        for node in optimized.nodes.values() {
            if let Node::Let { bindings, .. } | Node::Letrec { bindings, .. } = node {
                for (name, _) in bindings {
                    if name.starts_with("add_n_spec") {
                        found_specialized = true;
                        break;
                    }
                }
            }
        }
        assert!(found_specialized, "Should have created a specialized function");
    }

    #[test]
    fn test_no_specialize_all_variables() {
        let mut graph = Graph::new();
        
        // Create a function with only variable arguments
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let y = graph.add_node(Node::Variable { name: "y".to_string() }).unwrap();
        let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        
        let add_op = graph.add_node(Node::Application {
            function: plus,
            args: vec![x, y],
        }).unwrap();
        
        let add_func = graph.add_node(Node::Lambda {
            params: vec!["x".to_string(), "y".to_string()],
            body: add_op,
        }).unwrap();
        
        let add_var = graph.add_node(Node::Variable { name: "add".to_string() }).unwrap();
        
        // Calls with all variable arguments
        let a = graph.add_node(Node::Variable { name: "a".to_string() }).unwrap();
        let b = graph.add_node(Node::Variable { name: "b".to_string() }).unwrap();
        let call = graph.add_node(Node::Application {
            function: add_var,
            args: vec![a, b],
        }).unwrap();
        
        let let_node = graph.add_node(Node::Let {
            bindings: vec![("add".to_string(), add_func)],
            body: call,
        }).unwrap();
        
        graph.root_id = Some(let_node);
        
        // Run optimization
        let mut pass = FunctionSpecializationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should not specialize when all arguments are variables
        assert_eq!(pass.specialized_count, 0);
    }
}