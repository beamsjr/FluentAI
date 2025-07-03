//! Advanced optimizations with aggressive transformations

use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::{FxHashMap, FxHashSet};
use crate::stats::OptimizationStats;
use crate::analysis::EffectAnalysis;
use anyhow::Result;
use std::time::Instant;

/// Advanced optimizer with aggressive optimizations
pub struct AdvancedOptimizer {
    stats: OptimizationStats,
    graph: Option<Graph>,
    optimized: Graph,
    value_cache: FxHashMap<NodeId, Literal>,
    node_mapping: FxHashMap<NodeId, NodeId>,
    effect_analysis: Option<EffectAnalysis>,
    inline_threshold: usize,
}

impl AdvancedOptimizer {
    /// Create a new advanced optimizer
    pub fn new() -> Self {
        Self {
            stats: OptimizationStats::new(),
            graph: None,
            optimized: Graph::new(),
            value_cache: FxHashMap::default(),
            node_mapping: FxHashMap::default(),
            effect_analysis: None,
            inline_threshold: 10, // Default inline threshold
        }
    }

    /// Set inline threshold
    pub fn with_inline_threshold(mut self, threshold: usize) -> Self {
        self.inline_threshold = threshold;
        self
    }

    /// Optimize with multiple aggressive passes
    pub fn optimize(&mut self, graph: &Graph) -> Result<Graph> {
        let start = Instant::now();
        self.stats = OptimizationStats::new();
        self.stats.nodes_before = graph.nodes.len();
        
        self.graph = Some(graph.clone());
        self.optimized = Graph::new();
        self.value_cache.clear();
        self.node_mapping.clear();

        // Perform analyses
        self.effect_analysis = Some(EffectAnalysis::analyze(graph));

        // Build optimized graph
        if let Some(root_id) = graph.root_id {
            if let Some(new_root) = self.optimize_node(root_id)? {
                self.optimized.root_id = Some(new_root);
            }
        }

        // Apply additional passes
        self.inline_small_functions()?;
        self.optimize_tail_calls()?;
        self.beta_reduction()?;
        self.loop_optimizations()?;

        // Final cleanup
        self.eliminate_dead_code()?;

        self.stats.nodes_after = self.optimized.nodes.len();
        self.stats.optimization_time_us = start.elapsed().as_micros() as u64;

        Ok(self.optimized.clone())
    }

    /// Get optimization statistics
    pub fn stats(&self) -> &OptimizationStats {
        &self.stats
    }

    /// Optimize a node and return its ID in the optimized graph
    fn optimize_node(&mut self, node_id: NodeId) -> Result<Option<NodeId>> {
        // Check if already optimized
        if let Some(new_id) = self.node_mapping.get(&node_id) {
            return Ok(Some(*new_id));
        }

        let graph = self.graph.as_ref().unwrap();
        let _node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return Ok(None),
        };

        // Try to evaluate node completely
        if let Some(value) = self.try_evaluate(node_id)? {
            // Create literal node for computed value
            let literal_node = Node::Literal(value.clone());
            let new_id = self.optimized.add_node(literal_node);
            self.value_cache.insert(node_id, value);
            self.node_mapping.insert(node_id, new_id);
            self.stats.pure_expressions_evaluated += 1;
            return Ok(Some(new_id));
        }

        // Otherwise, copy node with optimized children
        self.copy_node_optimized(node_id)
    }

    /// Try to evaluate a node to a constant value
    fn try_evaluate(&mut self, node_id: NodeId) -> Result<Option<Literal>> {
        if let Some(value) = self.value_cache.get(&node_id) {
            return Ok(Some(value.clone()));
        }

        let graph = self.graph.as_ref().unwrap();
        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return Ok(None),
        };

        // Check if node is pure
        if !self.effect_analysis.as_ref()
            .map_or(false, |ea| ea.pure_nodes.contains(&node_id)) {
            return Ok(None);
        }

        match node {
            Node::Literal(lit) => Ok(Some(lit.clone())),
            Node::Application { .. } => self.evaluate_application(node_id),
            Node::If { .. } => self.evaluate_if(node_id),
            Node::Let { .. } => self.evaluate_let(node_id),
            _ => Ok(None),
        }
    }

    /// Evaluate function application
    fn evaluate_application(&mut self, node_id: NodeId) -> Result<Option<Literal>> {
        let graph = self.graph.as_ref().unwrap();
        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return Ok(None),
        };
        
        if let Node::Application { function, args } = node {
            let func_id = *function;
            let args = args.clone();
            
            let graph = self.graph.as_ref().unwrap();
            
            // Get function name
            let func_name = match graph.get_node(func_id) {
                Some(Node::Variable { name }) => name.clone(),
                _ => return Ok(None),
            };

            // Check if it's a pure primitive
            if !is_pure_primitive(&func_name) {
                return Ok(None);
            }

            // Evaluate all arguments
            let mut arg_values = Vec::new();
            for arg_id in args {
                match self.try_evaluate(arg_id)? {
                    Some(val) => arg_values.push(val),
                    None => return Ok(None),
                }
            }

            // Apply function
            evaluate_primitive(&func_name, &arg_values)
        } else {
            Ok(None)
        }
    }

    /// Evaluate conditional
    fn evaluate_if(&mut self, node_id: NodeId) -> Result<Option<Literal>> {
        let graph = self.graph.as_ref().unwrap();
        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return Ok(None),
        };
        
        if let Node::If { condition, then_branch, else_branch } = node {
            let cond_id = *condition;
            let then_id = *then_branch;
            let else_id = *else_branch;
            
            // Evaluate condition
            match self.try_evaluate(cond_id)? {
                Some(Literal::Boolean(true)) => {
                    self.stats.branches_eliminated += 1;
                    self.try_evaluate(then_id)
                }
                Some(Literal::Boolean(false)) => {
                    self.stats.branches_eliminated += 1;
                    self.try_evaluate(else_id)
                }
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    /// Evaluate let binding
    fn evaluate_let(&mut self, node_id: NodeId) -> Result<Option<Literal>> {
        let graph = self.graph.as_ref().unwrap();
        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return Ok(None),
        };
        
        if let Node::Let { bindings, body } = node {
            let bindings = bindings.clone();
            let body_id = *body;
            
            // Save current cache state
            let old_cache = self.value_cache.clone();

            // Evaluate bindings
            for (_, value_id) in bindings {
                if let Some(val) = self.try_evaluate(value_id)? {
                    self.value_cache.insert(value_id, val);
                } else {
                    // Restore cache and fail
                    self.value_cache = old_cache;
                    return Ok(None);
                }
            }

            // Evaluate body
            let result = self.try_evaluate(body_id)?;

            // Restore cache (remove let-bound values)
            self.value_cache = old_cache;

            Ok(result)
        } else {
            Ok(None)
        }
    }

    /// Copy node with optimized children
    fn copy_node_optimized(&mut self, node_id: NodeId) -> Result<Option<NodeId>> {
        let node = match self.graph.as_ref().unwrap().get_node(node_id) {
            Some(n) => n.clone(),
            None => return Ok(None),
        };
        let optimized_node = match node {
            Node::Application { function, args } => {
                let opt_func = self.optimize_node(function)?;
                let mut opt_args = Vec::new();
                for arg in args {
                    if let Some(opt_arg) = self.optimize_node(arg)? {
                        opt_args.push(opt_arg);
                    }
                }
                
                if let Some(func_id) = opt_func {
                    Node::Application {
                        function: func_id,
                        args: opt_args,
                    }
                } else {
                    return Ok(None);
                }
            }
            Node::Lambda { params, body } => {
                if let Some(opt_body) = self.optimize_node(body)? {
                    Node::Lambda {
                        params: params.clone(),
                        body: opt_body,
                    }
                } else {
                    return Ok(None);
                }
            }
            Node::Let { bindings, body } => {
                let bindings = bindings.clone();
                let mut opt_bindings = Vec::new();
                for (name, value_id) in bindings {
                    if let Some(opt_value) = self.optimize_node(value_id)? {
                        opt_bindings.push((name.clone(), opt_value));
                    }
                }
                
                if let Some(opt_body) = self.optimize_node(body)? {
                    // Check for inline opportunities
                    if opt_bindings.len() == 1 && self.should_inline_let(&opt_bindings[0]) {
                        self.stats.inlined_expressions += 1;
                        return Ok(Some(opt_body));
                    }
                    
                    Node::Let {
                        bindings: opt_bindings,
                        body: opt_body,
                    }
                } else {
                    return Ok(None);
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                if let Some(opt_cond) = self.optimize_node(condition)? {
                    // Check for constant condition
                    if let Some(Node::Literal(Literal::Boolean(value))) = self.optimized.get_node(opt_cond) {
                        self.stats.branches_eliminated += 1;
                        return if *value {
                            self.optimize_node(then_branch)
                        } else {
                            self.optimize_node(else_branch)
                        };
                    }
                    
                    let opt_then = self.optimize_node(then_branch)?;
                    let opt_else = self.optimize_node(else_branch)?;
                    
                    if let (Some(then_id), Some(else_id)) = (opt_then, opt_else) {
                        Node::If {
                            condition: opt_cond,
                            then_branch: then_id,
                            else_branch: else_id,
                        }
                    } else {
                        return Ok(None);
                    }
                } else {
                    return Ok(None);
                }
            }
            _ => node.clone(),
        };

        let new_id = self.optimized.add_node(optimized_node);
        self.node_mapping.insert(node_id, new_id);
        Ok(Some(new_id))
    }

    /// Check if a let binding should be inlined
    fn should_inline_let(&self, binding: &(String, NodeId)) -> bool {
        if let Some(node) = self.optimized.get_node(binding.1) {
            match node {
                Node::Literal(_) => true,
                Node::Variable { .. } => true,
                Node::Lambda { .. } if self.count_nodes(binding.1) < self.inline_threshold => true,
                _ => false,
            }
        } else {
            false
        }
    }

    /// Count nodes in a subgraph
    fn count_nodes(&self, root: NodeId) -> usize {
        let mut count = 0;
        let mut visited = FxHashSet::default();
        let mut queue = vec![root];

        while let Some(node_id) = queue.pop() {
            if !visited.insert(node_id) {
                continue;
            }
            count += 1;

            if let Some(node) = self.optimized.get_node(node_id) {
                match node {
                    Node::Application { function, args } => {
                        queue.push(*function);
                        queue.extend(args);
                    }
                    Node::Lambda { body, .. } => queue.push(*body),
                    Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                        for (_, value_id) in bindings {
                            queue.push(*value_id);
                        }
                        queue.push(*body);
                    }
                    Node::If { condition, then_branch, else_branch } => {
                        queue.push(*condition);
                        queue.push(*then_branch);
                        queue.push(*else_branch);
                    }
                    _ => {}
                }
            }
        }

        count
    }

    /// Inline small functions
    fn inline_small_functions(&mut self) -> Result<()> {
        // Find function applications that can be inlined
        let nodes: Vec<_> = self.optimized.nodes.keys().copied().collect();
        
        for node_id in nodes {
            if let Some(Node::Application { function, args }) = self.optimized.get_node(node_id).cloned() {
                if let Some(Node::Lambda { params, body }) = self.optimized.get_node(function).cloned() {
                    if params.len() == args.len() && self.count_nodes(body) < self.inline_threshold {
                        // Perform beta reduction
                        let mut substitutions = FxHashMap::default();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            substitutions.insert(param.clone(), *arg);
                        }
                        
                        if let Some(inlined) = self.substitute_node(body, &substitutions) {
                            self.optimized.nodes.insert(node_id, inlined);
                            self.stats.inlined_expressions += 1;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Substitute variables in a node
    fn substitute_node(&self, node_id: NodeId, substitutions: &FxHashMap<String, NodeId>) -> Option<Node> {
        let node = self.optimized.get_node(node_id)?;
        
        match node {
            Node::Variable { name } => {
                if let Some(subst_id) = substitutions.get(name) {
                    self.optimized.get_node(*subst_id).cloned()
                } else {
                    Some(node.clone())
                }
            }
            Node::Application { function, args } => {
                Some(Node::Application {
                    function: *function,
                    args: args.clone(),
                })
            }
            _ => Some(node.clone()),
        }
    }

    /// Optimize tail calls
    fn optimize_tail_calls(&mut self) -> Result<()> {
        // Find tail recursive functions
        let nodes: Vec<_> = self.optimized.nodes.keys().copied().collect();
        
        for node_id in nodes {
            if let Some(Node::Letrec { bindings, body: _ }) = self.optimized.get_node(node_id).cloned() {
                for (func_name, func_id) in &bindings {
                    if let Some(Node::Lambda { params: _, body: lambda_body }) = self.optimized.get_node(*func_id) {
                        if self.is_tail_recursive(func_name, *lambda_body) {
                            // Mark for tail call optimization
                            self.stats.tail_calls_optimized += 1;
                            // TODO: Transform to loop in VM
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Check if a function body contains tail recursion
    fn is_tail_recursive(&self, func_name: &str, body: NodeId) -> bool {
        self.is_tail_position(func_name, body, true)
    }

    /// Check if a node is in tail position
    fn is_tail_position(&self, func_name: &str, node_id: NodeId, is_tail: bool) -> bool {
        if let Some(node) = self.optimized.get_node(node_id) {
            match node {
                Node::Application { function, .. } => {
                    if is_tail {
                        if let Some(Node::Variable { name }) = self.optimized.get_node(*function) {
                            return name == func_name;
                        }
                    }
                    false
                }
                Node::If { then_branch, else_branch, .. } => {
                    self.is_tail_position(func_name, *then_branch, is_tail) ||
                    self.is_tail_position(func_name, *else_branch, is_tail)
                }
                Node::Let { body, .. } => {
                    self.is_tail_position(func_name, *body, is_tail)
                }
                _ => false,
            }
        } else {
            false
        }
    }

    /// Beta reduction for immediately applied lambdas
    fn beta_reduction(&mut self) -> Result<()> {
        // Find (lambda (...) body) applications
        let nodes: Vec<_> = self.optimized.nodes.keys().copied().collect();
        
        for node_id in nodes {
            if let Some(Node::Application { function, args }) = self.optimized.get_node(node_id).cloned() {
                if let Some(Node::Lambda { params, body }) = self.optimized.get_node(function).cloned() {
                    if params.len() == args.len() {
                        // Direct beta reduction
                        let mut substitutions = FxHashMap::default();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            substitutions.insert(param.clone(), *arg);
                        }
                        
                        if let Some(reduced) = self.substitute_node(body, &substitutions) {
                            self.optimized.nodes.insert(node_id, reduced);
                            self.stats.inlined_expressions += 1;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Loop optimizations
    fn loop_optimizations(&mut self) -> Result<()> {
        // Simple loop unrolling for small constant loops
        // This is a placeholder for more sophisticated loop optimizations
        
        // TODO: Implement loop detection and optimization
        // - Loop unrolling for small constant bounds
        // - Loop fusion for adjacent loops
        // - Loop invariant code motion
        
        Ok(())
    }

    /// Dead code elimination
    fn eliminate_dead_code(&mut self) -> Result<()> {
        let mut reachable = FxHashSet::default();
        
        // Mark reachable nodes
        if let Some(root) = self.optimized.root_id {
            self.mark_reachable(root, &mut reachable);
        }

        // Remove unreachable nodes
        let unreachable: Vec<_> = self.optimized.nodes.keys()
            .filter(|id| !reachable.contains(id))
            .copied()
            .collect();
        
        for id in unreachable {
            self.optimized.nodes.remove(&id);
            self.stats.dead_code_eliminated += 1;
        }

        Ok(())
    }

    /// Mark reachable nodes
    fn mark_reachable(&self, node_id: NodeId, reachable: &mut FxHashSet<NodeId>) {
        if !reachable.insert(node_id) {
            return;
        }

        if let Some(node) = self.optimized.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    self.mark_reachable(*function, reachable);
                    for arg in args {
                        self.mark_reachable(*arg, reachable);
                    }
                }
                Node::Lambda { body, .. } => {
                    self.mark_reachable(*body, reachable);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value_id) in bindings {
                        self.mark_reachable(*value_id, reachable);
                    }
                    self.mark_reachable(*body, reachable);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.mark_reachable(*condition, reachable);
                    self.mark_reachable(*then_branch, reachable);
                    self.mark_reachable(*else_branch, reachable);
                }
                _ => {}
            }
        }
    }
}

impl Default for AdvancedOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if a function is a pure primitive
fn is_pure_primitive(name: &str) -> bool {
    matches!(name,
        "+" | "-" | "*" | "/" | "mod" |
        "<" | ">" | "<=" | ">=" | "=" | "==" | "!=" | "<>" |
        "and" | "or" | "not" |
        "car" | "cdr" | "cons" | "list" |
        "list-len" | "list-empty?" |
        "str-len" | "str-concat" | "str-upper" | "str-lower"
    )
}

/// Evaluate a primitive function
fn evaluate_primitive(func_name: &str, args: &[Literal]) -> Result<Option<Literal>> {
    use Literal::*;

    let result = match (func_name, args) {
        // Arithmetic
        ("+", [Integer(a), Integer(b)]) => Integer(a + b),
        ("-", [Integer(a), Integer(b)]) => Integer(a - b),
        ("*", [Integer(a), Integer(b)]) => Integer(a * b),
        ("/", [Integer(a), Integer(b)]) if *b != 0 => Integer(a / b),
        ("mod", [Integer(a), Integer(b)]) if *b != 0 => Integer(a % b),
        
        // Floating point
        ("+", [Float(a), Float(b)]) => Float(a + b),
        ("-", [Float(a), Float(b)]) => Float(a - b),
        ("*", [Float(a), Float(b)]) => Float(a * b),
        ("/", [Float(a), Float(b)]) if *b != 0.0 => Float(a / b),
        
        // Comparison
        ("<", [Integer(a), Integer(b)]) => Boolean(a < b),
        (">", [Integer(a), Integer(b)]) => Boolean(a > b),
        ("<=", [Integer(a), Integer(b)]) => Boolean(a <= b),
        (">=", [Integer(a), Integer(b)]) => Boolean(a >= b),
        ("=" | "==", [Integer(a), Integer(b)]) => Boolean(a == b),
        ("!=" | "<>", [Integer(a), Integer(b)]) => Boolean(a != b),
        
        // Boolean
        ("and", [Boolean(a), Boolean(b)]) => Boolean(*a && *b),
        ("or", [Boolean(a), Boolean(b)]) => Boolean(*a || *b),
        ("not", [Boolean(a)]) => Boolean(!a),
        
        // String operations
        ("str-concat", [String(a), String(b)]) => String(format!("{}{}", a, b)),
        ("str-len", [String(s)]) => Integer(s.len() as i64),
        ("str-upper", [String(s)]) => String(s.to_uppercase()),
        ("str-lower", [String(s)]) => String(s.to_lowercase()),
        
        _ => return Ok(None),
    };

    Ok(Some(result))
}