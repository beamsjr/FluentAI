//! Partial evaluation pass

use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::{FxHashMap, FxHashSet};
use anyhow::Result;
use crate::passes::OptimizationPass;
use crate::analysis::EffectAnalysis;

/// Partial evaluation pass
pub struct PartialEvaluationPass {
    evaluated_count: usize,
}

impl PartialEvaluationPass {
    /// Create new partial evaluation pass
    pub fn new() -> Self {
        Self { evaluated_count: 0 }
    }

    /// Try to partially evaluate a node
    fn try_partial_eval(&self, 
                        graph: &Graph, 
                        node: &Node,
                        known_values: &FxHashMap<String, Literal>,
                        effect_analysis: &EffectAnalysis) -> Option<Node> {
        match node {
            Node::Variable { name } => {
                // If we know the value of this variable, replace it with a literal
                known_values.get(name).map(|lit| Node::Literal(lit.clone()))
            }
            Node::If { condition, then_branch, else_branch } => {
                // Try to evaluate the condition
                if let Some(Node::Literal(Literal::Boolean(cond_val))) = 
                    graph.get_node(*condition).and_then(|n| 
                        self.try_partial_eval(graph, n, known_values, effect_analysis)) {
                    // If condition is known, select the appropriate branch
                    let branch_id = if cond_val { *then_branch } else { *else_branch };
                    
                    // Check if the branch is pure and can be inlined
                    if effect_analysis.pure_nodes.contains(&branch_id) {
                        return graph.get_node(branch_id).cloned();
                    }
                }
                None
            }
            Node::Application { function, args } => {
                // Check if this is a pure function application with some known arguments
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    if is_pure_builtin(name) && effect_analysis.pure_nodes.contains(function) {
                        let mut arg_values = Vec::new();
                        let mut all_known = true;
                        
                        for arg_id in args {
                            if let Some(arg_node) = graph.get_node(*arg_id) {
                                if let Some(Node::Literal(lit)) = 
                                    self.try_partial_eval(graph, arg_node, known_values, effect_analysis) {
                                    arg_values.push(lit);
                                } else if let Node::Literal(lit) = arg_node {
                                    arg_values.push(lit.clone());
                                } else {
                                    all_known = false;
                                    break;
                                }
                            } else {
                                all_known = false;
                                break;
                            }
                        }
                        
                        if all_known {
                            // All arguments are known, evaluate the function
                            return evaluate_builtin(name, &arg_values);
                        } else if !arg_values.is_empty() {
                            // Some arguments are known, try partial evaluation
                            return partial_evaluate_builtin(name, args, &arg_values, graph);
                        }
                    }
                }
                None
            }
            Node::Let { bindings, body } => {
                // Build known values from bindings
                let mut new_known = known_values.clone();
                let mut any_evaluated = false;
                
                for (var_name, value_id) in bindings {
                    if let Some(value_node) = graph.get_node(*value_id) {
                        if let Some(Node::Literal(lit)) = 
                            self.try_partial_eval(graph, value_node, &new_known, effect_analysis) {
                            new_known.insert(var_name.clone(), lit);
                            any_evaluated = true;
                        }
                    }
                }
                
                if any_evaluated {
                    // Try to evaluate the body with new known values
                    if let Some(body_node) = graph.get_node(*body) {
                        return self.try_partial_eval(graph, body_node, &new_known, effect_analysis);
                    }
                }
                None
            }
            _ => None,
        }
    }
}

impl OptimizationPass for PartialEvaluationPass {
    fn name(&self) -> &str {
        "Partial Evaluation"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.evaluated_count = 0;
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        
        // Perform effect analysis
        let effect_analysis = EffectAnalysis::analyze(graph);
        
        // Track known values during evaluation
        let known_values = FxHashMap::default();
        
        // Process all nodes
        for (node_id, node) in &graph.nodes {
            if let Some(evaluated) = self.try_partial_eval(graph, node, &known_values, &effect_analysis) {
                let new_id = optimized.add_node(evaluated);
                node_mapping.insert(*node_id, new_id);
                self.evaluated_count += 1;
            } else {
                // Copy node with mapped references
                let mapped_node = map_node_refs(node, &node_mapping);
                let new_id = optimized.add_node(mapped_node);
                node_mapping.insert(*node_id, new_id);
            }
        }
        
        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }
        
        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!("{} pass: {} expressions partially evaluated", self.name(), self.evaluated_count)
    }
}

/// Check if a function is a pure builtin
fn is_pure_builtin(name: &str) -> bool {
    matches!(name,
        "+" | "-" | "*" | "/" | "mod" |
        "<" | ">" | "<=" | ">=" | "=" | "!=" |
        "and" | "or" | "not" |
        "str-len" | "str-concat" | "str-upper" | "str-lower"
    )
}

/// Evaluate a builtin function with all arguments known
fn evaluate_builtin(name: &str, args: &[Literal]) -> Option<Node> {
    use Literal::*;
    
    let result = match (name, args) {
        // Arithmetic
        ("+", [Integer(a), Integer(b)]) => Integer(a + b),
        ("-", [Integer(a), Integer(b)]) => Integer(a - b),
        ("*", [Integer(a), Integer(b)]) => Integer(a * b),
        ("/", [Integer(a), Integer(b)]) if *b != 0 => Integer(a / b),
        ("mod", [Integer(a), Integer(b)]) if *b != 0 => Integer(a % b),
        
        // Comparison
        ("<", [Integer(a), Integer(b)]) => Boolean(a < b),
        (">", [Integer(a), Integer(b)]) => Boolean(a > b),
        ("<=", [Integer(a), Integer(b)]) => Boolean(a <= b),
        (">=", [Integer(a), Integer(b)]) => Boolean(a >= b),
        ("=", [Integer(a), Integer(b)]) => Boolean(a == b),
        ("!=", [Integer(a), Integer(b)]) => Boolean(a != b),
        
        // Boolean
        ("and", [Boolean(a), Boolean(b)]) => Boolean(*a && *b),
        ("or", [Boolean(a), Boolean(b)]) => Boolean(*a || *b),
        ("not", [Boolean(a)]) => Boolean(!a),
        
        // String operations
        ("str-len", [String(s)]) => Integer(s.len() as i64),
        ("str-concat", [String(a), String(b)]) => String(format!("{}{}", a, b)),
        ("str-upper", [String(s)]) => String(s.to_uppercase()),
        ("str-lower", [String(s)]) => String(s.to_lowercase()),
        
        _ => return None,
    };
    
    Some(Node::Literal(result))
}

/// Partially evaluate a builtin when some arguments are known
fn partial_evaluate_builtin(name: &str, 
                            args: &[NodeId], 
                            known_args: &[Literal],
                            graph: &Graph) -> Option<Node> {
    use Literal::*;
    
    // Special cases where partial evaluation can simplify
    match (name, args.len()) {
        // Arithmetic identities
        ("+", 2) if known_args.len() == 1 => {
            if let Integer(0) = known_args[0] {
                // 0 + x = x or x + 0 = x
                let other_idx = if args[0] == args[0] { 1 } else { 0 };
                return graph.get_node(args[other_idx]).cloned();
            }
        }
        ("*", 2) if known_args.len() == 1 => {
            match known_args[0] {
                Integer(0) => {
                    // 0 * x = 0 or x * 0 = 0
                    return Some(Node::Literal(Integer(0)));
                }
                Integer(1) => {
                    // 1 * x = x or x * 1 = x
                    let other_idx = if args[0] == args[0] { 1 } else { 0 };
                    return graph.get_node(args[other_idx]).cloned();
                }
                _ => {}
            }
        }
        // Boolean short-circuit
        ("and", 2) if known_args.len() == 1 => {
            if let Boolean(false) = known_args[0] {
                // false && x = false
                return Some(Node::Literal(Boolean(false)));
            }
        }
        ("or", 2) if known_args.len() == 1 => {
            if let Boolean(true) = known_args[0] {
                // true || x = true
                return Some(Node::Literal(Boolean(true)));
            }
        }
        _ => {}
    }
    
    None
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
        Node::Match { expr, branches } => {
            let new_expr = mapping.get(expr).copied().unwrap_or(*expr);
            let new_branches: Vec<_> = branches.iter()
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
        _ => node.clone(),
    }
}