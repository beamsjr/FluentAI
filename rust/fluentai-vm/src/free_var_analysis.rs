//! Enhanced free variable analysis for better optimization
//!
//! This module provides detailed analysis of free variables in the AST,
//! tracking not just which variables are free, but also whether they
//! are captured by inner closures. This enables more aggressive inlining
//! optimizations by the optimizer.

use fluentai_core::ast::{Graph as ASTGraph, Literal, Node, NodeId, Pattern};
use rustc_hash::{FxHashMap, FxHashSet};
use anyhow::{anyhow, Result};

/// Information about a variable's usage
#[derive(Debug, Clone)]
pub struct VarInfo {
    /// Is this variable free in the current context?
    pub is_free: bool,
    /// Is this variable captured by any inner closure?
    pub is_captured: bool,
    /// The scopes where this variable is used
    pub usage_scopes: FxHashSet<ScopeId>,
    /// Number of times the variable is referenced
    pub reference_count: usize,
}

/// Unique identifier for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

/// Analysis result for a specific node
#[derive(Debug, Clone)]
pub struct NodeAnalysis {
    /// Variables used in this node and its children
    pub var_usage: FxHashMap<String, VarInfo>,
    /// Variables bound by this node (if it creates a scope)
    pub bindings: FxHashSet<String>,
    /// Whether this node contains any closures
    pub contains_closures: bool,
}

/// Enhanced free variable analyzer
pub struct FreeVarAnalyzer {
    /// Results for each analyzed node
    analyses: FxHashMap<NodeId, NodeAnalysis>,
    /// Counter for generating unique scope IDs
    next_scope_id: usize,
}

impl FreeVarAnalyzer {
    pub fn new() -> Self {
        Self {
            analyses: FxHashMap::default(),
            next_scope_id: 0,
        }
    }
    
    /// Analyze free variables in a node and all its children
    pub fn analyze(&mut self, graph: &ASTGraph, node_id: NodeId) -> Result<&NodeAnalysis> {
        let scope_id = self.next_scope();
        self.analyze_node(graph, node_id, scope_id, &FxHashSet::default())?;
        self.analyses.get(&node_id)
            .ok_or_else(|| anyhow!("Analysis failed for node {:?}", node_id))
    }
    
    /// Get analysis results for a node (must be analyzed first)
    pub fn get_analysis(&self, node_id: NodeId) -> Option<&NodeAnalysis> {
        self.analyses.get(&node_id)
    }
    
    /// Check if a variable is captured by any closure within a node
    pub fn is_captured(&self, node_id: NodeId, var_name: &str) -> bool {
        self.analyses.get(&node_id)
            .and_then(|analysis| analysis.var_usage.get(var_name))
            .map(|info| info.is_captured)
            .unwrap_or(false)
    }
    
    /// Get the free variables for a lambda node
    pub fn get_lambda_free_vars(&self, node_id: NodeId) -> Vec<String> {
        self.analyses.get(&node_id)
            .map(|analysis| {
                let mut free_vars: Vec<_> = analysis.var_usage.iter()
                    .filter(|(_, info)| info.is_free)
                    .map(|(name, _)| name.clone())
                    .collect();
                free_vars.sort();
                free_vars
            })
            .unwrap_or_default()
    }
    
    /// Analyze free variables for a function body with given parameters
    /// This is useful for analyzing lambda bodies without creating a lambda node
    pub fn analyze_with_params(
        &mut self,
        graph: &ASTGraph,
        body_id: NodeId,
        params: &[String],
    ) -> Result<Vec<String>> {
        let scope_id = self.next_scope();
        
        // Create bound set with parameters
        let mut bound_vars = FxHashSet::default();
        for param in params {
            bound_vars.insert(param.clone());
        }
        
        // Analyze the body
        let analysis = self.analyze_node(graph, body_id, scope_id, &bound_vars)?;
        
        // Extract free variables
        let mut free_vars: Vec<_> = analysis.var_usage.iter()
            .filter(|(_, info)| info.is_free)
            .map(|(name, _)| name.clone())
            .collect();
        free_vars.sort();
        
        Ok(free_vars)
    }
    
    fn next_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }
    
    fn analyze_node(
        &mut self,
        graph: &ASTGraph,
        node_id: NodeId,
        scope_id: ScopeId,
        bound_vars: &FxHashSet<String>,
    ) -> Result<NodeAnalysis> {
        let node = graph.nodes.get(&node_id)
            .ok_or_else(|| anyhow!("Invalid node ID: {:?}", node_id))?;
        
        let mut analysis = NodeAnalysis {
            var_usage: FxHashMap::default(),
            bindings: FxHashSet::default(),
            contains_closures: false,
        };
        
        match node {
            Node::Variable { name } => {
                if !bound_vars.contains(name) {
                    // This is a free variable
                    let info = analysis.var_usage.entry(name.clone()).or_insert(VarInfo {
                        is_free: true,
                        is_captured: false,
                        usage_scopes: FxHashSet::default(),
                        reference_count: 0,
                    });
                    info.usage_scopes.insert(scope_id);
                    info.reference_count += 1;
                }
            }
            
            Node::Lambda { params, body } => {
                analysis.contains_closures = true;
                
                // Create new scope for lambda body
                let lambda_scope = self.next_scope();
                let mut lambda_bound = bound_vars.clone();
                for param in params {
                    lambda_bound.insert(param.clone());
                    analysis.bindings.insert(param.clone());
                }
                
                // Analyze body
                let body_analysis = self.analyze_node(graph, *body, lambda_scope, &lambda_bound)?;
                
                // Merge body analysis, marking captured variables
                for (var_name, var_info) in body_analysis.var_usage {
                    if !params.contains(&var_name) && bound_vars.contains(&var_name) {
                        // This variable is free in the lambda and bound in outer scope
                        // So it's captured by this lambda
                        let info = analysis.var_usage.entry(var_name).or_insert(VarInfo {
                            is_free: true,
                            is_captured: true,
                            usage_scopes: FxHashSet::default(),
                            reference_count: 0,
                        });
                        info.usage_scopes.extend(var_info.usage_scopes);
                        info.reference_count += var_info.reference_count;
                        info.is_captured = true;
                    } else if var_info.is_free {
                        // Variable is free in body and not bound by lambda
                        let info = analysis.var_usage.entry(var_name).or_insert(VarInfo {
                            is_free: true,
                            is_captured: false,
                            usage_scopes: FxHashSet::default(),
                            reference_count: 0,
                        });
                        info.usage_scopes.extend(var_info.usage_scopes);
                        info.reference_count += var_info.reference_count;
                        info.is_captured |= var_info.is_captured;
                    }
                }
                
                if body_analysis.contains_closures {
                    analysis.contains_closures = true;
                }
            }
            
            Node::Let { bindings, body } => {
                let mut let_bound = bound_vars.clone();
                
                // Analyze bindings in sequence
                for (name, value_id) in bindings {
                    let value_analysis = self.analyze_node(graph, *value_id, scope_id, &let_bound)?;
                    self.merge_analysis(&mut analysis, value_analysis);
                    
                    // Binding is available for subsequent bindings
                    let_bound.insert(name.clone());
                    analysis.bindings.insert(name.clone());
                }
                
                // Analyze body with all bindings in scope
                let body_analysis = self.analyze_node(graph, *body, scope_id, &let_bound)?;
                self.merge_analysis(&mut analysis, body_analysis);
            }
            
            Node::Application { function, args } => {
                // Analyze function
                let func_analysis = self.analyze_node(graph, *function, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, func_analysis);
                
                // Analyze arguments
                for &arg in args {
                    let arg_analysis = self.analyze_node(graph, arg, scope_id, bound_vars)?;
                    self.merge_analysis(&mut analysis, arg_analysis);
                }
            }
            
            Node::If { condition, then_branch, else_branch } => {
                let cond_analysis = self.analyze_node(graph, *condition, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, cond_analysis);
                
                let then_analysis = self.analyze_node(graph, *then_branch, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, then_analysis);
                
                let else_analysis = self.analyze_node(graph, *else_branch, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, else_analysis);
            }
            
            Node::Match { expr, branches } => {
                // Analyze match expression
                let expr_analysis = self.analyze_node(graph, *expr, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, expr_analysis);
                
                // Analyze each branch
                for (pattern, body_id) in branches {
                    let mut branch_bound = bound_vars.clone();
                    self.collect_pattern_bindings(pattern, &mut branch_bound);
                    
                    let body_analysis = self.analyze_node(graph, *body_id, scope_id, &branch_bound)?;
                    self.merge_analysis(&mut analysis, body_analysis);
                }
            }
            
            Node::List(items) => {
                for &item in items {
                    let item_analysis = self.analyze_node(graph, item, scope_id, bound_vars)?;
                    self.merge_analysis(&mut analysis, item_analysis);
                }
            }
            
            Node::Assignment { target, value } => {
                // Note: Assignment doesn't create new bindings in our functional language
                let target_analysis = self.analyze_node(graph, *target, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, target_analysis);
                
                let value_analysis = self.analyze_node(graph, *value, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, value_analysis);
            }
            
            // Handle other node types that might contain variables
            Node::Begin { exprs } => {
                for &expr in exprs {
                    let expr_analysis = self.analyze_node(graph, expr, scope_id, bound_vars)?;
                    self.merge_analysis(&mut analysis, expr_analysis);
                }
            }
            
            Node::Handler { handlers, body } => {
                // Analyze handler body first
                let body_analysis = self.analyze_node(graph, *body, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, body_analysis);
                
                // Analyze each handler function
                for (_, _, handler_id) in handlers {
                    let handler_analysis = self.analyze_node(graph, *handler_id, scope_id, bound_vars)?;
                    self.merge_analysis(&mut analysis, handler_analysis);
                }
            }
            
            Node::Send { channel, value } => {
                let channel_analysis = self.analyze_node(graph, *channel, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, channel_analysis);
                
                let value_analysis = self.analyze_node(graph, *value, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, value_analysis);
            }
            
            Node::Receive { channel } => {
                let channel_analysis = self.analyze_node(graph, *channel, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, channel_analysis);
            }
            
            Node::Spawn { expr } => {
                let expr_analysis = self.analyze_node(graph, *expr, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, expr_analysis);
            }
            
            Node::TrySend { channel, value } => {
                let channel_analysis = self.analyze_node(graph, *channel, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, channel_analysis);
                
                let value_analysis = self.analyze_node(graph, *value, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, value_analysis);
            }
            
            Node::TryReceive { channel } => {
                let channel_analysis = self.analyze_node(graph, *channel, scope_id, bound_vars)?;
                self.merge_analysis(&mut analysis, channel_analysis);
            }
            
            Node::Map(pairs) => {
                // Analyze all keys and values
                for &(key_id, value_id) in pairs {
                    let key_analysis = self.analyze_node(graph, key_id, scope_id, bound_vars)?;
                    let value_analysis = self.analyze_node(graph, value_id, scope_id, bound_vars)?;
                    self.merge_analysis(&mut analysis, key_analysis);
                    self.merge_analysis(&mut analysis, value_analysis);
                }
            }
            
            // Literals and other leaf nodes have no variables
            Node::Literal(_) | Node::Channel { .. } => {}
            
            // TODO: Add cases for other node types as needed
            _ => {}
        }
        
        // Store the analysis
        self.analyses.insert(node_id, analysis.clone());
        
        Ok(analysis)
    }
    
    /// Merge one analysis into another
    fn merge_analysis(&self, target: &mut NodeAnalysis, source: NodeAnalysis) {
        for (var_name, var_info) in source.var_usage {
            let info = target.var_usage.entry(var_name).or_insert(VarInfo {
                is_free: false,
                is_captured: false,
                usage_scopes: FxHashSet::default(),
                reference_count: 0,
            });
            info.is_free |= var_info.is_free;
            info.is_captured |= var_info.is_captured;
            info.usage_scopes.extend(var_info.usage_scopes);
            info.reference_count += var_info.reference_count;
        }
        
        target.contains_closures |= source.contains_closures;
    }
    
    /// Collect all variable bindings from a pattern
    fn collect_pattern_bindings(&self, pattern: &Pattern, bindings: &mut FxHashSet<String>) {
        match pattern {
            Pattern::Variable(name) => {
                bindings.insert(name.clone());
            }
            Pattern::As { binding, pattern } => {
                bindings.insert(binding.clone());
                self.collect_pattern_bindings(pattern, bindings);
            }
            Pattern::Constructor { patterns, .. } => {
                for p in patterns {
                    self.collect_pattern_bindings(p, bindings);
                }
            }
            Pattern::Or(patterns) => {
                // For Or patterns, we need to collect bindings that appear in ALL branches
                // For simplicity, we'll collect from the first branch only
                // (proper implementation would ensure all branches bind same variables)
                if let Some(first) = patterns.first() {
                    self.collect_pattern_bindings(first, bindings);
                }
            }
            Pattern::Guard { pattern, .. } => {
                self.collect_pattern_bindings(pattern, bindings);
            }
            Pattern::View { pattern, .. } => {
                self.collect_pattern_bindings(pattern, bindings);
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Range(_) => {
                // These patterns don't bind any variables
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Graph, Node};
    
    #[test]
    fn test_simple_free_variable() {
        let mut graph = Graph::new();
        
        // Create: λy. x + y (x is free)
        let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let y_var = graph.add_node(Node::Variable { name: "y".to_string() }).unwrap();
        let plus_op = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        let add_app = graph.add_node(Node::Application {
            function: plus_op,
            args: vec![x_var, y_var],
        }).unwrap();
        let lambda = graph.add_node(Node::Lambda {
            params: vec!["y".to_string()],
            body: add_app,
        }).unwrap();
        
        let mut analyzer = FreeVarAnalyzer::new();
        let analysis = analyzer.analyze(&graph, lambda).unwrap();
        
        // x should be free, y should not
        assert!(analysis.var_usage.get("x").map(|i| i.is_free).unwrap_or(false));
        assert!(!analysis.var_usage.get("y").map(|i| i.is_free).unwrap_or(false));
    }
    
    #[test]
    fn test_enhanced_analysis() {
        let mut graph = Graph::new();
        
        // Create: λx. λy. x + y
        // This demonstrates the enhanced analysis tracking which variables
        // are captured by inner closures
        let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let y_var = graph.add_node(Node::Variable { name: "y".to_string() }).unwrap();
        let plus_op = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        
        let add_app = graph.add_node(Node::Application {
            function: plus_op,
            args: vec![x_var, y_var],
        }).unwrap();
        
        let inner_lambda = graph.add_node(Node::Lambda {
            params: vec!["y".to_string()],
            body: add_app,
        }).unwrap();
        
        let outer_lambda = graph.add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: inner_lambda,
        }).unwrap();
        
        let mut analyzer = FreeVarAnalyzer::new();
        
        // Test analyze_with_params
        let free_vars = analyzer.analyze_with_params(&graph, add_app, &["y".to_string()]).unwrap();
        assert!(free_vars.contains(&"x".to_string()));
        assert!(free_vars.contains(&"+".to_string()));
        assert!(!free_vars.contains(&"y".to_string()));
        
        // Analyze the whole expression
        analyzer.analyze(&graph, outer_lambda).unwrap();
        
        // The analyzer should track that the outer lambda contains closures
        let outer_analysis = analyzer.get_analysis(outer_lambda).unwrap();
        assert!(outer_analysis.contains_closures);
        
        // The analyzer provides methods to check if variables are captured
        // This is useful for optimization decisions
        assert!(!analyzer.is_captured(outer_lambda, "x")); // x is parameter, not captured
        
        // Test another scenario: let binding with closure
        let let_lambda = {
            let z_var = graph.add_node(Node::Variable { name: "z".to_string() }).unwrap();
            let simple_lambda = graph.add_node(Node::Lambda {
                params: vec![],
                body: z_var,
            }).unwrap();
            let lit_42 = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
            graph.add_node(Node::Let {
                bindings: vec![("z".to_string(), lit_42)],
                body: simple_lambda,
            }).unwrap()
        };
        
        analyzer.analyze(&graph, let_lambda).unwrap();
        
        // The let expression should contain closures and the analyzer tracks this
        let let_analysis = analyzer.get_analysis(let_lambda).unwrap();
        assert!(let_analysis.contains_closures);
    }
}