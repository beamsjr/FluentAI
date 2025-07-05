//! Pattern language for AST matching

use fluentai_core::ast::{Node, NodeId, Literal};
use serde::{Deserialize, Serialize};
use rustc_hash::FxHashMap;

/// Pattern for matching AST nodes
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    /// Match any node
    Any,
    
    /// Match a specific literal
    Literal(Literal),
    
    /// Match a variable and bind it
    Bind(String),
    
    /// Match a specific node type
    NodeType(NodePattern),
    
    /// Match with a predicate
    Predicate(String, Box<Pattern>),
    
    /// Match one of several patterns
    Or(Vec<Pattern>),
    
    /// Match all patterns
    And(Vec<Pattern>),
    
    /// Match if pattern doesn't match
    Not(Box<Pattern>),
    
    /// Reference to a previously bound variable
    Ref(String),
}

/// Pattern for specific node types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NodePattern {
    Variable { name: Option<String> },
    Lambda { params: Option<Vec<String>>, body: Box<Pattern> },
    Application { function: Box<Pattern>, args: Vec<Pattern> },
    Let { bindings: Vec<(String, Pattern)>, body: Box<Pattern> },
    If { condition: Box<Pattern>, then_branch: Box<Pattern>, else_branch: Box<Pattern> },
    List(Vec<Pattern>),
    Effect { effect_type: Option<String>, operation: Option<String>, args: Vec<Pattern> },
}

/// Result of pattern matching
#[derive(Debug, Clone)]
pub struct MatchResult {
    pub bindings: FxHashMap<String, NodeId>,
    pub matched_nodes: Vec<NodeId>,
}

impl MatchResult {
    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
            matched_nodes: Vec::new(),
        }
    }
    
    pub fn merge(&mut self, other: MatchResult) {
        self.bindings.extend(other.bindings);
        self.matched_nodes.extend(other.matched_nodes);
    }
}

/// Pattern matcher
pub struct PatternMatcher<'a> {
    graph: &'a fluentai_core::ast::Graph,
    bindings: FxHashMap<String, NodeId>,
}

impl<'a> PatternMatcher<'a> {
    pub fn new(graph: &'a fluentai_core::ast::Graph) -> Self {
        Self {
            graph,
            bindings: FxHashMap::default(),
        }
    }
    
    /// Match a pattern against a node
    pub fn match_pattern(&mut self, pattern: &Pattern, node_id: NodeId) -> Option<MatchResult> {
        let node = self.graph.get_node(node_id)?;
        
        match pattern {
            Pattern::Any => {
                let mut result = MatchResult::new();
                result.matched_nodes.push(node_id);
                Some(result)
            }
            
            Pattern::Literal(lit) => {
                if let Node::Literal(node_lit) = node {
                    if lit == node_lit {
                        let mut result = MatchResult::new();
                        result.matched_nodes.push(node_id);
                        Some(result)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            
            Pattern::Bind(name) => {
                let mut result = MatchResult::new();
                result.bindings.insert(name.clone(), node_id);
                result.matched_nodes.push(node_id);
                Some(result)
            }
            
            Pattern::NodeType(node_pattern) => {
                self.match_node_pattern(node_pattern, node, node_id)
            }
            
            Pattern::Predicate(name, inner) => {
                // First match the inner pattern
                let inner_result = self.match_pattern(inner, node_id)?;
                
                // Then apply predicate (would need predicate registry)
                if self.apply_predicate(name, node_id) {
                    Some(inner_result)
                } else {
                    None
                }
            }
            
            Pattern::Or(patterns) => {
                for pattern in patterns {
                    if let Some(result) = self.match_pattern(pattern, node_id) {
                        return Some(result);
                    }
                }
                None
            }
            
            Pattern::And(patterns) => {
                let mut combined_result = MatchResult::new();
                for pattern in patterns {
                    if let Some(result) = self.match_pattern(pattern, node_id) {
                        combined_result.merge(result);
                    } else {
                        return None;
                    }
                }
                Some(combined_result)
            }
            
            Pattern::Not(pattern) => {
                if self.match_pattern(pattern, node_id).is_none() {
                    let mut result = MatchResult::new();
                    result.matched_nodes.push(node_id);
                    Some(result)
                } else {
                    None
                }
            }
            
            Pattern::Ref(name) => {
                if let Some(&bound_id) = self.bindings.get(name) {
                    if bound_id == node_id {
                        let mut result = MatchResult::new();
                        result.matched_nodes.push(node_id);
                        Some(result)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
    
    fn match_node_pattern(&mut self, pattern: &NodePattern, node: &Node, node_id: NodeId) -> Option<MatchResult> {
        match (pattern, node) {
            (NodePattern::Variable { name }, Node::Variable { name: node_name }) => {
                if let Some(expected_name) = name {
                    if expected_name == node_name {
                        let mut result = MatchResult::new();
                        result.matched_nodes.push(node_id);
                        Some(result)
                    } else {
                        None
                    }
                } else {
                    let mut result = MatchResult::new();
                    result.matched_nodes.push(node_id);
                    Some(result)
                }
            }
            
            (NodePattern::Lambda { params, body }, Node::Lambda { params: node_params, body: node_body }) => {
                // Check params if specified
                if let Some(expected_params) = params {
                    if expected_params != node_params {
                        return None;
                    }
                }
                
                // Match body
                let mut result = self.match_pattern(body, *node_body)?;
                result.matched_nodes.push(node_id);
                Some(result)
            }
            
            (NodePattern::Application { function, args }, Node::Application { function: node_func, args: node_args }) => {
                if args.len() != node_args.len() {
                    return None;
                }
                
                let mut result = MatchResult::new();
                
                // Match function
                let func_result = self.match_pattern(function, *node_func)?;
                result.merge(func_result);
                
                // Match arguments
                for (pattern, &arg_id) in args.iter().zip(node_args.iter()) {
                    let arg_result = self.match_pattern(pattern, arg_id)?;
                    result.merge(arg_result);
                }
                
                result.matched_nodes.push(node_id);
                Some(result)
            }
            
            _ => None,
        }
    }
    
    fn apply_predicate(&self, name: &str, _node_id: NodeId) -> bool {
        // Placeholder for predicate evaluation
        // Would need a predicate registry
        match name {
            "pure" => true, // Check if node is pure
            "constant" => true, // Check if node is constant
            _ => false,
        }
    }
}

/// Pattern builder for ergonomic pattern construction
pub struct PatternBuilder;

impl PatternBuilder {
    pub fn any() -> Pattern {
        Pattern::Any
    }
    
    pub fn bind(name: impl Into<String>) -> Pattern {
        Pattern::Bind(name.into())
    }
    
    pub fn lit(literal: Literal) -> Pattern {
        Pattern::Literal(literal)
    }
    
    pub fn var(name: Option<impl Into<String>>) -> Pattern {
        Pattern::NodeType(NodePattern::Variable {
            name: name.map(|n| n.into()),
        })
    }
    
    pub fn lambda(body: Pattern) -> Pattern {
        Pattern::NodeType(NodePattern::Lambda {
            params: None,
            body: Box::new(body),
        })
    }
    
    pub fn app(function: Pattern, args: Vec<Pattern>) -> Pattern {
        Pattern::NodeType(NodePattern::Application {
            function: Box::new(function),
            args,
        })
    }
    
    pub fn or(patterns: Vec<Pattern>) -> Pattern {
        Pattern::Or(patterns)
    }
    
    pub fn and(patterns: Vec<Pattern>) -> Pattern {
        Pattern::And(patterns)
    }
    
    pub fn not(pattern: Pattern) -> Pattern {
        Pattern::Not(Box::new(pattern))
    }
}

#[cfg(test)]
#[path = "patterns_tests.rs"]
mod patterns_tests;