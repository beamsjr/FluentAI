//! Graph query language for searching and analyzing AST

use crate::patterns::{Pattern, PatternMatcher, MatchResult};
use crate::error::{MetaprogrammingError, Result};
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};

/// A query over the AST graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphQuery {
    /// The pattern to match
    pub pattern: Pattern,
    /// Optional constraints on the match
    pub constraints: Vec<Constraint>,
    /// What to select from matches
    pub select: SelectClause,
    /// Optional ordering
    pub order_by: Option<OrderBy>,
    /// Optional limit
    pub limit: Option<usize>,
}

/// Constraints on pattern matches
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constraint {
    /// Path constraint between nodes
    Path {
        from: String,
        to: String,
        max_length: Option<usize>,
    },
    /// Ancestor/descendant relationship
    Ancestor {
        ancestor: String,
        descendant: String,
    },
    /// Sibling relationship
    Sibling {
        node1: String,
        node2: String,
    },
    /// Custom predicate
    Predicate {
        name: String,
        args: Vec<String>,
    },
}

/// What to select from the query
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SelectClause {
    /// Select all matched nodes
    All,
    /// Select specific bound variables
    Bindings(Vec<String>),
    /// Select with transformation
    Transform(String),
    /// Count matches
    Count,
}

/// Ordering specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrderBy {
    pub field: String,
    pub ascending: bool,
}

/// Result of a graph query
#[derive(Debug, Clone)]
pub struct QueryResult {
    pub matches: Vec<MatchResult>,
    pub total_count: usize,
}

/// Query executor
pub struct QueryExecutor<'a> {
    graph: &'a Graph,
}

impl<'a> QueryExecutor<'a> {
    pub fn new(graph: &'a Graph) -> Self {
        Self { graph }
    }
    
    /// Execute a query
    pub fn execute(&self, query: &GraphQuery) -> Result<QueryResult> {
        // Find all matches
        let matches = self.find_matches(&query.pattern)?;
        
        // Apply constraints
        let filtered = self.apply_constraints(matches, &query.constraints)?;
        
        // Apply ordering
        let ordered = if let Some(order) = &query.order_by {
            self.apply_ordering(filtered, order)?
        } else {
            filtered
        };
        
        // Apply limit
        let limited = if let Some(limit) = query.limit {
            ordered.into_iter().take(limit).collect()
        } else {
            ordered
        };
        
        // Apply selection
        let selected = self.apply_selection(limited, &query.select)?;
        
        Ok(QueryResult {
            total_count: selected.len(),
            matches: selected,
        })
    }
    
    /// Find all matches for a pattern in the graph
    fn find_matches(&self, pattern: &Pattern) -> Result<Vec<MatchResult>> {
        let mut matches = Vec::new();
        let mut matcher = PatternMatcher::new(self.graph);
        
        // Try to match pattern against every node
        for &node_id in self.graph.nodes.keys() {
            if let Some(result) = matcher.match_pattern(pattern, node_id) {
                matches.push(result);
            }
        }
        
        Ok(matches)
    }
    
    /// Apply constraints to filter matches
    fn apply_constraints(&self, matches: Vec<MatchResult>, constraints: &[Constraint]) -> Result<Vec<MatchResult>> {
        let mut filtered = matches;
        
        for constraint in constraints {
            filtered = filtered.into_iter()
                .filter(|m| self.check_constraint(m, constraint))
                .collect();
        }
        
        Ok(filtered)
    }
    
    /// Check if a match satisfies a constraint
    fn check_constraint(&self, match_result: &MatchResult, constraint: &Constraint) -> bool {
        match constraint {
            Constraint::Path { from, to, max_length } => {
                if let (Some(&from_id), Some(&to_id)) = 
                    (match_result.bindings.get(from), match_result.bindings.get(to)) {
                    self.has_path(from_id, to_id, *max_length)
                } else {
                    false
                }
            }
            
            Constraint::Ancestor { ancestor, descendant } => {
                if let (Some(&anc_id), Some(&desc_id)) = 
                    (match_result.bindings.get(ancestor), match_result.bindings.get(descendant)) {
                    self.is_ancestor(anc_id, desc_id)
                } else {
                    false
                }
            }
            
            Constraint::Sibling { node1, node2 } => {
                if let (Some(&id1), Some(&id2)) = 
                    (match_result.bindings.get(node1), match_result.bindings.get(node2)) {
                    self.are_siblings(id1, id2)
                } else {
                    false
                }
            }
            
            Constraint::Predicate { name, args } => {
                self.check_predicate(match_result, name, args)
            }
        }
    }
    
    /// Check if there's a path between two nodes
    fn has_path(&self, from: NodeId, to: NodeId, max_length: Option<usize>) -> bool {
        // Simple BFS for path finding
        let mut visited = FxHashSet::default();
        let mut queue = vec![(from, 0)];
        
        while let Some((current, depth)) = queue.pop() {
            if current == to {
                return true;
            }
            
            if let Some(max) = max_length {
                if depth >= max {
                    continue;
                }
            }
            
            if !visited.insert(current) {
                continue;
            }
            
            // Get children of current node
            if let Some(node) = self.graph.get_node(current) {
                for child in self.get_children(node) {
                    queue.push((child, depth + 1));
                }
            }
        }
        
        false
    }
    
    /// Check if one node is ancestor of another
    fn is_ancestor(&self, ancestor: NodeId, descendant: NodeId) -> bool {
        self.has_path(ancestor, descendant, None)
    }
    
    /// Check if two nodes are siblings
    fn are_siblings(&self, _id1: NodeId, _id2: NodeId) -> bool {
        // Would need parent tracking in the graph
        false
    }
    
    /// Check a custom predicate
    fn check_predicate(&self, _match_result: &MatchResult, name: &str, _args: &[String]) -> bool {
        match name {
            "is_pure" => true,
            "is_tail_recursive" => true,
            _ => false,
        }
    }
    
    /// Get children of a node
    fn get_children(&self, node: &Node) -> Vec<NodeId> {
        match node {
            Node::Lambda { body, .. } => vec![*body],
            Node::Application { function, args } => {
                let mut children = vec![*function];
                children.extend(args);
                children
            }
            Node::Let { bindings, body } => {
                let mut children: Vec<NodeId> = bindings.iter().map(|(_, v)| *v).collect();
                children.push(*body);
                children
            }
            Node::If { condition, then_branch, else_branch } => {
                vec![*condition, *then_branch, *else_branch]
            }
            Node::List(items) => items.clone(),
            _ => vec![],
        }
    }
    
    /// Apply ordering to matches
    fn apply_ordering(&self, matches: Vec<MatchResult>, _order: &OrderBy) -> Result<Vec<MatchResult>> {
        // Simplified - would need to implement proper ordering
        Ok(matches)
    }
    
    /// Apply selection clause
    fn apply_selection(&self, matches: Vec<MatchResult>, select: &SelectClause) -> Result<Vec<MatchResult>> {
        match select {
            SelectClause::All => Ok(matches),
            SelectClause::Bindings(names) => {
                // Filter bindings to only requested ones
                Ok(matches.into_iter().map(|mut m| {
                    m.bindings.retain(|k, _| names.contains(k));
                    m
                }).collect())
            }
            SelectClause::Transform(_transform) => {
                // Would apply transformation
                Ok(matches)
            }
            SelectClause::Count => {
                // For count, we just need the matches
                Ok(matches)
            }
        }
    }
}

/// Query builder for ergonomic query construction
pub struct QueryBuilder {
    pattern: Option<Pattern>,
    constraints: Vec<Constraint>,
    select: SelectClause,
    order_by: Option<OrderBy>,
    limit: Option<usize>,
}

impl QueryBuilder {
    pub fn new() -> Self {
        Self {
            pattern: None,
            constraints: Vec::new(),
            select: SelectClause::All,
            order_by: None,
            limit: None,
        }
    }
    
    pub fn pattern(mut self, pattern: Pattern) -> Self {
        self.pattern = Some(pattern);
        self
    }
    
    pub fn constraint(mut self, constraint: Constraint) -> Self {
        self.constraints.push(constraint);
        self
    }
    
    pub fn select(mut self, select: SelectClause) -> Self {
        self.select = select;
        self
    }
    
    pub fn order_by(mut self, field: impl Into<String>, ascending: bool) -> Self {
        self.order_by = Some(OrderBy {
            field: field.into(),
            ascending,
        });
        self
    }
    
    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }
    
    pub fn build(self) -> Result<GraphQuery> {
        Ok(GraphQuery {
            pattern: self.pattern.ok_or_else(|| {
                MetaprogrammingError::QueryError("Pattern is required".to_string())
            })?,
            constraints: self.constraints,
            select: self.select,
            order_by: self.order_by,
            limit: self.limit,
        })
    }
}

#[cfg(test)]
#[path = "query_tests.rs"]
mod query_tests;

