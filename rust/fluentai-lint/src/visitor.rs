//! AST visitor for traversing FluentAi code

use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashSet;

/// Trait for visiting AST nodes
pub trait Visitor {
    /// Visit a node
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        // Default implementation visits children
        match node {
            Node::Lambda { body, .. } => {
                self.visit_node_id(graph, *body);
            }
            Node::Application { function, args } => {
                self.visit_node_id(graph, *function);
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            Node::Let { bindings, body } => {
                for (_, value) in bindings {
                    self.visit_node_id(graph, *value);
                }
                self.visit_node_id(graph, *body);
            }
            Node::If { condition, then_branch, else_branch } => {
                self.visit_node_id(graph, *condition);
                self.visit_node_id(graph, *then_branch);
                self.visit_node_id(graph, *else_branch);
            }
            Node::List(items) => {
                for item in items {
                    self.visit_node_id(graph, *item);
                }
            }
            Node::Match { expr, branches } => {
                self.visit_node_id(graph, *expr);
                for (_pattern, body) in branches {
                    self.visit_node_id(graph, *body);
                }
            }
            Node::Effect { args, .. } => {
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            Node::Module { body, .. } => {
                self.visit_node_id(graph, *body);
            }
            Node::Async { body } => {
                self.visit_node_id(graph, *body);
            }
            Node::Await { expr } => {
                self.visit_node_id(graph, *expr);
            }
            Node::Spawn { expr } => {
                self.visit_node_id(graph, *expr);
            }
            Node::Send { channel, value } => {
                self.visit_node_id(graph, *channel);
                self.visit_node_id(graph, *value);
            }
            Node::Receive { channel } => {
                self.visit_node_id(graph, *channel);
            }
            _ => {
                // Leaf nodes: Literal, Variable, Import, Export, etc.
            }
        }
    }
    
    /// Visit a node by ID
    fn visit_node_id(&mut self, graph: &Graph, node_id: NodeId) {
        if let Some(node) = graph.get_node(node_id) {
            self.visit_node(graph, node_id, node);
        }
    }
    
    /// Visit the entire graph
    fn visit_graph(&mut self, graph: &Graph) {
        if let Some(root_id) = graph.root_id {
            self.visit_node_id(graph, root_id);
        }
    }
}

/// A visitor that tracks visited nodes to avoid cycles
pub struct SafeVisitor<V> {
    inner: V,
    visited: FxHashSet<NodeId>,
}

impl<V: Visitor> SafeVisitor<V> {
    pub fn new(inner: V) -> Self {
        Self {
            inner,
            visited: FxHashSet::default(),
        }
    }
}

impl<V: Visitor> Visitor for SafeVisitor<V> {
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        if self.visited.insert(node_id) {
            self.inner.visit_node(graph, node_id, node);
        }
    }
}

/// Visitor that collects all nodes of a specific type
pub struct NodeCollector<F> {
    predicate: F,
    collected: Vec<NodeId>,
}

impl<F> NodeCollector<F>
where
    F: FnMut(&Node) -> bool,
{
    pub fn new(predicate: F) -> Self {
        Self {
            predicate,
            collected: Vec::new(),
        }
    }
    
    pub fn collect(mut self, graph: &Graph) -> Vec<NodeId> {
        self.visit_graph(graph);
        self.collected
    }
}

impl<F> Visitor for NodeCollector<F>
where
    F: FnMut(&Node) -> bool,
{
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        if (self.predicate)(node) {
            self.collected.push(node_id);
        }
        // Continue traversal
        match node {
            Node::Lambda { body, .. } => {
                self.visit_node_id(graph, *body);
            }
            Node::Application { function, args } => {
                self.visit_node_id(graph, *function);
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            Node::Let { bindings, body } => {
                for (_, value) in bindings {
                    self.visit_node_id(graph, *value);
                }
                self.visit_node_id(graph, *body);
            }
            Node::If { condition, then_branch, else_branch } => {
                self.visit_node_id(graph, *condition);
                self.visit_node_id(graph, *then_branch);
                self.visit_node_id(graph, *else_branch);
            }
            Node::List(items) => {
                for item in items {
                    self.visit_node_id(graph, *item);
                }
            }
            Node::Match { expr, branches } => {
                self.visit_node_id(graph, *expr);
                for (_pattern, body) in branches {
                    self.visit_node_id(graph, *body);
                }
            }
            Node::Effect { args, .. } => {
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            Node::Module { body, .. } => {
                self.visit_node_id(graph, *body);
            }
            _ => {}
        }
    }
}