//! AI-driven analysis interface for the AST
//! 
//! This module provides traits and utilities to make the AST graph structure
//! compatible with graph neural networks and other AI/ML analysis tools.

use crate::ast::{Graph, Node, NodeId, NodeMetadata};

/// Trait that enables AI/ML analysis on AST graphs
/// 
/// This trait provides a unified interface for accessing graph structure
/// and node features, making it compatible with graph neural network
/// libraries without requiring data structure conversion.
#[cfg(feature = "ai-analysis")]
pub trait AstGraphInterface {
    /// Get all node IDs in the graph
    fn node_ids(&self) -> Vec<NodeId>;
    
    /// Get the feature vector for a specific node
    /// 
    /// Returns an empty vector if the node doesn't exist or has no metadata
    fn node_feature(&self, node: NodeId) -> Vec<f32>;
    
    /// Get all neighbors (children) of a node
    /// 
    /// For AST graphs, this returns the direct children of a node
    fn neighbors(&self, node: NodeId) -> Vec<NodeId>;
    
    /// Get the total number of nodes in the graph
    fn node_count(&self) -> usize;
    
    /// Get the total number of edges in the graph
    fn edge_count(&self) -> usize;
    
    /// Get all edges as (source, target) pairs
    fn edges(&self) -> Vec<(NodeId, NodeId)>;
    
    /// Get node type as a categorical feature
    fn node_type_id(&self, node: NodeId) -> u32;
    
    /// Check if a node exists in the graph
    fn has_node(&self, node: NodeId) -> bool;
    
    /// Get the root node ID if it exists
    fn root_node(&self) -> Option<NodeId>;
    
    /// Get node metadata if available
    fn node_metadata(&self, node: NodeId) -> Option<&NodeMetadata>;
    
    /// Get mutable node metadata for updating
    fn node_metadata_mut(&mut self, node: NodeId) -> Option<&mut NodeMetadata>;
}

/// Implementation of AstGraphInterface for the Graph type
#[cfg(feature = "ai-analysis")]
impl AstGraphInterface for Graph {
    fn node_ids(&self) -> Vec<NodeId> {
        self.nodes.keys().copied().collect()
    }
    
    fn node_feature(&self, node: NodeId) -> Vec<f32> {
        self.metadata
            .get(&node)
            .map(|m| m.feature_vector())
            .unwrap_or_else(|| vec![0.0; 16]) // Return zero vector for missing metadata
    }
    
    fn neighbors(&self, node: NodeId) -> Vec<NodeId> {
        self.children(node)
    }
    
    fn node_count(&self) -> usize {
        self.nodes.len()
    }
    
    fn edge_count(&self) -> usize {
        self.nodes
            .keys()
            .map(|&node_id| self.children(node_id).len())
            .sum()
    }
    
    fn edges(&self) -> Vec<(NodeId, NodeId)> {
        let mut edges = Vec::new();
        for &node_id in self.nodes.keys() {
            for child_id in self.children(node_id) {
                edges.push((node_id, child_id));
            }
        }
        edges
    }
    
    fn node_type_id(&self, node: NodeId) -> u32 {
        self.nodes
            .get(&node)
            .map(|n| node_type_to_id(n))
            .unwrap_or(0)
    }
    
    fn has_node(&self, node: NodeId) -> bool {
        self.nodes.contains_key(&node)
    }
    
    fn root_node(&self) -> Option<NodeId> {
        self.root_id
    }
    
    fn node_metadata(&self, node: NodeId) -> Option<&NodeMetadata> {
        self.metadata.get(&node)
    }
    
    fn node_metadata_mut(&mut self, node: NodeId) -> Option<&mut NodeMetadata> {
        self.metadata.get_mut(&node)
    }
}

/// Convert node type to a numeric ID for ML models
#[cfg(feature = "ai-analysis")]
fn node_type_to_id(node: &Node) -> u32 {
    match node {
        Node::Literal(_) => 1,
        Node::Variable { .. } => 2,
        Node::Lambda { .. } => 3,
        Node::Let { .. } => 4,
        Node::Letrec { .. } => 5,
        Node::If { .. } => 6,
        Node::Application { .. } => 7,
        Node::Effect { .. } => 8,
        Node::Handler { .. } => 9,
        Node::List(_) => 10,
        Node::Map(_) => 11,
        Node::Match { .. } => 12,
        Node::Module { .. } => 13,
        Node::Import { .. } => 14,
        Node::Export { .. } => 15,
        Node::QualifiedVariable { .. } => 16,
        Node::Define { .. } => 17,
        Node::Assignment { .. } => 18,
        Node::Begin { .. } => 19,
        Node::Async { .. } => 20,
        Node::Await { .. } => 21,
        Node::Spawn { .. } => 22,
        Node::Channel { .. } => 23,
        Node::Send { .. } => 24,
        Node::Receive { .. } => 25,
        Node::TrySend { .. } => 26,
        Node::TryReceive { .. } => 27,
        Node::Select { .. } => 28,
        Node::Actor { .. } => 29,
        Node::ActorSend { .. } => 30,
        Node::ActorReceive { .. } => 31,
        Node::Become { .. } => 32,
        Node::Try { .. } => 33,
        Node::Throw { .. } => 34,
        Node::Promise { .. } => 35,
        Node::PromiseAll { .. } => 36,
        Node::PromiseRace { .. } => 37,
        Node::Timeout { .. } => 38,
        Node::Contract { .. } => 39,
        Node::Surface { .. } => 40,
        Node::Space { .. } => 41,
        Node::Element { .. } => 42,
        Node::StateField { .. } => 43,
        Node::When { .. } => 44,
        Node::Disturb { .. } => 45,
        Node::Extern { .. } => 46,
        Node::Range { .. } => 47,
    }
}

/// Batch operations for efficient AI analysis
#[cfg(feature = "ai-analysis")]
pub trait BatchAstOperations: AstGraphInterface {
    /// Get feature vectors for multiple nodes at once
    fn batch_node_features(&self, nodes: &[NodeId]) -> Vec<Vec<f32>> {
        nodes.iter().map(|&id| self.node_feature(id)).collect()
    }
    
    /// Get neighbors for multiple nodes at once
    fn batch_neighbors(&self, nodes: &[NodeId]) -> Vec<Vec<NodeId>> {
        nodes.iter().map(|&id| self.neighbors(id)).collect()
    }
    
    /// Get node types for multiple nodes at once
    fn batch_node_types(&self, nodes: &[NodeId]) -> Vec<u32> {
        nodes.iter().map(|&id| self.node_type_id(id)).collect()
    }
}

/// Automatically implement BatchAstOperations for any type that implements AstGraphInterface
#[cfg(feature = "ai-analysis")]
impl<T: AstGraphInterface> BatchAstOperations for T {}

/// Tensor conversion utilities for AI models
#[cfg(feature = "ai-analysis")]
pub mod tensor_utils {
    use super::*;
    
    /// Convert graph to adjacency list representation
    pub fn to_adjacency_list<G: AstGraphInterface>(graph: &G) -> Vec<Vec<usize>> {
        let node_ids = graph.node_ids();
        let id_to_index: std::collections::HashMap<NodeId, usize> = 
            node_ids.iter().enumerate().map(|(i, &id)| (id, i)).collect();
        
        let mut adj_list = vec![vec![]; node_ids.len()];
        
        for (i, &node_id) in node_ids.iter().enumerate() {
            for neighbor in graph.neighbors(node_id) {
                if let Some(&j) = id_to_index.get(&neighbor) {
                    adj_list[i].push(j);
                }
            }
        }
        
        adj_list
    }
    
    /// Convert graph to edge index format (COO format for sparse matrices)
    /// Returns (edge_index, num_nodes) where edge_index is a 2xE matrix
    pub fn to_edge_index<G: AstGraphInterface>(graph: &G) -> (Vec<[usize; 2]>, usize) {
        let node_ids = graph.node_ids();
        let id_to_index: std::collections::HashMap<NodeId, usize> = 
            node_ids.iter().enumerate().map(|(i, &id)| (id, i)).collect();
        
        let mut edge_index = Vec::new();
        
        for edge in graph.edges() {
            if let (Some(&i), Some(&j)) = (id_to_index.get(&edge.0), id_to_index.get(&edge.1)) {
                edge_index.push([i, j]);
            }
        }
        
        (edge_index, node_ids.len())
    }
    
    /// Extract feature matrix from graph
    /// Returns a 2D matrix where each row is a node's feature vector
    pub fn extract_feature_matrix<G: AstGraphInterface>(graph: &G) -> Vec<Vec<f32>> {
        let node_ids = graph.node_ids();
        node_ids.iter().map(|&id| graph.node_feature(id)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Graph, Literal};
    
    #[test]
    fn test_ast_graph_interface_basic() {
        let mut graph = Graph::new();
        
        // Add some nodes
        let n1 = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let n2 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let n3 = graph.add_node(Node::Application {
            function: n2,
            args: vec![n1],
        }).unwrap();
        
        graph.root_id = Some(n3);
        
        // Test basic interface methods
        assert_eq!(graph.node_count(), 3);
        assert_eq!(graph.edge_count(), 2);
        assert!(graph.has_node(n1));
        assert_eq!(graph.root_node(), Some(n3));
        
        // Test node types
        assert_eq!(graph.node_type_id(n1), 1); // Literal
        assert_eq!(graph.node_type_id(n2), 2); // Variable
        assert_eq!(graph.node_type_id(n3), 7); // Application
        
        // Test neighbors
        assert_eq!(graph.neighbors(n3), vec![n2, n1]);
        assert!(graph.neighbors(n1).is_empty());
    }
    
    #[test]
    fn test_batch_operations() {
        let mut graph = Graph::new();
        
        let n1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let n2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let n3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
        
        let batch_types = graph.batch_node_types(&[n1, n2, n3]);
        assert_eq!(batch_types, vec![1, 1, 1]); // All literals
        
        let batch_features = graph.batch_node_features(&[n1, n2]);
        assert_eq!(batch_features.len(), 2);
        assert_eq!(batch_features[0].len(), 16); // Feature vector size
    }
    
    #[test]
    fn test_tensor_conversion() {
        use tensor_utils::*;
        
        let mut graph = Graph::new();
        
        let n1 = graph.add_node(Node::Literal(Literal::Boolean(true))).unwrap();
        let n2 = graph.add_node(Node::Variable { name: "y".to_string() }).unwrap();
        let _n3 = graph.add_node(Node::If {
            condition: n1,
            then_branch: n2,
            else_branch: n2, // If nodes require all three branches
        }).unwrap();
        
        // Test adjacency list
        let adj_list = to_adjacency_list(&graph);
        assert_eq!(adj_list.len(), 3);
        
        // Test edge index
        let (edge_index, num_nodes) = to_edge_index(&graph);
        assert_eq!(num_nodes, 3);
        assert_eq!(edge_index.len(), 3); // Three edges: n3->n1, n3->n2 (then), n3->n2 (else)
        
        // Test feature matrix
        let features = extract_feature_matrix(&graph);
        assert_eq!(features.len(), 3);
        assert_eq!(features[0].len(), 16);
    }
}