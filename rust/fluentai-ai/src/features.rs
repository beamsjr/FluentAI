//! Advanced feature extraction for AST nodes

use crate::error::Result;
use fluentai_core::ai_interface::AstGraphInterface;
use fluentai_core::ast::{Graph, Node, NodeId};
use std::collections::HashMap;

/// Advanced feature extractor for AST graphs
pub struct FeatureExtractor {
    /// Feature configuration
    config: FeatureConfig,
    /// Feature cache
    cache: HashMap<NodeId, Vec<f32>>,
}

/// Configuration for feature extraction
#[derive(Debug, Clone)]
pub struct FeatureConfig {
    /// Include structural features
    pub structural: bool,
    /// Include semantic features
    pub semantic: bool,
    /// Include context features
    pub contextual: bool,
    /// Feature vector dimension
    pub dimension: usize,
    /// Maximum depth for contextual features
    pub max_depth: usize,
}

impl Default for FeatureConfig {
    fn default() -> Self {
        Self {
            structural: true,
            semantic: true,
            contextual: true,
            dimension: 128,
            max_depth: 3,
        }
    }
}

impl FeatureExtractor {
    /// Create a new feature extractor
    pub fn new(config: FeatureConfig) -> Self {
        Self {
            config,
            cache: HashMap::new(),
        }
    }
    
    /// Extract features for all nodes in a graph
    pub fn extract_all(&mut self, graph: &Graph) -> Result<HashMap<NodeId, Vec<f32>>> {
        let mut features = HashMap::new();
        
        for node_id in graph.node_ids() {
            let feature_vec = self.extract_node_features(graph, node_id)?;
            features.insert(node_id, feature_vec);
        }
        
        Ok(features)
    }
    
    /// Extract features for a single node
    pub fn extract_node_features(&mut self, graph: &Graph, node_id: NodeId) -> Result<Vec<f32>> {
        // Check cache first
        if let Some(cached) = self.cache.get(&node_id) {
            return Ok(cached.clone());
        }
        
        let mut features = Vec::with_capacity(self.config.dimension);
        
        // Basic features from node metadata
        let basic_features = graph.node_feature(node_id);
        features.extend_from_slice(&basic_features);
        
        // Structural features
        if self.config.structural {
            let structural = self.extract_structural_features(graph, node_id)?;
            features.extend(structural);
        }
        
        // Semantic features
        if self.config.semantic {
            let semantic = self.extract_semantic_features(graph, node_id)?;
            features.extend(semantic);
        }
        
        // Contextual features
        if self.config.contextual {
            let contextual = self.extract_contextual_features(graph, node_id)?;
            features.extend(contextual);
        }
        
        // Pad or truncate to target dimension
        features.resize(self.config.dimension, 0.0);
        
        // Cache the result
        self.cache.insert(node_id, features.clone());
        
        Ok(features)
    }
    
    /// Extract structural features (graph topology)
    fn extract_structural_features(&self, graph: &Graph, node_id: NodeId) -> Result<Vec<f32>> {
        let mut features = Vec::with_capacity(32);
        
        // Degree features
        let in_degree = 0.0; // TODO: Implement parent tracking
        let out_degree = graph.children(node_id).len() as f32;
        features.push(in_degree);
        features.push(out_degree);
        features.push((in_degree + out_degree) / 2.0);
        
        // Depth approximation (using tree depth heuristic)
        let depth = 5.0; // TODO: Implement proper depth calculation
        features.push(depth / 10.0); // Normalize
        
        // Height of subtree
        let height = self.compute_height(graph, node_id) as f32;
        features.push(height / 10.0); // Normalize
        
        // Number of descendants
        let descendants = self.count_descendants(graph, node_id) as f32;
        features.push(descendants.ln() / 10.0); // Log scale
        
        // Branching factor statistics
        let (avg_branch, max_branch) = self.compute_branching_stats(graph, node_id);
        features.push(avg_branch / 5.0); // Normalize
        features.push(max_branch as f32 / 10.0); // Normalize
        
        // Path features
        let is_leaf = out_degree == 0.0;
        let is_root = false; // TODO: Implement root detection
        features.push(if is_leaf { 1.0 } else { 0.0 });
        features.push(if is_root { 1.0 } else { 0.0 });
        
        // Sibling features
        let sibling_count = self.count_siblings(graph, node_id) as f32;
        features.push(sibling_count / 10.0); // Normalize
        
        Ok(features)
    }
    
    /// Extract semantic features (node type specific)
    fn extract_semantic_features(&self, graph: &Graph, node_id: NodeId) -> Result<Vec<f32>> {
        let mut features = Vec::with_capacity(32);
        
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => {
                    // Variable name features
                    features.push(name.len() as f32 / 20.0);
                    features.push(if name.chars().any(|c| c.is_uppercase()) { 1.0 } else { 0.0 });
                    features.push(if name.contains('_') { 1.0 } else { 0.0 });
                    features.push(self.hash_string(name) as f32 / u32::MAX as f32);
                }
                Node::Lambda { .. } => {
                    // Lambda features
                    features.push(1.0); // Is lambda
                    let arity = graph.children(node_id).len() as f32;
                    features.push(arity / 10.0);
                }
                Node::Application { .. } => {
                    // Application features
                    features.push(0.5); // Is application
                    let arg_count = graph.children(node_id).len() as f32 - 1.0; // Minus function
                    features.push(arg_count / 10.0);
                }
                Node::Literal(lit) => {
                    // Literal features
                    use fluentai_core::ast::Literal;
                    match lit {
                        Literal::Integer(n) => {
                            features.push(0.1); // Type: integer
                            features.push(*n as f32 / 1000.0); // Normalized value
                        }
                        Literal::Float(f) => {
                            features.push(0.2); // Type: float
                            features.push(*f as f32 / 1000.0); // Normalized value
                        }
                        Literal::String(s) => {
                            features.push(0.3); // Type: string
                            features.push(s.len() as f32 / 100.0);
                        }
                        Literal::Boolean(b) => {
                            features.push(0.4); // Type: boolean
                            features.push(if *b { 1.0 } else { 0.0 });
                        }
                        _ => {
                            features.push(0.0); // Unknown type
                            features.push(0.0);
                        }
                    }
                }
                _ => {
                    // Default features for other node types
                    features.push(0.0);
                    features.push(0.0);
                }
            }
        }
        
        // Pad to fixed size
        while features.len() < 32 {
            features.push(0.0);
        }
        
        Ok(features)
    }
    
    /// Extract contextual features (neighborhood information)
    fn extract_contextual_features(&self, graph: &Graph, node_id: NodeId) -> Result<Vec<f32>> {
        let mut features = Vec::with_capacity(64);
        
        // Parent context - TODO: Implement parent tracking
        features.extend(vec![0.0; 16]);
        
        // Children context
        let children = graph.children(node_id);
        if !children.is_empty() {
            let child_features = self.aggregate_neighbor_features(graph, &children);
            features.extend_from_slice(&child_features[..16.min(child_features.len())]);
        } else {
            features.extend(vec![0.0; 16]);
        }
        
        // Sibling context
        let siblings = self.get_siblings(graph, node_id);
        if !siblings.is_empty() {
            let sibling_features = self.aggregate_neighbor_features(graph, &siblings);
            features.extend_from_slice(&sibling_features[..16.min(sibling_features.len())]);
        } else {
            features.extend(vec![0.0; 16]);
        }
        
        // Extended neighborhood (2-hop)
        let extended = self.get_extended_neighborhood(graph, node_id, 2);
        features.push(extended.len() as f32 / 50.0); // Normalize
        
        Ok(features)
    }
    
    /// Aggregate features from neighboring nodes
    fn aggregate_neighbor_features(&self, graph: &Graph, neighbors: &[NodeId]) -> Vec<f32> {
        let mut aggregated = vec![0.0; 16];
        
        for &neighbor in neighbors {
            let features = graph.node_feature(neighbor);
            for (i, &feat) in features.iter().enumerate() {
                if i < aggregated.len() {
                    aggregated[i] += feat;
                }
            }
        }
        
        // Average the features
        let count = neighbors.len() as f32;
        for feat in &mut aggregated {
            *feat /= count;
        }
        
        aggregated
    }
    
    
    /// Compute height of subtree rooted at node
    fn compute_height(&self, graph: &Graph, node_id: NodeId) -> usize {
        let children = graph.children(node_id);
        if children.is_empty() {
            return 0;
        }
        
        children.iter()
            .map(|&child| self.compute_height(graph, child) + 1)
            .max()
            .unwrap_or(0)
    }
    
    /// Count descendants of a node
    fn count_descendants(&self, graph: &Graph, node_id: NodeId) -> usize {
        let mut count = 0;
        let mut stack = vec![node_id];
        let mut visited = std::collections::HashSet::new();
        
        while let Some(current) = stack.pop() {
            if visited.insert(current) {
                count += 1;
                stack.extend(graph.children(current));
            }
        }
        
        count - 1 // Exclude the node itself
    }
    
    /// Compute branching factor statistics
    fn compute_branching_stats(&self, graph: &Graph, node_id: NodeId) -> (f32, usize) {
        let mut total_branches = 0;
        let mut max_branches = 0;
        let mut node_count = 0;
        
        let mut stack = vec![node_id];
        let mut visited = std::collections::HashSet::new();
        
        while let Some(current) = stack.pop() {
            if visited.insert(current) {
                let children = graph.children(current);
                let branch_count = children.len();
                
                if branch_count > 0 {
                    total_branches += branch_count;
                    max_branches = max_branches.max(branch_count);
                    node_count += 1;
                }
                
                stack.extend(children);
            }
        }
        
        let avg_branches = if node_count > 0 {
            total_branches as f32 / node_count as f32
        } else {
            0.0
        };
        
        (avg_branches, max_branches)
    }
    
    /// Count siblings of a node
    fn count_siblings(&self, graph: &Graph, node_id: NodeId) -> usize {
        self.get_siblings(graph, node_id).len()
    }
    
    /// Get siblings of a node
    fn get_siblings(&self, _graph: &Graph, _node_id: NodeId) -> Vec<NodeId> {
        // TODO: Implement sibling detection when parent tracking is available
        Vec::new()
    }
    
    /// Get extended neighborhood
    fn get_extended_neighborhood(&self, graph: &Graph, node_id: NodeId, max_hops: usize) -> Vec<NodeId> {
        let mut neighborhood = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut current_level = vec![node_id];
        
        for _ in 0..max_hops {
            let mut next_level = Vec::new();
            
            for &current in &current_level {
                if visited.insert(current) {
                    neighborhood.push(current);
                    
                    // Add neighbors
                    next_level.extend(graph.children(current));
                    // TODO: Add parents when available
                }
            }
            
            current_level = next_level;
        }
        
        neighborhood
    }
    
    /// Simple string hashing for features
    fn hash_string(&self, s: &str) -> u32 {
        let mut hash = 0u32;
        for (_i, byte) in s.bytes().enumerate() {
            hash = hash.wrapping_add(byte as u32);
            hash = hash.wrapping_add(hash << 10);
            hash ^= hash >> 6;
        }
        hash = hash.wrapping_add(hash << 3);
        hash ^= hash >> 11;
        hash = hash.wrapping_add(hash << 15);
        hash
    }
}

/// Extract feature vector from a graph (convenience function)
pub fn extract_graph_features(graph: &Graph) -> Result<Vec<f32>> {
    let mut extractor = FeatureExtractor::new(FeatureConfig::default());
    
    // Extract basic graph-level features
    let node_count = graph.nodes.len() as f32;
    // TODO: Re-enable when compute_depth and compute_branching_factor are implemented
    let max_depth = 0.0; // extractor.compute_depth(graph, graph.root()) as f32;
    let (avg_branching, max_branching) = (0.0, 0.0); // extractor.compute_branching_factor(graph, graph.root());
    
    // Count different node types
    let mut type_counts = HashMap::new();
    for (_, node) in &graph.nodes {
        let node_type = match node {
            Node::Literal(_) => "literal",
            Node::Variable { .. } => "variable",
            Node::Lambda { .. } => "lambda",
            Node::Application { .. } => "application",
            Node::Let { .. } => "let",
            Node::If { .. } => "if",
            Node::Begin { .. } => "begin",
            Node::Define { .. } => "define",
            // Node::Set { .. } => "set", // TODO: Add when Set node type is added
            Node::List { .. } => "list",
            Node::Map { .. } => "map",
            Node::Channel { .. } => "channel",
            Node::Spawn { .. } => "spawn",
            Node::Send { .. } => "send",
            Node::Receive { .. } => "receive",
            Node::Select { .. } => "select",
            Node::Actor { .. } => "actor",
            Node::Handler { .. } => "handler", // Fixed: was Handle
            Node::Match { .. } => "match",
            Node::Effect { .. } => "effect",
            // Node::Perform { .. } => "perform", // TODO: Add when Perform node type is added
            _ => "other",
        };
        *type_counts.entry(node_type).or_insert(0.0) += 1.0;
    }
    
    // Build feature vector
    let mut features = vec![
        node_count,
        max_depth,
        avg_branching,
        max_branching as f32,
        type_counts.get("literal").cloned().unwrap_or(0.0),
        type_counts.get("variable").cloned().unwrap_or(0.0),
        type_counts.get("lambda").cloned().unwrap_or(0.0),
        type_counts.get("application").cloned().unwrap_or(0.0),
        type_counts.get("let").cloned().unwrap_or(0.0),
        type_counts.get("if").cloned().unwrap_or(0.0),
        type_counts.get("begin").cloned().unwrap_or(0.0),
        type_counts.get("define").cloned().unwrap_or(0.0),
        type_counts.get("list").cloned().unwrap_or(0.0),
        type_counts.get("map").cloned().unwrap_or(0.0),
        type_counts.get("channel").cloned().unwrap_or(0.0),
        type_counts.get("spawn").cloned().unwrap_or(0.0),
        type_counts.get("actor").cloned().unwrap_or(0.0),
        type_counts.get("effect").cloned().unwrap_or(0.0),
        type_counts.get("perform").cloned().unwrap_or(0.0),
        type_counts.get("match").cloned().unwrap_or(0.0),
    ];
    
    // Normalize features
    let max_val = features.iter().cloned().fold(0.0f32, f32::max).max(1.0);
    for f in &mut features {
        *f /= max_val;
    }
    
    // Pad to expected size
    while features.len() < 32 {
        features.push(0.0);
    }
    features.truncate(32);
    
    Ok(features)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Node, Literal};
    
    #[test]
    fn test_feature_extraction() {
        let mut graph = Graph::new();
        let n1 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let n2 = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let _n3 = graph.add_node(Node::Application {
            function: n1,
            args: vec![n2],
        }).unwrap();
        
        let mut extractor = FeatureExtractor::new(FeatureConfig::default());
        let features = extractor.extract_all(&graph).unwrap();
        
        assert_eq!(features.len(), 3);
        for (_, feat_vec) in features {
            assert_eq!(feat_vec.len(), 128); // Default dimension
        }
    }
    
    #[test]
    fn test_structural_features() {
        let mut graph = Graph::new();
        
        // Create a tree structure
        let root = graph.add_node(Node::Variable { name: "root".to_string() }).unwrap();
        let c1 = graph.add_node(Node::Variable { name: "c1".to_string() }).unwrap();
        let c2 = graph.add_node(Node::Variable { name: "c2".to_string() }).unwrap();
        let _app1 = graph.add_node(Node::Application {
            function: root,
            args: vec![c1, c2],
        }).unwrap();
        
        let extractor = FeatureExtractor::new(FeatureConfig {
            structural: true,
            semantic: false,
            contextual: false,
            ..Default::default()
        });
        
        let features = extractor.extract_structural_features(&graph, root).unwrap();
        assert!(!features.is_empty());
        
        // Application should have children (function and args)
        let app_children = graph.children(_app1).len() as f32;
        assert!(app_children > 0.0);
    }
    
    #[test]
    fn test_caching() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Variable { name: "test".to_string() }).unwrap();
        
        let mut extractor = FeatureExtractor::new(FeatureConfig::default());
        
        // Extract features twice
        let features1 = extractor.extract_node_features(&graph, node).unwrap();
        let features2 = extractor.extract_node_features(&graph, node).unwrap();
        
        // Should be identical (from cache)
        assert_eq!(features1, features2);
        
        // Cache should contain the node
        assert!(extractor.cache.contains_key(&node));
    }
}