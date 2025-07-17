//! Tensor conversion utilities for AST graphs

use crate::error::{AiError, Result};
use fluentai_core::ai_interface::{AstGraphInterface, BatchAstOperations};
use fluentai_core::ast::NodeId;
use ndarray::{Array1, Array2, ArrayView2};
use std::collections::HashMap;

/// Format for tensor representation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TensorFormat {
    /// Dense feature matrix [num_nodes, feature_dim]
    Dense,
    /// Sparse COO format for large graphs
    Sparse,
    /// Batched format for multiple graphs
    Batched,
}

/// Builder for converting AST graphs to tensors
pub struct TensorBuilder {
    format: TensorFormat,
    normalize: bool,
    include_positional: bool,
    max_nodes: Option<usize>,
}

impl Default for TensorBuilder {
    fn default() -> Self {
        Self {
            format: TensorFormat::Dense,
            normalize: true,
            include_positional: false,
            max_nodes: None,
        }
    }
}

impl TensorBuilder {
    /// Create a new tensor builder
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Set the tensor format
    pub fn format(mut self, format: TensorFormat) -> Self {
        self.format = format;
        self
    }
    
    /// Enable or disable feature normalization
    pub fn normalize(mut self, normalize: bool) -> Self {
        self.normalize = normalize;
        self
    }
    
    /// Include positional encodings
    pub fn with_positional_encoding(mut self, include: bool) -> Self {
        self.include_positional = include;
        self
    }
    
    /// Set maximum number of nodes (for batching)
    pub fn max_nodes(mut self, max: usize) -> Self {
        self.max_nodes = Some(max);
        self
    }
    
    /// Build tensors from an AST graph
    pub fn build<G: AstGraphInterface>(&self, graph: &G) -> Result<GraphTensors> {
        let node_ids = graph.node_ids();
        let num_nodes = node_ids.len();
        
        if num_nodes == 0 {
            return Err(AiError::invalid_graph("Graph has no nodes"));
        }
        
        // Check max nodes constraint
        if let Some(max) = self.max_nodes {
            if num_nodes > max {
                return Err(AiError::invalid_graph(
                    format!("Graph has {} nodes, exceeds maximum of {}", num_nodes, max)
                ));
            }
        }
        
        // Create node ID to index mapping
        let id_to_idx: HashMap<NodeId, usize> = node_ids
            .iter()
            .enumerate()
            .map(|(i, &id)| (id, i))
            .collect();
        
        // Extract features
        let features = self.extract_features(graph, &node_ids)?;
        
        // Build edge indices
        let (edge_index, edge_attr) = self.build_edges(graph, &node_ids, &id_to_idx)?;
        
        // Extract node types
        let node_types = self.extract_node_types(graph, &node_ids);
        
        Ok(GraphTensors {
            features,
            edge_index,
            edge_attr,
            node_types,
            num_nodes,
            node_mapping: id_to_idx,
        })
    }
    
    /// Extract feature matrix from graph
    fn extract_features<G: AstGraphInterface>(
        &self,
        graph: &G,
        node_ids: &[NodeId],
    ) -> Result<Array2<f32>> {
        let batch_features = graph.batch_node_features(node_ids);
        
        if batch_features.is_empty() {
            return Err(AiError::feature_extraction("No features extracted"));
        }
        
        let feature_dim = batch_features[0].len();
        let num_nodes = batch_features.len();
        
        // Convert to ndarray
        let mut features = Array2::zeros((num_nodes, feature_dim));
        for (i, node_features) in batch_features.iter().enumerate() {
            if node_features.len() != feature_dim {
                return Err(AiError::feature_extraction(
                    "Inconsistent feature dimensions"
                ));
            }
            for (j, &val) in node_features.iter().enumerate() {
                features[[i, j]] = val;
            }
        }
        
        // Add positional encoding if requested
        if self.include_positional {
            features = self.add_positional_encoding(features, num_nodes)?;
        }
        
        // Normalize if requested
        if self.normalize {
            features = self.normalize_features(features);
        }
        
        Ok(features)
    }
    
    /// Build edge indices and attributes
    fn build_edges<G: AstGraphInterface>(
        &self,
        graph: &G,
        _node_ids: &[NodeId],
        id_to_idx: &HashMap<NodeId, usize>,
    ) -> Result<(Array2<i64>, Option<Array2<f32>>)> {
        let edges = graph.edges();
        let num_edges = edges.len();
        
        if num_edges == 0 {
            // Return empty edge arrays
            return Ok((Array2::zeros((2, 0)), None));
        }
        
        // Build edge index array [2, num_edges]
        let mut edge_index = Array2::zeros((2, num_edges));
        
        for (i, (src, dst)) in edges.iter().enumerate() {
            let src_idx = id_to_idx.get(src)
                .ok_or_else(|| AiError::invalid_graph("Source node not in mapping"))?;
            let dst_idx = id_to_idx.get(dst)
                .ok_or_else(|| AiError::invalid_graph("Destination node not in mapping"))?;
                
            edge_index[[0, i]] = *src_idx as i64;
            edge_index[[1, i]] = *dst_idx as i64;
        }
        
        // TODO: Add edge attributes if needed
        let edge_attr = None;
        
        Ok((edge_index, edge_attr))
    }
    
    /// Extract node types as categorical features
    fn extract_node_types<G: AstGraphInterface>(
        &self,
        graph: &G,
        node_ids: &[NodeId],
    ) -> Array1<i64> {
        let node_types = graph.batch_node_types(node_ids);
        Array1::from_vec(node_types.into_iter().map(|t| t as i64).collect())
    }
    
    /// Add positional encoding to features
    fn add_positional_encoding(
        &self,
        features: Array2<f32>,
        num_nodes: usize,
    ) -> Result<Array2<f32>> {
        let (rows, cols) = features.dim();
        let pos_dim = 16; // Positional encoding dimension
        
        // Extend feature matrix with positional encoding columns
        let mut extended = Array2::zeros((rows, cols + pos_dim));
        extended.slice_mut(ndarray::s![.., ..cols]).assign(&features);
        
        // Add sinusoidal positional encoding
        for i in 0..num_nodes {
            for j in 0..pos_dim {
                let pos = i as f32;
                let dim = j as f32;
                let angle = pos / 10000_f32.powf(2.0 * dim / pos_dim as f32);
                
                let value = if j % 2 == 0 {
                    angle.sin()
                } else {
                    angle.cos()
                };
                
                extended[[i, cols + j]] = value;
            }
        }
        
        Ok(extended)
    }
    
    /// Normalize features to zero mean and unit variance
    fn normalize_features(&self, mut features: Array2<f32>) -> Array2<f32> {
        let epsilon = 1e-6;
        
        // Compute mean and std per feature
        let mean = features.mean_axis(ndarray::Axis(0)).unwrap();
        let std = features.std_axis(ndarray::Axis(0), 0.0);
        
        // Normalize
        for (_i, mut row) in features.rows_mut().into_iter().enumerate() {
            for (j, val) in row.iter_mut().enumerate() {
                let std_val = std[j].max(epsilon);
                *val = (*val - mean[j]) / std_val;
            }
        }
        
        features
    }
}

/// Tensors representing an AST graph
#[derive(Debug)]
pub struct GraphTensors {
    /// Node feature matrix [num_nodes, feature_dim]
    pub features: Array2<f32>,
    /// Edge indices [2, num_edges]
    pub edge_index: Array2<i64>,
    /// Optional edge attributes [num_edges, edge_attr_dim]
    pub edge_attr: Option<Array2<f32>>,
    /// Node types [num_nodes]
    pub node_types: Array1<i64>,
    /// Number of nodes
    pub num_nodes: usize,
    /// Mapping from NodeId to tensor index
    pub node_mapping: HashMap<NodeId, usize>,
}

impl GraphTensors {
    /// Get the feature dimension
    pub fn feature_dim(&self) -> usize {
        self.features.ncols()
    }
    
    /// Get the number of edges
    pub fn num_edges(&self) -> usize {
        self.edge_index.ncols()
    }
    
    /// Get features for a specific node
    pub fn node_features(&self, node_id: NodeId) -> Option<ArrayView2<f32>> {
        self.node_mapping.get(&node_id)
            .map(|&idx| self.features.slice(ndarray::s![idx..idx+1, ..]))
    }
    
}

/// Batch multiple graphs for efficient processing
pub struct GraphBatch {
    /// Concatenated node features
    pub features: Array2<f32>,
    /// Concatenated edge indices (with offsets)
    pub edge_index: Array2<i64>,
    /// Batch assignment for each node
    pub batch: Array1<i64>,
    /// Number of nodes per graph
    pub num_nodes_per_graph: Vec<usize>,
}

impl GraphBatch {
    /// Create a batch from multiple graph tensors
    pub fn from_graphs(graphs: Vec<GraphTensors>) -> Result<Self> {
        if graphs.is_empty() {
            return Err(AiError::invalid_graph("No graphs to batch"));
        }
        
        // Check feature dimensions match
        let feature_dim = graphs[0].feature_dim();
        for g in &graphs {
            if g.feature_dim() != feature_dim {
                return Err(AiError::tensor_conversion(
                    "Inconsistent feature dimensions in batch"
                ));
            }
        }
        
        // Concatenate features
        let total_nodes: usize = graphs.iter().map(|g| g.num_nodes).sum();
        let mut features = Array2::zeros((total_nodes, feature_dim));
        let mut batch = Array1::zeros(total_nodes);
        let mut node_offset = 0;
        
        for (batch_idx, graph) in graphs.iter().enumerate() {
            let end = node_offset + graph.num_nodes;
            features.slice_mut(ndarray::s![node_offset..end, ..])
                .assign(&graph.features);
            batch.slice_mut(ndarray::s![node_offset..end])
                .fill(batch_idx as i64);
            node_offset = end;
        }
        
        // Concatenate edges with offset
        let total_edges: usize = graphs.iter().map(|g| g.num_edges()).sum();
        let mut edge_index = Array2::zeros((2, total_edges));
        let mut edge_offset = 0;
        node_offset = 0;
        
        for graph in &graphs {
            let num_edges = graph.num_edges();
            if num_edges > 0 {
                let end = edge_offset + num_edges;
                let mut edges = graph.edge_index.clone();
                edges += node_offset as i64;
                edge_index.slice_mut(ndarray::s![.., edge_offset..end])
                    .assign(&edges);
                edge_offset = end;
            }
            node_offset += graph.num_nodes;
        }
        
        let num_nodes_per_graph = graphs.into_iter()
            .map(|g| g.num_nodes)
            .collect();
        
        Ok(Self {
            features,
            edge_index,
            batch,
            num_nodes_per_graph,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Graph, Node, Literal};
    
    #[test]
    fn test_tensor_builder_basic() {
        let mut graph = Graph::new();
        let n1 = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let n2 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let _n3 = graph.add_node(Node::Application {
            function: n2,
            args: vec![n1],
        }).unwrap();
        
        let builder = TensorBuilder::new();
        let tensors = builder.build(&graph).unwrap();
        
        assert_eq!(tensors.num_nodes, 3);
        assert_eq!(tensors.features.nrows(), 3);
        assert_eq!(tensors.edge_index.ncols(), 2); // Two edges from Application
    }
    
    #[test]
    fn test_empty_graph_error() {
        let graph = Graph::new();
        let builder = TensorBuilder::new();
        let result = builder.build(&graph);
        
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AiError::InvalidGraph(_)));
    }
    
    #[test]
    fn test_normalization() {
        let mut graph = Graph::new();
        for i in 0..5 {
            graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
        }
        
        let builder = TensorBuilder::new().normalize(true);
        let tensors = builder.build(&graph).unwrap();
        
        // Check that features are normalized (mean ~0, std ~1)
        let mean = tensors.features.mean_axis(ndarray::Axis(0)).unwrap();
        let std = tensors.features.std_axis(ndarray::Axis(0), 0.0);
        
        for &m in mean.iter() {
            assert!((m.abs()) < 0.1); // Mean should be close to 0
        }
        
        for &s in std.iter() {
            if s > 0.0 { // Skip constant features
                assert!((s - 1.0).abs() < 0.2); // Std should be close to 1
            }
        }
    }
}