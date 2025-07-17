//! Machine learning model interfaces for different backends

use crate::error::{AiError, Result};
use crate::tensor::GraphTensors;
use std::path::Path;

/// Trait for ML models that can process AST graphs
pub trait GraphModel: Send + Sync {
    /// Model output type
    type Output;
    
    /// Run inference on graph tensors
    fn forward(&self, tensors: &GraphTensors) -> Result<Self::Output>;
    
    /// Load model from file
    fn load_from_file(path: &Path) -> Result<Self> where Self: Sized;
    
    /// Save model to file
    fn save_to_file(&self, path: &Path) -> Result<()>;
    
    /// Get model name/description
    fn name(&self) -> &str;
}

/// Output from graph neural network models
#[derive(Debug, Clone)]
pub struct GnnOutput {
    /// Node embeddings [num_nodes, embedding_dim]
    pub node_embeddings: ndarray::Array2<f32>,
    /// Graph-level embedding
    pub graph_embedding: Vec<f32>,
    /// Attention weights if available
    pub attention_weights: Option<ndarray::Array2<f32>>,
}

/// Configuration for GNN models
#[derive(Debug, Clone)]
pub struct GnnConfig {
    /// Number of GNN layers
    pub num_layers: usize,
    /// Hidden dimension
    pub hidden_dim: usize,
    /// Output embedding dimension
    pub embedding_dim: usize,
    /// Dropout rate
    pub dropout: f32,
    /// Activation function
    pub activation: ActivationType,
    /// Aggregation method
    pub aggregation: AggregationType,
}

impl Default for GnnConfig {
    fn default() -> Self {
        Self {
            num_layers: 3,
            hidden_dim: 128,
            embedding_dim: 64,
            dropout: 0.1,
            activation: ActivationType::ReLU,
            aggregation: AggregationType::Mean,
        }
    }
}

/// Activation functions
#[derive(Debug, Clone, Copy)]
pub enum ActivationType {
    ReLU,
    GeLU,
    Tanh,
    Sigmoid,
}

/// Aggregation methods for GNN
#[derive(Debug, Clone, Copy)]
pub enum AggregationType {
    Mean,
    Sum,
    Max,
    Attention,
}

/// Mock implementation for testing
pub struct MockGnnModel {
    config: GnnConfig,
}

impl MockGnnModel {
    pub fn new(config: GnnConfig) -> Self {
        Self { config }
    }
}

impl GraphModel for MockGnnModel {
    type Output = GnnOutput;
    
    fn forward(&self, tensors: &GraphTensors) -> Result<Self::Output> {
        let num_nodes = tensors.num_nodes;
        let embedding_dim = self.config.embedding_dim;
        
        // Create mock embeddings
        let node_embeddings = ndarray::Array2::zeros((num_nodes, embedding_dim));
        let graph_embedding = vec![0.0; embedding_dim];
        
        Ok(GnnOutput {
            node_embeddings,
            graph_embedding,
            attention_weights: None,
        })
    }
    
    fn load_from_file(_path: &Path) -> Result<Self> {
        Ok(Self::new(GnnConfig::default()))
    }
    
    fn save_to_file(&self, _path: &Path) -> Result<()> {
        Ok(())
    }
    
    fn name(&self) -> &str {
        "MockGNN"
    }
}

// Re-export backends from models_simple
#[cfg(feature = "burn-backend")]
pub use crate::models_simple::burn_simple as burn_models;

/// Candle backend implementation
#[cfg(feature = "candle-backend")]
pub mod candle {
    use super::*;
    
    pub struct CandleGraphModel {
        // TODO: Implement Candle backend
    }
    
    impl GraphModel for CandleGraphModel {
        type Output = GnnOutput;
        
        fn forward(&self, _tensors: &GraphTensors) -> Result<Self::Output> {
            Err(AiError::backend("Candle backend not yet implemented"))
        }
        
        fn load_from_file(_path: &Path) -> Result<Self> {
            Err(AiError::backend("Candle backend not yet implemented"))
        }
        
        fn save_to_file(&self, _path: &Path) -> Result<()> {
            Err(AiError::backend("Candle backend not yet implemented"))
        }
        
        fn name(&self) -> &str {
            "CandleGNN"
        }
    }
}

/// ONNX backend implementation
#[cfg(feature = "onnx-backend")]
pub mod onnx {
    use super::*;
    use ort::{Environment, Session, SessionBuilder, Value};
    
    pub struct OnnxGraphModel {
        session: Session,
        _environment: Environment,
    }
    
    impl OnnxGraphModel {
        pub fn new(model_path: &Path) -> Result<Self> {
            let environment = Environment::builder()
                .with_name("fluentai")
                .build()
                .map_err(|e| AiError::model_loading(format!("Failed to create ONNX environment: {}", e)))?;
            
            let session = SessionBuilder::new(&environment)
                .map_err(|e| AiError::model_loading(format!("Failed to create session builder: {}", e)))?
                .with_model_from_file(model_path)
                .map_err(|e| AiError::model_loading(format!("Failed to load model: {}", e)))?;
            
            Ok(Self { session, _environment: environment })
        }
    }
    
    impl GraphModel for OnnxGraphModel {
        type Output = GnnOutput;
        
        fn forward(&self, tensors: &GraphTensors) -> Result<Self::Output> {
            // Convert tensors to ONNX format
            let features_shape = vec![tensors.features.nrows() as i64, tensors.features.ncols() as i64];
            let features_value = Value::from_array(&self.session, &features_shape, tensors.features.as_slice().unwrap())
                .map_err(|e| AiError::inference(format!("Failed to create features tensor: {}", e)))?;
            
            let edge_shape = vec![tensors.edge_index.nrows() as i64, tensors.edge_index.ncols() as i64];
            let edge_value = Value::from_array(&self.session, &edge_shape, tensors.edge_index.as_slice().unwrap())
                .map_err(|e| AiError::inference(format!("Failed to create edge tensor: {}", e)))?;
            
            // Run inference
            let outputs = self.session.run(vec![features_value, edge_value])
                .map_err(|e| AiError::inference(format!("Inference failed: {}", e)))?;
            
            // Extract embeddings
            if outputs.is_empty() {
                return Err(AiError::inference("No outputs from model"));
            }
            
            let embeddings = outputs[0].try_extract::<f32>()
                .map_err(|e| AiError::inference(format!("Failed to extract embeddings: {}", e)))?;
            
            let view = embeddings.view();
            let shape = view.shape();
            if shape.len() != 2 {
                return Err(AiError::inference("Expected 2D embeddings"));
            }
            
            let node_embeddings = ndarray::Array2::from_shape_vec(
                (shape[0], shape[1]),
                view.as_slice().unwrap().to_vec(),
            ).map_err(|e| AiError::inference(format!("Failed to convert embeddings: {}", e)))?;
            
            // Simple graph embedding
            let graph_embedding = node_embeddings.mean_axis(ndarray::Axis(0))
                .map(|v| v.to_vec())
                .unwrap_or_else(|| vec![0.0; shape[1]]);
            
            Ok(GnnOutput {
                node_embeddings,
                graph_embedding,
                attention_weights: None,
            })
        }
        
        fn load_from_file(path: &Path) -> Result<Self> {
            Self::new(path)
        }
        
        fn save_to_file(&self, _path: &Path) -> Result<()> {
            Err(AiError::model_loading("Cannot save ONNX models from runtime"))
        }
        
        fn name(&self) -> &str {
            "ONNXGNN"
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tensor::TensorBuilder;
    use fluentai_core::ast::{Graph, Node, Literal};
    
    #[test]
    fn test_mock_model() {
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        let tensors = TensorBuilder::new().build(&graph).unwrap();
        let model = MockGnnModel::new(GnnConfig::default());
        
        let output = model.forward(&tensors).unwrap();
        assert_eq!(output.node_embeddings.nrows(), 1);
        assert_eq!(output.node_embeddings.ncols(), 64); // Default embedding dim
    }
}