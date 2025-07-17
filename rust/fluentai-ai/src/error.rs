//! Error types for AI analysis

use thiserror::Error;

/// Result type for AI operations
pub type Result<T> = std::result::Result<T, AiError>;

/// Errors that can occur during AI analysis
#[derive(Debug, Error)]
pub enum AiError {
    /// Error in tensor conversion
    #[error("Tensor conversion error: {0}")]
    TensorConversion(String),
    
    /// Error in model loading
    #[error("Model loading error: {0}")]
    ModelLoading(String),
    
    /// Error during inference
    #[error("Inference error: {0}")]
    Inference(String),
    
    /// Invalid graph structure
    #[error("Invalid graph structure: {0}")]
    InvalidGraph(String),
    
    /// Feature extraction error
    #[error("Feature extraction error: {0}")]
    FeatureExtraction(String),
    
    /// Cache error
    #[error("Cache error: {0}")]
    Cache(String),
    
    /// Backend-specific error
    #[error("Backend error: {0}")]
    Backend(String),
    
    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    /// JSON error
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),
    
    /// Generic error from anyhow
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl AiError {
    /// Create a tensor conversion error
    pub fn tensor_conversion(msg: impl Into<String>) -> Self {
        Self::TensorConversion(msg.into())
    }
    
    /// Create a model loading error
    pub fn model_loading(msg: impl Into<String>) -> Self {
        Self::ModelLoading(msg.into())
    }
    
    /// Create an inference error
    pub fn inference(msg: impl Into<String>) -> Self {
        Self::Inference(msg.into())
    }
    
    /// Create an invalid graph error
    pub fn invalid_graph(msg: impl Into<String>) -> Self {
        Self::InvalidGraph(msg.into())
    }
    
    /// Create a feature extraction error
    pub fn feature_extraction(msg: impl Into<String>) -> Self {
        Self::FeatureExtraction(msg.into())
    }
}