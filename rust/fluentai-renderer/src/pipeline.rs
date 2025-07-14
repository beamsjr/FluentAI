//! Rendering pipeline management

/// Pipeline configuration
#[derive(Debug, Clone)]
pub struct PipelineConfig {
    /// Enable anti-aliasing
    pub antialiasing: bool,
    /// Enable shadows
    pub shadows: bool,
    /// Maximum texture size
    pub max_texture_size: u32,
}

impl Default for PipelineConfig {
    fn default() -> Self {
        Self {
            antialiasing: true,
            shadows: false,
            max_texture_size: 2048,
        }
    }
}

/// Manages rendering pipelines
pub struct PipelineManager {
    config: PipelineConfig,
}

impl PipelineManager {
    /// Create a new pipeline manager
    pub fn new(config: PipelineConfig) -> Self {
        Self { config }
    }
    
    /// Get current configuration
    pub fn config(&self) -> &PipelineConfig {
        &self.config
    }
    
    /// Update configuration
    pub fn set_config(&mut self, config: PipelineConfig) {
        self.config = config;
    }
}