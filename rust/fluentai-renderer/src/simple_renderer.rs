//! Simplified renderer for initial implementation

use crate::Scene;
use anyhow::Result;

/// Simplified renderer that doesn't require a window
pub struct SimpleRenderer {
    scene: Scene,
}

impl SimpleRenderer {
    /// Create a new simple renderer
    pub fn new() -> Self {
        Self {
            scene: Scene::new(),
        }
    }
    
    /// Get a reference to the scene
    pub fn scene(&self) -> &Scene {
        &self.scene
    }
    
    /// Get a mutable reference to the scene
    pub fn scene_mut(&mut self) -> &mut Scene {
        &mut self.scene
    }
    
    /// Render the scene (placeholder for now)
    pub fn render(&self) -> Result<()> {
        // This is where we would actually render
        // For now, just return Ok
        Ok(())
    }
}