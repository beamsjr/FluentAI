// Render batching and optimization for GPU performance

use crate::primitives::{Renderable, Color, Transform, Vertex};
use crate::renderer::Vertex as RendererVertex;
use std::collections::HashMap;
use cgmath::{Matrix4, Vector3, Rad};

/// Batch key for grouping similar draw calls
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct BatchKey {
    /// Shader type
    pub shader: ShaderType,
    /// Texture ID (if any)
    pub texture_id: Option<u32>,
    /// Blend mode
    pub blend_mode: BlendMode,
    /// Depth layer
    pub z_layer: i32,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum ShaderType {
    Solid,
    Textured,
    Gradient,
    Text,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum BlendMode {
    Opaque,
    Alpha,
    Additive,
    Multiply,
}

/// A batch of similar renderables
pub struct RenderBatch {
    pub key: BatchKey,
    pub vertices: Vec<RendererVertex>,
    pub indices: Vec<u16>,
    pub instance_data: Vec<InstanceData>,
}

/// Per-instance data for GPU instancing
#[repr(C)]
#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
pub struct InstanceData {
    /// Model matrix (4x4)
    pub model_matrix: [[f32; 4]; 4],
    /// Color tint
    pub color: [f32; 4],
    /// UV offset and scale for texture atlasing
    pub uv_offset_scale: [f32; 4], // (offset_x, offset_y, scale_x, scale_y)
}

impl InstanceData {
    pub fn new(transform: &Transform, color: Color) -> Self {
        let translation = Matrix4::from_translation(Vector3::new(
            transform.position.x,
            transform.position.y,
            transform.position.z,
        ));
        
        let rotation = Matrix4::from_angle_x(Rad(transform.rotation.0))
            * Matrix4::from_angle_y(Rad(transform.rotation.1))
            * Matrix4::from_angle_z(Rad(transform.rotation.2));
        
        let scale = Matrix4::from_nonuniform_scale(
            transform.scale.0,
            transform.scale.1,
            transform.scale.2,
        );
        
        let model_matrix = translation * rotation * scale;
        
        Self {
            model_matrix: model_matrix.into(),
            color: color.to_array(),
            uv_offset_scale: [0.0, 0.0, 1.0, 1.0],
        }
    }
}

/// Render batcher for optimizing draw calls
pub struct RenderBatcher {
    batches: HashMap<BatchKey, RenderBatch>,
    /// Maximum vertices per batch
    max_vertices_per_batch: usize,
    /// Maximum instances per batch
    max_instances_per_batch: usize,
}

impl RenderBatcher {
    pub fn new() -> Self {
        Self {
            batches: HashMap::new(),
            max_vertices_per_batch: 65536, // 16-bit indices limit
            max_instances_per_batch: 1024,
        }
    }
    
    /// Clear all batches
    pub fn clear(&mut self) {
        self.batches.clear();
    }
    
    /// Add a renderable to the appropriate batch
    pub fn add_renderable(&mut self, renderable: &Renderable) {
        let (key, vertices, indices, instance_data) = self.extract_batch_data(renderable);
        
        let batch = self.batches.entry(key.clone()).or_insert_with(|| RenderBatch {
            key: key.clone(),
            vertices: Vec::new(),
            indices: Vec::new(),
            instance_data: Vec::new(),
        });
        
        // Check if we can add to this batch
        if batch.vertices.len() + vertices.len() > self.max_vertices_per_batch {
            // Create a new batch with a modified key
            let mut new_key = key;
            new_key.z_layer += 1; // Increment layer to create unique key
            
            let new_batch = RenderBatch {
                key: new_key.clone(),
                vertices,
                indices,
                instance_data: if let Some(data) = instance_data {
                    vec![data]
                } else {
                    vec![]
                },
            };
            
            self.batches.insert(new_key, new_batch);
        } else if let Some(instance_data) = instance_data {
            // Add as instance if possible
            if batch.instance_data.len() < self.max_instances_per_batch {
                batch.instance_data.push(instance_data);
            } else {
                // Add as separate geometry
                let base_index = batch.vertices.len() as u16;
                batch.vertices.extend(vertices);
                batch.indices.extend(indices.iter().map(|&i| i + base_index));
            }
        } else {
            // Add geometry to batch
            let base_index = batch.vertices.len() as u16;
            batch.vertices.extend(vertices);
            batch.indices.extend(indices.iter().map(|&i| i + base_index));
        }
    }
    
    /// Extract batch data from a renderable
    fn extract_batch_data(&self, renderable: &Renderable) -> (BatchKey, Vec<RendererVertex>, Vec<u16>, Option<InstanceData>) {
        match renderable {
            Renderable::Rect { transform, size, color, radius } => {
                let key = BatchKey {
                    shader: if *radius > 0.0 { ShaderType::Solid } else { ShaderType::Solid },
                    texture_id: None,
                    blend_mode: if color.a < 1.0 { BlendMode::Alpha } else { BlendMode::Opaque },
                    z_layer: (transform.position.z * 1000.0) as i32,
                };
                
                let half_width = size.width / 2.0;
                let half_height = size.height / 2.0;
                let color_array = color.to_array();
                
                let vertices = vec![
                    RendererVertex { position: [-half_width, -half_height, 0.0], color: color_array },
                    RendererVertex { position: [half_width, -half_height, 0.0], color: color_array },
                    RendererVertex { position: [half_width, half_height, 0.0], color: color_array },
                    RendererVertex { position: [-half_width, half_height, 0.0], color: color_array },
                ];
                
                let indices = vec![0, 1, 2, 0, 2, 3];
                
                let instance_data = Some(InstanceData::new(transform, *color));
                
                (key, vertices, indices, instance_data)
            }
            
            Renderable::Circle { transform, radius, color } => {
                let key = BatchKey {
                    shader: ShaderType::Solid,
                    texture_id: None,
                    blend_mode: if color.a < 1.0 { BlendMode::Alpha } else { BlendMode::Opaque },
                    z_layer: (transform.position.z * 1000.0) as i32,
                };
                
                // Use a pre-tessellated unit circle and scale via instance transform
                let vertices = self.get_unit_circle_vertices(*color);
                let indices = self.get_unit_circle_indices();
                
                // Scale the transform for the radius
                let mut scaled_transform = transform.clone();
                scaled_transform.scale = (
                    scaled_transform.scale.0 * radius,
                    scaled_transform.scale.1 * radius,
                    scaled_transform.scale.2,
                );
                
                let instance_data = Some(InstanceData::new(&scaled_transform, *color));
                
                (key, vertices, indices, instance_data)
            }
            
            Renderable::Text { transform, content: _, size: _, color, font: _ } => {
                let key = BatchKey {
                    shader: ShaderType::Text,
                    texture_id: Some(0), // Font atlas texture
                    blend_mode: BlendMode::Alpha,
                    z_layer: (transform.position.z * 1000.0) as i32,
                };
                
                // Text is handled separately with glyph instances
                let vertices = Vec::new();
                let indices = Vec::new();
                let instance_data = Some(InstanceData::new(transform, *color));
                
                (key, vertices, indices, instance_data)
            }
            
            _ => {
                // Default case for other renderables
                let key = BatchKey {
                    shader: ShaderType::Solid,
                    texture_id: None,
                    blend_mode: BlendMode::Opaque,
                    z_layer: 0,
                };
                
                (key, Vec::new(), Vec::new(), None)
            }
        }
    }
    
    /// Get pre-tessellated unit circle vertices
    fn get_unit_circle_vertices(&self, color: Color) -> Vec<RendererVertex> {
        const SEGMENTS: usize = 32;
        let mut vertices = Vec::with_capacity(SEGMENTS + 1);
        let color_array = color.to_array();
        
        // Center vertex
        vertices.push(RendererVertex {
            position: [0.0, 0.0, 0.0],
            color: color_array,
        });
        
        // Edge vertices
        for i in 0..=SEGMENTS {
            let angle = (i as f32 / SEGMENTS as f32) * std::f32::consts::PI * 2.0;
            vertices.push(RendererVertex {
                position: [angle.cos(), angle.sin(), 0.0],
                color: color_array,
            });
        }
        
        vertices
    }
    
    /// Get unit circle indices
    fn get_unit_circle_indices(&self) -> Vec<u16> {
        const SEGMENTS: u16 = 32;
        let mut indices = Vec::with_capacity((SEGMENTS * 3) as usize);
        
        for i in 0..SEGMENTS {
            indices.push(0); // Center
            indices.push(i + 1);
            indices.push(i + 2);
        }
        
        indices
    }
    
    /// Get sorted batches for rendering
    pub fn get_sorted_batches(&self) -> Vec<&RenderBatch> {
        let mut batches: Vec<_> = self.batches.values().collect();
        
        // Sort by:
        // 1. Opaque before transparent
        // 2. Z-layer (back to front for transparent, front to back for opaque)
        // 3. Shader type
        // 4. Texture ID
        batches.sort_by(|a, b| {
            // Opaque first
            if a.key.blend_mode == BlendMode::Opaque && b.key.blend_mode != BlendMode::Opaque {
                return std::cmp::Ordering::Less;
            }
            if a.key.blend_mode != BlendMode::Opaque && b.key.blend_mode == BlendMode::Opaque {
                return std::cmp::Ordering::Greater;
            }
            
            // Z-layer sorting
            if a.key.blend_mode == BlendMode::Opaque {
                // Front to back for opaque
                b.key.z_layer.cmp(&a.key.z_layer)
            } else {
                // Back to front for transparent
                a.key.z_layer.cmp(&b.key.z_layer)
            }
            .then_with(|| a.key.shader.cmp(&b.key.shader))
            .then_with(|| a.key.texture_id.cmp(&b.key.texture_id))
        });
        
        batches
    }
    
    /// Get batch statistics
    pub fn get_stats(&self) -> BatchStats {
        let total_batches = self.batches.len();
        let total_vertices = self.batches.values()
            .map(|b| b.vertices.len())
            .sum();
        let total_instances = self.batches.values()
            .map(|b| b.instance_data.len())
            .sum();
        let total_draw_calls = self.batches.values()
            .map(|b| if b.instance_data.is_empty() { 1 } else { 1 })
            .sum();
        
        BatchStats {
            total_batches,
            total_vertices,
            total_instances,
            total_draw_calls,
            batches_by_shader: self.count_batches_by_shader(),
        }
    }
    
    fn count_batches_by_shader(&self) -> HashMap<ShaderType, usize> {
        let mut counts = HashMap::new();
        for batch in self.batches.values() {
            *counts.entry(batch.key.shader).or_insert(0) += 1;
        }
        counts
    }
}

/// Batch statistics for debugging and optimization
#[derive(Debug)]
pub struct BatchStats {
    pub total_batches: usize,
    pub total_vertices: usize,
    pub total_instances: usize,
    pub total_draw_calls: usize,
    pub batches_by_shader: HashMap<ShaderType, usize>,
}

impl BatchStats {
    pub fn print_summary(&self) {
        println!("=== Render Batch Statistics ===");
        println!("Total batches: {}", self.total_batches);
        println!("Total vertices: {}", self.total_vertices);
        println!("Total instances: {}", self.total_instances);
        println!("Total draw calls: {}", self.total_draw_calls);
        println!("\nBatches by shader type:");
        for (shader, count) in &self.batches_by_shader {
            println!("  {:?}: {}", shader, count);
        }
        
        let savings = if self.total_instances > 0 {
            ((self.total_instances - self.total_draw_calls) as f32 / self.total_instances as f32) * 100.0
        } else {
            0.0
        };
        println!("\nDraw call reduction: {:.1}%", savings);
    }
}

/// Derive implementations for ShaderType
impl ShaderType {
    pub fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        match (self, other) {
            (ShaderType::Solid, ShaderType::Solid) => Ordering::Equal,
            (ShaderType::Solid, _) => Ordering::Less,
            (_, ShaderType::Solid) => Ordering::Greater,
            (ShaderType::Textured, ShaderType::Textured) => Ordering::Equal,
            (ShaderType::Textured, _) => Ordering::Less,
            (_, ShaderType::Textured) => Ordering::Greater,
            (ShaderType::Gradient, ShaderType::Gradient) => Ordering::Equal,
            (ShaderType::Gradient, _) => Ordering::Less,
            (_, ShaderType::Gradient) => Ordering::Greater,
            (ShaderType::Text, ShaderType::Text) => Ordering::Equal,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::{Position3D, Size2D};
    
    #[test]
    fn test_batching() {
        let mut batcher = RenderBatcher::new();
        
        // Add similar rectangles (should batch together)
        for i in 0..5 {
            let rect = Renderable::Rect {
                transform: Transform {
                    position: Position3D { x: i as f32 * 10.0, y: 0.0, z: 0.0 },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                size: Size2D::new(10.0, 10.0),
                color: Color::new(1.0, 0.0, 0.0, 1.0),
                radius: 0.0,
            };
            batcher.add_renderable(&rect);
        }
        
        // Add circles (should create separate batch due to different vertices)
        for i in 0..3 {
            let circle = Renderable::Circle {
                transform: Transform {
                    position: Position3D { x: i as f32 * 20.0, y: 50.0, z: 0.0 },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                radius: 5.0,
                color: Color::new(0.0, 1.0, 0.0, 1.0),
            };
            batcher.add_renderable(&circle);
        }
        
        let stats = batcher.get_stats();
        assert!(stats.total_batches <= 2); // Should batch similar items
        assert_eq!(stats.total_instances, 8); // All items should be instanced
    }
}