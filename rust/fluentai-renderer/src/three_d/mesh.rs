//! 3D mesh representation

use glam::{Vec2, Vec3};
use serde::{Deserialize, Serialize};

/// A 3D vertex with position, normal, and texture coordinates
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Vertex3D {
    /// Position in 3D space
    pub position: Vec3,
    /// Surface normal
    pub normal: Vec3,
    /// Texture coordinates
    pub tex_coords: Vec2,
    /// Vertex color (optional)
    pub color: Option<[f32; 4]>,
}

impl Vertex3D {
    /// Create a new vertex
    pub fn new(position: Vec3, normal: Vec3, tex_coords: Vec2) -> Self {
        Self {
            position,
            normal,
            tex_coords,
            color: None,
        }
    }
    
    /// Create a vertex with color
    pub fn with_color(position: Vec3, normal: Vec3, tex_coords: Vec2, color: [f32; 4]) -> Self {
        Self {
            position,
            normal,
            tex_coords,
            color: Some(color),
        }
    }
}

/// A 3D mesh containing vertices and indices
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mesh3D {
    /// Mesh name
    pub name: String,
    /// Vertex data
    pub vertices: Vec<Vertex3D>,
    /// Index data (triangles)
    pub indices: Vec<u32>,
    /// Material index
    pub material_index: Option<usize>,
}

impl Mesh3D {
    /// Create a new mesh
    pub fn new(name: String) -> Self {
        Self {
            name,
            vertices: Vec::new(),
            indices: Vec::new(),
            material_index: None,
        }
    }
    
    /// Calculate bounding box
    pub fn bounding_box(&self) -> (Vec3, Vec3) {
        if self.vertices.is_empty() {
            return (Vec3::ZERO, Vec3::ZERO);
        }
        
        let mut min = self.vertices[0].position;
        let mut max = self.vertices[0].position;
        
        for vertex in &self.vertices[1..] {
            min = min.min(vertex.position);
            max = max.max(vertex.position);
        }
        
        (min, max)
    }
    
    /// Calculate center point
    pub fn center(&self) -> Vec3 {
        let (min, max) = self.bounding_box();
        (min + max) * 0.5
    }
    
    /// Create a cube mesh
    pub fn create_cube(size: f32) -> Self {
        let half = size * 0.5;
        
        let vertices = vec![
            // Front face
            Vertex3D::new(Vec3::new(-half, -half, half), Vec3::Z, Vec2::new(0.0, 0.0)),
            Vertex3D::new(Vec3::new(half, -half, half), Vec3::Z, Vec2::new(1.0, 0.0)),
            Vertex3D::new(Vec3::new(half, half, half), Vec3::Z, Vec2::new(1.0, 1.0)),
            Vertex3D::new(Vec3::new(-half, half, half), Vec3::Z, Vec2::new(0.0, 1.0)),
            
            // Back face
            Vertex3D::new(Vec3::new(-half, -half, -half), -Vec3::Z, Vec2::new(1.0, 0.0)),
            Vertex3D::new(Vec3::new(-half, half, -half), -Vec3::Z, Vec2::new(1.0, 1.0)),
            Vertex3D::new(Vec3::new(half, half, -half), -Vec3::Z, Vec2::new(0.0, 1.0)),
            Vertex3D::new(Vec3::new(half, -half, -half), -Vec3::Z, Vec2::new(0.0, 0.0)),
            
            // Top face
            Vertex3D::new(Vec3::new(-half, half, -half), Vec3::Y, Vec2::new(0.0, 1.0)),
            Vertex3D::new(Vec3::new(-half, half, half), Vec3::Y, Vec2::new(0.0, 0.0)),
            Vertex3D::new(Vec3::new(half, half, half), Vec3::Y, Vec2::new(1.0, 0.0)),
            Vertex3D::new(Vec3::new(half, half, -half), Vec3::Y, Vec2::new(1.0, 1.0)),
            
            // Bottom face
            Vertex3D::new(Vec3::new(-half, -half, -half), -Vec3::Y, Vec2::new(1.0, 1.0)),
            Vertex3D::new(Vec3::new(half, -half, -half), -Vec3::Y, Vec2::new(0.0, 1.0)),
            Vertex3D::new(Vec3::new(half, -half, half), -Vec3::Y, Vec2::new(0.0, 0.0)),
            Vertex3D::new(Vec3::new(-half, -half, half), -Vec3::Y, Vec2::new(1.0, 0.0)),
            
            // Right face
            Vertex3D::new(Vec3::new(half, -half, -half), Vec3::X, Vec2::new(1.0, 0.0)),
            Vertex3D::new(Vec3::new(half, half, -half), Vec3::X, Vec2::new(1.0, 1.0)),
            Vertex3D::new(Vec3::new(half, half, half), Vec3::X, Vec2::new(0.0, 1.0)),
            Vertex3D::new(Vec3::new(half, -half, half), Vec3::X, Vec2::new(0.0, 0.0)),
            
            // Left face
            Vertex3D::new(Vec3::new(-half, -half, -half), -Vec3::X, Vec2::new(0.0, 0.0)),
            Vertex3D::new(Vec3::new(-half, -half, half), -Vec3::X, Vec2::new(1.0, 0.0)),
            Vertex3D::new(Vec3::new(-half, half, half), -Vec3::X, Vec2::new(1.0, 1.0)),
            Vertex3D::new(Vec3::new(-half, half, -half), -Vec3::X, Vec2::new(0.0, 1.0)),
        ];
        
        let indices = vec![
            0, 1, 2, 2, 3, 0,       // front
            4, 5, 6, 6, 7, 4,       // back
            8, 9, 10, 10, 11, 8,    // top
            12, 13, 14, 14, 15, 12, // bottom
            16, 17, 18, 18, 19, 16, // right
            20, 21, 22, 22, 23, 20, // left
        ];
        
        Self {
            name: "Cube".to_string(),
            vertices,
            indices,
            material_index: None,
        }
    }
    
    /// Create a sphere mesh
    pub fn create_sphere(radius: f32, segments: u32, rings: u32) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        for ring in 0..=rings {
            let phi = std::f32::consts::PI * (ring as f32) / (rings as f32);
            let y = phi.cos();
            let r = phi.sin();
            
            for segment in 0..=segments {
                let theta = 2.0 * std::f32::consts::PI * (segment as f32) / (segments as f32);
                let x = r * theta.cos();
                let z = r * theta.sin();
                
                let position = Vec3::new(x, y, z) * radius;
                let normal = Vec3::new(x, y, z).normalize();
                let tex_coords = Vec2::new(
                    (segment as f32) / (segments as f32),
                    (ring as f32) / (rings as f32),
                );
                
                vertices.push(Vertex3D::new(position, normal, tex_coords));
            }
        }
        
        // Generate indices
        for ring in 0..rings {
            for segment in 0..segments {
                let current = ring * (segments + 1) + segment;
                let next = current + segments + 1;
                
                indices.push(current);
                indices.push(next);
                indices.push(current + 1);
                
                indices.push(current + 1);
                indices.push(next);
                indices.push(next + 1);
            }
        }
        
        Self {
            name: "Sphere".to_string(),
            vertices,
            indices,
            material_index: None,
        }
    }
}