//! Debug overlay visualization for AR physics

use crate::ar::living_cards::{DebugInfo, PhysicsDebugBody, CollisionBound, ForceVector, ColumnZone, CardStatus};
use crate::three_d::{Mesh3D, Material3D, MaterialType}; // TODO: PrimitiveGeometry not implemented
use crate::primitives::Color;
use glam::{Vec3, Vec2, Mat4};
use std::collections::HashMap;

/// Debug overlay renderer
pub struct DebugOverlay {
    /// Physics body visualizations
    body_meshes: HashMap<String, Mesh3D>,
    /// Collision bound meshes
    collision_meshes: Vec<Mesh3D>,
    /// Force vector lines
    force_lines: Vec<LineMesh>,
    /// Column zone meshes
    column_meshes: Vec<Mesh3D>,
    /// Debug colors
    colors: DebugColors,
    /// Visibility flags
    show_physics_bodies: bool,
    show_collision_bounds: bool,
    show_force_vectors: bool,
    show_column_zones: bool,
}

/// Debug color scheme
struct DebugColors {
    physics_body: Color,
    collision_bound: Color,
    force_vector: Color,
    column_todo: Color,
    column_progress: Color,
    column_done: Color,
}

impl Default for DebugColors {
    fn default() -> Self {
        Self {
            physics_body: Color::new(0.0, 1.0, 0.0, 0.5),      // Green
            collision_bound: Color::new(1.0, 1.0, 0.0, 0.3),   // Yellow
            force_vector: Color::new(1.0, 0.0, 1.0, 0.8),      // Magenta
            column_todo: Color::new(1.0, 0.0, 0.0, 0.2),       // Red
            column_progress: Color::new(1.0, 0.5, 0.0, 0.2),   // Orange
            column_done: Color::new(0.0, 1.0, 0.0, 0.2),       // Green
        }
    }
}

/// Simple line mesh for vectors
pub struct LineMesh {
    pub start: Vec3,
    pub end: Vec3,
    pub color: Color,
    pub width: f32,
}

impl DebugOverlay {
    /// Create a new debug overlay
    pub fn new() -> Self {
        Self {
            body_meshes: HashMap::new(),
            collision_meshes: Vec::new(),
            force_lines: Vec::new(),
            column_meshes: Vec::new(),
            colors: DebugColors::default(),
            show_physics_bodies: true,
            show_collision_bounds: true,
            show_force_vectors: true,
            show_column_zones: true,
        }
    }
    
    /// Update debug visualization from debug info
    pub fn update(&mut self, debug_info: &DebugInfo) {
        // Update physics bodies
        if self.show_physics_bodies {
            self.update_physics_bodies(&debug_info.physics_bodies);
        }
        
        // Update collision bounds
        if self.show_collision_bounds {
            self.update_collision_bounds(&debug_info.collision_bounds);
        }
        
        // Update force vectors
        if self.show_force_vectors {
            self.update_force_vectors(&debug_info.force_vectors);
        }
        
        // Update column zones
        if self.show_column_zones {
            self.update_column_zones(&debug_info.column_zones);
        }
    }
    
    /// Update physics body visualizations
    fn update_physics_bodies(&mut self, bodies: &[PhysicsDebugBody]) {
        self.body_meshes.clear();
        
        for body in bodies {
            // Create a small sphere at body position
            // TODO: PrimitiveGeometry not implemented
            // let mut mesh = PrimitiveGeometry::create_sphere(0.05, 16, 8);
            let mut mesh = Mesh3D::new("debug_sphere".to_string());
            
            // Create debug material
            let material = Material3D {
                name: format!("debug_body_{}", body.id),
                material_type: MaterialType::Unlit {
                    color: self.colors.physics_body,
                    texture: None,
                },
                opacity: self.colors.physics_body.a,
                double_sided: true,
                // TODO: BlendMode not implemented
            };
            
            // Position the mesh
            // TODO: Mesh3D doesn't have transform field
            // mesh.transform = Mat4::from_translation(body.position);
            
            self.body_meshes.insert(body.id.clone(), mesh);
        }
    }
    
    /// Update collision bound visualizations
    fn update_collision_bounds(&mut self, bounds: &[CollisionBound]) {
        self.collision_meshes.clear();
        
        for bound in bounds {
            // Create a box for the collision bound
            // TODO: PrimitiveGeometry not implemented
            // let mut mesh = PrimitiveGeometry::create_box(bound.size);
            let mut mesh = Mesh3D::new("debug_box".to_string());
            
            // Create debug material
            let material = Material3D {
                name: "debug_collision".to_string(),
                material_type: MaterialType::Unlit {
                    color: self.colors.collision_bound,
                    texture: None,
                },
                opacity: self.colors.collision_bound.a,
                double_sided: true,
                // TODO: BlendMode not implemented
            };
            
            // Position the mesh
            // TODO: Mesh3D doesn't have transform field
            // mesh.transform = Mat4::from_translation(bound.center);
            
            self.collision_meshes.push(mesh);
        }
    }
    
    /// Update force vector visualizations
    fn update_force_vectors(&mut self, vectors: &[ForceVector]) {
        self.force_lines.clear();
        
        for vector in vectors {
            // Scale vector for visualization
            let scaled_length = vector.magnitude.min(1.0) * 0.5; // Cap at 50cm
            let end = vector.origin + vector.direction * scaled_length;
            
            self.force_lines.push(LineMesh {
                start: vector.origin,
                end,
                color: self.colors.force_vector,
                width: 0.005, // 5mm thick
            });
            
            // Add arrowhead
            let arrow_size = 0.05;
            let right = vector.direction.cross(Vec3::Y).normalize() * arrow_size;
            let back = -vector.direction * arrow_size;
            
            // Arrow wings
            self.force_lines.push(LineMesh {
                start: end,
                end: end + back + right,
                color: self.colors.force_vector,
                width: 0.003,
            });
            
            self.force_lines.push(LineMesh {
                start: end,
                end: end + back - right,
                color: self.colors.force_vector,
                width: 0.003,
            });
        }
    }
    
    /// Update column zone visualizations
    fn update_column_zones(&mut self, zones: &[ColumnZone]) {
        self.column_meshes.clear();
        
        for zone in zones {
            // Create a transparent box for the column
            let size = Vec3::new(zone.width, zone.height, 0.02); // 2cm thick
            // TODO: PrimitiveGeometry not implemented
            // let mut mesh = PrimitiveGeometry::create_box(size);
            let mut mesh = Mesh3D::new("debug_zone".to_string());
            
            // Get color based on status
            let color = match zone.status {
                CardStatus::Todo => self.colors.column_todo,
                CardStatus::InProgress => self.colors.column_progress,
                CardStatus::Done => self.colors.column_done,
            };
            
            // Create debug material
            let material = Material3D {
                name: format!("debug_column_{:?}", zone.status),
                material_type: MaterialType::Unlit { color, texture: None },
                opacity: color.a,
                double_sided: true,
                // TODO: BlendMode not implemented
            };
            
            // Position the mesh
            // TODO: Mesh3D doesn't have transform field
            // mesh.transform = Mat4::from_translation(zone.position);
            
            self.column_meshes.push(mesh);
        }
    }
    
    /// Get all debug meshes
    pub fn get_meshes(&self) -> Vec<&Mesh3D> {
        let mut meshes = Vec::new();
        
        // Add physics body meshes
        if self.show_physics_bodies {
            meshes.extend(self.body_meshes.values());
        }
        
        // Add collision meshes
        if self.show_collision_bounds {
            meshes.extend(&self.collision_meshes);
        }
        
        // Add column meshes
        if self.show_column_zones {
            meshes.extend(&self.column_meshes);
        }
        
        meshes
    }
    
    /// Get force vector lines
    pub fn get_lines(&self) -> &[LineMesh] {
        if self.show_force_vectors {
            &self.force_lines
        } else {
            &[]
        }
    }
    
    /// Toggle physics body visibility
    pub fn toggle_physics_bodies(&mut self) {
        self.show_physics_bodies = !self.show_physics_bodies;
    }
    
    /// Toggle collision bound visibility
    pub fn toggle_collision_bounds(&mut self) {
        self.show_collision_bounds = !self.show_collision_bounds;
    }
    
    /// Toggle force vector visibility
    pub fn toggle_force_vectors(&mut self) {
        self.show_force_vectors = !self.show_force_vectors;
    }
    
    /// Toggle column zone visibility
    pub fn toggle_column_zones(&mut self) {
        self.show_column_zones = !self.show_column_zones;
    }
}