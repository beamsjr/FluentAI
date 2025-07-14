//! 3D scene management

use crate::three_d::{Camera3D, Light, Mesh3D, Material3D};
use glam::{Mat4, Vec3};
use std::collections::HashMap;
use serde::{Deserialize, Serialize};

/// A node in the 3D scene graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node3D {
    /// Node name
    pub name: String,
    /// Local transform
    pub transform: Mat4,
    /// Mesh index (if this node has geometry)
    pub mesh_index: Option<usize>,
    /// Children node indices
    pub children: Vec<usize>,
}

impl Node3D {
    /// Create a new node
    pub fn new(name: String) -> Self {
        Self {
            name,
            transform: Mat4::IDENTITY,
            mesh_index: None,
            children: Vec::new(),
        }
    }
    
    /// Set position
    pub fn set_position(&mut self, position: Vec3) {
        self.transform = Mat4::from_translation(position);
    }
    
    /// Set scale
    pub fn set_scale(&mut self, scale: Vec3) {
        self.transform = Mat4::from_scale(scale);
    }
    
    /// Set rotation (euler angles in radians)
    pub fn set_rotation(&mut self, euler: Vec3) {
        self.transform = Mat4::from_euler(glam::EulerRot::XYZ, euler.x, euler.y, euler.z);
    }
}

/// A complete 3D scene
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scene3D {
    /// Scene name
    pub name: String,
    /// All nodes in the scene
    pub nodes: Vec<Node3D>,
    /// Root node indices
    pub roots: Vec<usize>,
    /// All meshes
    pub meshes: Vec<Mesh3D>,
    /// All materials
    pub materials: Vec<Material3D>,
    /// All lights
    pub lights: Vec<Light>,
    /// Active camera
    pub camera: Camera3D,
    /// Named node lookup
    node_map: HashMap<String, usize>,
}

impl Scene3D {
    /// Create a new empty scene
    pub fn new(name: String) -> Self {
        Self {
            name,
            nodes: Vec::new(),
            roots: Vec::new(),
            meshes: Vec::new(),
            materials: Vec::new(),
            lights: crate::three_d::lighting::default_lighting(),
            camera: Camera3D::new_perspective(
                Vec3::new(5.0, 5.0, 5.0),
                Vec3::ZERO,
                std::f32::consts::FRAC_PI_4,
                1.0,
            ),
            node_map: HashMap::new(),
        }
    }
    
    /// Add a node to the scene
    pub fn add_node(&mut self, node: Node3D) -> usize {
        let index = self.nodes.len();
        self.node_map.insert(node.name.clone(), index);
        self.nodes.push(node);
        self.roots.push(index);
        index
    }
    
    /// Add a child node
    pub fn add_child(&mut self, parent_index: usize, child: Node3D) -> Option<usize> {
        if parent_index >= self.nodes.len() {
            return None;
        }
        
        let child_index = self.nodes.len();
        self.node_map.insert(child.name.clone(), child_index);
        self.nodes.push(child);
        self.nodes[parent_index].children.push(child_index);
        
        // Remove from roots if it was there
        self.roots.retain(|&idx| idx != child_index);
        
        Some(child_index)
    }
    
    /// Add a mesh
    pub fn add_mesh(&mut self, mesh: Mesh3D) -> usize {
        let index = self.meshes.len();
        self.meshes.push(mesh);
        index
    }
    
    /// Add a material
    pub fn add_material(&mut self, material: Material3D) -> usize {
        let index = self.materials.len();
        self.materials.push(material);
        index
    }
    
    /// Add a light
    pub fn add_light(&mut self, light: Light) {
        self.lights.push(light);
    }
    
    /// Get a node by name
    pub fn get_node(&self, name: &str) -> Option<&Node3D> {
        self.node_map.get(name).and_then(|&idx| self.nodes.get(idx))
    }
    
    /// Get a mutable node by name
    pub fn get_node_mut(&mut self, name: &str) -> Option<&mut Node3D> {
        self.node_map.get(name).copied().and_then(|idx| self.nodes.get_mut(idx))
    }
    
    /// Calculate world transform for a node
    pub fn world_transform(&self, node_index: usize) -> Mat4 {
        self.world_transform_recursive(node_index, Mat4::IDENTITY)
    }
    
    fn world_transform_recursive(&self, node_index: usize, parent_transform: Mat4) -> Mat4 {
        if let Some(node) = self.nodes.get(node_index) {
            parent_transform * node.transform
        } else {
            parent_transform
        }
    }
    
    /// Create a simple test scene
    pub fn create_test_scene() -> Self {
        let mut scene = Self::new("Test Scene".to_string());
        
        // Add materials
        let red_material = Material3D::new_pbr(
            "Red".to_string(),
            crate::primitives::Color::new(0.8, 0.2, 0.2, 1.0),
            0.0,
            0.5,
        );
        let green_material = Material3D::new_pbr(
            "Green".to_string(),
            crate::primitives::Color::new(0.2, 0.8, 0.2, 1.0),
            0.0,
            0.5,
        );
        let blue_material = Material3D::new_pbr(
            "Blue".to_string(),
            crate::primitives::Color::new(0.2, 0.2, 0.8, 1.0),
            0.0,
            0.5,
        );
        
        let red_idx = scene.add_material(red_material);
        let green_idx = scene.add_material(green_material);
        let _blue_idx = scene.add_material(blue_material);
        
        // Add meshes
        let mut cube = Mesh3D::create_cube(2.0);
        cube.material_index = Some(red_idx);
        let cube_idx = scene.add_mesh(cube);
        
        let mut sphere = Mesh3D::create_sphere(1.0, 32, 16);
        sphere.material_index = Some(green_idx);
        let sphere_idx = scene.add_mesh(sphere);
        
        // Add nodes
        let mut cube_node = Node3D::new("Cube".to_string());
        cube_node.mesh_index = Some(cube_idx);
        cube_node.set_position(Vec3::new(-2.0, 0.0, 0.0));
        scene.add_node(cube_node);
        
        let mut sphere_node = Node3D::new("Sphere".to_string());
        sphere_node.mesh_index = Some(sphere_idx);
        sphere_node.set_position(Vec3::new(2.0, 0.0, 0.0));
        scene.add_node(sphere_node);
        
        scene
    }
}