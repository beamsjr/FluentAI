//! Material system for 3D objects

use crate::primitives::Color;
use serde::{Deserialize, Serialize};

/// Types of materials
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MaterialType {
    /// Basic unlit material
    Unlit {
        /// Base color
        color: Color,
        /// Texture path (optional)
        texture: Option<String>,
    },
    /// Physically-based rendering material
    PBR {
        /// Base color
        base_color: Color,
        /// Metallic factor (0.0 = dielectric, 1.0 = metal)
        metallic: f32,
        /// Roughness factor (0.0 = smooth, 1.0 = rough)
        roughness: f32,
        /// Emissive color (for glowing materials)
        emissive: Color,
        /// Base color texture
        base_color_texture: Option<String>,
        /// Metallic-roughness texture
        metallic_roughness_texture: Option<String>,
        /// Normal map
        normal_texture: Option<String>,
        /// Emissive texture
        emissive_texture: Option<String>,
    },
    /// Simple Phong shading
    Phong {
        /// Ambient color
        ambient: Color,
        /// Diffuse color
        diffuse: Color,
        /// Specular color
        specular: Color,
        /// Shininess
        shininess: f32,
        /// Diffuse texture
        texture: Option<String>,
    },
}

/// A material defines how a surface looks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Material3D {
    /// Material name
    pub name: String,
    /// Material type
    pub material_type: MaterialType,
    /// Transparency (0.0 = opaque, 1.0 = transparent)
    pub opacity: f32,
    /// Double-sided rendering
    pub double_sided: bool,
}

impl Material3D {
    /// Create a new unlit material
    pub fn new_unlit(name: String, color: Color) -> Self {
        Self {
            name,
            material_type: MaterialType::Unlit { 
                color,
                texture: None,
            },
            opacity: 1.0,
            double_sided: false,
        }
    }
    
    /// Create a new PBR material
    pub fn new_pbr(name: String, base_color: Color, metallic: f32, roughness: f32) -> Self {
        Self {
            name,
            material_type: MaterialType::PBR {
                base_color,
                metallic,
                roughness,
                emissive: Color::new(0.0, 0.0, 0.0, 1.0),
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 1.0,
            double_sided: false,
        }
    }
    
    /// Create a new Phong material
    pub fn new_phong(name: String, diffuse: Color, specular: Color, shininess: f32) -> Self {
        Self {
            name,
            material_type: MaterialType::Phong {
                ambient: Color::new(0.1, 0.1, 0.1, 1.0),
                diffuse,
                specular,
                shininess,
                texture: None,
            },
            opacity: 1.0,
            double_sided: false,
        }
    }
    
    /// Create default material
    pub fn default() -> Self {
        Self::new_pbr(
            "Default".to_string(),
            Color::new(0.8, 0.8, 0.8, 1.0),
            0.0,
            0.5,
        )
    }
}