//! Lighting system for 3D scenes

use glam::Vec3;
use serde::{Deserialize, Serialize};
use crate::primitives::Color;

/// Types of lights supported
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LightType {
    /// Ambient light - uniform lighting from all directions
    Ambient {
        /// Light color and intensity
        color: Color,
    },
    /// Directional light - parallel rays (like the sun)
    Directional {
        /// Direction the light is pointing
        direction: Vec3,
        /// Light color
        color: Color,
        /// Intensity
        intensity: f32,
    },
    /// Point light - radiates from a point in all directions
    Point {
        /// Position of the light
        position: Vec3,
        /// Light color
        color: Color,
        /// Intensity
        intensity: f32,
        /// Attenuation radius
        radius: f32,
    },
    /// Spot light - cone of light
    Spot {
        /// Position of the light
        position: Vec3,
        /// Direction the light is pointing
        direction: Vec3,
        /// Light color
        color: Color,
        /// Intensity
        intensity: f32,
        /// Inner cone angle (full intensity)
        inner_angle: f32,
        /// Outer cone angle (falloff to zero)
        outer_angle: f32,
    },
}

/// A light source in the 3D scene
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Light {
    /// Unique identifier
    pub id: String,
    /// Type of light
    pub light_type: LightType,
    /// Whether the light is enabled
    pub enabled: bool,
    /// Whether the light casts shadows
    pub cast_shadows: bool,
}

impl Light {
    /// Create a new ambient light
    pub fn new_ambient(id: String, color: Color) -> Self {
        Self {
            id,
            light_type: LightType::Ambient { color },
            enabled: true,
            cast_shadows: false, // Ambient lights don't cast shadows
        }
    }
    
    /// Create a new directional light (sun-like)
    pub fn new_directional(id: String, direction: Vec3, color: Color, intensity: f32) -> Self {
        Self {
            id,
            light_type: LightType::Directional {
                direction: direction.normalize(),
                color,
                intensity,
            },
            enabled: true,
            cast_shadows: true,
        }
    }
    
    /// Create a new point light
    pub fn new_point(id: String, position: Vec3, color: Color, intensity: f32, radius: f32) -> Self {
        Self {
            id,
            light_type: LightType::Point {
                position,
                color,
                intensity,
                radius,
            },
            enabled: true,
            cast_shadows: true,
        }
    }
    
    /// Create a new spot light
    pub fn new_spot(
        id: String,
        position: Vec3,
        direction: Vec3,
        color: Color,
        intensity: f32,
        inner_angle: f32,
        outer_angle: f32,
    ) -> Self {
        Self {
            id,
            light_type: LightType::Spot {
                position,
                direction: direction.normalize(),
                color,
                intensity,
                inner_angle,
                outer_angle,
            },
            enabled: true,
            cast_shadows: true,
        }
    }
}

/// Default scene lighting setup
pub fn default_lighting() -> Vec<Light> {
    vec![
        // Ambient light for base illumination
        Light::new_ambient(
            "ambient".to_string(),
            Color::new(0.2, 0.2, 0.2, 1.0),
        ),
        // Main directional light (sun)
        Light::new_directional(
            "sun".to_string(),
            Vec3::new(-0.3, -1.0, -0.5),
            Color::new(1.0, 0.98, 0.95, 1.0),
            1.0,
        ),
    ]
}