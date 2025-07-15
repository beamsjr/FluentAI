//! Primitive types for rendering

use serde::{Deserialize, Serialize};

/// RGBA color representation
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Color {
    /// Red component (0.0 - 1.0)
    pub r: f32,
    /// Green component (0.0 - 1.0)
    pub g: f32,
    /// Blue component (0.0 - 1.0)
    pub b: f32,
    /// Alpha component (0.0 - 1.0)
    pub a: f32,
}

impl Color {
    /// Create a new color
    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    /// Create color from hex string
    pub fn from_hex(hex: &str) -> Result<Self, String> {
        let hex = hex.trim_start_matches('#');
        if hex.len() != 6 && hex.len() != 8 {
            return Err("Invalid hex color format".to_string());
        }

        let r = u8::from_str_radix(&hex[0..2], 16).map_err(|e| e.to_string())?;
        let g = u8::from_str_radix(&hex[2..4], 16).map_err(|e| e.to_string())?;
        let b = u8::from_str_radix(&hex[4..6], 16).map_err(|e| e.to_string())?;
        let a = if hex.len() == 8 {
            u8::from_str_radix(&hex[6..8], 16).map_err(|e| e.to_string())?
        } else {
            255
        };

        Ok(Color {
            r: r as f32 / 255.0,
            g: g as f32 / 255.0,
            b: b as f32 / 255.0,
            a: a as f32 / 255.0,
        })
    }

    /// Convert to array format for GPU
    pub fn to_array(&self) -> [f32; 4] {
        [self.r, self.g, self.b, self.a]
    }
    
    /// Create from array format
    pub fn from_array(array: [f32; 4]) -> Self {
        Self {
            r: array[0],
            g: array[1],
            b: array[2],
            a: array[3],
        }
    }
}

/// 2D position
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Position2D {
    /// X coordinate
    pub x: f32,
    /// Y coordinate
    pub y: f32,
}

impl Position2D {
    /// Create a new 2D position
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}

/// 3D position
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Position3D {
    /// X coordinate
    pub x: f32,
    /// Y coordinate
    pub y: f32,
    /// Z coordinate
    pub z: f32,
}

/// Size in 2D space
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Size2D {
    /// Width
    pub width: f32,
    /// Height
    pub height: f32,
}

impl Size2D {
    /// Create a new 2D size
    pub fn new(width: f32, height: f32) -> Self {
        Self { width, height }
    }
}

/// Transform for positioning and rotating elements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transform {
    /// Position in 3D space
    pub position: Position3D,
    /// Rotation in radians (x, y, z)
    pub rotation: (f32, f32, f32),
    /// Scale factor (x, y, z)
    pub scale: (f32, f32, f32),
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            position: Position3D { x: 0.0, y: 0.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        }
    }
}

impl Transform {
    /// Create a new transform from a 2D position
    pub fn new(pos: Position2D) -> Self {
        Self {
            position: Position3D { x: pos.x, y: pos.y, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        }
    }
}

/// Vertex data for rendering
#[derive(Debug, Clone, Copy)]
pub struct Vertex {
    /// Position in 3D space
    pub position: [f32; 3],
    /// RGBA color
    pub color: [f32; 4],
}

/// Fill type for shapes
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Fill {
    /// Solid color fill
    Solid(Color),
    /// Gradient fill (gradient ID reference)
    Gradient(String),
}

/// Renderable primitive types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Renderable {
    /// Rectangle primitive
    Rect {
        /// Transform for positioning
        transform: Transform,
        /// Size of the rectangle
        size: Size2D,
        /// Fill color
        color: Color,
        /// Corner radius
        radius: f32,
    },
    /// Rectangle with gradient fill
    GradientRect {
        /// Transform for positioning
        transform: Transform,
        /// Size of the rectangle
        size: Size2D,
        /// Gradient ID
        gradient_id: String,
        /// Corner radius
        radius: f32,
    },
    /// Text primitive
    Text {
        /// Transform for positioning
        transform: Transform,
        /// Text content
        content: String,
        /// Font size
        size: f32,
        /// Text color
        color: Color,
        /// Font family
        font: Option<String>,
    },
    /// 3D model
    Model3D {
        /// Transform for positioning
        transform: Transform,
        /// Path to model file
        path: String,
        /// Tint color
        color: Option<Color>,
    },
    /// Circle primitive
    Circle {
        /// Transform for positioning
        transform: Transform,
        /// Radius
        radius: f32,
        /// Fill color
        color: Color,
    },
    /// Circle with gradient fill
    GradientCircle {
        /// Transform for positioning
        transform: Transform,
        /// Radius
        radius: f32,
        /// Gradient ID
        gradient_id: String,
    },
    /// Ellipse primitive
    Ellipse {
        /// Transform for positioning
        transform: Transform,
        /// Width (x-axis radius)
        width: f32,
        /// Height (y-axis radius)
        height: f32,
        /// Fill color
        color: Color,
    },
    /// Line primitive
    Line {
        /// Start position
        start: Position3D,
        /// End position
        end: Position3D,
        /// Line width
        width: f32,
        /// Line color
        color: Color,
    },
    /// Path primitive for complex shapes
    Path {
        /// Transform for positioning
        transform: Transform,
        /// Path data (SVG-style commands)
        data: String,
        /// Stroke color and width
        stroke: Option<(Color, f32)>,
        /// Fill color
        fill: Option<Color>,
    },
}

impl Renderable {
    /// Get the transform of this renderable
    pub fn transform(&self) -> Transform {
        match self {
            Renderable::Rect { transform, .. } |
            Renderable::GradientRect { transform, .. } |
            Renderable::Text { transform, .. } |
            Renderable::Model3D { transform, .. } |
            Renderable::Circle { transform, .. } |
            Renderable::GradientCircle { transform, .. } |
            Renderable::Ellipse { transform, .. } |
            Renderable::Path { transform, .. } => transform.clone(),
            Renderable::Line { .. } => Transform::default(),
        }
    }

    /// Get a mutable reference to the transform
    pub fn transform_mut(&mut self) -> Option<&mut Transform> {
        match self {
            Renderable::Rect { transform, .. } |
            Renderable::GradientRect { transform, .. } |
            Renderable::Text { transform, .. } |
            Renderable::Model3D { transform, .. } |
            Renderable::Circle { transform, .. } |
            Renderable::GradientCircle { transform, .. } |
            Renderable::Ellipse { transform, .. } |
            Renderable::Path { transform, .. } => Some(transform),
            Renderable::Line { .. } => None,
        }
    }
}