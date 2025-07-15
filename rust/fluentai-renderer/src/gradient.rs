// Gradient support for FluentAI renderer

use crate::primitives::Color;
use cgmath::{Vector2, Vector3};

/// Gradient stop with color and position
#[derive(Debug, Clone, Copy)]
pub struct GradientStop {
    /// Position along gradient (0.0 to 1.0)
    pub position: f32,
    /// Color at this stop
    pub color: Color,
}

/// Type of gradient
#[derive(Debug, Clone)]
pub enum GradientType {
    /// Linear gradient between two points
    Linear {
        start: Vector2<f32>,
        end: Vector2<f32>,
    },
    /// Radial gradient from center point
    Radial {
        center: Vector2<f32>,
        radius: f32,
    },
    /// Conic gradient (angle-based)
    Conic {
        center: Vector2<f32>,
        start_angle: f32, // in radians
    },
}

/// Gradient definition
#[derive(Debug, Clone)]
pub struct Gradient {
    pub gradient_type: GradientType,
    pub stops: Vec<GradientStop>,
    /// Spread method for gradient repetition
    pub spread: SpreadMethod,
}

/// How gradient behaves outside its bounds
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpreadMethod {
    /// Clamp to edge colors
    Pad,
    /// Repeat the gradient
    Repeat,
    /// Reflect/mirror the gradient
    Reflect,
}

impl Gradient {
    /// Create a new linear gradient
    pub fn linear(start: Vector2<f32>, end: Vector2<f32>) -> Self {
        Self {
            gradient_type: GradientType::Linear { start, end },
            stops: Vec::new(),
            spread: SpreadMethod::Pad,
        }
    }
    
    /// Create a new radial gradient
    pub fn radial(center: Vector2<f32>, radius: f32) -> Self {
        Self {
            gradient_type: GradientType::Radial { center, radius },
            stops: Vec::new(),
            spread: SpreadMethod::Pad,
        }
    }
    
    /// Create a new conic gradient
    pub fn conic(center: Vector2<f32>, start_angle: f32) -> Self {
        Self {
            gradient_type: GradientType::Conic { center, start_angle },
            stops: Vec::new(),
            spread: SpreadMethod::Pad,
        }
    }
    
    /// Add a color stop
    pub fn add_stop(mut self, position: f32, color: Color) -> Self {
        self.stops.push(GradientStop { position, color });
        // Keep stops sorted by position
        self.stops.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap());
        self
    }
    
    /// Set spread method
    pub fn spread(mut self, method: SpreadMethod) -> Self {
        self.spread = method;
        self
    }
    
    /// Sample the gradient at a given position (0.0 to 1.0)
    pub fn sample(&self, t: f32) -> Color {
        if self.stops.is_empty() {
            return Color::new(0.0, 0.0, 0.0, 1.0);
        }
        
        if self.stops.len() == 1 {
            return self.stops[0].color;
        }
        
        // Apply spread method
        let t = match self.spread {
            SpreadMethod::Pad => t.clamp(0.0, 1.0),
            SpreadMethod::Repeat => t.fract(),
            SpreadMethod::Reflect => {
                let t2 = t.abs() % 2.0;
                if t2 > 1.0 { 2.0 - t2 } else { t2 }
            }
        };
        
        // Find surrounding stops
        let mut lower_stop = &self.stops[0];
        let mut upper_stop = &self.stops[self.stops.len() - 1];
        
        for i in 0..self.stops.len() - 1 {
            if t >= self.stops[i].position && t <= self.stops[i + 1].position {
                lower_stop = &self.stops[i];
                upper_stop = &self.stops[i + 1];
                break;
            }
        }
        
        // Handle edge cases
        if t <= lower_stop.position {
            return lower_stop.color;
        }
        if t >= upper_stop.position {
            return upper_stop.color;
        }
        
        // Interpolate between stops
        let range = upper_stop.position - lower_stop.position;
        if range > 0.0 {
            let local_t = (t - lower_stop.position) / range;
            Self::lerp_color(lower_stop.color, upper_stop.color, local_t)
        } else {
            lower_stop.color
        }
    }
    
    /// Linear interpolation between colors
    fn lerp_color(a: Color, b: Color, t: f32) -> Color {
        Color::new(
            a.r + (b.r - a.r) * t,
            a.g + (b.g - a.g) * t,
            a.b + (b.b - a.b) * t,
            a.a + (b.a - a.a) * t,
        )
    }
    
    /// Calculate gradient value for a point
    pub fn calculate_gradient_coord(&self, point: Vector2<f32>) -> f32 {
        match &self.gradient_type {
            GradientType::Linear { start, end } => {
                // Project point onto gradient line
                let gradient_vec = end - start;
                let gradient_len_sq = gradient_vec.x * gradient_vec.x + gradient_vec.y * gradient_vec.y;
                
                if gradient_len_sq > 0.0 {
                    let point_vec = point - start;
                    let dot = point_vec.x * gradient_vec.x + point_vec.y * gradient_vec.y;
                    dot / gradient_len_sq
                } else {
                    0.0
                }
            }
            GradientType::Radial { center, radius } => {
                // Distance from center normalized by radius
                let dist = ((point.x - center.x).powi(2) + (point.y - center.y).powi(2)).sqrt();
                dist / radius
            }
            GradientType::Conic { center, start_angle } => {
                // Angle from center point
                let dx = point.x - center.x;
                let dy = point.y - center.y;
                let angle = dy.atan2(dx);
                let normalized_angle = (angle - start_angle) / (2.0 * std::f32::consts::PI);
                normalized_angle.fract()
            }
        }
    }
}

/// Gradient mesh for GPU rendering
pub struct GradientMesh {
    pub vertices: Vec<GradientVertex>,
    pub indices: Vec<u16>,
}

/// Vertex for gradient rendering
#[repr(C)]
#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
pub struct GradientVertex {
    pub position: [f32; 3],
    pub gradient_coord: f32, // 0.0 to 1.0 along gradient
}

impl GradientMesh {
    /// Create a gradient mesh for a rectangle
    pub fn for_rect(
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        gradient: &Gradient,
    ) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        // Create a grid of vertices for smooth gradient rendering
        const SUBDIVISIONS: usize = 10;
        
        for row in 0..=SUBDIVISIONS {
            for col in 0..=SUBDIVISIONS {
                let u = col as f32 / SUBDIVISIONS as f32;
                let v = row as f32 / SUBDIVISIONS as f32;
                
                let px = x + u * width;
                let py = y + v * height;
                let point = Vector2::new(px, py);
                
                let gradient_coord = gradient.calculate_gradient_coord(point);
                
                vertices.push(GradientVertex {
                    position: [px, py, 0.0],
                    gradient_coord,
                });
                
                // Add indices for triangles (except for last row/column)
                if row < SUBDIVISIONS && col < SUBDIVISIONS {
                    let base = (row * (SUBDIVISIONS + 1) + col) as u16;
                    let next_row = base + (SUBDIVISIONS + 1) as u16;
                    
                    // Two triangles per grid cell
                    indices.extend_from_slice(&[
                        base, base + 1, next_row + 1,
                        base, next_row + 1, next_row,
                    ]);
                }
            }
        }
        
        Self { vertices, indices }
    }
    
    /// Create a gradient mesh for a circle
    pub fn for_circle(
        center_x: f32,
        center_y: f32,
        radius: f32,
        gradient: &Gradient,
    ) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        // Add center vertex
        let center = Vector2::new(center_x, center_y);
        let center_coord = gradient.calculate_gradient_coord(center);
        vertices.push(GradientVertex {
            position: [center_x, center_y, 0.0],
            gradient_coord: center_coord,
        });
        
        // Add vertices in concentric rings
        const RINGS: usize = 8;
        const SEGMENTS: usize = 32;
        
        for ring in 1..=RINGS {
            let ring_radius = radius * (ring as f32 / RINGS as f32);
            
            for segment in 0..SEGMENTS {
                let angle = (segment as f32 / SEGMENTS as f32) * 2.0 * std::f32::consts::PI;
                let x = center_x + angle.cos() * ring_radius;
                let y = center_y + angle.sin() * ring_radius;
                let point = Vector2::new(x, y);
                
                let gradient_coord = gradient.calculate_gradient_coord(point);
                
                vertices.push(GradientVertex {
                    position: [x, y, 0.0],
                    gradient_coord,
                });
            }
        }
        
        // Connect center to first ring
        for i in 0..SEGMENTS {
            let next = (i + 1) % SEGMENTS;
            indices.extend_from_slice(&[
                0,
                1 + i as u16,
                1 + next as u16,
            ]);
        }
        
        // Connect rings
        for ring in 0..RINGS - 1 {
            let ring_base = 1 + ring * SEGMENTS;
            let next_ring_base = ring_base + SEGMENTS;
            
            for i in 0..SEGMENTS {
                let next = (i + 1) % SEGMENTS;
                let a = ring_base + i;
                let b = ring_base + next;
                let c = next_ring_base + next;
                let d = next_ring_base + i;
                
                indices.extend_from_slice(&[
                    a as u16, b as u16, c as u16,
                    a as u16, c as u16, d as u16,
                ]);
            }
        }
        
        Self { vertices, indices }
    }
}