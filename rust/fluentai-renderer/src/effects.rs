// Visual effects implementation for shadows, blur, and filters

use crate::primitives::{Color, Renderable, Transform, Position3D, Size2D};
use cgmath::{Vector2, Matrix4};

/// Box shadow effect
#[derive(Debug, Clone)]
pub struct BoxShadow {
    /// Horizontal offset
    pub offset_x: f32,
    /// Vertical offset  
    pub offset_y: f32,
    /// Blur radius
    pub blur_radius: f32,
    /// Spread radius
    pub spread_radius: f32,
    /// Shadow color
    pub color: Color,
    /// Inset shadow (inside the element)
    pub inset: bool,
}

impl BoxShadow {
    pub fn new(offset_x: f32, offset_y: f32, blur_radius: f32, color: Color) -> Self {
        Self {
            offset_x,
            offset_y,
            blur_radius,
            spread_radius: 0.0,
            color,
            inset: false,
        }
    }
    
    pub fn with_spread(mut self, spread: f32) -> Self {
        self.spread_radius = spread;
        self
    }
    
    pub fn inset(mut self) -> Self {
        self.inset = true;
        self
    }
    
    /// Generate shadow geometry for GPU rendering
    pub fn generate_shadow_quad(&self, base_rect: &Renderable) -> Option<Vec<Renderable>> {
        match base_rect {
            Renderable::Rect { transform, size, radius, .. } => {
                let mut shadows = Vec::new();
                
                // Generate multiple layers for blur effect
                let blur_steps = (self.blur_radius / 2.0).ceil() as usize;
                let blur_steps = blur_steps.min(10); // Limit for performance
                
                for i in 0..blur_steps {
                    let t = i as f32 / blur_steps as f32;
                    let current_blur = self.blur_radius * t;
                    let alpha_multiplier = 1.0 - t;
                    
                    // Adjust color alpha based on blur step
                    let mut shadow_color = self.color;
                    shadow_color.a *= alpha_multiplier * 0.3; // Soften shadow
                    
                    // Create shadow rectangle
                    let shadow_transform = Transform {
                        position: Position3D {
                            x: transform.position.x + self.offset_x,
                            y: transform.position.y + self.offset_y,
                            z: transform.position.z - 0.001 * (i + 1) as f32, // Layer shadows
                        },
                        rotation: transform.rotation,
                        scale: transform.scale,
                    };
                    
                    let shadow_size = Size2D {
                        width: size.width + self.spread_radius * 2.0 + current_blur * 2.0,
                        height: size.height + self.spread_radius * 2.0 + current_blur * 2.0,
                    };
                    
                    shadows.push(Renderable::Rect {
                        transform: shadow_transform,
                        size: shadow_size,
                        color: shadow_color,
                        radius: radius + current_blur,
                    });
                }
                
                Some(shadows)
            }
            _ => None,
        }
    }
}

/// Blur filter types
#[derive(Debug, Clone, Copy)]
pub enum BlurType {
    /// Standard Gaussian blur
    Gaussian { radius: f32 },
    /// Box blur (faster but lower quality)
    Box { radius: f32 },
    /// Motion blur
    Motion { angle: f32, distance: f32 },
}

/// Blur filter effect
#[derive(Debug, Clone)]
pub struct BlurFilter {
    pub blur_type: BlurType,
    pub preserve_alpha: bool,
}

impl BlurFilter {
    pub fn gaussian(radius: f32) -> Self {
        Self {
            blur_type: BlurType::Gaussian { radius },
            preserve_alpha: true,
        }
    }
    
    pub fn box_blur(radius: f32) -> Self {
        Self {
            blur_type: BlurType::Box { radius },
            preserve_alpha: true,
        }
    }
    
    pub fn motion(angle: f32, distance: f32) -> Self {
        Self {
            blur_type: BlurType::Motion { angle, distance },
            preserve_alpha: false,
        }
    }
    
    /// Generate blur kernel for shader
    pub fn generate_kernel(&self) -> Vec<f32> {
        match self.blur_type {
            BlurType::Gaussian { radius } => {
                // Generate 1D Gaussian kernel (will be applied in two passes)
                let kernel_size = ((radius * 3.0).ceil() as usize * 2 + 1).min(33);
                let mut kernel = vec![0.0; kernel_size];
                let center = kernel_size / 2;
                let sigma = radius / 3.0;
                let two_sigma_sq = 2.0 * sigma * sigma;
                
                let mut sum = 0.0;
                for i in 0..kernel_size {
                    let x = (i as i32 - center as i32) as f32;
                    let weight = (-x * x / two_sigma_sq).exp();
                    kernel[i] = weight;
                    sum += weight;
                }
                
                // Normalize
                for weight in &mut kernel {
                    *weight /= sum;
                }
                
                kernel
            }
            BlurType::Box { radius } => {
                // Simple box blur kernel
                let kernel_size = ((radius * 2.0).ceil() as usize + 1).min(33);
                let weight = 1.0 / kernel_size as f32;
                vec![weight; kernel_size]
            }
            BlurType::Motion { angle, distance } => {
                // Motion blur kernel along a line
                let steps = (distance.ceil() as usize).min(33);
                let weight = 1.0 / steps as f32;
                vec![weight; steps]
            }
        }
    }
}

/// Drop shadow filter (combines shadow and blur)
#[derive(Debug, Clone)]
pub struct DropShadowFilter {
    pub offset: Vector2<f32>,
    pub blur_radius: f32,
    pub color: Color,
}

impl DropShadowFilter {
    pub fn new(offset_x: f32, offset_y: f32, blur_radius: f32, color: Color) -> Self {
        Self {
            offset: Vector2::new(offset_x, offset_y),
            blur_radius,
            color,
        }
    }
}

/// Color matrix filter for effects like brightness, contrast, saturation
#[derive(Debug, Clone)]
pub struct ColorMatrixFilter {
    /// 5x4 color transformation matrix
    pub matrix: [[f32; 5]; 4],
}

impl ColorMatrixFilter {
    /// Create identity matrix (no change)
    pub fn identity() -> Self {
        Self {
            matrix: [
                [1.0, 0.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 1.0, 0.0],
            ],
        }
    }
    
    /// Brightness adjustment (-1.0 to 1.0)
    pub fn brightness(amount: f32) -> Self {
        Self {
            matrix: [
                [1.0, 0.0, 0.0, 0.0, amount],
                [0.0, 1.0, 0.0, 0.0, amount],
                [0.0, 0.0, 1.0, 0.0, amount],
                [0.0, 0.0, 0.0, 1.0, 0.0],
            ],
        }
    }
    
    /// Contrast adjustment (0.0 to 2.0, 1.0 = normal)
    pub fn contrast(amount: f32) -> Self {
        let offset = 0.5 - amount * 0.5;
        Self {
            matrix: [
                [amount, 0.0, 0.0, 0.0, offset],
                [0.0, amount, 0.0, 0.0, offset],
                [0.0, 0.0, amount, 0.0, offset],
                [0.0, 0.0, 0.0, 1.0, 0.0],
            ],
        }
    }
    
    /// Saturation adjustment (0.0 = grayscale, 1.0 = normal, >1.0 = oversaturated)
    pub fn saturation(amount: f32) -> Self {
        let r = 0.213 * (1.0 - amount);
        let g = 0.715 * (1.0 - amount);
        let b = 0.072 * (1.0 - amount);
        
        Self {
            matrix: [
                [r + amount, g, b, 0.0, 0.0],
                [r, g + amount, b, 0.0, 0.0],
                [r, g, b + amount, 0.0, 0.0],
                [0.0, 0.0, 0.0, 1.0, 0.0],
            ],
        }
    }
    
    /// Convert to grayscale
    pub fn grayscale() -> Self {
        Self::saturation(0.0)
    }
    
    /// Sepia tone effect
    pub fn sepia() -> Self {
        Self {
            matrix: [
                [0.393, 0.769, 0.189, 0.0, 0.0],
                [0.349, 0.686, 0.168, 0.0, 0.0],
                [0.272, 0.534, 0.131, 0.0, 0.0],
                [0.0, 0.0, 0.0, 1.0, 0.0],
            ],
        }
    }
}

/// Combined filter stack
#[derive(Debug, Clone)]
pub struct FilterStack {
    pub filters: Vec<Filter>,
}

/// Individual filter types
#[derive(Debug, Clone)]
pub enum Filter {
    Blur(BlurFilter),
    DropShadow(DropShadowFilter),
    ColorMatrix(ColorMatrixFilter),
}

impl FilterStack {
    pub fn new() -> Self {
        Self {
            filters: Vec::new(),
        }
    }
    
    pub fn add_blur(mut self, blur: BlurFilter) -> Self {
        self.filters.push(Filter::Blur(blur));
        self
    }
    
    pub fn add_drop_shadow(mut self, shadow: DropShadowFilter) -> Self {
        self.filters.push(Filter::DropShadow(shadow));
        self
    }
    
    pub fn add_color_matrix(mut self, matrix: ColorMatrixFilter) -> Self {
        self.filters.push(Filter::ColorMatrix(matrix));
        self
    }
}

/// Clipping mask types
#[derive(Debug, Clone)]
pub enum ClipMask {
    /// Rectangular clip region
    Rectangle {
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        radius: f32,
    },
    /// Circular clip region
    Circle {
        center_x: f32,
        center_y: f32,
        radius: f32,
    },
    /// Path-based clip region
    Path {
        path_data: String,
    },
}

impl ClipMask {
    /// Check if a point is inside the clip region
    pub fn contains_point(&self, x: f32, y: f32) -> bool {
        match self {
            ClipMask::Rectangle { x: rx, y: ry, width, height, radius: _ } => {
                x >= *rx && x <= *rx + *width && y >= *ry && y <= *ry + *height
            }
            ClipMask::Circle { center_x, center_y, radius } => {
                let dx = x - center_x;
                let dy = y - center_y;
                (dx * dx + dy * dy) <= radius * radius
            }
            ClipMask::Path { .. } => {
                // Path clipping requires more complex point-in-polygon test
                true // Placeholder
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_gaussian_kernel() {
        let blur = BlurFilter::gaussian(3.0);
        let kernel = blur.generate_kernel();
        
        // Kernel should be normalized (sum to 1.0)
        let sum: f32 = kernel.iter().sum();
        assert!((sum - 1.0).abs() < 0.001);
        
        // Center weight should be highest
        let center = kernel.len() / 2;
        for (i, &weight) in kernel.iter().enumerate() {
            if i != center {
                assert!(weight < kernel[center]);
            }
        }
    }
    
    #[test]
    fn test_clip_mask() {
        let rect_clip = ClipMask::Rectangle {
            x: 10.0,
            y: 10.0,
            width: 100.0,
            height: 100.0,
            radius: 0.0,
        };
        
        assert!(rect_clip.contains_point(50.0, 50.0));
        assert!(!rect_clip.contains_point(5.0, 5.0));
        assert!(!rect_clip.contains_point(150.0, 50.0));
    }
}