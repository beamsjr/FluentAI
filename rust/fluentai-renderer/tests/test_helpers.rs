/// Test helpers and utilities for renderer testing
use fluentai_renderer::primitives::{Color, Position2D, Size2D};
use cgmath;

/// Create a test color
pub fn test_color() -> Color {
    Color::new(1.0, 0.5, 0.25, 1.0)
}

/// Extension methods for Color in tests
pub trait ColorTestExt {
    fn gray() -> Self;
    fn yellow() -> Self;
}

impl ColorTestExt for Color {
    fn gray() -> Self {
        Color::new(0.5, 0.5, 0.5, 1.0)
    }
    
    fn yellow() -> Self {
        Color::new(1.0, 1.0, 0.0, 1.0)
    }
}

/// Create a test rectangle
#[derive(Debug, Clone)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl Rect {
    pub fn new(x: f32, y: f32, width: f32, height: f32) -> Self {
        Self { x, y, width, height }
    }
}

pub fn test_rect() -> Rect {
    Rect::new(10.0, 20.0, 100.0, 50.0)
}

/// Alias for 2D point
pub type Point2D = cgmath::Vector2<f32>;

/// Create a test point
pub fn test_point() -> Point2D {
    Point2D::new(50.0, 75.0)
}

/// Create a test size
pub fn test_size() -> Size2D {
    Size2D::new(200.0, 150.0)
}

/// Compare floats with tolerance
pub fn approx_equal(a: f32, b: f32, tolerance: f32) -> bool {
    (a - b).abs() < tolerance
}

/// Compare colors with tolerance
pub fn colors_equal(a: &Color, b: &Color, tolerance: f32) -> bool {
    approx_equal(a.r, b.r, tolerance)
        && approx_equal(a.g, b.g, tolerance)
        && approx_equal(a.b, b.b, tolerance)
        && approx_equal(a.a, b.a, tolerance)
}

/// Mock rendering context for testing
pub struct MockRenderContext {
    pub draw_calls: Vec<DrawCall>,
}

#[derive(Debug, Clone)]
pub enum DrawCall {
    Rectangle { rect: Rect, color: Color },
    Circle { center: Point2D, radius: f32, color: Color },
    Text { text: String, position: Point2D, color: Color },
    Path { points: Vec<Point2D>, color: Color },
}

impl MockRenderContext {
    pub fn new() -> Self {
        Self {
            draw_calls: Vec::new(),
        }
    }

    pub fn draw_rect(&mut self, rect: Rect, color: Color) {
        self.draw_calls.push(DrawCall::Rectangle { rect, color });
    }

    pub fn draw_circle(&mut self, center: Point2D, radius: f32, color: Color) {
        self.draw_calls.push(DrawCall::Circle { center, radius, color });
    }

    pub fn draw_text(&mut self, text: String, position: Point2D, color: Color) {
        self.draw_calls.push(DrawCall::Text { text, position, color });
    }

    pub fn draw_path(&mut self, points: Vec<Point2D>, color: Color) {
        self.draw_calls.push(DrawCall::Path { points, color });
    }

    pub fn clear(&mut self) {
        self.draw_calls.clear();
    }

    pub fn draw_call_count(&self) -> usize {
        self.draw_calls.len()
    }
}

/// Create a test gradient stop list
pub fn test_gradient_stops() -> Vec<(f32, Color)> {
    vec![
        (0.0, Color::red()),
        (0.5, Color::new(1.0, 1.0, 0.0, 1.0)), // yellow
        (1.0, Color::blue()),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_approx_equal() {
        assert!(approx_equal(1.0, 1.0001, 0.001));
        assert!(!approx_equal(1.0, 1.1, 0.001));
    }

    #[test]
    fn test_colors_equal() {
        let c1 = Color::new(0.5, 0.5, 0.5, 1.0);
        let c2 = Color::new(0.5001, 0.4999, 0.5, 1.0);
        let c3 = Color::new(0.6, 0.5, 0.5, 1.0);
        
        assert!(colors_equal(&c1, &c2, 0.001));
        assert!(!colors_equal(&c1, &c3, 0.001));
    }

    #[test]
    fn test_mock_render_context() {
        let mut ctx = MockRenderContext::new();
        assert_eq!(ctx.draw_call_count(), 0);
        
        ctx.draw_rect(test_rect(), test_color());
        assert_eq!(ctx.draw_call_count(), 1);
        
        ctx.draw_circle(test_point(), 25.0, Color::blue());
        assert_eq!(ctx.draw_call_count(), 2);
        
        ctx.clear();
        assert_eq!(ctx.draw_call_count(), 0);
    }
}