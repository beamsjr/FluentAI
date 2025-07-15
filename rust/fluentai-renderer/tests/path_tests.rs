/// Tests for path rendering functionality
use fluentai_renderer::path::{Path, PathBuilder, PathCommand};
use fluentai_renderer::primitives::{Point2D, Color};

mod test_helpers;
use test_helpers::*;

#[test]
fn test_path_builder_basic() {
    let mut builder = PathBuilder::new();
    
    builder.move_to(Point2D::new(0.0, 0.0));
    builder.line_to(Point2D::new(100.0, 0.0));
    builder.line_to(Point2D::new(100.0, 100.0));
    builder.close();
    
    let path = builder.build();
    
    assert_eq!(path.commands.len(), 4);
    
    match &path.commands[0] {
        PathCommand::MoveTo(p) => {
            assert_eq!(p.x, 0.0);
            assert_eq!(p.y, 0.0);
        }
        _ => panic!("Expected MoveTo command"),
    }
    
    match &path.commands[3] {
        PathCommand::Close => {}
        _ => panic!("Expected Close command"),
    }
}

#[test]
fn test_path_quadratic_bezier() {
    let mut builder = PathBuilder::new();
    
    let start = Point2D::new(0.0, 0.0);
    let control = Point2D::new(50.0, -50.0);
    let end = Point2D::new(100.0, 0.0);
    
    builder.move_to(start);
    builder.quadratic_to(control, end);
    
    let path = builder.build();
    
    assert_eq!(path.commands.len(), 2);
    
    match &path.commands[1] {
        PathCommand::QuadraticTo(cp, ep) => {
            assert_eq!(cp.x, control.x);
            assert_eq!(cp.y, control.y);
            assert_eq!(ep.x, end.x);
            assert_eq!(ep.y, end.y);
        }
        _ => panic!("Expected QuadraticTo command"),
    }
}

#[test]
fn test_path_cubic_bezier() {
    let mut builder = PathBuilder::new();
    
    let start = Point2D::new(0.0, 0.0);
    let control1 = Point2D::new(30.0, -30.0);
    let control2 = Point2D::new(70.0, -30.0);
    let end = Point2D::new(100.0, 0.0);
    
    builder.move_to(start);
    builder.cubic_to(control1, control2, end);
    
    let path = builder.build();
    
    assert_eq!(path.commands.len(), 2);
    
    match &path.commands[1] {
        PathCommand::CubicTo(cp1, cp2, ep) => {
            assert_eq!(cp1.x, control1.x);
            assert_eq!(cp1.y, control1.y);
            assert_eq!(cp2.x, control2.x);
            assert_eq!(cp2.y, control2.y);
            assert_eq!(ep.x, end.x);
            assert_eq!(ep.y, end.y);
        }
        _ => panic!("Expected CubicTo command"),
    }
}

#[test]
fn test_path_arc() {
    let mut builder = PathBuilder::new();
    
    let center = Point2D::new(50.0, 50.0);
    let radius = 25.0;
    let start_angle = 0.0;
    let end_angle = std::f32::consts::PI / 2.0; // 90 degrees
    
    builder.move_to(Point2D::new(center.x + radius, center.y));
    builder.arc(center, radius, start_angle, end_angle, false);
    
    let path = builder.build();
    
    // Arc should be approximated with multiple segments
    assert!(path.commands.len() > 2, "Arc should generate multiple commands");
}

#[test]
fn test_path_rect() {
    let mut builder = PathBuilder::new();
    
    let origin = Point2D::new(10.0, 20.0);
    let width = 100.0;
    let height = 50.0;
    
    builder.rect(origin, width, height);
    
    let path = builder.build();
    
    // Rectangle should have: MoveTo + 4 LineTo + Close = 6 commands
    assert_eq!(path.commands.len(), 6);
    
    // Verify it's closed
    match path.commands.last() {
        Some(PathCommand::Close) => {}
        _ => panic!("Rectangle path should be closed"),
    }
}

#[test]
fn test_path_circle() {
    let mut builder = PathBuilder::new();
    
    let center = Point2D::new(50.0, 50.0);
    let radius = 30.0;
    
    builder.circle(center, radius);
    
    let path = builder.build();
    
    // Circle should have many commands for smooth curve
    assert!(path.commands.len() > 4, "Circle should have multiple segments");
    
    // Should be closed
    match path.commands.last() {
        Some(PathCommand::Close) => {}
        _ => panic!("Circle path should be closed"),
    }
}

#[test]
fn test_path_tessellation() {
    let mut builder = PathBuilder::new();
    
    // Create a simple triangle
    builder.move_to(Point2D::new(50.0, 0.0));
    builder.line_to(Point2D::new(100.0, 100.0));
    builder.line_to(Point2D::new(0.0, 100.0));
    builder.close();
    
    let path = builder.build();
    let vertices = path.tessellate(Color::red());
    
    // Triangle should tessellate to at least 3 vertices
    assert!(vertices.len() >= 3, "Triangle should have at least 3 vertices");
    
    // All vertices should have the specified color
    for vertex in &vertices {
        assert!(colors_equal(&Color::from_array(vertex.color), &Color::red(), 0.001));
    }
}

#[test]
fn test_path_stroke_tessellation() {
    let mut builder = PathBuilder::new();
    
    // Create a simple line
    builder.move_to(Point2D::new(0.0, 0.0));
    builder.line_to(Point2D::new(100.0, 0.0));
    
    let path = builder.build();
    let stroke_width = 5.0;
    let vertices = path.tessellate_stroke(Color::blue(), stroke_width);
    
    // Stroke should create a rectangle around the line
    assert!(vertices.len() >= 4, "Stroke should create at least 4 vertices");
}

#[test]
fn test_path_svg_parsing() {
    let svg_path = "M 10 10 L 90 10 L 90 90 L 10 90 Z";
    let path = Path::from_svg(svg_path).expect("Should parse valid SVG path");
    
    assert_eq!(path.commands.len(), 5);
    
    // Verify parsed commands
    match &path.commands[0] {
        PathCommand::MoveTo(p) => {
            assert_eq!(p.x, 10.0);
            assert_eq!(p.y, 10.0);
        }
        _ => panic!("Expected MoveTo"),
    }
    
    match &path.commands[4] {
        PathCommand::Close => {}
        _ => panic!("Expected Close"),
    }
}

#[test]
fn test_path_svg_curves() {
    let svg_path = "M 10 10 Q 50 -20 90 10";
    let path = Path::from_svg(svg_path).expect("Should parse quadratic curve");
    
    assert_eq!(path.commands.len(), 2);
    
    match &path.commands[1] {
        PathCommand::QuadraticTo(cp, ep) => {
            assert_eq!(cp.x, 50.0);
            assert_eq!(cp.y, -20.0);
            assert_eq!(ep.x, 90.0);
            assert_eq!(ep.y, 10.0);
        }
        _ => panic!("Expected QuadraticTo"),
    }
}

#[test]
fn test_path_bounds_calculation() {
    let mut builder = PathBuilder::new();
    
    builder.move_to(Point2D::new(10.0, 20.0));
    builder.line_to(Point2D::new(100.0, 20.0));
    builder.line_to(Point2D::new(100.0, 80.0));
    builder.line_to(Point2D::new(10.0, 80.0));
    builder.close();
    
    let path = builder.build();
    let bounds = path.bounds();
    
    assert_eq!(bounds.x, 10.0);
    assert_eq!(bounds.y, 20.0);
    assert_eq!(bounds.width, 90.0);
    assert_eq!(bounds.height, 60.0);
}

#[test]
fn test_path_empty() {
    let builder = PathBuilder::new();
    let path = builder.build();
    
    assert_eq!(path.commands.len(), 0);
    assert!(path.is_empty());
    
    let vertices = path.tessellate(Color::white());
    assert_eq!(vertices.len(), 0, "Empty path should produce no vertices");
}

#[test]
fn test_path_multiple_subpaths() {
    let mut builder = PathBuilder::new();
    
    // First subpath - square
    builder.move_to(Point2D::new(0.0, 0.0));
    builder.line_to(Point2D::new(50.0, 0.0));
    builder.line_to(Point2D::new(50.0, 50.0));
    builder.line_to(Point2D::new(0.0, 50.0));
    builder.close();
    
    // Second subpath - triangle
    builder.move_to(Point2D::new(60.0, 0.0));
    builder.line_to(Point2D::new(110.0, 50.0));
    builder.line_to(Point2D::new(60.0, 50.0));
    builder.close();
    
    let path = builder.build();
    
    // Count MoveTo commands to verify two subpaths
    let move_count = path.commands.iter().filter(|cmd| {
        matches!(cmd, PathCommand::MoveTo(_))
    }).count();
    
    assert_eq!(move_count, 2, "Should have two subpaths");
}

#[test]
fn test_path_transform() {
    let mut builder = PathBuilder::new();
    
    builder.move_to(Point2D::new(10.0, 10.0));
    builder.line_to(Point2D::new(20.0, 10.0));
    
    let mut path = builder.build();
    
    // Apply translation
    let translation = Point2D::new(5.0, 5.0);
    path.transform_translate(translation);
    
    match &path.commands[0] {
        PathCommand::MoveTo(p) => {
            assert_eq!(p.x, 15.0);
            assert_eq!(p.y, 15.0);
        }
        _ => panic!("Expected MoveTo"),
    }
}

// Mock implementations for testing
impl Path {
    fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
    
    fn bounds(&self) -> test_helpers::Rect {
        let mut min_x = f32::MAX;
        let mut min_y = f32::MAX;
        let mut max_x = f32::MIN;
        let mut max_y = f32::MIN;
        
        for cmd in &self.commands {
            let point = match cmd {
                PathCommand::MoveTo(p) | PathCommand::LineTo(p) => p,
                PathCommand::QuadraticTo(_, p) | PathCommand::CubicTo(_, _, p) => p,
                PathCommand::Close => continue,
            };
            
            min_x = min_x.min(point.x);
            min_y = min_y.min(point.y);
            max_x = max_x.max(point.x);
            max_y = max_y.max(point.y);
        }
        
        test_helpers::Rect::new(min_x, min_y, max_x - min_x, max_y - min_y)
    }
    
    fn transform_translate(&mut self, offset: Point2D) {
        for cmd in &mut self.commands {
            match cmd {
                PathCommand::MoveTo(p) | PathCommand::LineTo(p) => {
                    p.x += offset.x;
                    p.y += offset.y;
                }
                PathCommand::QuadraticTo(cp, ep) => {
                    cp.x += offset.x;
                    cp.y += offset.y;
                    ep.x += offset.x;
                    ep.y += offset.y;
                }
                PathCommand::CubicTo(cp1, cp2, ep) => {
                    cp1.x += offset.x;
                    cp1.y += offset.y;
                    cp2.x += offset.x;
                    cp2.y += offset.y;
                    ep.x += offset.x;
                    ep.y += offset.y;
                }
                PathCommand::Close => {}
            }
        }
    }
}