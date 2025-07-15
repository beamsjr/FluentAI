/// Tests for gradient rendering functionality
use fluentai_renderer::gradient::{Gradient, GradientMesh};
use fluentai_renderer::primitives::{Color, Point2D};

mod test_helpers;
use test_helpers::*;

#[test]
fn test_linear_gradient_creation() {
    let stops = test_gradient_stops();
    let gradient = Gradient::linear(0.0, stops.clone());
    
    match gradient {
        Gradient::Linear { angle, stops: grad_stops } => {
            assert_eq!(angle, 0.0);
            assert_eq!(grad_stops.len(), stops.len());
            
            // Verify stops are preserved
            for (i, (pos, color)) in grad_stops.iter().enumerate() {
                assert_eq!(*pos, stops[i].0);
                assert!(colors_equal(color, &stops[i].1, 0.001));
            }
        }
        _ => panic!("Expected linear gradient"),
    }
}

#[test]
fn test_radial_gradient_creation() {
    let center = Point2D::new(50.0, 50.0);
    let radius = 100.0;
    let stops = vec![
        (0.0, Color::white()),
        (1.0, Color::black()),
    ];
    
    let gradient = Gradient::radial(center, radius, stops.clone());
    
    match gradient {
        Gradient::Radial { center: g_center, radius: g_radius, stops: g_stops } => {
            assert_eq!(g_center.x, center.x);
            assert_eq!(g_center.y, center.y);
            assert_eq!(g_radius, radius);
            assert_eq!(g_stops.len(), 2);
        }
        _ => panic!("Expected radial gradient"),
    }
}

#[test]
fn test_conic_gradient_creation() {
    let center = Point2D::new(100.0, 100.0);
    let angle = 45.0;
    let stops = vec![
        (0.0, Color::red()),
        (0.5, Color::green()),
        (1.0, Color::blue()),
    ];
    
    let gradient = Gradient::conic(center, angle, stops.clone());
    
    match gradient {
        Gradient::Conic { center: g_center, angle: g_angle, stops: g_stops } => {
            assert_eq!(g_center.x, center.x);
            assert_eq!(g_center.y, center.y);
            assert_eq!(g_angle, angle);
            assert_eq!(g_stops.len(), 3);
        }
        _ => panic!("Expected conic gradient"),
    }
}

#[test]
fn test_gradient_mesh_generation_linear() {
    let gradient = Gradient::linear(
        90.0, // Vertical gradient
        vec![
            (0.0, Color::red()),
            (1.0, Color::blue()),
        ]
    );
    
    let width = 100.0;
    let height = 100.0;
    let mesh = GradientMesh::from_gradient(&gradient, width, height);
    
    // Should have 4 vertices for a rectangle
    assert_eq!(mesh.vertices.len(), 4);
    
    // Check corners
    assert_eq!(mesh.vertices[0].position[0], 0.0);    // Top-left X
    assert_eq!(mesh.vertices[0].position[1], 0.0);    // Top-left Y
    assert_eq!(mesh.vertices[1].position[0], width);  // Top-right X
    assert_eq!(mesh.vertices[1].position[1], 0.0);    // Top-right Y
    assert_eq!(mesh.vertices[2].position[0], 0.0);    // Bottom-left X
    assert_eq!(mesh.vertices[2].position[1], height); // Bottom-left Y
    assert_eq!(mesh.vertices[3].position[0], width);  // Bottom-right X
    assert_eq!(mesh.vertices[3].position[1], height); // Bottom-right Y
    
    // Should have 6 indices (2 triangles)
    assert_eq!(mesh.indices.len(), 6);
    
    // Verify triangle winding
    assert_eq!(mesh.indices, vec![0, 1, 2, 2, 1, 3]);
}

#[test]
fn test_gradient_mesh_generation_radial() {
    let gradient = Gradient::radial(
        Point2D::new(50.0, 50.0),
        50.0,
        vec![
            (0.0, Color::white()),
            (1.0, Color::black()),
        ]
    );
    
    let width = 100.0;
    let height = 100.0;
    let mesh = GradientMesh::from_gradient(&gradient, width, height);
    
    // Radial gradient should have more vertices for smooth circle
    // At least center + points around the circle
    assert!(mesh.vertices.len() > 4, "Radial gradient should have more than 4 vertices");
    
    // Verify center vertex exists and has correct color
    let center_vertex = &mesh.vertices[0];
    assert_eq!(center_vertex.position[0], 50.0);
    assert_eq!(center_vertex.position[1], 50.0);
    
    // Center should have the first stop color (white)
    assert!(approx_equal(center_vertex.color[0], 1.0, 0.001));
    assert!(approx_equal(center_vertex.color[1], 1.0, 0.001));
    assert!(approx_equal(center_vertex.color[2], 1.0, 0.001));
}

#[test]
fn test_gradient_interpolation_linear() {
    let gradient = Gradient::linear(
        0.0, // Horizontal
        vec![
            (0.0, Color::new(0.0, 0.0, 0.0, 1.0)),
            (1.0, Color::new(1.0, 1.0, 1.0, 1.0)),
        ]
    );
    
    // Test interpolation at various positions
    let color_at_0 = gradient.interpolate_color(0.0);
    assert!(colors_equal(&color_at_0, &Color::new(0.0, 0.0, 0.0, 1.0), 0.001));
    
    let color_at_half = gradient.interpolate_color(0.5);
    assert!(colors_equal(&color_at_half, &Color::new(0.5, 0.5, 0.5, 1.0), 0.001));
    
    let color_at_1 = gradient.interpolate_color(1.0);
    assert!(colors_equal(&color_at_1, &Color::new(1.0, 1.0, 1.0, 1.0), 0.001));
}

#[test]
fn test_gradient_multiple_stops() {
    let gradient = Gradient::linear(
        0.0,
        vec![
            (0.0, Color::red()),
            (0.33, Color::green()),
            (0.66, Color::blue()),
            (1.0, Color::white()),
        ]
    );
    
    match gradient {
        Gradient::Linear { stops, .. } => {
            assert_eq!(stops.len(), 4);
            
            // Verify stops are in order
            for i in 1..stops.len() {
                assert!(stops[i].0 >= stops[i-1].0, "Stops should be in ascending order");
            }
        }
        _ => panic!("Expected linear gradient"),
    }
}

#[test]
fn test_gradient_edge_cases() {
    // Single stop gradient
    let gradient = Gradient::linear(0.0, vec![(0.5, Color::red())]);
    match gradient {
        Gradient::Linear { stops, .. } => {
            assert_eq!(stops.len(), 1);
        }
        _ => panic!("Expected linear gradient"),
    }
    
    // Empty stops (should handle gracefully or panic in constructor)
    // This test depends on implementation - adjust based on actual behavior
}

#[test]
fn test_conic_gradient_angle_normalization() {
    let angles = vec![0.0, 90.0, 180.0, 270.0, 360.0, 450.0, -90.0];
    
    for angle in angles {
        let gradient = Gradient::conic(
            Point2D::new(50.0, 50.0),
            angle,
            vec![(0.0, Color::red()), (1.0, Color::blue())]
        );
        
        match gradient {
            Gradient::Conic { angle: g_angle, .. } => {
                // Angle should be stored as provided (normalization happens during rendering)
                assert_eq!(g_angle, angle);
            }
            _ => panic!("Expected conic gradient"),
        }
    }
}

#[test]
fn test_gradient_mesh_texture_coordinates() {
    let gradient = Gradient::linear(
        0.0,
        vec![(0.0, Color::red()), (1.0, Color::blue())]
    );
    
    let mesh = GradientMesh::from_gradient(&gradient, 100.0, 100.0);
    
    // Verify texture coordinates are set correctly
    for vertex in &mesh.vertices {
        assert!(vertex.tex_coords[0] >= 0.0 && vertex.tex_coords[0] <= 1.0);
        assert!(vertex.tex_coords[1] >= 0.0 && vertex.tex_coords[1] <= 1.0);
    }
}

#[test]
fn test_gradient_color_interpolation_clamping() {
    let gradient = Gradient::linear(
        0.0,
        vec![
            (0.25, Color::black()),
            (0.75, Color::white()),
        ]
    );
    
    // Test interpolation outside stop range
    let color_before = gradient.interpolate_color(0.0); // Before first stop
    let color_after = gradient.interpolate_color(1.0);  // After last stop
    
    // Should clamp to nearest stop color
    assert!(colors_equal(&color_before, &Color::black(), 0.001));
    assert!(colors_equal(&color_after, &Color::white(), 0.001));
}

impl Gradient {
    /// Helper method for testing - interpolate color at position
    fn interpolate_color(&self, t: f32) -> Color {
        match self {
            Gradient::Linear { stops, .. } => {
                if stops.is_empty() {
                    return Color::black();
                }
                if stops.len() == 1 {
                    return stops[0].1;
                }
                
                // Find surrounding stops
                let t_clamped = t.clamp(0.0, 1.0);
                
                for i in 1..stops.len() {
                    if t_clamped <= stops[i].0 {
                        let t0 = stops[i-1].0;
                        let t1 = stops[i].0;
                        let factor = (t_clamped - t0) / (t1 - t0);
                        
                        let c0 = &stops[i-1].1;
                        let c1 = &stops[i].1;
                        
                        return Color::new(
                            c0.r + (c1.r - c0.r) * factor,
                            c0.g + (c1.g - c0.g) * factor,
                            c0.b + (c1.b - c0.b) * factor,
                            c0.a + (c1.a - c0.a) * factor,
                        );
                    }
                }
                
                stops.last().unwrap().1
            }
            _ => Color::black(), // Simplified for other gradient types
        }
    }
}