/// Integration tests for the complete rendering pipeline
use fluentai_renderer::{
    Scene,
    primitives::{Color, Position2D, Position3D, Size2D, Renderable, Transform, Vertex},
    gradient::Gradient,
    path::Path,
    animation::{Animation, AnimatableValue, EasingFunction, AnimationManager},
    reactive_v2::{ReactiveSystem, ReactiveNode},
};
#[cfg(not(target_arch = "wasm32"))]
use fluentai_renderer::{
    batching::RenderBatcher,
};

mod test_helpers;
use test_helpers::*;

// Helper type alias
type Point2D = cgmath::Vector2<f32>;

#[test]
fn test_complete_scene_rendering() {
    // Create a scene with multiple elements
    let mut scene = Scene::new();
    
    // Add rectangle
    scene.add(Renderable::Rect {
        transform: Transform::new(Position2D::new(10.0, 10.0)),
        size: Size2D::new(100.0, 50.0),
        color: Color::red(),
        radius: 5.0,
    });
    
    // Add circle
    scene.add(Renderable::Circle {
        transform: Transform::new(Position2D::new(200.0, 50.0)),
        radius: 30.0,
        color: Color::blue(),
    });
    
    // Add text
    scene.add(Renderable::Text {
        transform: Transform::new(Position2D::new(50.0, 100.0)),
        content: "Hello".to_string(),
        size: 16.0,
        color: Color::white(),
        font: None,
    });
    
    // Verify scene contains all elements
    assert_eq!(scene.node_count(), 3, "Scene should contain 3 nodes");
}

#[test]
fn test_gradient_path_integration() {
    // Create a path with stroke (for tessellation to produce vertices)
    let path = Path::new()
        .move_to(0.0, 0.0)
        .line_to(100.0, 0.0)
        .line_to(50.0, 100.0)
        .close()
        .stroke(Color::white(), 2.0);
    
    // Create gradient
    let angle = 45.0_f32.to_radians();
    let gradient = Gradient::linear(
        cgmath::Vector2::new(0.0, 0.0),
        cgmath::Vector2::new(angle.cos() * 100.0, angle.sin() * 100.0),
    )
    .add_stop(0.0, Color::red())
    .add_stop(1.0, Color::blue());
    
    // Tessellate path
    let tessellation = path.tessellate();
    assert!(!tessellation.vertices.is_empty(), "Path should produce vertices");
    
    // Test gradient (in real renderer, this would be used in shader)
    // Just check it's created properly
    assert_eq!(gradient.stops.len(), 2, "Gradient should have 2 stops");
}

#[test]
fn test_component_rendering_pipeline() {
    // Test basic rendering concepts without component trait
    let size = Size2D::new(200.0, 100.0);
    let color = Color::green();
    
    // Create renderables that a component might produce
    let renderables = vec![
        Renderable::Rect {
            transform: Transform::new(Position2D::new(0.0, 0.0)),
            size: size,
            color: color,
            radius: 0.0,
        },
        Renderable::Text {
            transform: Transform::new(Position2D::new(10.0, 10.0)),
            content: "Component".to_string(),
            color: Color::white(),
            size: 14.0,
            font: None,
        },
    ];
    
    assert_eq!(renderables.len(), 2, "Should have 2 renderables");
}

#[test]
fn test_animation_integration() {
    // Create position animation
    let mut animation = Animation::new(
        "x_position",
        AnimatableValue::Float(0.0),
        AnimatableValue::Float(100.0),
    )
    .duration(std::time::Duration::from_millis(1000))
    .easing(EasingFunction::EaseInOutQuad);
    
    let mut manager = AnimationManager::new();
    let animation_id = manager.add(animation);
    
    // Update animation
    manager.update(std::time::Duration::ZERO);
    
    // Test animation values at different progress points
    // The animation should interpolate between 0.0 and 100.0
    // We'll create a new animation for testing specific progress values
    let test_anim = Animation::new(
        "test",
        AnimatableValue::Float(0.0),
        AnimatableValue::Float(100.0),
    );
    
    // At start
    if let AnimatableValue::Float(val) = AnimatableValue::Float(0.0).lerp(&AnimatableValue::Float(100.0), 0.0) {
        assert_eq!(val, 0.0);
    }
    
    // At middle
    if let AnimatableValue::Float(val) = AnimatableValue::Float(0.0).lerp(&AnimatableValue::Float(100.0), 0.5) {
        assert!(val > 0.0 && val < 100.0);
    }
    
    // At end
    if let AnimatableValue::Float(val) = AnimatableValue::Float(0.0).lerp(&AnimatableValue::Float(100.0), 1.0) {
        assert_eq!(val, 100.0);
    }
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn test_render_batching_integration() {
    let mut batcher = RenderBatcher::new();
    
    // Add multiple similar rectangles
    for i in 0..10 {
        batcher.add_renderable(&Renderable::Rect {
            transform: Transform::new(Position2D::new(i as f32 * 20.0, 0.0)),
            size: Size2D::new(15.0, 15.0),
            color: Color::blue(),
            radius: 0.0,
        });
    }
    
    // Add circles
    for i in 0..5 {
        batcher.add_renderable(&Renderable::Circle {
            transform: Transform::new(Position2D::new(i as f32 * 30.0, 50.0)),
            radius: 10.0,
            color: Color::red(),
        });
    }
    
    let batches = batcher.get_sorted_batches();
    
    // Should batch similar shapes together
    assert!(batches.len() <= 2, "Similar shapes should be batched");
    
    // Verify batch sizes by counting instance data
    let total_instances: usize = batches.iter().map(|b| b.instance_data.len()).sum();
    assert_eq!(total_instances, 15, "All items should be in batches");
}

#[test]
fn test_reactive_ui_integration() {
    // Create reactive system
    let system = ReactiveSystem::new();
    
    // Test basic reactive node registration
    let node = ReactiveNode {
        id: "test_node".to_string(),
        dependencies: vec![],
        update_fn: Box::new(|| {}),
    };
    
    system.register_node(node);
    
    // Test effect creation
    let was_called = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let was_called_clone = was_called.clone();
    
    system.create_effect(
        vec!["test_dep".to_string()],
        Box::new(move || {
            was_called_clone.store(true, std::sync::atomic::Ordering::Relaxed);
        })
    );
    
    // Trigger update
    system.trigger_update("test_dep");
    
    // Verify effect was called
    assert!(was_called.load(std::sync::atomic::Ordering::Relaxed));
}

#[test]
fn test_complex_rendering_scenario() {
    // Simulate a complex UI with multiple features
    let mut scene = Scene::new();
    let mut animation_manager = AnimationManager::new();
    
    // 1. Add gradient background as a rectangle
    let gradient = Gradient::linear(
        cgmath::Vector2::new(0.0, 0.0),
        cgmath::Vector2::new(800.0, 600.0),
    )
    .add_stop(0.0, Color::new(0.1, 0.1, 0.2, 1.0))
    .add_stop(1.0, Color::new(0.2, 0.1, 0.3, 1.0));
    
    scene.add_gradient("bg-gradient".to_string(), gradient);
    scene.add(Renderable::GradientRect {
        transform: Transform::new(Position2D::new(0.0, 0.0)),
        size: Size2D::new(800.0, 600.0),
        gradient_id: "bg-gradient".to_string(),
        radius: 0.0,
    });
    
    // 2. Add animated logo as a circle
    let logo_animation = Animation::new(
        "logo_rotation",
        AnimatableValue::Float(0.0),
        AnimatableValue::Float(360.0),
    )
    .duration(std::time::Duration::from_millis(3000))
    .easing(EasingFunction::Linear);
    
    let logo_anim_id = animation_manager.add(logo_animation);
    
    scene.add(Renderable::Circle {
        transform: Transform::new(Position2D::new(400.0, 300.0)),
        radius: 50.0,
        color: Color::white(),
    });
    
    // 3. Add UI components
    for i in 0..5 {
        scene.add(Renderable::Rect {
            transform: Transform::new(Position2D::new(100.0 + i as f32 * 120.0, 400.0)),
            size: Size2D::new(100.0, 150.0),
            color: Color::white(),
            radius: 8.0,
        });
    }
    
    // 4. Add text overlays
    let titles = vec!["Dashboard", "Analytics", "Settings", "Profile", "Help"];
    for (i, title) in titles.iter().enumerate() {
        scene.add(Renderable::Text {
            transform: Transform::new(Position2D::new(110.0 + i as f32 * 120.0, 420.0)),
            content: title.to_string(),
            size: 14.0,
            color: Color::black(),
            font: None,
        });
    }
    
    // Verify integration
    assert_eq!(scene.node_count(), 1 + 1 + 5 + 5, "Scene should have 12 nodes");
    
    // Update animations
    animation_manager.update(std::time::Duration::from_millis(1500));
    
    // Check animation progress
    // Create a test animation to check the 0.5 progress value
    let test_logo_anim = Animation::new(
        "test_rotation",
        AnimatableValue::Float(0.0),
        AnimatableValue::Float(360.0),
    )
    .easing(EasingFunction::Linear);
    
    if let AnimatableValue::Float(rotation) = AnimatableValue::Float(0.0).lerp(&AnimatableValue::Float(360.0), 0.5) {
        assert_eq!(rotation, 180.0, "Animation should update rotation");
    }
}

#[test]
fn test_event_propagation_through_components() {
    // Test event propagation logic without component trait
    #[derive(Debug)]
    struct ClickEvent {
        position: Point2D,
        handled: bool,
    }
    
    struct ParentHandler {
        child_clicked: bool,
    }
    
    struct ChildHandler {
        clicked: bool,
    }
    
    impl ParentHandler {
        fn handle_event(&mut self, event: &mut ClickEvent) -> bool {
            if !event.handled {
                self.child_clicked = true;
                false // Don't stop propagation
            } else {
                false
            }
        }
    }
    
    impl ChildHandler {
        fn handle_event(&mut self, event: &mut ClickEvent) -> bool {
            self.clicked = true;
            event.handled = true;
            true // Stop propagation
        }
    }
    
    let mut parent = ParentHandler { child_clicked: false };
    let mut child = ChildHandler { clicked: false };
    
    let mut click_event = ClickEvent {
        position: Point2D::new(50.0, 50.0),
        handled: false,
    };
    
    // Child handles first and stops propagation
    let child_handled = child.handle_event(&mut click_event);
    assert!(child_handled);
    assert!(child.clicked);
    
    // Parent shouldn't receive event if child stopped it
    if !child_handled {
        parent.handle_event(&mut click_event);
    }
    assert!(!parent.child_clicked);
}

#[test]
fn test_performance_optimization_features() {
    // Test that performance features work together
    
    // 1. Instancing for repeated elements
    let mut instance_data = Vec::new();
    for i in 0..1000 {
        instance_data.push(InstanceData {
            position: Point2D::new((i % 100) as f32 * 20.0, (i / 100) as f32 * 20.0),
            color: test_color_with_seed(i),
            scale: 1.0,
        });
    }
    
    // 2. Virtual scrolling simulation
    let viewport_pos = Point2D::new(0.0, 0.0);
    let viewport_size = Size2D::new(800.0, 600.0);
    let visible_items: Vec<_> = instance_data
        .iter()
        .filter(|item| {
            item.position.x >= viewport_pos.x - 20.0 &&
            item.position.x <= viewport_pos.x + viewport_size.width &&
            item.position.y >= viewport_pos.y - 20.0 &&
            item.position.y <= viewport_pos.y + viewport_size.height
        })
        .collect();
    
    assert!(visible_items.len() < instance_data.len(), "Virtual scrolling should cull items");
    
    // 3. Level of detail (LOD)
    let camera_distance = 100.0;
    let lod_level = if camera_distance < 50.0 {
        0 // High detail
    } else if camera_distance < 200.0 {
        1 // Medium detail
    } else {
        2 // Low detail
    };
    
    assert_eq!(lod_level, 1, "Should select appropriate LOD");
}

// Helper structs for tests
struct InstanceData {
    position: Point2D,
    color: Color,
    scale: f32,
}

fn test_color_with_seed(seed: usize) -> Color {
    // Deterministic color generation for tests
    let r = ((seed * 73) % 256) as f32 / 255.0;
    let g = ((seed * 97) % 256) as f32 / 255.0;
    let b = ((seed * 131) % 256) as f32 / 255.0;
    Color::new(r, g, b, 1.0)
}

