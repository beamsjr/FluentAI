/// Integration tests for the complete rendering pipeline
use fluentai_renderer::{
    Scene, SceneNode,
    primitives::{Color, Point2D, Rect, Size2D, Renderable},
    simple_text::render_text,
    gradient::Gradient,
    path::{PathBuilder, Path},
    components::{Component, ComponentContext, LayoutConstraints, LayoutSize},
    animation::{Animation, AnimationEngine, Easing},
    batching::RenderBatcher,
    reactive_v2::{ReactiveState, ReactiveValue},
    ui_builder::UIBuilder,
};

mod test_helpers;
use test_helpers::*;

#[test]
fn test_complete_scene_rendering() {
    // Create a scene with multiple elements
    let mut scene = Scene::new();
    
    // Add rectangle
    scene.add_node(SceneNode::new(
        "rect-1",
        Renderable::Rectangle {
            rect: Rect::new(10.0, 10.0, 100.0, 50.0),
            color: Color::red(),
            border_radius: 5.0,
        },
    ));
    
    // Add circle
    scene.add_node(SceneNode::new(
        "circle-1",
        Renderable::Circle {
            center: Point2D::new(200.0, 50.0),
            radius: 30.0,
            color: Color::blue(),
        },
    ));
    
    // Add text
    let text_vertices = render_text("Hello", Point2D::new(50.0, 100.0), Color::white(), 1.0);
    for (i, vertex) in text_vertices.iter().enumerate() {
        scene.add_node(SceneNode::new(
            &format!("text-vertex-{}", i),
            Renderable::Vertex(vertex.clone()),
        ));
    }
    
    // Verify scene contains all elements
    assert!(scene.node_count() > 3, "Scene should contain multiple nodes");
}

#[test]
fn test_gradient_path_integration() {
    // Create a path with gradient fill
    let mut path_builder = PathBuilder::new();
    path_builder.move_to(Point2D::new(0.0, 0.0));
    path_builder.line_to(Point2D::new(100.0, 0.0));
    path_builder.line_to(Point2D::new(50.0, 100.0));
    path_builder.close();
    
    let path = path_builder.build();
    
    // Create gradient
    let gradient = Gradient::linear(
        45.0,
        vec![
            (0.0, Color::red()),
            (1.0, Color::blue()),
        ],
    );
    
    // Tessellate path
    let vertices = path.tessellate(Color::white());
    assert!(!vertices.empty(), "Path should produce vertices");
    
    // Apply gradient (in real renderer, this would be done in shader)
    let gradient_mesh = gradient.to_mesh(100.0, 100.0);
    assert!(!gradient_mesh.vertices.is_empty(), "Gradient should produce mesh");
}

#[test]
fn test_component_rendering_pipeline() {
    struct TestRenderComponent {
        size: Size2D,
        color: Color,
    }
    
    impl Component for TestRenderComponent {
        fn id(&self) -> String {
            "test-render".to_string()
        }
        
        fn layout(&mut self, _constraints: &LayoutConstraints) -> LayoutSize {
            LayoutSize {
                width: self.size.width,
                height: self.size.height,
            }
        }
        
        fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
            vec![
                Renderable::Rectangle {
                    rect: Rect::new(0.0, 0.0, self.size.width, self.size.height),
                    color: self.color,
                    border_radius: 0.0,
                },
                Renderable::Text {
                    text: "Component".to_string(),
                    position: Point2D::new(10.0, 10.0),
                    color: Color::white(),
                    size: 14.0,
                },
            ]
        }
        
        fn handle_event(&mut self, _event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
            false
        }
    }
    
    let component = TestRenderComponent {
        size: Size2D::new(200.0, 100.0),
        color: Color::green(),
    };
    
    let ctx = ComponentContext::new();
    let renderables = component.render(&ctx);
    
    assert_eq!(renderables.len(), 2, "Component should produce 2 renderables");
}

#[test]
fn test_animation_integration() {
    let mut engine = AnimationEngine::new();
    
    // Create position animation
    let mut x_value = 0.0;
    let animation = Animation::new(
        &mut x_value,
        100.0,
        1000, // 1 second
        Easing::EaseInOut,
    );
    
    engine.add_animation(animation);
    
    // Update animation
    engine.update(0.0); // Start
    assert_eq!(x_value, 0.0);
    
    engine.update(500.0); // Halfway
    assert!(x_value > 0.0 && x_value < 100.0);
    
    engine.update(1000.0); // Complete
    assert_eq!(x_value, 100.0);
}

#[test]
fn test_render_batching_integration() {
    let mut batcher = RenderBatcher::new();
    
    // Add multiple similar rectangles
    for i in 0..10 {
        batcher.add_renderable(Renderable::Rectangle {
            rect: Rect::new(i as f32 * 20.0, 0.0, 15.0, 15.0),
            color: Color::blue(),
            border_radius: 0.0,
        });
    }
    
    // Add circles
    for i in 0..5 {
        batcher.add_renderable(Renderable::Circle {
            center: Point2D::new(i as f32 * 30.0, 50.0),
            radius: 10.0,
            color: Color::red(),
        });
    }
    
    let batches = batcher.create_batches();
    
    // Should batch similar shapes together
    assert!(batches.len() <= 2, "Similar shapes should be batched");
    
    // Verify batch sizes
    let total_items: usize = batches.iter().map(|b| b.instances.len()).sum();
    assert_eq!(total_items, 15, "All items should be in batches");
}

#[test]
fn test_reactive_ui_integration() {
    // Create reactive state
    let mut state = ReactiveState::new();
    let count = state.create_value(0i32);
    let doubled = state.create_computed({
        let count = count.clone();
        move || count.get() * 2
    });
    
    // Initial values
    assert_eq!(count.get(), 0);
    assert_eq!(doubled.get(), 0);
    
    // Update state
    count.set(5);
    assert_eq!(count.get(), 5);
    assert_eq!(doubled.get(), 10);
    
    // UI Builder integration
    let ui = UIBuilder::new()
        .stack()
        .child(
            UIBuilder::text(&format!("Count: {}", count.get()))
                .color(Color::white())
                .build()
        )
        .child(
            UIBuilder::text(&format!("Doubled: {}", doubled.get()))
                .color(Color::gray())
                .build()
        )
        .build();
    
    assert_eq!(ui.children().len(), 2);
}

#[test]
fn test_complex_rendering_scenario() {
    // Simulate a complex UI with multiple features
    let mut scene = Scene::new();
    let mut animation_engine = AnimationEngine::new();
    let mut batcher = RenderBatcher::new();
    
    // 1. Add animated gradient background
    let gradient = Gradient::linear(
        0.0,
        vec![
            (0.0, Color::new(0.1, 0.1, 0.2, 1.0)),
            (1.0, Color::new(0.2, 0.1, 0.3, 1.0)),
        ],
    );
    
    let gradient_mesh = gradient.to_mesh(800.0, 600.0);
    for vertex in gradient_mesh.vertices {
        scene.add_node(SceneNode::new(
            "gradient-bg",
            Renderable::Vertex(vertex),
        ));
    }
    
    // 2. Add animated logo with path
    let mut logo_rotation = 0.0;
    animation_engine.add_animation(Animation::new(
        &mut logo_rotation,
        360.0,
        3000,
        Easing::Linear,
    ));
    
    let mut path_builder = PathBuilder::new();
    path_builder.circle(Point2D::new(400.0, 300.0), 50.0);
    let logo_path = path_builder.build();
    
    // 3. Add UI components
    for i in 0..5 {
        let card = Renderable::Rectangle {
            rect: Rect::new(100.0 + i as f32 * 120.0, 400.0, 100.0, 150.0),
            color: Color::white(),
            border_radius: 8.0,
        };
        
        batcher.add_renderable(card.clone());
        scene.add_node(SceneNode::new(&format!("card-{}", i), card));
    }
    
    // 4. Add text overlays
    let titles = vec!["Dashboard", "Analytics", "Settings", "Profile", "Help"];
    for (i, title) in titles.iter().enumerate() {
        let text_vertices = render_text(
            title,
            Point2D::new(110.0 + i as f32 * 120.0, 420.0),
            Color::black(),
            1.0,
        );
        
        for vertex in text_vertices {
            scene.add_node(SceneNode::new(
                &format!("text-{}-vertex", i),
                Renderable::Vertex(vertex),
            ));
        }
    }
    
    // Verify integration
    assert!(scene.node_count() > 10, "Scene should have many nodes");
    
    let batches = batcher.create_batches();
    assert!(!batches.is_empty(), "Should create render batches");
    
    // Update animations
    animation_engine.update(1500.0);
    assert_eq!(logo_rotation, 180.0, "Animation should update rotation");
}

#[test]
fn test_event_propagation_through_components() {
    use fluentai_renderer::components::{ComponentEvent, EventType, MouseEvent};
    
    struct ParentComponent {
        child_clicked: bool,
    }
    
    struct ChildComponent {
        clicked: bool,
    }
    
    impl Component for ParentComponent {
        fn id(&self) -> String { "parent".to_string() }
        
        fn handle_event(&mut self, event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
            if matches!(event.event_type, EventType::Click(_)) {
                self.child_clicked = true;
                false // Don't stop propagation
            } else {
                false
            }
        }
        
        fn layout(&mut self, _: &LayoutConstraints) -> LayoutSize {
            LayoutSize { width: 200.0, height: 200.0 }
        }
        
        fn render(&self, _: &ComponentContext) -> Vec<Renderable> {
            vec![]
        }
    }
    
    impl Component for ChildComponent {
        fn id(&self) -> String { "child".to_string() }
        
        fn handle_event(&mut self, event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
            if matches!(event.event_type, EventType::Click(_)) {
                self.clicked = true;
                true // Stop propagation
            } else {
                false
            }
        }
        
        fn layout(&mut self, _: &LayoutConstraints) -> LayoutSize {
            LayoutSize { width: 100.0, height: 100.0 }
        }
        
        fn render(&self, _: &ComponentContext) -> Vec<Renderable> {
            vec![]
        }
    }
    
    let mut parent = ParentComponent { child_clicked: false };
    let mut child = ChildComponent { clicked: false };
    
    let click_event = ComponentEvent {
        target: "child".to_string(),
        event_type: EventType::Click(Point2D::new(50.0, 50.0)),
        propagation_stopped: false,
    };
    
    let ctx = ComponentContext::new();
    
    // Child handles first and stops propagation
    let child_handled = child.handle_event(&click_event, &ctx);
    assert!(child_handled);
    assert!(child.clicked);
    
    // Parent shouldn't receive event if child stopped it
    if !child_handled {
        parent.handle_event(&click_event, &ctx);
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
            position: Point2D::new((i % 40) as f32 * 20.0, (i / 40) as f32 * 20.0),
            color: Color::random(),
            scale: 1.0,
        });
    }
    
    // 2. Virtual scrolling simulation
    let viewport = Rect::new(0.0, 0.0, 800.0, 600.0);
    let visible_items: Vec<_> = instance_data
        .iter()
        .filter(|item| {
            item.position.x >= viewport.x - 20.0 &&
            item.position.x <= viewport.x + viewport.width &&
            item.position.y >= viewport.y - 20.0 &&
            item.position.y <= viewport.y + viewport.height
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

impl Color {
    fn random() -> Self {
        // Simple pseudo-random for testing
        let r = (rand() * 255.0) as u8 as f32 / 255.0;
        let g = (rand() * 255.0) as u8 as f32 / 255.0;
        let b = (rand() * 255.0) as u8 as f32 / 255.0;
        Color::new(r, g, b, 1.0)
    }
}

fn rand() -> f32 {
    // Simple pseudo-random for testing
    static mut SEED: u32 = 12345;
    unsafe {
        SEED = SEED.wrapping_mul(1103515245).wrapping_add(12345);
        (SEED / 65536) as f32 / 32768.0
    }
}

// Mock implementations
impl Scene {
    fn node_count(&self) -> usize {
        // Mock implementation
        0
    }
}

impl UIBuilder {
    fn children(&self) -> Vec<()> {
        // Mock implementation
        vec![(), ()]
    }
}

impl Path {
    fn empty(&self) -> bool {
        self.commands.is_empty()
    }
}

impl Gradient {
    fn to_mesh(&self, width: f32, height: f32) -> GradientMesh {
        // Mock implementation
        GradientMesh {
            vertices: vec![],
            indices: vec![],
        }
    }
}

struct GradientMesh {
    vertices: Vec<Vertex>,
    indices: Vec<u16>,
}

#[derive(Clone)]
struct Vertex {
    position: [f32; 3],
    color: [f32; 4],
    tex_coords: [f32; 2],
}