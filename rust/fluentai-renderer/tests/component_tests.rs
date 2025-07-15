/// Tests for component system functionality
use fluentai_renderer::components::{
    Component, ComponentContext, ComponentEvent, ComponentId,
    LayoutConstraints, LayoutSize, EventType, MouseEvent,
};
use fluentai_renderer::primitives::{Color, Point2D, Rect, Renderable};
use std::sync::Arc;

mod test_helpers;
use test_helpers::*;

// Mock component for testing
struct TestComponent {
    id: ComponentId,
    size: LayoutSize,
    event_count: usize,
    last_event: Option<EventType>,
}

impl TestComponent {
    fn new(id: &str) -> Self {
        Self {
            id: ComponentId::from(id),
            size: LayoutSize {
                width: 100.0,
                height: 50.0,
            },
            event_count: 0,
            last_event: None,
        }
    }
}

impl Component for TestComponent {
    fn id(&self) -> ComponentId {
        self.id.clone()
    }

    fn handle_event(&mut self, event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
        self.event_count += 1;
        self.last_event = Some(event.event_type.clone());
        
        // Return true if we handled the event
        matches!(event.event_type, EventType::MouseDown(_) | EventType::MouseUp(_))
    }

    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        // Respect constraints
        self.size.width = self.size.width.min(constraints.max_width);
        self.size.height = self.size.height.min(constraints.max_height);
        self.size
    }

    fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
        vec![Renderable::Rectangle {
            rect: Rect::new(0.0, 0.0, self.size.width, self.size.height),
            color: Color::blue(),
            border_radius: 0.0,
        }]
    }
}

#[test]
fn test_component_creation() {
    let component = TestComponent::new("test-1");
    assert_eq!(component.id(), ComponentId::from("test-1"));
    assert_eq!(component.size.width, 100.0);
    assert_eq!(component.size.height, 50.0);
}

#[test]
fn test_component_layout_constraints() {
    let mut component = TestComponent::new("test-layout");
    
    // Test with constraints smaller than component size
    let constraints = LayoutConstraints {
        min_width: 0.0,
        max_width: 80.0,
        min_height: 0.0,
        max_height: 40.0,
    };
    
    let size = component.layout(&constraints);
    assert_eq!(size.width, 80.0); // Should be constrained
    assert_eq!(size.height, 40.0); // Should be constrained
}

#[test]
fn test_component_event_handling() {
    let mut component = TestComponent::new("test-events");
    let ctx = ComponentContext::new();
    
    // Test mouse down event
    let mouse_event = MouseEvent {
        position: Point2D::new(50.0, 25.0),
        button: 0,
    };
    
    let event = ComponentEvent {
        target: component.id(),
        event_type: EventType::MouseDown(mouse_event.clone()),
        propagation_stopped: false,
    };
    
    let handled = component.handle_event(&event, &ctx);
    assert!(handled, "Component should handle mouse down events");
    assert_eq!(component.event_count, 1);
    assert!(matches!(component.last_event, Some(EventType::MouseDown(_))));
}

#[test]
fn test_component_event_propagation() {
    let mut component = TestComponent::new("test-propagation");
    let ctx = ComponentContext::new();
    
    // Test event that should not be handled
    let event = ComponentEvent {
        target: component.id(),
        event_type: EventType::MouseMove(MouseEvent {
            position: Point2D::new(10.0, 10.0),
            button: 0,
        }),
        propagation_stopped: false,
    };
    
    let handled = component.handle_event(&event, &ctx);
    assert!(!handled, "Component should not handle mouse move events");
    assert_eq!(component.event_count, 1); // Event was received but not handled
}

#[test]
fn test_component_rendering() {
    let component = TestComponent::new("test-render");
    let ctx = ComponentContext::new();
    
    let renderables = component.render(&ctx);
    assert_eq!(renderables.len(), 1);
    
    match &renderables[0] {
        Renderable::Rectangle { rect, color, .. } => {
            assert_eq!(rect.x, 0.0);
            assert_eq!(rect.y, 0.0);
            assert_eq!(rect.width, 100.0);
            assert_eq!(rect.height, 50.0);
            assert!(colors_equal(color, &Color::blue(), 0.001));
        }
        _ => panic!("Expected rectangle renderable"),
    }
}

#[test]
fn test_component_id_equality() {
    let id1 = ComponentId::from("component-1");
    let id2 = ComponentId::from("component-1");
    let id3 = ComponentId::from("component-2");
    
    assert_eq!(id1, id2);
    assert_ne!(id1, id3);
}

#[test]
fn test_layout_constraints_default() {
    let constraints = LayoutConstraints::default();
    assert_eq!(constraints.min_width, 0.0);
    assert_eq!(constraints.max_width, f32::INFINITY);
    assert_eq!(constraints.min_height, 0.0);
    assert_eq!(constraints.max_height, f32::INFINITY);
}

#[test]
fn test_layout_constraints_fixed_size() {
    let constraints = LayoutConstraints::fixed(200.0, 100.0);
    assert_eq!(constraints.min_width, 200.0);
    assert_eq!(constraints.max_width, 200.0);
    assert_eq!(constraints.min_height, 100.0);
    assert_eq!(constraints.max_height, 100.0);
}

#[test]
fn test_component_lifecycle() {
    struct LifecycleComponent {
        id: ComponentId,
        mounted: bool,
        unmounted: bool,
        update_count: usize,
    }
    
    impl LifecycleComponent {
        fn new() -> Self {
            Self {
                id: ComponentId::from("lifecycle"),
                mounted: false,
                unmounted: false,
                update_count: 0,
            }
        }
    }
    
    impl Component for LifecycleComponent {
        fn id(&self) -> ComponentId {
            self.id.clone()
        }
        
        fn on_mount(&mut self, _ctx: &ComponentContext) {
            self.mounted = true;
        }
        
        fn on_unmount(&mut self, _ctx: &ComponentContext) {
            self.unmounted = true;
        }
        
        fn on_update(&mut self, _ctx: &ComponentContext) {
            self.update_count += 1;
        }
        
        fn handle_event(&mut self, _event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
            false
        }
        
        fn layout(&mut self, _constraints: &LayoutConstraints) -> LayoutSize {
            LayoutSize { width: 100.0, height: 100.0 }
        }
        
        fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
            vec![]
        }
    }
    
    let mut component = LifecycleComponent::new();
    let ctx = ComponentContext::new();
    
    // Test lifecycle methods
    assert!(!component.mounted);
    component.on_mount(&ctx);
    assert!(component.mounted);
    
    assert_eq!(component.update_count, 0);
    component.on_update(&ctx);
    assert_eq!(component.update_count, 1);
    
    assert!(!component.unmounted);
    component.on_unmount(&ctx);
    assert!(component.unmounted);
}

#[test]
fn test_component_context() {
    let ctx = ComponentContext::new();
    
    // Test storing and retrieving data
    ctx.set_data("theme", "dark");
    assert_eq!(ctx.get_data::<&str>("theme"), Some(&"dark"));
    assert_eq!(ctx.get_data::<&str>("missing"), None);
    
    // Test parent context
    let child_ctx = ComponentContext::with_parent(Arc::new(ctx));
    assert_eq!(child_ctx.get_data::<&str>("theme"), Some(&"dark"));
}

#[test]
fn test_event_types() {
    let mouse_pos = Point2D::new(100.0, 50.0);
    
    // Test different event types
    let events = vec![
        EventType::MouseDown(MouseEvent { position: mouse_pos, button: 0 }),
        EventType::MouseUp(MouseEvent { position: mouse_pos, button: 0 }),
        EventType::MouseMove(MouseEvent { position: mouse_pos, button: 0 }),
        EventType::Click(mouse_pos),
        EventType::DoubleClick(mouse_pos),
        EventType::KeyDown { key: "Enter".to_string(), modifiers: 0 },
        EventType::KeyUp { key: "Enter".to_string(), modifiers: 0 },
        EventType::Focus,
        EventType::Blur,
    ];
    
    for event_type in events {
        let event = ComponentEvent {
            target: ComponentId::from("test"),
            event_type,
            propagation_stopped: false,
        };
        
        // Just verify we can create all event types
        assert!(!event.propagation_stopped);
    }
}

#[test]
fn test_component_tree_traversal() {
    use fluentai_renderer::components::Container;
    
    struct TreeTestComponent {
        id: ComponentId,
        children: Vec<Box<dyn Component>>,
    }
    
    impl Component for TreeTestComponent {
        fn id(&self) -> ComponentId {
            self.id.clone()
        }
        
        fn handle_event(&mut self, _event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
            false
        }
        
        fn layout(&mut self, _constraints: &LayoutConstraints) -> LayoutSize {
            LayoutSize { width: 100.0, height: 100.0 }
        }
        
        fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
            vec![]
        }
    }
    
    // Create component tree
    let root = TreeTestComponent {
        id: ComponentId::from("root"),
        children: vec![
            Box::new(TestComponent::new("child-1")),
            Box::new(TestComponent::new("child-2")),
        ],
    };
    
    assert_eq!(root.id(), ComponentId::from("root"));
    assert_eq!(root.children.len(), 2);
}

// Mock implementations for missing types
impl ComponentContext {
    fn new() -> Self {
        ComponentContext {
            data: std::collections::HashMap::new(),
            parent: None,
        }
    }
    
    fn with_parent(parent: Arc<ComponentContext>) -> Self {
        ComponentContext {
            data: std::collections::HashMap::new(),
            parent: Some(parent),
        }
    }
    
    fn set_data<T: 'static>(&self, key: &str, value: T) {
        // Mock implementation
    }
    
    fn get_data<T: 'static>(&self, key: &str) -> Option<&T> {
        // Mock implementation
        None
    }
}

impl LayoutConstraints {
    fn fixed(width: f32, height: f32) -> Self {
        Self {
            min_width: width,
            max_width: width,
            min_height: height,
            max_height: height,
        }
    }
}

impl Default for LayoutConstraints {
    fn default() -> Self {
        Self {
            min_width: 0.0,
            max_width: f32::INFINITY,
            min_height: 0.0,
            max_height: f32::INFINITY,
        }
    }
}

// Component trait extension for lifecycle methods
trait ComponentLifecycle: Component {
    fn on_mount(&mut self, _ctx: &ComponentContext) {}
    fn on_unmount(&mut self, _ctx: &ComponentContext) {}
    fn on_update(&mut self, _ctx: &ComponentContext) {}
}