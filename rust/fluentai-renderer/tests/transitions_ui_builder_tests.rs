/// Tests for transitions and UI builder functionality
use fluentai_renderer::{
    transitions::{Transition, TransitionProperty, TransitionEngine},
    ui_builder::{UIBuilder, UIElement, UIElementType},
    animation::Easing,
    primitives::{Color, Point2D, Size2D},
};
use std::time::Duration;

mod test_helpers;
use test_helpers::*;

#[test]
fn test_transition_creation() {
    let transition = Transition::new(
        TransitionProperty::Opacity,
        Duration::from_millis(300),
        Easing::EaseOut,
        Duration::from_millis(0),
    );
    
    assert_eq!(transition.property(), &TransitionProperty::Opacity);
    assert_eq!(transition.duration().as_millis(), 300);
    assert!(matches!(transition.easing(), Easing::EaseOut));
    assert_eq!(transition.delay().as_millis(), 0);
}

#[test]
fn test_transition_property_types() {
    let properties = vec![
        TransitionProperty::Opacity,
        TransitionProperty::Transform,
        TransitionProperty::Color,
        TransitionProperty::Width,
        TransitionProperty::Height,
        TransitionProperty::All,
    ];
    
    for prop in properties {
        let transition = Transition::new(
            prop.clone(),
            Duration::from_millis(200),
            Easing::Linear,
            Duration::from_millis(0),
        );
        
        assert_eq!(transition.property(), &prop);
    }
}

#[test]
fn test_transition_engine() {
    let mut engine = TransitionEngine::new();
    
    // Add opacity transition
    let element_id = "test-element";
    engine.add_transition(
        element_id,
        Transition::new(
            TransitionProperty::Opacity,
            Duration::from_millis(500),
            Easing::EaseInOut,
            Duration::from_millis(0),
        ),
    );
    
    // Start transition
    engine.start_transition(element_id, TransitionProperty::Opacity, 0.0, 1.0);
    
    // Update at different times
    let value_at_0 = engine.get_value(element_id, TransitionProperty::Opacity, 0.0);
    assert_eq!(value_at_0, Some(0.0));
    
    let value_at_250 = engine.get_value(element_id, TransitionProperty::Opacity, 250.0);
    assert!(value_at_250.is_some());
    assert!(value_at_250.unwrap() > 0.0 && value_at_250.unwrap() < 1.0);
    
    let value_at_500 = engine.get_value(element_id, TransitionProperty::Opacity, 500.0);
    assert_eq!(value_at_500, Some(1.0));
}

#[test]
fn test_transition_with_delay() {
    let mut engine = TransitionEngine::new();
    
    engine.add_transition(
        "delayed",
        Transition::new(
            TransitionProperty::Width,
            Duration::from_millis(300),
            Easing::Linear,
            Duration::from_millis(200), // 200ms delay
        ),
    );
    
    engine.start_transition("delayed", TransitionProperty::Width, 100.0, 200.0);
    
    // During delay, value should remain at start
    let value_at_100 = engine.get_value("delayed", TransitionProperty::Width, 100.0);
    assert_eq!(value_at_100, Some(100.0));
    
    // After delay, transition should start
    let value_at_350 = engine.get_value("delayed", TransitionProperty::Width, 350.0); // 150ms into transition
    assert!(value_at_350.unwrap() > 100.0 && value_at_350.unwrap() < 200.0);
}

#[test]
fn test_ui_builder_text() {
    let text_element = UIBuilder::text("Hello, World!")
        .color(Color::white())
        .size(16.0)
        .position(Point2D::new(10.0, 20.0))
        .build();
    
    assert!(matches!(text_element.element_type(), UIElementType::Text(_)));
    assert_eq!(text_element.color(), Some(&Color::white()));
    assert_eq!(text_element.position(), Some(&Point2D::new(10.0, 20.0)));
}

#[test]
fn test_ui_builder_container() {
    let container = UIBuilder::new()
        .stack()
        .spacing(10.0)
        .padding(20.0)
        .child(UIBuilder::text("Child 1").build())
        .child(UIBuilder::text("Child 2").build())
        .build();
    
    assert!(matches!(container.element_type(), UIElementType::Stack));
    assert_eq!(container.children().len(), 2);
    assert_eq!(container.spacing(), Some(10.0));
    assert_eq!(container.padding(), Some(20.0));
}

#[test]
fn test_ui_builder_button() {
    let button = UIBuilder::button("Click Me")
        .on_click(|| println!("Clicked!"))
        .color(Color::blue())
        .size(Size2D::new(100.0, 40.0))
        .build();
    
    assert!(matches!(button.element_type(), UIElementType::Button(_)));
    assert_eq!(button.color(), Some(&Color::blue()));
    assert_eq!(button.size(), Some(&Size2D::new(100.0, 40.0)));
}

#[test]
fn test_ui_builder_nested_structure() {
    let ui = UIBuilder::new()
        .column()
        .child(
            UIBuilder::row()
                .child(UIBuilder::text("Label:").build())
                .child(UIBuilder::text("Value").build())
                .build()
        )
        .child(
            UIBuilder::button("Submit")
                .color(Color::green())
                .build()
        )
        .build();
    
    assert!(matches!(ui.element_type(), UIElementType::Column));
    assert_eq!(ui.children().len(), 2);
    
    // First child should be a row with 2 text elements
    let first_child = &ui.children()[0];
    assert!(matches!(first_child.element_type(), UIElementType::Row));
    assert_eq!(first_child.children().len(), 2);
}

#[test]
fn test_ui_builder_with_transitions() {
    let element = UIBuilder::new()
        .div()
        .id("animated-box")
        .transition(Transition::new(
            TransitionProperty::All,
            Duration::from_millis(300),
            Easing::EaseOut,
            Duration::from_millis(0),
        ))
        .build();
    
    assert_eq!(element.id(), Some("animated-box"));
    assert_eq!(element.transitions().len(), 1);
}

#[test]
fn test_ui_builder_conditional() {
    let show_button = true;
    let hide_text = false;
    
    let ui = UIBuilder::new()
        .stack()
        .child_if(show_button, || {
            UIBuilder::button("Visible Button").build()
        })
        .child_if(hide_text, || {
            UIBuilder::text("Hidden Text").build()
        })
        .build();
    
    // Should only have the button child
    assert_eq!(ui.children().len(), 1);
    assert!(matches!(ui.children()[0].element_type(), UIElementType::Button(_)));
}

#[test]
fn test_ui_builder_list() {
    let items = vec!["Item 1", "Item 2", "Item 3"];
    
    let list = UIBuilder::new()
        .list()
        .items(items.iter().map(|&item| {
            UIBuilder::text(item).build()
        }).collect())
        .build();
    
    assert!(matches!(list.element_type(), UIElementType::List));
    assert_eq!(list.children().len(), 3);
}

#[test]
fn test_transition_all_property() {
    let mut engine = TransitionEngine::new();
    
    // TransitionProperty::All should affect multiple properties
    engine.add_transition(
        "multi",
        Transition::new(
            TransitionProperty::All,
            Duration::from_millis(1000),
            Easing::Linear,
            Duration::from_millis(0),
        ),
    );
    
    // Start multiple property transitions
    engine.start_transition("multi", TransitionProperty::Width, 0.0, 100.0);
    engine.start_transition("multi", TransitionProperty::Height, 0.0, 200.0);
    engine.start_transition("multi", TransitionProperty::Opacity, 0.0, 1.0);
    
    // All should transition
    let width = engine.get_value("multi", TransitionProperty::Width, 500.0);
    let height = engine.get_value("multi", TransitionProperty::Height, 500.0);
    let opacity = engine.get_value("multi", TransitionProperty::Opacity, 500.0);
    
    assert_eq!(width, Some(50.0));
    assert_eq!(height, Some(100.0));
    assert_eq!(opacity, Some(0.5));
}

#[test]
fn test_ui_builder_style_modifiers() {
    let element = UIBuilder::new()
        .div()
        .class("custom-class")
        .style("border-radius", "8px")
        .style("box-shadow", "0 2px 4px rgba(0,0,0,0.1)")
        .visible(true)
        .opacity(0.8)
        .build();
    
    assert_eq!(element.class(), Some("custom-class"));
    assert_eq!(element.styles().get("border-radius"), Some(&"8px".to_string()));
    assert_eq!(element.styles().get("box-shadow"), Some(&"0 2px 4px rgba(0,0,0,0.1)".to_string()));
    assert_eq!(element.visible(), true);
    assert_eq!(element.opacity(), Some(0.8));
}

#[test]
fn test_ui_builder_grid() {
    let grid = UIBuilder::new()
        .grid()
        .columns(3)
        .gap(10.0)
        .child(UIBuilder::text("Cell 1").build())
        .child(UIBuilder::text("Cell 2").build())
        .child(UIBuilder::text("Cell 3").build())
        .child(UIBuilder::text("Cell 4").build())
        .build();
    
    assert!(matches!(grid.element_type(), UIElementType::Grid));
    assert_eq!(grid.columns(), Some(3));
    assert_eq!(grid.gap(), Some(10.0));
    assert_eq!(grid.children().len(), 4);
}

#[test]
fn test_transition_interruption() {
    let mut engine = TransitionEngine::new();
    
    engine.add_transition(
        "interrupt",
        Transition::new(
            TransitionProperty::Width,
            Duration::from_millis(1000),
            Easing::Linear,
            Duration::from_millis(0),
        ),
    );
    
    // Start first transition
    engine.start_transition("interrupt", TransitionProperty::Width, 0.0, 100.0);
    
    // Get value midway
    let mid_value = engine.get_value("interrupt", TransitionProperty::Width, 500.0).unwrap();
    assert_eq!(mid_value, 50.0);
    
    // Interrupt with new transition
    engine.start_transition("interrupt", TransitionProperty::Width, mid_value, 200.0);
    
    // Should start from interrupted value
    let new_start = engine.get_value("interrupt", TransitionProperty::Width, 500.0);
    assert_eq!(new_start, Some(50.0));
}

// Mock implementations
impl UIElement {
    fn element_type(&self) -> &UIElementType {
        // Mock implementation
        &UIElementType::Text("".to_string())
    }
    
    fn color(&self) -> Option<&Color> {
        // Mock implementation
        None
    }
    
    fn position(&self) -> Option<&Point2D> {
        // Mock implementation
        None
    }
    
    fn size(&self) -> Option<&Size2D> {
        // Mock implementation
        None
    }
    
    fn children(&self) -> &[UIElement] {
        // Mock implementation
        &[]
    }
    
    fn spacing(&self) -> Option<f32> {
        // Mock implementation
        None
    }
    
    fn padding(&self) -> Option<f32> {
        // Mock implementation
        None
    }
    
    fn id(&self) -> Option<&str> {
        // Mock implementation
        None
    }
    
    fn transitions(&self) -> &[Transition] {
        // Mock implementation
        &[]
    }
    
    fn class(&self) -> Option<&str> {
        // Mock implementation
        None
    }
    
    fn styles(&self) -> &std::collections::HashMap<String, String> {
        // Mock implementation
        static EMPTY: std::collections::HashMap<String, String> = std::collections::HashMap::new();
        &EMPTY
    }
    
    fn visible(&self) -> bool {
        // Mock implementation
        true
    }
    
    fn opacity(&self) -> Option<f32> {
        // Mock implementation
        None
    }
    
    fn columns(&self) -> Option<u32> {
        // Mock implementation
        None
    }
    
    fn gap(&self) -> Option<f32> {
        // Mock implementation
        None
    }
}

impl Transition {
    fn property(&self) -> &TransitionProperty {
        // Mock implementation
        &TransitionProperty::Opacity
    }
    
    fn duration(&self) -> Duration {
        // Mock implementation
        Duration::from_millis(300)
    }
    
    fn easing(&self) -> &Easing {
        // Mock implementation
        &Easing::Linear
    }
    
    fn delay(&self) -> Duration {
        // Mock implementation
        Duration::from_millis(0)
    }
}

impl TransitionEngine {
    fn get_value(&self, element_id: &str, property: TransitionProperty, time: f32) -> Option<f32> {
        // Mock implementation
        Some(0.0)
    }
}