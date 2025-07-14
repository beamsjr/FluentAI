//! Integration layer that connects all Phase 3 components
//! 
//! This module ties together physics, rendering, reactive state, and events
//! to create the complete Continuum UI system.

use crate::{
    physics::{PhysicsLayout, ConstraintType},
    reactive::ReactiveEngine,
    events::EventHandler,
    scene::Scene,
    primitives::{Renderable, Position2D, Position3D, Size2D, Transform, Color},
};
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use serde_json::json;

/// The main Continuum UI runtime that integrates all components
pub struct ContinuumRuntime {
    /// Physics simulation for layout
    physics: Arc<Mutex<PhysicsLayout>>,
    
    /// Reactive state management
    reactive: Arc<Mutex<ReactiveEngine>>,
    
    /// Event handling
    event_handler: Arc<Mutex<EventHandler>>,
    
    /// Rendering scene
    scene: Arc<Mutex<Scene>>,
    
    /// Mapping from element IDs to scene node IDs
    element_to_scene_node: HashMap<String, usize>,
    
    /// Parent-child relationships for elements
    element_children: HashMap<String, Vec<String>>,
    
    /// Frame counter for animations
    frame_count: u64,
    
    /// Counter for creating unique todo IDs
    next_todo_id: u32,
}

impl ContinuumRuntime {
    /// Create a new Continuum runtime
    pub fn new() -> Self {
        let physics = Arc::new(Mutex::new(PhysicsLayout::new()));
        let reactive = Arc::new(Mutex::new(ReactiveEngine::new()));
        let event_handler = Arc::new(Mutex::new(EventHandler::new(physics.clone(), reactive.clone())));
        let scene = Arc::new(Mutex::new(Scene::new()));
        
        Self {
            physics,
            reactive,
            event_handler,
            scene,
            element_to_scene_node: HashMap::new(),
            element_children: HashMap::new(),
            frame_count: 0,
            next_todo_id: 3,  // Start after initial 3 todos
        }
    }
    
    /// Add a UI element to the runtime
    pub fn add_element(&mut self, id: String, element_type: &str, props: HashMap<String, serde_json::Value>) {
        // Extract position and size from properties
        let x = props.get("x").and_then(|v| v.as_f64()).unwrap_or(0.0) as f32;
        let y = props.get("y").and_then(|v| v.as_f64()).unwrap_or(0.0) as f32;
        let width = props.get("width").and_then(|v| v.as_f64()).unwrap_or(100.0) as f32;
        let height = props.get("height").and_then(|v| v.as_f64()).unwrap_or(50.0) as f32;
        let color_str = props.get("color").and_then(|v| v.as_str()).unwrap_or("#000000");
        let is_static = props.get("static").and_then(|v| v.as_bool()).unwrap_or(false);
        
        // Add to physics
        {
            let mut physics = self.physics.lock().unwrap();
            physics.add_element(id.clone(), Position2D::new(x, y), Size2D::new(width, height), is_static);
        }
        
        // Create renderable based on element type
        let renderable = match element_type {
            "button" | "rect" => {
                let color = Color::from_hex(color_str).unwrap_or(Color::new(0.5, 0.5, 0.5, 1.0));
                Renderable::Rect {
                    transform: Transform::new(Position2D::new(x, y)),
                    size: Size2D::new(width, height),
                    color,
                    radius: props.get("radius").and_then(|v| v.as_f64()).unwrap_or(5.0) as f32,
                }
            }
            "text" => {
                let content = props.get("content").and_then(|v| v.as_str()).unwrap_or("");
                let size = props.get("fontSize").and_then(|v| v.as_f64()).unwrap_or(16.0) as f32;
                let color = Color::from_hex(color_str).unwrap_or(Color::new(0.0, 0.0, 0.0, 1.0));
                Renderable::Text {
                    transform: Transform::new(Position2D::new(x, y)),
                    content: content.to_string(),
                    size,
                    color,
                    font: None,
                }
            }
            "circle" => {
                let radius = props.get("radius").and_then(|v| v.as_f64()).unwrap_or(25.0) as f32;
                let color = Color::from_hex(color_str).unwrap_or(Color::new(0.5, 0.5, 0.5, 1.0));
                Renderable::Circle {
                    transform: Transform::new(Position2D::new(x, y)),
                    radius,
                    color,
                }
            }
            _ => {
                // Default to rectangle
                Renderable::Rect {
                    transform: Transform::new(Position2D::new(x, y)),
                    size: Size2D::new(width, height),
                    color: Color::new(0.5, 0.5, 0.5, 1.0),
                    radius: 0.0,
                }
            }
        };
        
        // Add to scene
        {
            let mut scene = self.scene.lock().unwrap();
            let node_id = scene.add(renderable);
            self.element_to_scene_node.insert(id.clone(), node_id);
        }
        
        // Update event handler bounds
        {
            let event_handler = self.event_handler.lock().unwrap();
            event_handler.update_element_bounds(id, x, y, width, height);
        }
    }
    
    /// Link a child element to a parent (child will follow parent's position)
    pub fn add_child(&mut self, parent_id: String, child_id: String) {
        self.element_children
            .entry(parent_id)
            .or_insert_with(Vec::new)
            .push(child_id);
    }
    
    /// Add a constraint between elements
    pub fn add_constraint(&mut self, elem1: &str, elem2: &str, constraint: ConstraintType) {
        let mut physics = self.physics.lock().unwrap();
        physics.add_constraint(elem1, elem2, constraint);
    }
    
    /// Register a state field
    pub fn add_state_field(&mut self, name: String, field_type: Option<String>, initial: Option<serde_json::Value>) {
        let mut reactive = self.reactive.lock().unwrap();
        reactive.register_state_field(name, field_type, initial);
    }
    
    /// Add a reactive computation that updates UI based on state
    pub fn add_reactive_update<F>(&mut self, computation: F) 
    where
        F: Fn() + 'static
    {
        let mut reactive = self.reactive.lock().unwrap();
        reactive.add_computation(computation);
    }
    
    /// Register an event handler for an element
    pub fn on_event<F>(&mut self, element_id: String, event_type: String, handler: F)
    where
        F: Fn(serde_json::Value) + 'static
    {
        let mut reactive = self.reactive.lock().unwrap();
        reactive.register_event_handler(element_id, event_type, handler);
    }
    
    /// Update the runtime (call this each frame)
    pub fn update(&mut self, _delta_time: f32) {
        self.frame_count += 1;
        
        // Step physics simulation
        {
            let mut physics = self.physics.lock().unwrap();
            physics.step();
            
            // Update scene positions from physics
            let positions = physics.get_all_positions();
            let mut scene = self.scene.lock().unwrap();
            
            for (element_id, position) in positions {
                // Skip text elements - they are positioned via parent-child relationships
                if element_id.contains("text") {
                    continue;
                }
                
                if let Some(&node_id) = self.element_to_scene_node.get(&element_id) {
                    if let Some(node) = scene.get_mut(node_id) {
                        // Update the transform position
                        if let Some(transform) = node.renderable.transform_mut() {
                            transform.position = Position3D { x: position.x, y: position.y, z: 0.0 };
                        }
                    }
                    
                    // Update event handler bounds
                    let event_handler = self.event_handler.lock().unwrap();
                    if let Some(node) = scene.get(node_id) {
                        match &node.renderable {
                            Renderable::Rect { transform, size, .. } => {
                                event_handler.update_element_bounds(
                                    element_id.clone(),
                                    transform.position.x,
                                    transform.position.y,
                                    size.width,
                                    size.height,
                                );
                            }
                            Renderable::Circle { transform, radius, .. } => {
                                event_handler.update_element_bounds(
                                    element_id.clone(),
                                    transform.position.x - radius,
                                    transform.position.y - radius,
                                    radius * 2.0,
                                    radius * 2.0,
                                );
                            }
                            _ => {}
                        }
                    }
                }
                
                // Update children positions to follow parent
                if let Some(children) = self.element_children.get(&element_id).cloned() {
                    // Get parent node data first
                    if let Some(&parent_node_id) = self.element_to_scene_node.get(&element_id) {
                        // Clone parent renderable to avoid borrow issues
                        let parent_renderable = scene.get(parent_node_id).map(|n| n.renderable.clone());
                        
                        if let Some(parent_renderable) = parent_renderable {
                            for child_id in children {
                                if let Some(&child_node_id) = self.element_to_scene_node.get(&child_id) {
                                    if let Some(child_node) = scene.get_mut(child_node_id) {
                                        // Calculate offset based on parent type
                                        if let Some(child_transform) = child_node.renderable.transform_mut() {
                                            match &parent_renderable {
                                                Renderable::Rect { transform, size, .. } => {
                                                    // Center text on rectangle
                                                    child_transform.position = Position3D {
                                                        x: transform.position.x + size.width / 2.0,
                                                        y: transform.position.y + size.height / 2.0,
                                                        z: 0.0,
                                                    };
                                                }
                                                _ => {
                                                    // Default: same position as parent
                                                    child_transform.position = Position3D {
                                                        x: position.x,
                                                        y: position.y,
                                                        z: 0.0,
                                                    };
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    /// Get the current scene for rendering
    pub fn get_scene(&self) -> Arc<Mutex<Scene>> {
        self.scene.clone()
    }
    
    /// Get the event handler
    pub fn get_event_handler(&self) -> Arc<Mutex<EventHandler>> {
        self.event_handler.clone()
    }
    
    /// Add a new todo item dynamically
    pub fn add_todo_item(&mut self, content: String) -> String {
        let todo_id = format!("todo_{}", self.next_todo_id);
        let text_id = format!("todo_text_{}", self.next_todo_id);
        self.next_todo_id += 1;
        
        // Calculate position for new todo
        let y = 200.0 + ((self.next_todo_id - 1) as f32 * 80.0);
        
        // Add rectangle
        self.add_element(todo_id.clone(), "rect", HashMap::from([
            ("x".to_string(), json!(300)),
            ("y".to_string(), json!(y)),
            ("width".to_string(), json!(200)),
            ("height".to_string(), json!(50)),
            ("color".to_string(), json!("#F18F01")),
            ("radius".to_string(), json!(10)),
        ]));
        
        // Add text
        self.add_element(text_id.clone(), "text", HashMap::from([
            ("content".to_string(), json!(content)),
            ("x".to_string(), json!(400)),
            ("y".to_string(), json!(y + 25.0)),
            ("fontSize".to_string(), json!(14)),
            ("color".to_string(), json!("#FFFFFF")),
            ("static".to_string(), json!(true)),
        ]));
        
        // Link text to rectangle
        self.add_child(todo_id.clone(), text_id);
        
        todo_id
    }
}

/// Helper to create a To-Do list demo
pub fn create_todo_demo(runtime: &mut ContinuumRuntime) {
    // Add state fields
    runtime.add_state_field("todo_count".to_string(), Some("int".to_string()), Some(json!(0)));
    runtime.add_state_field("todos".to_string(), Some("array".to_string()), Some(json!([])));
    
    // Add UI elements
    runtime.add_element("header".to_string(), "text", HashMap::from([
        ("content".to_string(), json!("Physics-Based To-Do List")),
        ("x".to_string(), json!(400)),
        ("y".to_string(), json!(50)),
        ("fontSize".to_string(), json!(24)),
        ("color".to_string(), json!("#2E86AB")),
        ("static".to_string(), json!(true)),
    ]));
    
    runtime.add_element("add_button".to_string(), "button", HashMap::from([
        ("x".to_string(), json!(100)),
        ("y".to_string(), json!(100)),
        ("width".to_string(), json!(150)),
        ("height".to_string(), json!(40)),
        ("color".to_string(), json!("#4CAF50")),
    ]));
    
    runtime.add_element("add_text".to_string(), "text", HashMap::from([
        ("content".to_string(), json!("Add Todo")),
        ("x".to_string(), json!(175)),
        ("y".to_string(), json!(120)),
        ("fontSize".to_string(), json!(16)),
        ("color".to_string(), json!("#FFFFFF")),
        ("static".to_string(), json!(true)),
    ]));
    
    // Link text to button
    runtime.add_child("add_button".to_string(), "add_text".to_string());
    
    // Add some example to-do items
    for i in 0..3 {
        let y = 200.0 + (i as f32 * 80.0);  // More spacing between items
        
        let todo_id = format!("todo_{}", i);
        let text_id = format!("todo_text_{}", i);
        
        runtime.add_element(todo_id.clone(), "rect", HashMap::from([
            ("x".to_string(), json!(300)),
            ("y".to_string(), json!(y)),
            ("width".to_string(), json!(200)),
            ("height".to_string(), json!(50)),
            ("color".to_string(), json!("#F18F01")),
            ("radius".to_string(), json!(10)),
        ]));
        
        runtime.add_element(text_id.clone(), "text", HashMap::from([
            ("content".to_string(), json!(format!("Todo Item {}", i + 1))),
            ("x".to_string(), json!(400)),  // This will be overridden by parent-child positioning
            ("y".to_string(), json!(y + 25.0)),  // This will be overridden
            ("fontSize".to_string(), json!(14)),
            ("color".to_string(), json!("#FFFFFF")),
            ("static".to_string(), json!(true)),  // Make text static so it doesn't have physics
        ]));
        
        // Link text to its parent rectangle
        runtime.add_child(todo_id, text_id);
    }
    
    // Add click handler for add button
    runtime.on_event("add_button".to_string(), "click".to_string(), move |_event| {
        log::info!("Add button clicked!");
        // TODO: In a real implementation, this would:
        // 1. Create new todo rectangle and text
        // 2. Add them to the scene
        // 3. Link them with parent-child relationship
        // 4. Update reactive state
    });
}
