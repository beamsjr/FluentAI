//! Event handling system for Continuum UI
//! 
//! Connects browser/native events to the reactive engine and physics system

use crate::physics::PhysicsLayout;
use crate::reactive::ReactiveEngine;
use crate::primitives::Position2D;
use std::sync::{Arc, Mutex};
use serde_json::json;

/// Event type names
pub type EventType = String;

/// Event data as JSON
pub type EventData = serde_json::Value;

/// Types of UI events
#[derive(Debug, Clone)]
pub enum UIEvent {
    Click { element_id: String, position: Position2D },
    MouseDown { element_id: String, position: Position2D },
    MouseUp { element_id: String, position: Position2D },
    MouseMove { position: Position2D },
    Drag { element_id: String, position: Position2D },
    KeyPress { key: String },
}

/// Manages event handling and routing
pub struct EventHandler {
    physics: Arc<Mutex<PhysicsLayout>>,
    reactive: Arc<Mutex<ReactiveEngine>>,
    
    // Drag state
    dragging: Option<DragState>,
    
    // Element hit testing (maps position to element)
    element_bounds: Arc<Mutex<Vec<ElementBounds>>>,
}

struct DragState {
    element_id: String,
    start_pos: Position2D,
    offset: Position2D,
}

struct ElementBounds {
    id: String,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
}

impl EventHandler {
    pub fn new(physics: Arc<Mutex<PhysicsLayout>>, reactive: Arc<Mutex<ReactiveEngine>>) -> Self {
        Self {
            physics,
            reactive,
            dragging: None,
            element_bounds: Arc::new(Mutex::new(Vec::new())),
        }
    }
    
    /// Update element bounds for hit testing
    pub fn update_element_bounds(&self, id: String, x: f32, y: f32, width: f32, height: f32) {
        let mut bounds = self.element_bounds.lock().unwrap();
        
        // Find or create entry
        if let Some(elem) = bounds.iter_mut().find(|e| e.id == id) {
            elem.x = x;
            elem.y = y;
            elem.width = width;
            elem.height = height;
        } else {
            bounds.push(ElementBounds { id, x, y, width, height });
        }
    }
    
    /// Find which element is at a given position
    fn hit_test(&self, position: &Position2D) -> Option<String> {
        let bounds = self.element_bounds.lock().unwrap();
        
        // Check from top to bottom (reverse order for proper z-ordering)
        for elem in bounds.iter().rev() {
            if position.x >= elem.x && position.x <= elem.x + elem.width &&
               position.y >= elem.y && position.y <= elem.y + elem.height {
                return Some(elem.id.clone());
            }
        }
        
        None
    }
    
    /// Handle incoming UI events
    pub fn handle_event(&mut self, event: UIEvent) {
        match event {
            UIEvent::Click { element_id, position } => {
                self.handle_click(element_id, position);
            }
            
            UIEvent::MouseDown { element_id, position } => {
                self.handle_mouse_down(element_id, position);
            }
            
            UIEvent::MouseUp { element_id, position } => {
                self.handle_mouse_up(element_id, position);
            }
            
            UIEvent::MouseMove { position } => {
                self.handle_mouse_move(position);
            }
            
            UIEvent::Drag { element_id, position } => {
                self.handle_drag(element_id, position);
            }
            
            UIEvent::KeyPress { key } => {
                self.handle_key_press(key);
            }
        }
    }
    
    fn handle_click(&self, element_id: String, position: Position2D) {
        // Send click event to reactive engine
        let reactive = self.reactive.lock().unwrap();
        reactive.handle_event(&element_id, "click", json!({
            "x": position.x,
            "y": position.y,
        }));
        
        // Check if this element has an on_click handler that disturbs state
        if let Some(field) = reactive.get_field(&element_id) {
            field.disturb();
        }
    }
    
    fn handle_mouse_down(&mut self, element_id: String, position: Position2D) {
        // Start dragging
        self.dragging = Some(DragState {
            element_id: element_id.clone(),
            start_pos: position.clone(),
            offset: Position2D::new(0.0, 0.0), // Will calculate actual offset
        });
        
        // Tell physics to start dragging
        let mut physics = self.physics.lock().unwrap();
        physics.start_drag(&element_id, position);
        
        // Send event to reactive engine
        let reactive = self.reactive.lock().unwrap();
        reactive.handle_event(&element_id, "mousedown", json!({
            "x": position.x,
            "y": position.y,
        }));
    }
    
    fn handle_mouse_up(&mut self, element_id: String, position: Position2D) {
        // End dragging if active
        if let Some(drag_state) = &self.dragging {
            let mut physics = self.physics.lock().unwrap();
            physics.end_drag(&drag_state.element_id);
            self.dragging = None;
        }
        
        // Send event to reactive engine
        let reactive = self.reactive.lock().unwrap();
        reactive.handle_event(&element_id, "mouseup", json!({
            "x": position.x,
            "y": position.y,
        }));
    }
    
    fn handle_mouse_move(&mut self, position: Position2D) {
        // If dragging, update physics
        if let Some(drag_state) = &self.dragging {
            let mut physics = self.physics.lock().unwrap();
            physics.update_drag(&drag_state.element_id, position);
        }
        
        // Check for hover events
        if let Some(element_id) = self.hit_test(&position) {
            let reactive = self.reactive.lock().unwrap();
            reactive.handle_event(&element_id, "mousemove", json!({
                "x": position.x,
                "y": position.y,
            }));
        }
    }
    
    fn handle_drag(&mut self, element_id: String, position: Position2D) {
        // Update physics drag position
        let mut physics = self.physics.lock().unwrap();
        physics.update_drag(&element_id, position);
        
        // Send drag event to reactive engine
        let reactive = self.reactive.lock().unwrap();
        reactive.handle_event(&element_id, "drag", json!({
            "x": position.x,
            "y": position.y,
        }));
    }
    
    fn handle_key_press(&self, key: String) {
        // Send to reactive engine as global event
        let reactive = self.reactive.lock().unwrap();
        reactive.handle_event("global", "keypress", json!({
            "key": key,
        }));
    }
}

/// Convert browser mouse events to UI events
#[cfg(target_arch = "wasm32")]
pub mod browser_events {
    use super::*;
    use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;
    use web_sys::{MouseEvent, KeyboardEvent};
    
    /// Set up browser event listeners
    pub fn setup_event_listeners(
        canvas: &web_sys::HtmlCanvasElement,
        event_handler: Arc<Mutex<EventHandler>>,
    ) -> Result<(), JsValue> {
        let window = web_sys::window().unwrap();
        let document = window.document().unwrap();
        
        // Mouse down
        {
            let handler = event_handler.clone();
            let canvas_clone = canvas.clone();
            let closure = Closure::wrap(Box::new(move |event: MouseEvent| {
                let rect = canvas_clone.get_bounding_client_rect();
                let x = (event.client_x() as f32 - rect.left() as f32) as f32;
                let y = (event.client_y() as f32 - rect.top() as f32) as f32;
                let position = Position2D::new(x, y);
                
                let mut handler = handler.lock().unwrap();
                if let Some(element_id) = handler.hit_test(&position) {
                    handler.handle_event(UIEvent::MouseDown { element_id, position });
                }
            }) as Box<dyn FnMut(_)>);
            
            canvas.add_event_listener_with_callback("mousedown", closure.as_ref().unchecked_ref())?;
            closure.forget();
        }
        
        // Mouse up
        {
            let handler = event_handler.clone();
            let canvas_clone = canvas.clone();
            let closure = Closure::wrap(Box::new(move |event: MouseEvent| {
                let rect = canvas_clone.get_bounding_client_rect();
                let x = (event.client_x() as f32 - rect.left() as f32) as f32;
                let y = (event.client_y() as f32 - rect.top() as f32) as f32;
                let position = Position2D::new(x, y);
                
                let mut handler = handler.lock().unwrap();
                if let Some(element_id) = handler.hit_test(&position) {
                    handler.handle_event(UIEvent::MouseUp { element_id, position });
                }
            }) as Box<dyn FnMut(_)>);
            
            canvas.add_event_listener_with_callback("mouseup", closure.as_ref().unchecked_ref())?;
            closure.forget();
        }
        
        // Mouse move
        {
            let handler = event_handler.clone();
            let canvas_clone = canvas.clone();
            let closure = Closure::wrap(Box::new(move |event: MouseEvent| {
                let rect = canvas_clone.get_bounding_client_rect();
                let x = (event.client_x() as f32 - rect.left() as f32) as f32;
                let y = (event.client_y() as f32 - rect.top() as f32) as f32;
                let position = Position2D::new(x, y);
                
                let mut handler = handler.lock().unwrap();
                handler.handle_event(UIEvent::MouseMove { position });
            }) as Box<dyn FnMut(_)>);
            
            canvas.add_event_listener_with_callback("mousemove", closure.as_ref().unchecked_ref())?;
            closure.forget();
        }
        
        // Click
        {
            let handler = event_handler.clone();
            let canvas_clone = canvas.clone();
            let closure = Closure::wrap(Box::new(move |event: MouseEvent| {
                let rect = canvas_clone.get_bounding_client_rect();
                let x = (event.client_x() as f32 - rect.left() as f32) as f32;
                let y = (event.client_y() as f32 - rect.top() as f32) as f32;
                let position = Position2D::new(x, y);
                
                let mut handler = handler.lock().unwrap();
                if let Some(element_id) = handler.hit_test(&position) {
                    handler.handle_event(UIEvent::Click { element_id, position });
                }
            }) as Box<dyn FnMut(_)>);
            
            canvas.add_event_listener_with_callback("click", closure.as_ref().unchecked_ref())?;
            closure.forget();
        }
        
        // Keyboard
        {
            let handler = event_handler.clone();
            let closure = Closure::wrap(Box::new(move |event: KeyboardEvent| {
                let mut handler = handler.lock().unwrap();
                handler.handle_event(UIEvent::KeyPress { key: event.key() });
            }) as Box<dyn FnMut(_)>);
            
            document.add_event_listener_with_callback("keypress", closure.as_ref().unchecked_ref())?;
            closure.forget();
        }
        
        Ok(())
    }
}