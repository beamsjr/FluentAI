//! Bridge between FluentAI VM and the renderer

use crate::{Scene, primitives::{Renderable, Color, Transform, Position3D, Size2D}};
use std::sync::{Arc, Mutex};
use anyhow::{Result, anyhow};

/// Thread-safe handle to the renderer scene
#[derive(Clone)]
pub struct RendererBridge {
    scene: Arc<Mutex<Scene>>,
}

impl RendererBridge {
    /// Create a new renderer bridge
    pub fn new() -> Self {
        Self {
            scene: Arc::new(Mutex::new(Scene::new())),
        }
    }
    
    /// Get a reference to the scene (for rendering)
    pub fn scene(&self) -> Arc<Mutex<Scene>> {
        self.scene.clone()
    }
    
    /// Handle a Dom effect operation
    pub fn handle_dom_effect(&self, operation: &str, args: Vec<serde_json::Value>) -> Result<serde_json::Value> {
        match operation {
            "render" => self.render_elements(args),
            "clear" => self.clear_scene(),
            "add" => self.add_element(args),
            "remove" => self.remove_element(args),
            "update" => self.update_element(args),
            "get_scene" => self.get_scene_state(),
            _ => Err(anyhow!("Unknown Dom operation: {}", operation)),
        }
    }
    
    /// Render a list of elements to the scene
    fn render_elements(&self, args: Vec<serde_json::Value>) -> Result<serde_json::Value> {
        if args.is_empty() {
            return Err(anyhow!("Dom.render requires at least one argument"));
        }
        
        let elements = &args[0];
        if let Some(element_array) = elements.as_array() {
            let mut scene = self.scene.lock().unwrap();
            scene.clear();
            
            for element in element_array {
                if let Ok(renderable) = self.parse_element(element) {
                    scene.add(renderable);
                }
            }
            
            Ok(serde_json::json!({ "status": "rendered", "count": element_array.len() }))
        } else {
            Err(anyhow!("Dom.render expects an array of elements"))
        }
    }
    
    /// Clear the scene
    fn clear_scene(&self) -> Result<serde_json::Value> {
        let mut scene = self.scene.lock().unwrap();
        scene.clear();
        Ok(serde_json::json!({ "status": "cleared" }))
    }
    
    /// Add a single element to the scene
    fn add_element(&self, args: Vec<serde_json::Value>) -> Result<serde_json::Value> {
        if args.is_empty() {
            return Err(anyhow!("Dom.add requires an element"));
        }
        
        let element = &args[0];
        let renderable = self.parse_element(element)?;
        
        let mut scene = self.scene.lock().unwrap();
        let node_id = scene.add(renderable);
        
        Ok(serde_json::json!({ "status": "added", "id": node_id }))
    }
    
    /// Remove an element from the scene
    fn remove_element(&self, args: Vec<serde_json::Value>) -> Result<serde_json::Value> {
        if args.is_empty() {
            return Err(anyhow!("Dom.remove requires an element ID"));
        }
        
        if let Some(id) = args[0].as_u64() {
            let mut scene = self.scene.lock().unwrap();
            scene.remove(id as usize);
            Ok(serde_json::json!({ "status": "removed", "id": id }))
        } else {
            Err(anyhow!("Invalid element ID"))
        }
    }
    
    /// Update an element in the scene
    fn update_element(&self, args: Vec<serde_json::Value>) -> Result<serde_json::Value> {
        if args.len() < 2 {
            return Err(anyhow!("Dom.update requires an ID and properties"));
        }
        
        if let Some(id) = args[0].as_u64() {
            let properties = &args[1];
            let mut scene = self.scene.lock().unwrap();
            
            if let Some(node) = scene.get_mut(id as usize) {
                // Update properties based on the provided JSON
                if let Some(visible) = properties.get("visible").and_then(|v| v.as_bool()) {
                    node.visible = visible;
                }
                
                // TODO: Update transform and other properties
                
                Ok(serde_json::json!({ "status": "updated", "id": id }))
            } else {
                Err(anyhow!("Element not found"))
            }
        } else {
            Err(anyhow!("Invalid element ID"))
        }
    }
    
    /// Get the current scene state
    fn get_scene_state(&self) -> Result<serde_json::Value> {
        let scene = self.scene.lock().unwrap();
        Ok(serde_json::json!({
            "node_count": scene.node_count(),
            "roots": scene.roots()
        }))
    }
    
    /// Parse a JSON element into a Renderable
    fn parse_element(&self, element: &serde_json::Value) -> Result<Renderable> {
        let element_type = element.get("type")
            .and_then(|t| t.as_str())
            .ok_or_else(|| anyhow!("Element must have a 'type' field"))?;
        
        match element_type {
            "rect" => {
                let position = self.parse_position(element.get("position"))?;
                let size = self.parse_size(element.get("size"))?;
                let color = self.parse_color(element.get("color"))?;
                let radius = element.get("radius")
                    .and_then(|r| r.as_f64())
                    .unwrap_or(0.0) as f32;
                
                Ok(Renderable::Rect {
                    transform: Transform {
                        position,
                        rotation: (0.0, 0.0, 0.0),
                        scale: (1.0, 1.0, 1.0),
                    },
                    size,
                    color,
                    radius,
                })
            }
            "text" => {
                let position = self.parse_position(element.get("position"))?;
                let content = element.get("content")
                    .and_then(|c| c.as_str())
                    .unwrap_or("")
                    .to_string();
                let size = element.get("size")
                    .and_then(|s| s.as_f64())
                    .unwrap_or(16.0) as f32;
                let color = self.parse_color(element.get("color"))?;
                let font = element.get("font")
                    .and_then(|f| f.as_str())
                    .map(|s| s.to_string());
                
                Ok(Renderable::Text {
                    transform: Transform {
                        position,
                        rotation: (0.0, 0.0, 0.0),
                        scale: (1.0, 1.0, 1.0),
                    },
                    content,
                    size,
                    color,
                    font,
                })
            }
            "circle" => {
                let position = self.parse_position(element.get("position"))?;
                let radius = element.get("radius")
                    .and_then(|r| r.as_f64())
                    .unwrap_or(50.0) as f32;
                let color = self.parse_color(element.get("color"))?;
                
                Ok(Renderable::Circle {
                    transform: Transform {
                        position,
                        rotation: (0.0, 0.0, 0.0),
                        scale: (1.0, 1.0, 1.0),
                    },
                    radius,
                    color,
                })
            }
            _ => Err(anyhow!("Unknown element type: {}", element_type)),
        }
    }
    
    /// Parse position from JSON
    fn parse_position(&self, pos: Option<&serde_json::Value>) -> Result<Position3D> {
        if let Some(pos) = pos {
            if let Some(arr) = pos.as_array() {
                if arr.len() >= 2 {
                    let x = arr[0].as_f64().unwrap_or(0.0) as f32;
                    let y = arr[1].as_f64().unwrap_or(0.0) as f32;
                    let z = arr.get(2).and_then(|v| v.as_f64()).unwrap_or(0.0) as f32;
                    return Ok(Position3D { x, y, z });
                }
            }
        }
        Ok(Position3D { x: 0.0, y: 0.0, z: 0.0 })
    }
    
    /// Parse size from JSON
    fn parse_size(&self, size: Option<&serde_json::Value>) -> Result<Size2D> {
        if let Some(size) = size {
            if let Some(arr) = size.as_array() {
                if arr.len() >= 2 {
                    let width = arr[0].as_f64().unwrap_or(100.0) as f32;
                    let height = arr[1].as_f64().unwrap_or(100.0) as f32;
                    return Ok(Size2D { width, height });
                }
            }
        }
        Ok(Size2D { width: 100.0, height: 100.0 })
    }
    
    /// Parse color from JSON
    fn parse_color(&self, color: Option<&serde_json::Value>) -> Result<Color> {
        if let Some(color) = color {
            if let Some(hex) = color.as_str() {
                return Color::from_hex(hex).map_err(|e| anyhow!(e));
            }
        }
        Ok(Color::new(1.0, 1.0, 1.0, 1.0))
    }
}