//! WASM bindings for the complete Continuum UI system

use wasm_bindgen::prelude::*;
use crate::integration::{ContinuumRuntime, create_todo_demo};
use crate::events::browser_events;
use web_sys::HtmlCanvasElement;
use std::cell::RefCell;
use std::rc::Rc;

/// WASM-exposed Continuum UI runtime
#[wasm_bindgen]
pub struct ContinuumUI {
    runtime: Rc<RefCell<ContinuumRuntime>>,
    canvas: HtmlCanvasElement,
}

#[wasm_bindgen]
impl ContinuumUI {
    /// Create a new Continuum UI instance
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<ContinuumUI, JsValue> {
        // Initialize logging
        console_error_panic_hook::set_once();
        console_log::init_with_level(log::Level::Info).ok();
        
        // Get canvas
        let window = web_sys::window().unwrap();
        let document = window.document().unwrap();
        let canvas = document
            .get_element_by_id(canvas_id)
            .ok_or_else(|| JsValue::from_str(&format!("Canvas '{}' not found", canvas_id)))?;
        let canvas: HtmlCanvasElement = canvas.dyn_into()?;
        
        // Create runtime
        let runtime = Rc::new(RefCell::new(ContinuumRuntime::new()));
        
        Ok(ContinuumUI {
            runtime,
            canvas,
        })
    }
    
    /// Initialize the To-Do demo
    #[wasm_bindgen]
    pub fn init_todo_demo(&mut self) -> Result<(), JsValue> {
        let mut runtime = self.runtime.borrow_mut();
        create_todo_demo(&mut runtime);
        
        // Set up event listeners
        let event_handler = runtime.get_event_handler();
        browser_events::setup_event_listeners(&self.canvas, event_handler)?;
        
        log::info!("To-Do demo initialized with physics!");
        Ok(())
    }
    
    /// Update the UI (call this in animation loop)
    #[wasm_bindgen]
    pub fn update(&mut self, delta_time: f32) {
        let mut runtime = self.runtime.borrow_mut();
        runtime.update(delta_time);
    }
    
    /// Add a new todo item
    #[wasm_bindgen]
    pub fn add_todo(&mut self, content: String) -> String {
        let mut runtime = self.runtime.borrow_mut();
        runtime.add_todo_item(content)
    }
    
    /// Render the current scene to canvas
    #[wasm_bindgen]
    pub fn render(&self) -> Result<(), JsValue> {
        let runtime = self.runtime.borrow();
        let scene = runtime.get_scene();
        let scene = scene.lock().unwrap();
        
        // Get 2D context
        let context = self.canvas
            .get_context("2d")?
            .unwrap()
            .dyn_into::<web_sys::CanvasRenderingContext2d>()?;
        
        // Clear canvas
        context.clear_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64);
        
        // Render each element from the scene
        for renderable in scene.get_renderables() {
            match renderable {
                crate::primitives::Renderable::Rect { transform, size, color, radius } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    if *radius > 0.0 {
                        // Rounded rectangle
                        context.begin_path();
                        let x = transform.position.x as f64;
                        let y = transform.position.y as f64;
                        let w = size.width as f64;
                        let h = size.height as f64;
                        let r = *radius as f64;
                        
                        context.move_to(x + r, y);
                        context.line_to(x + w - r, y);
                        context.quadratic_curve_to(x + w, y, x + w, y + r);
                        context.line_to(x + w, y + h - r);
                        context.quadratic_curve_to(x + w, y + h, x + w - r, y + h);
                        context.line_to(x + r, y + h);
                        context.quadratic_curve_to(x, y + h, x, y + h - r);
                        context.line_to(x, y + r);
                        context.quadratic_curve_to(x, y, x + r, y);
                        context.close_path();
                        context.fill();
                    } else {
                        context.fill_rect(
                            transform.position.x as f64,
                            transform.position.y as f64,
                            size.width as f64,
                            size.height as f64,
                        );
                    }
                }
                crate::primitives::Renderable::Circle { transform, radius, color } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    context.begin_path();
                    context.arc(
                        transform.position.x as f64,
                        transform.position.y as f64,
                        *radius as f64,
                        0.0,
                        std::f64::consts::PI * 2.0,
                    )?;
                    context.fill();
                }
                crate::primitives::Renderable::Text { transform, content, size, color, .. } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    context.set_font(&format!("{}px sans-serif", size));
                    context.set_text_align("center");
                    context.set_text_baseline("middle");
                    context.fill_text(
                        content,
                        transform.position.x as f64,
                        transform.position.y as f64,
                    )?;
                }
                _ => {}
            }
        }
        
        Ok(())
    }
}

/// Create and initialize a Continuum UI runtime
#[wasm_bindgen]
pub fn create_continuum_ui(canvas_id: &str) -> Result<ContinuumUI, JsValue> {
    ContinuumUI::new(canvas_id)
}