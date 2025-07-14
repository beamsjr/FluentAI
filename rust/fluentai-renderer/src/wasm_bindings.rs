//! WebAssembly bindings for the renderer

use wasm_bindgen::prelude::*;
use crate::{RendererBridge, primitives::Renderable};
use web_sys::HtmlCanvasElement;

/// WASM-exposed renderer handle
#[wasm_bindgen]
pub struct WasmRenderer {
    bridge: RendererBridge,
    canvas: HtmlCanvasElement,
}

#[wasm_bindgen]
impl WasmRenderer {
    /// Create a new renderer for the given canvas ID
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<WasmRenderer, JsValue> {
        // Initialize logging
        console_error_panic_hook::set_once();
        console_log::init_with_level(log::Level::Info).ok();
        
        // Get canvas element
        let window = web_sys::window().unwrap();
        let document = window.document().unwrap();
        let canvas = document
            .get_element_by_id(canvas_id)
            .ok_or_else(|| JsValue::from_str("Canvas not found"))?;
        let canvas: HtmlCanvasElement = canvas.dyn_into()?;
        
        Ok(WasmRenderer {
            bridge: RendererBridge::new(),
            canvas,
        })
    }
    
    /// Render a scene from JSON
    #[wasm_bindgen]
    pub fn render_json(&self, scene_json: &str) -> Result<(), JsValue> {
        let elements: Vec<serde_json::Value> = serde_json::from_str(scene_json)
            .map_err(|e| JsValue::from_str(&format!("JSON parse error: {}", e)))?;
        
        self.bridge.handle_dom_effect("render", elements)
            .map_err(|e| JsValue::from_str(&format!("Render error: {}", e)))?;
        
        // For now, just draw to 2D canvas context
        self.draw_to_canvas()?;
        
        Ok(())
    }
    
    /// Clear the scene
    #[wasm_bindgen]
    pub fn clear(&self) -> Result<(), JsValue> {
        self.bridge.handle_dom_effect("clear", vec![])
            .map_err(|e| JsValue::from_str(&format!("Clear error: {}", e)))?;
        
        self.clear_canvas()?;
        Ok(())
    }
    
    // Helper to draw to 2D canvas (temporary until WebGPU is set up)
    fn draw_to_canvas(&self) -> Result<(), JsValue> {
        let context = self.canvas
            .get_context("2d")?
            .unwrap()
            .dyn_into::<web_sys::CanvasRenderingContext2d>()?;
        
        // Clear canvas
        context.clear_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64);
        
        // Get scene from bridge
        let scene = self.bridge.scene();
        let scene = scene.lock().unwrap();
        
        // Draw each renderable
        for renderable in scene.get_renderables() {
            match renderable {
                Renderable::Rect { transform, size, color, radius } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    if *radius > 0.0 {
                        // Rounded rectangle
                        self.draw_rounded_rect(
                            &context,
                            transform.position.x as f64,
                            transform.position.y as f64,
                            size.width as f64,
                            size.height as f64,
                            *radius as f64,
                        )?;
                    } else {
                        context.fill_rect(
                            transform.position.x as f64,
                            transform.position.y as f64,
                            size.width as f64,
                            size.height as f64,
                        );
                    }
                }
                Renderable::Circle { transform, radius, color } => {
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
                Renderable::Text { transform, content, size, color, .. } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    context.set_font(&format!("{}px sans-serif", size));
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
    
    fn clear_canvas(&self) -> Result<(), JsValue> {
        let context = self.canvas
            .get_context("2d")?
            .unwrap()
            .dyn_into::<web_sys::CanvasRenderingContext2d>()?;
        
        context.clear_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64);
        Ok(())
    }
    
    fn draw_rounded_rect(
        &self,
        ctx: &web_sys::CanvasRenderingContext2d,
        x: f64,
        y: f64,
        width: f64,
        height: f64,
        radius: f64,
    ) -> Result<(), JsValue> {
        ctx.begin_path();
        ctx.move_to(x + radius, y);
        ctx.line_to(x + width - radius, y);
        ctx.arc(x + width - radius, y + radius, radius, -std::f64::consts::PI / 2.0, 0.0)?;
        ctx.line_to(x + width, y + height - radius);
        ctx.arc(x + width - radius, y + height - radius, radius, 0.0, std::f64::consts::PI / 2.0)?;
        ctx.line_to(x + radius, y + height);
        ctx.arc(x + radius, y + height - radius, radius, std::f64::consts::PI / 2.0, std::f64::consts::PI)?;
        ctx.line_to(x, y + radius);
        ctx.arc(x + radius, y + radius, radius, std::f64::consts::PI, std::f64::consts::PI * 3.0 / 2.0)?;
        ctx.close_path();
        ctx.fill();
        Ok(())
    }
}

/// Create a renderer for the given canvas ID
#[wasm_bindgen]
pub fn create_renderer(canvas_id: &str) -> Result<WasmRenderer, JsValue> {
    WasmRenderer::new(canvas_id)
}

/// Render a scene
#[wasm_bindgen]
pub fn render_scene(renderer: &WasmRenderer, scene: JsValue) -> Result<(), JsValue> {
    let scene_str = js_sys::JSON::stringify(&scene)?
        .as_string()
        .ok_or_else(|| JsValue::from_str("Failed to stringify scene"))?;
    
    renderer.render_json(&scene_str)
}