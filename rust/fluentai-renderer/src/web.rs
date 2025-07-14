//! WebAssembly support for the renderer

use wasm_bindgen::prelude::*;
use web_sys::{HtmlCanvasElement, Window};

/// Get the window object
pub fn window() -> Window {
    web_sys::window().expect("no global `window` exists")
}

/// Get a canvas element by ID
pub fn get_canvas(id: &str) -> Result<HtmlCanvasElement, JsValue> {
    let document = window().document().expect("should have a document on window");
    let canvas = document.get_element_by_id(id)
        .ok_or_else(|| JsValue::from_str(&format!("Canvas with id '{}' not found", id)))?;
    
    Ok(canvas.dyn_into::<HtmlCanvasElement>()?)
}

/// Create a winit window for WebAssembly
#[cfg(target_arch = "wasm32")]
pub fn create_wasm_window(canvas_id: &str) -> Result<winit::window::Window, Box<dyn std::error::Error>> {
    use winit::dpi::LogicalSize;
    use winit::event_loop::EventLoop;
    use winit::platform::web::WindowBuilderExtWebSys;
    use winit::window::WindowBuilder;
    
    let event_loop = EventLoop::new()?;
    let canvas = get_canvas(canvas_id).map_err(|e| format!("Canvas error: {:?}", e))?;
    
    let window = WindowBuilder::new()
        .with_canvas(Some(canvas))
        .with_inner_size(LogicalSize::new(800, 600))
        .build(&event_loop)?;
    
    Ok(window)
}