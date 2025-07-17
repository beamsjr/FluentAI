// Canvas 2D renderer for WASM - renders our Renderables using HTML5 Canvas
use wasm_bindgen::prelude::*;
use web_sys::{HtmlCanvasElement, CanvasRenderingContext2d};
use crate::primitives::{Renderable, Color};

pub struct Canvas2DRenderer {
    canvas: HtmlCanvasElement,
    context: CanvasRenderingContext2d,
}

impl Canvas2DRenderer {
    pub fn new(canvas: HtmlCanvasElement) -> Result<Self, JsValue> {
        let context = canvas
            .get_context("2d")?
            .ok_or_else(|| JsValue::from_str("Failed to get 2D context"))?
            .dyn_into::<CanvasRenderingContext2d>()?;
        
        Ok(Canvas2DRenderer { canvas, context })
    }
    
    pub fn clear(&self) {
        self.context.set_fill_style(&JsValue::from_str("rgba(10, 10, 10, 0.1)"));
        self.context.fill_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64);
    }
    
    pub fn render(&self, renderables: &[Renderable]) {
        self.clear();
        
        for renderable in renderables {
            match renderable {
                Renderable::Circle { transform, radius, color } => {
                    self.render_circle(
                        transform.position.x as f64,
                        transform.position.y as f64,
                        *radius as f64,
                        color,
                    );
                }
                Renderable::Rect { transform, size, color, radius } => {
                    self.render_rect(
                        transform.position.x as f64,
                        transform.position.y as f64,
                        size.width as f64,
                        size.height as f64,
                        *radius as f64,
                        color,
                    );
                }
                Renderable::Text { transform, content, size, color, .. } => {
                    self.render_text(
                        transform.position.x as f64,
                        transform.position.y as f64,
                        content,
                        *size as f64,
                        color,
                    );
                }
                Renderable::Line { start, end, color, width } => {
                    self.render_line(
                        start.x as f64,
                        start.y as f64,
                        end.x as f64,
                        end.y as f64,
                        *width as f64,
                        color,
                    );
                }
                _ => {} // Skip other renderables for now
            }
        }
    }
    
    fn render_circle(&self, x: f64, y: f64, radius: f64, color: &Color) {
        self.context.begin_path();
        self.context.arc(x, y, radius, 0.0, std::f64::consts::TAU).unwrap();
        
        // Set color with alpha
        let color_str = format!("rgba({}, {}, {}, {})", 
            (color.r * 255.0) as u8,
            (color.g * 255.0) as u8,
            (color.b * 255.0) as u8,
            color.a
        );
        
        // Add glow effect
        self.context.set_shadow_blur(10.0);
        self.context.set_shadow_color(&color_str);
        
        self.context.set_fill_style(&JsValue::from_str(&color_str));
        self.context.fill();
        
        // Reset shadow
        self.context.set_shadow_blur(0.0);
    }
    
    fn render_rect(&self, x: f64, y: f64, width: f64, height: f64, radius: f64, color: &Color) {
        let color_str = format!("rgba({}, {}, {}, {})", 
            (color.r * 255.0) as u8,
            (color.g * 255.0) as u8,
            (color.b * 255.0) as u8,
            color.a
        );
        
        self.context.set_fill_style(&JsValue::from_str(&color_str));
        
        if radius > 0.0 {
            // Rounded rectangle
            self.context.begin_path();
            self.context.move_to(x + radius, y);
            self.context.line_to(x + width - radius, y);
            self.context.arc(x + width - radius, y + radius, radius, -std::f64::consts::FRAC_PI_2, 0.0).unwrap();
            self.context.line_to(x + width, y + height - radius);
            self.context.arc(x + width - radius, y + height - radius, radius, 0.0, std::f64::consts::FRAC_PI_2).unwrap();
            self.context.line_to(x + radius, y + height);
            self.context.arc(x + radius, y + height - radius, radius, std::f64::consts::FRAC_PI_2, std::f64::consts::PI).unwrap();
            self.context.line_to(x, y + radius);
            self.context.arc(x + radius, y + radius, radius, std::f64::consts::PI, -std::f64::consts::FRAC_PI_2).unwrap();
            self.context.close_path();
            self.context.fill();
        } else {
            self.context.fill_rect(x, y, width, height);
        }
    }
    
    fn render_text(&self, x: f64, y: f64, text: &str, size: f64, color: &Color) {
        let color_str = format!("rgba({}, {}, {}, {})", 
            (color.r * 255.0) as u8,
            (color.g * 255.0) as u8,
            (color.b * 255.0) as u8,
            color.a
        );
        
        self.context.set_font(&format!("{}px sans-serif", size));
        self.context.set_fill_style(&JsValue::from_str(&color_str));
        self.context.fill_text(text, x, y).unwrap();
    }
    
    fn render_line(&self, x1: f64, y1: f64, x2: f64, y2: f64, width: f64, color: &Color) {
        let color_str = format!("rgba({}, {}, {}, {})", 
            (color.r * 255.0) as u8,
            (color.g * 255.0) as u8,
            (color.b * 255.0) as u8,
            color.a
        );
        
        self.context.set_stroke_style(&JsValue::from_str(&color_str));
        self.context.set_line_width(width);
        
        self.context.begin_path();
        self.context.move_to(x1, y1);
        self.context.line_to(x2, y2);
        self.context.stroke();
    }
}