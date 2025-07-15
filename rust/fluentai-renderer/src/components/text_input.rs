// Text input component

use super::base::{Component, ComponentContext, ComponentEvent, ComponentId, generate_component_id, LayoutConstraints, LayoutSize};
use super::style::Style;
use crate::primitives::{Renderable, Transform, Position3D, Size2D, Color};
use cgmath::Vector2;

/// Text input component
pub struct TextInput {
    id: ComponentId,
    value: String,
    placeholder: String,
    focused: bool,
    cursor_position: usize,
    style: Style,
    size: Vector2<f32>,
    on_change: Option<Box<dyn Fn(&str) + Send + Sync>>,
}

impl TextInput {
    /// Create a new text input
    pub fn new() -> Self {
        Self {
            id: generate_component_id(),
            value: String::new(),
            placeholder: String::new(),
            focused: false,
            cursor_position: 0,
            style: Style::new(),
            size: Vector2::new(200.0, 30.0),
            on_change: None,
        }
    }
    
    /// Set the input value
    pub fn value(mut self, value: impl Into<String>) -> Self {
        self.value = value.into();
        self.cursor_position = self.value.len();
        self
    }
    
    /// Set the placeholder text
    pub fn placeholder(mut self, placeholder: impl Into<String>) -> Self {
        self.placeholder = placeholder.into();
        self
    }
    
    /// Set the change handler
    pub fn on_change<F>(mut self, handler: F) -> Self
    where
        F: Fn(&str) + Send + Sync + 'static
    {
        self.on_change = Some(Box::new(handler));
        self
    }
    
    /// Set the input style
    pub fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
    
    /// Handle text input
    fn handle_text_input(&mut self, text: &str) {
        // Insert text at cursor position
        self.value.insert_str(self.cursor_position, text);
        self.cursor_position += text.len();
        
        // Call change handler
        if let Some(handler) = &self.on_change {
            handler(&self.value);
        }
    }
    
    /// Handle keyboard input
    fn handle_key(&mut self, key: &str) {
        match key.as_ref() {
            "Backspace" => {
                if self.cursor_position > 0 {
                    self.cursor_position -= 1;
                    self.value.remove(self.cursor_position);
                    
                    if let Some(handler) = &self.on_change {
                        handler(&self.value);
                    }
                }
            }
            "Delete" => {
                if self.cursor_position < self.value.len() {
                    self.value.remove(self.cursor_position);
                    
                    if let Some(handler) = &self.on_change {
                        handler(&self.value);
                    }
                }
            }
            "ArrowLeft" => {
                if self.cursor_position > 0 {
                    self.cursor_position -= 1;
                }
            }
            "ArrowRight" => {
                if self.cursor_position < self.value.len() {
                    self.cursor_position += 1;
                }
            }
            "Home" => {
                self.cursor_position = 0;
            }
            "End" => {
                self.cursor_position = self.value.len();
            }
            _ => {}
        }
    }
}

impl Component for TextInput {
    fn id(&self) -> ComponentId {
        self.id
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
        match event {
            ComponentEvent::Click(_) => {
                self.focused = true;
                // TODO: Calculate cursor position from click coordinates
                true
            }
            ComponentEvent::Focus => {
                self.focused = true;
                true
            }
            ComponentEvent::Blur => {
                self.focused = false;
                true
            }
            ComponentEvent::TextInput(text) => {
                if self.focused {
                    self.handle_text_input(text);
                    true
                } else {
                    false
                }
            }
            ComponentEvent::KeyDown(key) => {
                if self.focused {
                    self.handle_key(key);
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        // Apply style dimensions if set
        let width = self.style.width().unwrap_or(self.size.x);
        let height = self.style.height().unwrap_or(self.size.y);
        
        // Constrain to layout constraints
        self.size.x = width.max(constraints.min_size.x).min(constraints.max_size.x);
        self.size.y = height.max(constraints.min_size.y).min(constraints.max_size.y);
        
        LayoutSize {
            width: self.size.x,
            height: self.size.y,
        }
    }
    
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        let mut renderables = Vec::new();
        
        // Background
        let bg_color = self.style.background_color()
            .cloned()
            .unwrap_or(Color::new(1.0, 1.0, 1.0, 1.0));
        
        // Border
        let border_color = if self.focused {
            Color::new(0.2, 0.5, 0.8, 1.0)
        } else {
            Color::new(0.7, 0.7, 0.7, 1.0)
        };
        
        let border_width = 2.0;
        
        // Render border
        renderables.push(Renderable::Rect {
            transform: Transform {
                position: Position3D {
                    x: ctx.position.x,
                    y: ctx.position.y,
                    z: 0.0,
                },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            size: Size2D::new(self.size.x, self.size.y),
            color: border_color,
            radius: 4.0,
        });
        
        // Render background (inset by border width)
        renderables.push(Renderable::Rect {
            transform: Transform {
                position: Position3D {
                    x: ctx.position.x + border_width,
                    y: ctx.position.y + border_width,
                    z: 0.0,
                },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            size: Size2D::new(self.size.x - border_width * 2.0, self.size.y - border_width * 2.0),
            color: bg_color,
            radius: 2.0,
        });
        
        // Render text or placeholder
        let (_padding_top, _padding_right, _padding_bottom, padding_left) = self.style.padding();
        let text_x = ctx.position.x + padding_left + 5.0;
        let text_y = ctx.position.y + self.size.y / 2.0 - 8.0;
        
        let (text, color) = if self.value.is_empty() {
            (&self.placeholder, Color::new(0.6, 0.6, 0.6, 1.0))
        } else {
            (&self.value, Color::new(0.0, 0.0, 0.0, 1.0))
        };
        
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D {
                    x: text_x,
                    y: text_y,
                    z: 0.0,
                },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: text.clone(),
            size: 16.0,
            color,
            font: None,
        });
        
        // Render cursor if focused
        if self.focused {
            let cursor_x = text_x + (self.cursor_position as f32 * 8.0); // Approximate char width
            
            renderables.push(Renderable::Line {
                start: Position3D {
                    x: cursor_x,
                    y: text_y - 2.0,
                    z: 0.0,
                },
                end: Position3D {
                    x: cursor_x,
                    y: text_y + 18.0,
                    z: 0.0,
                },
                width: 1.0,
                color: Color::new(0.0, 0.0, 0.0, 1.0),
            });
        }
        
        renderables
    }
}