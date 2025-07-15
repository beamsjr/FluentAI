// Button component

use super::base::{Component, ComponentContext, ComponentEvent, ComponentId, generate_component_id, LayoutConstraints, LayoutSize};
use super::style::Style;
use crate::primitives::{Renderable, Transform, Position3D, Size2D, Color};
use cgmath::Vector2;

/// Button states
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ButtonState {
    Normal,
    Hovered,
    Pressed,
    Disabled,
}

/// Button component
pub struct Button {
    id: ComponentId,
    label: String,
    state: ButtonState,
    style: Style,
    on_click: Option<Box<dyn Fn() + Send + Sync>>,
    size: Vector2<f32>,
}

impl Button {
    /// Create a new button
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            id: generate_component_id(),
            label: label.into(),
            state: ButtonState::Normal,
            style: Style::new(),
            on_click: None,
            size: Vector2::new(100.0, 40.0),
        }
    }
    
    /// Set the click handler
    pub fn on_click<F>(mut self, handler: F) -> Self 
    where 
        F: Fn() + Send + Sync + 'static
    {
        self.on_click = Some(Box::new(handler));
        self
    }
    
    /// Set the button style
    pub fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
    
    /// Set disabled state
    pub fn disabled(mut self, disabled: bool) -> Self {
        if disabled {
            self.state = ButtonState::Disabled;
        } else if self.state == ButtonState::Disabled {
            self.state = ButtonState::Normal;
        }
        self
    }
    
    /// Get the current button color based on state
    fn get_color(&self) -> Color {
        match self.state {
            ButtonState::Normal => {
                self.style.background_color()
                    .cloned()
                    .unwrap_or(Color::new(0.2, 0.5, 0.8, 1.0))
            }
            ButtonState::Hovered => Color::new(0.3, 0.6, 0.9, 1.0),
            ButtonState::Pressed => Color::new(0.1, 0.4, 0.7, 1.0),
            ButtonState::Disabled => Color::new(0.5, 0.5, 0.5, 1.0),
        }
    }
}

impl Component for Button {
    fn id(&self) -> ComponentId {
        self.id
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, ctx: &ComponentContext) -> bool {
        if self.state == ButtonState::Disabled {
            return false;
        }
        
        match event {
            ComponentEvent::MouseEnter => {
                if self.state == ButtonState::Normal {
                    self.state = ButtonState::Hovered;
                }
                true
            }
            ComponentEvent::MouseLeave => {
                if self.state == ButtonState::Hovered || self.state == ButtonState::Pressed {
                    self.state = ButtonState::Normal;
                }
                true
            }
            ComponentEvent::MouseDown(_) => {
                self.state = ButtonState::Pressed;
                true
            }
            ComponentEvent::MouseUp(pos) => {
                if self.state == ButtonState::Pressed && self.contains_point(*pos, ctx) {
                    self.state = ButtonState::Hovered;
                    if let Some(handler) = &self.on_click {
                        handler();
                    }
                }
                true
            }
            _ => false,
        }
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        // Calculate text size (approximate)
        let text_width = self.label.len() as f32 * 8.0;
        let text_height = 16.0;
        
        // Add padding
        let (padding_top, padding_right, padding_bottom, padding_left) = self.style.padding();
        let content_width = text_width + padding_left + padding_right;
        let content_height = text_height + padding_top + padding_bottom;
        
        // Apply style dimensions if set
        let width = self.style.width().unwrap_or(content_width);
        let height = self.style.height().unwrap_or(content_height);
        
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
        
        // Background rectangle
        let bg_color = self.get_color();
        let border_radius = self.style.get("border-radius")
            .and_then(|p| match p {
                super::style::StyleProperty::BorderRadius(r) => Some(*r),
                _ => None,
            })
            .unwrap_or(4.0);
        
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
            color: bg_color,
            radius: border_radius,
        });
        
        // Text label
        let text_color = match self.state {
            ButtonState::Disabled => Color::new(0.7, 0.7, 0.7, 1.0),
            _ => Color::new(1.0, 1.0, 1.0, 1.0),
        };
        
        // Center text in button
        let text_x = ctx.position.x + self.size.x / 2.0 - (self.label.len() as f32 * 4.0);
        let text_y = ctx.position.y + self.size.y / 2.0 - 8.0;
        
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
            content: self.label.clone(),
            size: 16.0,
            color: text_color,
            font: None,
        });
        
        renderables
    }
}