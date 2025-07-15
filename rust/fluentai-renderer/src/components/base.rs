// Base component trait and infrastructure

use crate::primitives::Renderable;
use cgmath::Vector2;
use std::any::Any;

/// Unique identifier for components
pub type ComponentId = u64;

/// Component lifecycle context
pub struct ComponentContext {
    /// Current frame time
    pub frame_time: f64,
    /// Delta time since last frame
    pub delta_time: f32,
    /// Screen size
    pub screen_size: Vector2<f32>,
    /// Component's computed size
    pub size: Vector2<f32>,
    /// Component's position
    pub position: Vector2<f32>,
}

/// Events that can be sent to components
#[derive(Debug, Clone)]
pub enum ComponentEvent {
    /// Mouse/touch events
    Click(Vector2<f32>),
    MouseDown(Vector2<f32>),
    MouseUp(Vector2<f32>),
    MouseMove(Vector2<f32>),
    MouseEnter,
    MouseLeave,
    
    /// Keyboard events
    KeyDown(String),
    KeyUp(String),
    TextInput(String),
    
    /// Focus events
    Focus,
    Blur,
    
    /// Custom events (without Box<dyn Any> to avoid Clone issues)
    Custom(String),
}

/// Layout information from parent
#[derive(Debug, Clone, Copy)]
pub struct LayoutConstraints {
    /// Minimum size
    pub min_size: Vector2<f32>,
    /// Maximum size
    pub max_size: Vector2<f32>,
    /// Available space
    pub available_size: Vector2<f32>,
}

impl LayoutConstraints {
    /// Create unconstrained layout
    pub fn unconstrained() -> Self {
        Self {
            min_size: Vector2::new(0.0, 0.0),
            max_size: Vector2::new(f32::INFINITY, f32::INFINITY),
            available_size: Vector2::new(f32::INFINITY, f32::INFINITY),
        }
    }
    
    /// Create fixed size constraints
    pub fn fixed(size: Vector2<f32>) -> Self {
        Self {
            min_size: size,
            max_size: size,
            available_size: size,
        }
    }
    
    /// Create constraints with bounds
    pub fn bounded(min: Vector2<f32>, max: Vector2<f32>) -> Self {
        Self {
            min_size: min,
            max_size: max,
            available_size: max,
        }
    }
}

/// Computed layout size
#[derive(Debug, Clone, Copy)]
pub struct LayoutSize {
    /// Computed width
    pub width: f32,
    /// Computed height
    pub height: f32,
}

impl From<Vector2<f32>> for LayoutSize {
    fn from(v: Vector2<f32>) -> Self {
        Self {
            width: v.x,
            height: v.y,
        }
    }
}

impl From<LayoutSize> for Vector2<f32> {
    fn from(s: LayoutSize) -> Self {
        Vector2::new(s.width, s.height)
    }
}

/// Core component trait
pub trait Component: Send + Sync {
    /// Get the component's unique ID
    fn id(&self) -> ComponentId;
    
    /// Handle an event
    fn handle_event(&mut self, _event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
        false // By default, don't consume events
    }
    
    /// Update the component (called each frame)
    fn update(&mut self, _ctx: &ComponentContext) {}
    
    /// Calculate the component's desired size given constraints
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize;
    
    /// Render the component to primitives
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable>;
    
    // Note: Child access methods removed due to lifetime complexity
    // Container components handle children internally
    
    /// Check if point is inside component
    fn contains_point(&self, point: Vector2<f32>, ctx: &ComponentContext) -> bool {
        point.x >= ctx.position.x && 
        point.x <= ctx.position.x + ctx.size.x &&
        point.y >= ctx.position.y && 
        point.y <= ctx.position.y + ctx.size.y
    }
    
    /// Get component state for hot reloading
    fn get_state(&self) -> Option<Box<dyn Any>> {
        None
    }
    
    /// Set component state for hot reloading
    fn set_state(&mut self, _state: Box<dyn Any>) {}
    
    /// Get self as Any for downcasting
    fn as_any(&self) -> &dyn Any {
        // Default implementation returns a dummy value
        // Components that need downcasting should override this
        &()
    }
}

/// Component builder pattern helper
pub struct ComponentBuilder<T> {
    component: T,
}

impl<T> ComponentBuilder<T> {
    pub fn new(component: T) -> Self {
        Self { component }
    }
    
    pub fn build(self) -> T {
        self.component
    }
}

/// ID generator for components
static mut NEXT_COMPONENT_ID: ComponentId = 0;

pub fn generate_component_id() -> ComponentId {
    unsafe {
        NEXT_COMPONENT_ID += 1;
        NEXT_COMPONENT_ID
    }
}