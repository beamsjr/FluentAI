// Container components for layout

use super::base::{Component, ComponentContext, ComponentEvent, ComponentId, generate_component_id, LayoutConstraints, LayoutSize};
use super::style::{Style, FlexDirection, JustifyContent, AlignItems, AlignSelf, StyleProperty};
use super::flexbox::FlexboxLayout;
use crate::primitives::{Renderable, Transform, Position3D, Size2D};
use cgmath::Vector2;

/// Generic container component
pub struct Container {
    id: ComponentId,
    children: Vec<Box<dyn Component>>,
    style: Style,
    size: Vector2<f32>,
    layout_direction: FlexDirection,
    child_sizes: Vec<LayoutSize>,
    child_positions: Vec<Vector2<f32>>,
}

impl Container {
    pub fn new() -> Self {
        Self {
            id: generate_component_id(),
            children: Vec::new(),
            style: Style::new(),
            size: Vector2::new(0.0, 0.0),
            layout_direction: FlexDirection::Row,
            child_sizes: Vec::new(),
            child_positions: Vec::new(),
        }
    }
    
    /// Add a child component
    pub fn add_child(mut self, child: impl Component + 'static) -> Self {
        self.children.push(Box::new(child));
        self
    }
    
    /// Set the container style
    pub fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
    
    /// Set layout direction
    pub fn direction(mut self, direction: FlexDirection) -> Self {
        self.layout_direction = direction;
        self
    }
    
    /// Perform flexbox layout on children
    fn layout_children(&mut self, constraints: &LayoutConstraints) -> Vector2<f32> {
        // Extract style properties
        let direction = self.style.get("flex-direction")
            .and_then(|p| match p {
                StyleProperty::FlexDirection(d) => Some(*d),
                _ => None,
            })
            .unwrap_or(self.layout_direction);
            
        let justify = self.style.get("justify-content")
            .and_then(|p| match p {
                StyleProperty::JustifyContent(j) => Some(*j),
                _ => None,
            })
            .unwrap_or(JustifyContent::Start);
            
        let align = self.style.get("align-items")
            .and_then(|p| match p {
                StyleProperty::AlignItems(a) => Some(*a),
                _ => None,
            })
            .unwrap_or(AlignItems::Stretch);
        let padding = self.style.padding();
        let gap = self.style.get("gap")
            .and_then(|p| match p {
                StyleProperty::Gap(g) => Some(*g),
                _ => None,
            })
            .unwrap_or(0.0);
        
        // Create flexbox layout engine
        let layout = FlexboxLayout::new(direction)
            .with_justify(justify)
            .with_align(align)
            .with_gap(gap);
        
        // Convert children to flex items
        let mut flex_items = Vec::new();
        
        for child in &mut self.children {
            // Calculate natural size for each child
            let child_constraints = LayoutConstraints::unconstrained();
            let natural_size = child.layout(&child_constraints);
            
            // Extract flex properties from child styles
            // TODO: Add a way to get style properties from components
            let flex_grow = 0.0;
            let flex_shrink = 1.0;
            let flex_basis = None;
            let align_self = AlignSelf::Auto;
            
            flex_items.push(super::flexbox::FlexItem {
                id: child.id(),
                natural_size,
                computed_size: natural_size,
                position: Vector2::new(0.0, 0.0),
                flex_grow,
                flex_shrink,
                flex_basis,
                align_self,
                margin: (0.0, 0.0, 0.0, 0.0),
            });
        }
        
        // Calculate layout
        let container_size = layout.calculate_layout(&mut flex_items, constraints, padding);
        
        // Update child sizes and positions
        self.child_sizes.clear();
        self.child_positions.clear();
        
        for item in flex_items {
            self.child_sizes.push(item.computed_size);
            self.child_positions.push(item.position);
        }
        
        // Re-layout children with their computed sizes
        for (i, child) in self.children.iter_mut().enumerate() {
            if i < self.child_sizes.len() {
                let size = self.child_sizes[i];
                let constrained = LayoutConstraints::fixed(Vector2::new(size.width, size.height));
                child.layout(&constrained);
            }
        }
        
        Vector2::new(container_size.width, container_size.height)
    }
}

impl Component for Container {
    fn id(&self) -> ComponentId {
        self.id
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, ctx: &ComponentContext) -> bool {
        // Forward events to children
        for child in &mut self.children {
            // TODO: Create child context with proper position/size
            if child.handle_event(event, ctx) {
                return true;
            }
        }
        false
    }
    
    fn update(&mut self, ctx: &ComponentContext) {
        // Update all children
        for child in &mut self.children {
            child.update(ctx);
        }
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        let size = self.layout_children(constraints);
        
        // Apply style dimensions if set
        self.size.x = self.style.width().unwrap_or(size.x);
        self.size.y = self.style.height().unwrap_or(size.y);
        
        // Constrain to layout constraints
        self.size.x = self.size.x.max(constraints.min_size.x).min(constraints.max_size.x);
        self.size.y = self.size.y.max(constraints.min_size.y).min(constraints.max_size.y);
        
        LayoutSize {
            width: self.size.x,
            height: self.size.y,
        }
    }
    
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        let mut renderables = Vec::new();
        
        // Render background if specified
        if let Some(bg_color) = self.style.background_color() {
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
                color: *bg_color,
                radius: 0.0,
            });
        }
        
        // Use pre-calculated child positions
        // Note: positions need to be offset by current context position
        let position_offset = ctx.position;
        
        // Render children
        for (i, child) in self.children.iter().enumerate() {
            if i < self.child_positions.len() && i < self.child_sizes.len() {
                // Calculate actual position by adding offset
                let child_position = Vector2::new(
                    self.child_positions[i].x + position_offset.x,
                    self.child_positions[i].y + position_offset.y
                );
                
                // Create child context
                let child_ctx = ComponentContext {
                    frame_time: ctx.frame_time,
                    delta_time: ctx.delta_time,
                    screen_size: ctx.screen_size,
                    size: Vector2::new(self.child_sizes[i].width, self.child_sizes[i].height),
                    position: child_position,
                };
                
                // Render child
                let child_renderables = child.render(&child_ctx);
                renderables.extend(child_renderables);
            }
        }
        
        renderables
    }
    
    // Note: Child access is handled internally by Container
    // No external access to children due to lifetime complexity
}

/// Column container (vertical flex)
pub struct Column {
    container: Container,
}

impl Column {
    pub fn new() -> Self {
        let mut container = Container::new();
        container = container.direction(FlexDirection::Column);
        Self { container }
    }
    
    pub fn add_child(mut self, child: impl Component + 'static) -> Self {
        self.container = self.container.add_child(child);
        self
    }
    
    pub fn style(mut self, style: Style) -> Self {
        self.container = self.container.style(style);
        self
    }
}

impl Component for Column {
    fn id(&self) -> ComponentId {
        self.container.id()
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, ctx: &ComponentContext) -> bool {
        self.container.handle_event(event, ctx)
    }
    
    fn update(&mut self, ctx: &ComponentContext) {
        self.container.update(ctx)
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        self.container.layout(constraints)
    }
    
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        self.container.render(ctx)
    }
    
    // Child access delegated to internal container
}

/// Row container (horizontal flex)
pub struct Row {
    container: Container,
}

impl Row {
    pub fn new() -> Self {
        Self {
            container: Container::new(),
        }
    }
    
    pub fn add_child(mut self, child: impl Component + 'static) -> Self {
        self.container = self.container.add_child(child);
        self
    }
    
    pub fn style(mut self, style: Style) -> Self {
        self.container = self.container.style(style);
        self
    }
}

impl Component for Row {
    fn id(&self) -> ComponentId {
        self.container.id()
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, ctx: &ComponentContext) -> bool {
        self.container.handle_event(event, ctx)
    }
    
    fn update(&mut self, ctx: &ComponentContext) {
        self.container.update(ctx)
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        self.container.layout(constraints)
    }
    
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        self.container.render(ctx)
    }
    
    // Child access delegated to internal container
}