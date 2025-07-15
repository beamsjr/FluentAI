// Grid container component

use super::base::{Component, ComponentContext, ComponentEvent, ComponentId, generate_component_id, LayoutConstraints, LayoutSize};
use super::style::{Style, StyleProperty};
use super::grid::{GridLayout, GridTemplate, GridItem, GridGap, GridAlign, GridPlacement};
use crate::primitives::{Renderable, Transform, Position3D, Size2D};
use cgmath::Vector2;

/// Grid container component
pub struct Grid {
    id: ComponentId,
    children: Vec<Box<dyn Component>>,
    style: Style,
    size: Vector2<f32>,
    child_sizes: Vec<LayoutSize>,
    child_positions: Vec<Vector2<f32>>,
    grid_template_rows: Option<String>,
    grid_template_columns: Option<String>,
    gap: GridGap,
    align_items: GridAlign,
    justify_items: GridAlign,
}

impl Grid {
    pub fn new() -> Self {
        Self {
            id: generate_component_id(),
            children: Vec::new(),
            style: Style::new(),
            size: Vector2::new(0.0, 0.0),
            child_sizes: Vec::new(),
            child_positions: Vec::new(),
            grid_template_rows: None,
            grid_template_columns: None,
            gap: GridGap::uniform(0.0),
            align_items: GridAlign::Stretch,
            justify_items: GridAlign::Stretch,
        }
    }
    
    /// Set grid template for rows (e.g., "100px 1fr auto")
    pub fn rows(mut self, template: &str) -> Self {
        self.grid_template_rows = Some(template.to_string());
        self
    }
    
    /// Set grid template for columns (e.g., "200px 1fr 1fr")
    pub fn columns(mut self, template: &str) -> Self {
        self.grid_template_columns = Some(template.to_string());
        self
    }
    
    /// Set grid gap
    pub fn gap(mut self, gap: f32) -> Self {
        self.gap = GridGap::uniform(gap);
        self
    }
    
    /// Set row and column gaps separately
    pub fn row_column_gap(mut self, row_gap: f32, column_gap: f32) -> Self {
        self.gap = GridGap {
            row_gap,
            column_gap,
        };
        self
    }
    
    /// Set align items
    pub fn align_items(mut self, align: GridAlign) -> Self {
        self.align_items = align;
        self
    }
    
    /// Set justify items
    pub fn justify_items(mut self, justify: GridAlign) -> Self {
        self.justify_items = justify;
        self
    }
    
    /// Add a child component
    pub fn add_child(mut self, child: impl Component + 'static) -> Self {
        self.children.push(Box::new(child));
        self
    }
    
    /// Add a child with specific grid placement
    pub fn add_child_at(mut self, child: impl Component + 'static, row: i32, column: i32) -> Self {
        self.children.push(Box::new(GridChild::new(child).at(row, column)));
        self
    }
    
    /// Add a child that spans multiple cells
    pub fn add_child_span(
        mut self, 
        child: impl Component + 'static,
        row_start: i32,
        row_span: i32,
        col_start: i32,
        col_span: i32,
    ) -> Self {
        self.children.push(Box::new(
            GridChild::new(child)
                .at(row_start, col_start)
                .span(row_span, col_span)
        ));
        self
    }
    
    /// Set the container style
    pub fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
    
    /// Perform grid layout on children
    fn layout_children(&mut self, constraints: &LayoutConstraints) -> Vector2<f32> {
        let padding = self.style.padding();
        
        // Create grid layout engine
        let mut layout = GridLayout::new()
            .with_gap(self.gap)
            .with_align_items(self.align_items)
            .with_justify_items(self.justify_items);
        
        // Set grid templates
        if let Some(ref rows) = self.grid_template_rows {
            layout = layout.with_rows(GridTemplate::from_string(rows));
        }
        
        if let Some(ref cols) = self.grid_template_columns {
            layout = layout.with_columns(GridTemplate::from_string(cols));
        }
        
        // Convert children to grid items
        let mut grid_items = Vec::new();
        
        for (i, child) in self.children.iter_mut().enumerate() {
            // Get natural size
            let child_constraints = LayoutConstraints::unconstrained();
            let natural_size = child.layout(&child_constraints);
            
            // Get placement if child is a GridChild
            let placement = if let Some(grid_child) = child.as_any().downcast_ref::<GridChild>() {
                grid_child.placement.clone()
            } else {
                // Auto placement
                GridPlacement::default()
            };
            
            grid_items.push(GridItem {
                id: child.id(),
                natural_size,
                computed_size: natural_size,
                position: Vector2::new(0.0, 0.0),
                placement,
                align_self: None,
                justify_self: None,
            });
        }
        
        // Calculate layout
        let container_size = layout.calculate_layout(&mut grid_items, constraints, padding);
        
        // Update child sizes and positions
        self.child_sizes.clear();
        self.child_positions.clear();
        
        for item in grid_items {
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

impl Component for Grid {
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
        
        // Render children
        for (i, child) in self.children.iter().enumerate() {
            if i < self.child_positions.len() && i < self.child_sizes.len() {
                // Create child context
                let child_ctx = ComponentContext {
                    frame_time: ctx.frame_time,
                    delta_time: ctx.delta_time,
                    screen_size: ctx.screen_size,
                    size: Vector2::new(self.child_sizes[i].width, self.child_sizes[i].height),
                    position: Vector2::new(
                        ctx.position.x + self.child_positions[i].x,
                        ctx.position.y + self.child_positions[i].y,
                    ),
                };
                
                // Render child
                let child_renderables = child.render(&child_ctx);
                renderables.extend(child_renderables);
            }
        }
        
        renderables
    }
    
    // Note: Child access is handled internally by Grid
    // No external access to children due to lifetime complexity
}

/// Wrapper for grid children with placement information
struct GridChild {
    inner: Box<dyn Component>,
    placement: GridPlacement,
}

impl GridChild {
    fn new(component: impl Component + 'static) -> Self {
        Self {
            inner: Box::new(component),
            placement: GridPlacement::default(),
        }
    }
    
    fn at(mut self, row: i32, column: i32) -> Self {
        self.placement.row_start = row;
        self.placement.column_start = column;
        self
    }
    
    fn span(mut self, row_span: i32, col_span: i32) -> Self {
        if row_span > 1 {
            self.placement.row_end = Some(self.placement.row_start + row_span);
        }
        if col_span > 1 {
            self.placement.column_end = Some(self.placement.column_start + col_span);
        }
        self
    }
}

impl Component for GridChild {
    fn id(&self) -> ComponentId {
        self.inner.id()
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, ctx: &ComponentContext) -> bool {
        self.inner.handle_event(event, ctx)
    }
    
    fn update(&mut self, ctx: &ComponentContext) {
        self.inner.update(ctx)
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        self.inner.layout(constraints)
    }
    
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        self.inner.render(ctx)
    }
    
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}