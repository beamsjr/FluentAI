/// Component inspector for debugging UI hierarchies
use crate::components::{Component, ComponentId, ComponentContext};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Component inspector for real-time UI debugging
pub struct ComponentInspector {
    component_tree: Option<ComponentNode>,
    selected_component: Option<ComponentId>,
    component_props: HashMap<ComponentId, ComponentProperties>,
    breakpoints: HashMap<ComponentId, Vec<Breakpoint>>,
}

/// Component tree node
#[derive(Debug, Clone)]
pub struct ComponentNode {
    pub id: ComponentId,
    pub name: String,
    pub selected: bool,
    pub expanded: bool,
    pub children: Vec<ComponentNode>,
    pub props: ComponentProperties,
}

/// Component properties for inspection
#[derive(Debug, Clone)]
pub struct ComponentProperties {
    pub position: (f32, f32),
    pub size: (f32, f32),
    pub visible: bool,
    pub custom_props: HashMap<String, PropertyValue>,
}

#[derive(Debug, Clone)]
pub enum PropertyValue {
    String(String),
    Number(f64),
    Bool(bool),
    Color(f32, f32, f32, f32),
    List(Vec<PropertyValue>),
}

/// Breakpoint for component debugging
#[derive(Debug, Clone)]
pub struct Breakpoint {
    pub event_type: String,
    pub condition: Option<String>,
    pub enabled: bool,
}

impl ComponentInspector {
    pub fn new() -> Self {
        Self {
            component_tree: None,
            selected_component: None,
            component_props: HashMap::new(),
            breakpoints: HashMap::new(),
        }
    }
    
    /// Update component tree
    pub fn update_tree(&mut self, root: &dyn Component, ctx: &ComponentContext) {
        self.component_tree = Some(self.build_tree_node(root, ctx));
    }
    
    /// Build tree node from component
    fn build_tree_node(&self, component: &dyn Component, ctx: &ComponentContext) -> ComponentNode {
        let id = component.id();
        let selected = self.selected_component == Some(id);
        
        // Get component properties
        let props = self.extract_properties(component, ctx);
        
        ComponentNode {
            id,
            name: self.get_component_name(component),
            selected,
            expanded: true, // TODO: Track expansion state
            children: Vec::new(), // TODO: Get children from component
            props,
        }
    }
    
    /// Extract properties from component
    fn extract_properties(&self, component: &dyn Component, ctx: &ComponentContext) -> ComponentProperties {
        // NOTE: We can't call layout() here because it requires &mut self
        // In a real implementation, layout would be cached or calculated elsewhere
        
        ComponentProperties {
            position: (ctx.position.x, ctx.position.y),
            size: (ctx.size.x, ctx.size.y),
            visible: true, // TODO: Get visibility
            custom_props: HashMap::new(), // TODO: Extract custom properties
        }
    }
    
    /// Get component name
    fn get_component_name(&self, component: &dyn Component) -> String {
        // Use type name or custom debug name
        std::any::type_name_of_val(component)
            .split("::")
            .last()
            .unwrap_or("Unknown")
            .to_string()
    }
    
    /// Select a component
    pub fn select_component(&mut self, id: ComponentId) {
        self.selected_component = Some(id);
        
        // Update tree to reflect selection
        if let Some(ref mut tree) = self.component_tree {
            Self::update_selection_recursive(tree, id);
        }
    }
    
    /// Update selection state recursively
    fn update_selection_recursive(node: &mut ComponentNode, selected_id: ComponentId) {
        node.selected = node.id == selected_id;
        for child in &mut node.children {
            Self::update_selection_recursive(child, selected_id);
        }
    }
    
    /// Get component tree
    pub fn get_component_tree(&self) -> Option<&ComponentNode> {
        self.component_tree.as_ref()
    }
    
    /// Get selected component properties
    pub fn get_selected_properties(&self) -> Option<&ComponentProperties> {
        self.selected_component
            .and_then(|id| self.component_props.get(&id))
    }
    
    /// Set property value
    pub fn set_property(&mut self, component_id: ComponentId, prop_name: &str, value: PropertyValue) {
        if let Some(props) = self.component_props.get_mut(&component_id) {
            props.custom_props.insert(prop_name.to_string(), value);
        }
    }
    
    /// Add breakpoint
    pub fn add_breakpoint(&mut self, component_id: ComponentId, breakpoint: Breakpoint) {
        self.breakpoints
            .entry(component_id)
            .or_insert_with(Vec::new)
            .push(breakpoint);
    }
    
    /// Remove breakpoint
    pub fn remove_breakpoint(&mut self, component_id: ComponentId, index: usize) {
        if let Some(breakpoints) = self.breakpoints.get_mut(&component_id) {
            if index < breakpoints.len() {
                breakpoints.remove(index);
            }
        }
    }
    
    /// Check if should break on event
    pub fn should_break(&self, component_id: ComponentId, event_type: &str) -> bool {
        if let Some(breakpoints) = self.breakpoints.get(&component_id) {
            breakpoints.iter().any(|bp| {
                bp.enabled && bp.event_type == event_type
            })
        } else {
            false
        }
    }
    
    /// Export component tree as JSON
    pub fn export_tree_json(&self) -> serde_json::Value {
        if let Some(ref tree) = self.component_tree {
            self.node_to_json(tree)
        } else {
            serde_json::json!(null)
        }
    }
    
    fn node_to_json(&self, node: &ComponentNode) -> serde_json::Value {
        serde_json::json!({
            "id": node.id.to_string(),
            "name": node.name,
            "selected": node.selected,
            "expanded": node.expanded,
            "props": {
                "position": node.props.position,
                "size": node.props.size,
                "visible": node.props.visible,
                "custom": self.props_to_json(&node.props.custom_props)
            },
            "children": node.children.iter()
                .map(|child| self.node_to_json(child))
                .collect::<Vec<_>>()
        })
    }
    
    fn props_to_json(&self, props: &HashMap<String, PropertyValue>) -> serde_json::Value {
        let mut obj = serde_json::Map::new();
        
        for (key, value) in props {
            obj.insert(key.clone(), self.prop_value_to_json(value));
        }
        
        serde_json::Value::Object(obj)
    }
    
    fn prop_value_to_json(&self, value: &PropertyValue) -> serde_json::Value {
        match value {
            PropertyValue::String(s) => serde_json::json!(s),
            PropertyValue::Number(n) => serde_json::json!(n),
            PropertyValue::Bool(b) => serde_json::json!(b),
            PropertyValue::Color(r, g, b, a) => serde_json::json!({
                "r": r, "g": g, "b": b, "a": a
            }),
            PropertyValue::List(items) => serde_json::json!(
                items.iter().map(|v| self.prop_value_to_json(v)).collect::<Vec<_>>()
            ),
        }
    }
}

impl ComponentNode {
    /// Count total descendants
    pub fn count_descendants(&self) -> usize {
        self.children.iter()
            .map(|child| 1 + child.count_descendants())
            .sum()
    }
    
    /// Find node by ID
    pub fn find_by_id(&self, id: ComponentId) -> Option<&ComponentNode> {
        if self.id == id {
            Some(self)
        } else {
            self.children.iter()
                .find_map(|child| child.find_by_id(id))
        }
    }
    
    /// Find node by ID (mutable)
    pub fn find_by_id_mut(&mut self, id: ComponentId) -> Option<&mut ComponentNode> {
        if self.id == id {
            Some(self)
        } else {
            self.children.iter_mut()
                .find_map(|child| child.find_by_id_mut(id))
        }
    }
}

/// Component highlighting for visual debugging
pub struct ComponentHighlighter {
    highlights: HashMap<ComponentId, HighlightStyle>,
}

#[derive(Debug, Clone)]
pub struct HighlightStyle {
    pub color: crate::primitives::Color,
    pub line_width: f32,
    pub fill_opacity: f32,
}

impl ComponentHighlighter {
    pub fn new() -> Self {
        Self {
            highlights: HashMap::new(),
        }
    }
    
    /// Highlight a component
    pub fn highlight(&mut self, id: ComponentId, style: HighlightStyle) {
        self.highlights.insert(id, style);
    }
    
    /// Remove highlight
    pub fn unhighlight(&mut self, id: ComponentId) {
        self.highlights.remove(&id);
    }
    
    /// Clear all highlights
    pub fn clear(&mut self) {
        self.highlights.clear();
    }
    
    /// Get highlight renderables
    pub fn get_highlight_renderables(&self) -> Vec<crate::primitives::Renderable> {
        // TODO: Generate outline renderables for highlighted components
        Vec::new()
    }
}

/// Layout debugger
pub struct LayoutDebugger {
    show_bounds: bool,
    show_padding: bool,
    show_margins: bool,
    show_flex_lines: bool,
    show_grid_lines: bool,
}

impl LayoutDebugger {
    pub fn new() -> Self {
        Self {
            show_bounds: false,
            show_padding: false,
            show_margins: false,
            show_flex_lines: false,
            show_grid_lines: false,
        }
    }
    
    /// Toggle bound visualization
    pub fn toggle_bounds(&mut self) {
        self.show_bounds = !self.show_bounds;
    }
    
    /// Get debug renderables
    pub fn get_debug_renderables(&self) -> Vec<crate::primitives::Renderable> {
        let renderables = Vec::new();
        
        if self.show_bounds {
            // Add bounds visualization
        }
        
        if self.show_padding {
            // Add padding visualization
        }
        
        if self.show_margins {
            // Add margin visualization
        }
        
        renderables
    }
}