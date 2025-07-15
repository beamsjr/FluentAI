/// UI Builder for declarative UI creation with FluentAI macros
use crate::components::{
    Component, ComponentId, ComponentEvent, ComponentContext, 
    LayoutConstraints, LayoutSize, Container, Button,
    style::{FlexDirection, AlignItems, JustifyContent}
};
use crate::primitives::Renderable;
use crate::primitives::Color;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

/// Builder trait for fluent API
pub trait UIBuilder: Sized {
    fn id(self, id: &str) -> Self;
    fn class(self, class: &str) -> Self;
    fn style(self, key: &str, value: &str) -> Self;
    fn on(self, event: &str, handler: Box<dyn Fn(&ComponentEvent) + Send + Sync>) -> Self;
    fn child(self, child: Box<dyn Component>) -> Self;
    fn children(self, children: Vec<Box<dyn Component>>) -> Self;
}

/// Stack component for vertical/horizontal layouts
pub struct Stack {
    container: Container,
    props: StackProps,
}

#[derive(Clone)]
pub struct StackProps {
    pub direction: StackDirection,
    pub spacing: f32,
    pub padding: f32,
    pub alignment: StackAlignment,
}

#[derive(Clone, Copy, PartialEq)]
pub enum StackDirection {
    Horizontal,
    Vertical,
}

#[derive(Clone, Copy, PartialEq)]
pub enum StackAlignment {
    Start,
    Center,
    End,
    Stretch,
}

impl Default for StackProps {
    fn default() -> Self {
        Self {
            direction: StackDirection::Vertical,
            spacing: 0.0,
            padding: 0.0,
            alignment: StackAlignment::Start,
        }
    }
}

impl Stack {
    pub fn new() -> Self {
        Self {
            container: Container::new(ContainerProps::default()),
            props: StackProps::default(),
        }
    }
    
    pub fn direction(mut self, direction: StackDirection) -> Self {
        self.props.direction = direction;
        self.update_container_props();
        self
    }
    
    pub fn spacing(mut self, spacing: f32) -> Self {
        self.props.spacing = spacing;
        self.update_container_props();
        self
    }
    
    pub fn padding(mut self, padding: f32) -> Self {
        self.props.padding = padding;
        self.update_container_props();
        self
    }
    
    pub fn alignment(mut self, alignment: StackAlignment) -> Self {
        self.props.alignment = alignment;
        self.update_container_props();
        self
    }
    
    fn update_container_props(&mut self) {
        let mut props = ContainerProps::default();
        
        match self.props.direction {
            StackDirection::Horizontal => {
                props.flex_direction = crate::components::flexbox::FlexDirection::Row;
            }
            StackDirection::Vertical => {
                props.flex_direction = crate::components::flexbox::FlexDirection::Column;
            }
        }
        
        match self.props.alignment {
            StackAlignment::Start => {
                props.align_items = crate::components::flexbox::AlignItems::FlexStart;
            }
            StackAlignment::Center => {
                props.align_items = crate::components::flexbox::AlignItems::Center;
            }
            StackAlignment::End => {
                props.align_items = crate::components::flexbox::AlignItems::FlexEnd;
            }
            StackAlignment::Stretch => {
                props.align_items = crate::components::flexbox::AlignItems::Stretch;
            }
        }
        
        props.gap = self.props.spacing;
        props.padding = (self.props.padding, self.props.padding, self.props.padding, self.props.padding);
        
        self.container = Container::new(props);
    }
}

impl UIBuilder for Stack {
    fn id(mut self, id: &str) -> Self {
        self.container.set_id(id);
        self
    }
    
    fn class(self, _class: &str) -> Self {
        // TODO: Implement class support
        self
    }
    
    fn style(self, _key: &str, _value: &str) -> Self {
        // TODO: Implement style support
        self
    }
    
    fn on(self, _event: &str, _handler: Box<dyn Fn(&ComponentEvent) + Send + Sync>) -> Self {
        // TODO: Implement event support
        self
    }
    
    fn child(mut self, child: Box<dyn Component>) -> Self {
        self.container.add_child(child);
        self
    }
    
    fn children(mut self, children: Vec<Box<dyn Component>>) -> Self {
        for child in children {
            self.container.add_child(child);
        }
        self
    }
}

impl Component for Stack {
    fn id(&self) -> ComponentId {
        self.container.id()
    }
    
    fn handle_event(&mut self, event: &ComponentEvent, ctx: &ComponentContext) -> bool {
        self.container.handle_event(event, ctx)
    }
    
    fn layout(&mut self, constraints: &LayoutConstraints) -> LayoutSize {
        self.container.layout(constraints)
    }
    
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        self.container.render(ctx)
    }
}

/// Text builder for fluent API
pub struct Text {
    component: TextComponent,
}

impl Text {
    pub fn new(content: &str) -> Self {
        Self {
            component: TextComponent::new(content.to_string()),
        }
    }
    
    pub fn font_size(mut self, size: f32) -> Self {
        self.component.set_font_size(size);
        self
    }
    
    pub fn color(mut self, color: Color) -> Self {
        self.component.set_color(color);
        self
    }
    
    pub fn bold(mut self) -> Self {
        self.component.set_bold(true);
        self
    }
    
    pub fn italic(mut self) -> Self {
        self.component.set_italic(true);
        self
    }
}

impl From<Text> for Box<dyn Component> {
    fn from(text: Text) -> Self {
        Box::new(text.component)
    }
}

/// Button builder for fluent API
pub struct Button {
    component: ButtonComponent,
}

impl Button {
    pub fn new(label: &str) -> Self {
        Self {
            component: ButtonComponent::new(label.to_string()),
        }
    }
    
    pub fn on_click<F>(mut self, handler: F) -> Self 
    where
        F: Fn() + Send + Sync + 'static
    {
        self.component.on_click(handler);
        self
    }
    
    pub fn variant(mut self, variant: ButtonVariant) -> Self {
        match variant {
            ButtonVariant::Primary => {
                self.component.set_background_color(Color::new(0.2, 0.4, 0.8, 1.0));
                self.component.set_text_color(Color::new(1.0, 1.0, 1.0, 1.0));
            }
            ButtonVariant::Secondary => {
                self.component.set_background_color(Color::new(0.8, 0.8, 0.8, 1.0));
                self.component.set_text_color(Color::new(0.2, 0.2, 0.2, 1.0));
            }
            ButtonVariant::Danger => {
                self.component.set_background_color(Color::new(0.8, 0.2, 0.2, 1.0));
                self.component.set_text_color(Color::new(1.0, 1.0, 1.0, 1.0));
            }
        }
        self
    }
}

#[derive(Clone, Copy)]
pub enum ButtonVariant {
    Primary,
    Secondary,
    Danger,
}

impl From<Button> for Box<dyn Component> {
    fn from(button: Button) -> Self {
        Box::new(button.component)
    }
}

/// Conditional rendering helper
pub struct If {
    condition: bool,
    then_branch: Option<Box<dyn Component>>,
    else_branch: Option<Box<dyn Component>>,
}

impl If {
    pub fn new(condition: bool) -> Self {
        Self {
            condition,
            then_branch: None,
            else_branch: None,
        }
    }
    
    pub fn then(mut self, component: Box<dyn Component>) -> Self {
        self.then_branch = Some(component);
        self
    }
    
    pub fn else_then(mut self, component: Box<dyn Component>) -> Self {
        self.else_branch = Some(component);
        self
    }
    
    pub fn build(self) -> Option<Box<dyn Component>> {
        if self.condition {
            self.then_branch
        } else {
            self.else_branch
        }
    }
}

/// Loop rendering helper
pub struct For<T> {
    items: Vec<T>,
    render_fn: Box<dyn Fn(&T, usize) -> Box<dyn Component>>,
}

impl<T> For<T> {
    pub fn new(items: Vec<T>) -> Self {
        Self {
            items,
            render_fn: Box::new(|_, _| Box::new(EmptyComponent)),
        }
    }
    
    pub fn render<F>(mut self, render_fn: F) -> Self
    where
        F: Fn(&T, usize) -> Box<dyn Component> + 'static
    {
        self.render_fn = Box::new(render_fn);
        self
    }
    
    pub fn build(self) -> Vec<Box<dyn Component>> {
        self.items
            .iter()
            .enumerate()
            .map(|(i, item)| (self.render_fn)(item, i))
            .collect()
    }
}

/// Empty component for placeholders
struct EmptyComponent;

impl Component for EmptyComponent {
    fn id(&self) -> ComponentId {
        ComponentId::new()
    }
    
    fn handle_event(&mut self, _event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
        false
    }
    
    fn layout(&mut self, _constraints: &LayoutConstraints) -> LayoutSize {
        LayoutSize { width: 0.0, height: 0.0 }
    }
    
    fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
        Vec::new()
    }
}

/// Reactive state wrapper
pub struct ReactiveState<T: Clone> {
    value: Arc<Mutex<T>>,
    listeners: Arc<Mutex<Vec<Box<dyn Fn(&T) + Send + Sync>>>>,
}

impl<T: Clone> ReactiveState<T> {
    pub fn new(initial: T) -> Self {
        Self {
            value: Arc::new(Mutex::new(initial)),
            listeners: Arc::new(Mutex::new(Vec::new())),
        }
    }
    
    pub fn get(&self) -> T {
        self.value.lock().unwrap().clone()
    }
    
    pub fn set(&self, new_value: T) {
        {
            let mut value = self.value.lock().unwrap();
            *value = new_value.clone();
        }
        
        // Notify listeners
        let listeners = self.listeners.lock().unwrap();
        for listener in listeners.iter() {
            listener(&new_value);
        }
    }
    
    pub fn subscribe<F>(&self, listener: F)
    where
        F: Fn(&T) + Send + Sync + 'static
    {
        self.listeners.lock().unwrap().push(Box::new(listener));
    }
}

/// Helper functions for common UI patterns
pub mod helpers {
    use super::*;
    
    pub fn row(children: Vec<Box<dyn Component>>) -> Stack {
        Stack::new()
            .direction(StackDirection::Horizontal)
            .children(children)
    }
    
    pub fn column(children: Vec<Box<dyn Component>>) -> Stack {
        Stack::new()
            .direction(StackDirection::Vertical)
            .children(children)
    }
    
    pub fn spacer(size: f32) -> Box<dyn Component> {
        Box::new(SpacerComponent { size })
    }
    
    struct SpacerComponent {
        size: f32,
    }
    
    impl Component for SpacerComponent {
        fn id(&self) -> ComponentId {
            ComponentId::new()
        }
        
        fn handle_event(&mut self, _event: &ComponentEvent, _ctx: &ComponentContext) -> bool {
            false
        }
        
        fn layout(&mut self, _constraints: &LayoutConstraints) -> LayoutSize {
            LayoutSize { width: self.size, height: self.size }
        }
        
        fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
            Vec::new()
        }
    }
}