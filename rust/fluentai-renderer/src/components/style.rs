// Style system for components

use crate::primitives::Color;
use crate::effects::{BoxShadow as EffectBoxShadow, BlurFilter, FilterStack};
use cgmath::Vector2;
use std::collections::HashMap;

/// Style properties that can be applied to components
#[derive(Debug, Clone)]
pub enum StyleProperty {
    // Layout
    Width(f32),
    Height(f32),
    MinWidth(f32),
    MinHeight(f32),
    MaxWidth(f32),
    MaxHeight(f32),
    Padding(f32),
    PaddingTop(f32),
    PaddingRight(f32),
    PaddingBottom(f32),
    PaddingLeft(f32),
    Margin(f32),
    MarginTop(f32),
    MarginRight(f32),
    MarginBottom(f32),
    MarginLeft(f32),
    
    // Positioning
    Position(PositionType),
    Top(f32),
    Right(f32),
    Bottom(f32),
    Left(f32),
    
    // Flexbox
    Display(DisplayType),
    FlexDirection(FlexDirection),
    JustifyContent(JustifyContent),
    AlignItems(AlignItems),
    AlignSelf(AlignSelf),
    FlexGrow(f32),
    FlexShrink(f32),
    FlexBasis(f32),
    Gap(f32),
    
    // Appearance
    BackgroundColor(Color),
    BackgroundGradient(String), // Gradient ID
    BorderColor(Color),
    BorderWidth(f32),
    BorderRadius(f32),
    Opacity(f32),
    
    // Text
    Color(Color),
    FontSize(f32),
    FontFamily(String),
    TextAlign(TextAlign),
    LineHeight(f32),
    
    // Effects
    BoxShadow(BoxShadow),
    Filter(Filter),
    Transform(Transform),
    
    // Other
    Cursor(CursorType),
    PointerEvents(PointerEvents),
    Overflow(Overflow),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PositionType {
    Static,
    Relative,
    Absolute,
    Fixed,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DisplayType {
    None,
    Block,
    Flex,
    Grid,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FlexDirection {
    Row,
    RowReverse,
    Column,
    ColumnReverse,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JustifyContent {
    Start,
    End,
    Center,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AlignItems {
    Start,
    End,
    Center,
    Stretch,
    Baseline,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AlignSelf {
    Auto,
    Start,
    End,
    Center,
    Stretch,
    Baseline,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextAlign {
    Left,
    Center,
    Right,
    Justify,
}

#[derive(Debug, Clone)]
pub struct BoxShadow {
    pub offset: Vector2<f32>,
    pub blur_radius: f32,
    pub spread_radius: f32,
    pub color: Color,
    pub inset: bool,
}

impl From<BoxShadow> for EffectBoxShadow {
    fn from(shadow: BoxShadow) -> Self {
        let mut effect = EffectBoxShadow::new(
            shadow.offset.x,
            shadow.offset.y,
            shadow.blur_radius,
            shadow.color,
        )
        .with_spread(shadow.spread_radius);
        
        if shadow.inset {
            effect = effect.inset();
        }
        
        effect
    }
}

#[derive(Debug, Clone)]
pub enum Filter {
    Blur(f32),
    Brightness(f32),
    Contrast(f32),
    Grayscale(f32),
    Saturate(f32),
    DropShadow { offset_x: f32, offset_y: f32, blur: f32, color: Color },
    Custom(FilterStack),
}

#[derive(Debug, Clone)]
pub struct Transform {
    pub translate: Vector2<f32>,
    pub rotate: f32,
    pub scale: Vector2<f32>,
    pub skew: Vector2<f32>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CursorType {
    Default,
    Pointer,
    Text,
    Move,
    NotAllowed,
    Resize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PointerEvents {
    Auto,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Overflow {
    Visible,
    Hidden,
    Scroll,
    Auto,
}

/// Style collection for a component
#[derive(Debug, Clone, Default)]
pub struct Style {
    properties: HashMap<String, StyleProperty>,
}

impl Style {
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Set a style property
    pub fn set(&mut self, name: &str, property: StyleProperty) {
        self.properties.insert(name.to_string(), property);
    }
    
    /// Get a style property
    pub fn get(&self, name: &str) -> Option<&StyleProperty> {
        self.properties.get(name)
    }
    
    /// Remove a style property
    pub fn remove(&mut self, name: &str) {
        self.properties.remove(name);
    }
    
    /// Clear all properties
    pub fn clear(&mut self) {
        self.properties.clear();
    }
    
    /// Merge another style into this one
    pub fn merge(&mut self, other: &Style) {
        for (key, value) in &other.properties {
            self.properties.insert(key.clone(), value.clone());
        }
    }
    
    // Helper methods for common properties
    
    pub fn width(&self) -> Option<f32> {
        match self.get("width") {
            Some(StyleProperty::Width(w)) => Some(*w),
            _ => None,
        }
    }
    
    pub fn height(&self) -> Option<f32> {
        match self.get("height") {
            Some(StyleProperty::Height(h)) => Some(*h),
            _ => None,
        }
    }
    
    pub fn background_color(&self) -> Option<&Color> {
        match self.get("background-color") {
            Some(StyleProperty::BackgroundColor(c)) => Some(c),
            _ => None,
        }
    }
    
    pub fn padding(&self) -> (f32, f32, f32, f32) {
        let default = match self.get("padding") {
            Some(StyleProperty::Padding(p)) => *p,
            _ => 0.0,
        };
        
        let top = match self.get("padding-top") {
            Some(StyleProperty::PaddingTop(p)) => *p,
            _ => default,
        };
        
        let right = match self.get("padding-right") {
            Some(StyleProperty::PaddingRight(p)) => *p,
            _ => default,
        };
        
        let bottom = match self.get("padding-bottom") {
            Some(StyleProperty::PaddingBottom(p)) => *p,
            _ => default,
        };
        
        let left = match self.get("padding-left") {
            Some(StyleProperty::PaddingLeft(p)) => *p,
            _ => default,
        };
        
        (top, right, bottom, left)
    }
    
    pub fn margin(&self) -> (f32, f32, f32, f32) {
        let default = match self.get("margin") {
            Some(StyleProperty::Margin(m)) => *m,
            _ => 0.0,
        };
        
        let top = match self.get("margin-top") {
            Some(StyleProperty::MarginTop(m)) => *m,
            _ => default,
        };
        
        let right = match self.get("margin-right") {
            Some(StyleProperty::MarginRight(m)) => *m,
            _ => default,
        };
        
        let bottom = match self.get("margin-bottom") {
            Some(StyleProperty::MarginBottom(m)) => *m,
            _ => default,
        };
        
        let left = match self.get("margin-left") {
            Some(StyleProperty::MarginLeft(m)) => *m,
            _ => default,
        };
        
        (top, right, bottom, left)
    }
}

/// Builder for creating styles fluently
pub struct StyleBuilder {
    style: Style,
}

impl StyleBuilder {
    pub fn new() -> Self {
        Self {
            style: Style::new(),
        }
    }
    
    pub fn width(mut self, width: f32) -> Self {
        self.style.set("width", StyleProperty::Width(width));
        self
    }
    
    pub fn height(mut self, height: f32) -> Self {
        self.style.set("height", StyleProperty::Height(height));
        self
    }
    
    pub fn background_color(mut self, color: Color) -> Self {
        self.style.set("background-color", StyleProperty::BackgroundColor(color));
        self
    }
    
    pub fn padding(mut self, padding: f32) -> Self {
        self.style.set("padding", StyleProperty::Padding(padding));
        self
    }
    
    pub fn margin(mut self, margin: f32) -> Self {
        self.style.set("margin", StyleProperty::Margin(margin));
        self
    }
    
    pub fn border_radius(mut self, radius: f32) -> Self {
        self.style.set("border-radius", StyleProperty::BorderRadius(radius));
        self
    }
    
    pub fn build(self) -> Style {
        self.style
    }
}