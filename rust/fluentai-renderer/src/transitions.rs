// CSS-style transitions for component properties

use crate::animation::{Animation, AnimatableValue, EasingFunction, AnimationManager};
use crate::components::{Component, ComponentId};
use crate::primitives::Color;
use cgmath::Vector2;
use web_time::Duration;
use std::collections::HashMap;

/// Transitionable property names
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TransitionProperty {
    // Layout
    Width,
    Height,
    Padding,
    PaddingTop,
    PaddingRight,
    PaddingBottom,
    PaddingLeft,
    Margin,
    MarginTop,
    MarginRight,
    MarginBottom,
    MarginLeft,
    
    // Position
    X,
    Y,
    Top,
    Right,
    Bottom,
    Left,
    
    // Appearance
    Opacity,
    BackgroundColor,
    BorderColor,
    BorderWidth,
    BorderRadius,
    
    // Transform
    Scale,
    ScaleX,
    ScaleY,
    Rotate,
    TranslateX,
    TranslateY,
    
    // Effects
    BlurRadius,
    ShadowOffsetX,
    ShadowOffsetY,
    ShadowBlur,
    ShadowSpread,
    
    // All properties
    All,
    
    // Custom property
    Custom(String),
}

impl TransitionProperty {
    fn as_string(&self) -> String {
        match self {
            TransitionProperty::Width => "width".to_string(),
            TransitionProperty::Height => "height".to_string(),
            TransitionProperty::Opacity => "opacity".to_string(),
            TransitionProperty::BackgroundColor => "background-color".to_string(),
            TransitionProperty::X => "x".to_string(),
            TransitionProperty::Y => "y".to_string(),
            TransitionProperty::Scale => "scale".to_string(),
            TransitionProperty::Rotate => "rotate".to_string(),
            TransitionProperty::Custom(s) => s.clone(),
            _ => format!("{:?}", self).to_lowercase(),
        }
    }
}

/// Transition definition
#[derive(Clone)]
pub struct Transition {
    /// Property to transition
    pub property: TransitionProperty,
    /// Duration of the transition
    pub duration: Duration,
    /// Easing function
    pub easing: EasingFunction,
    /// Delay before starting
    pub delay: Duration,
}

impl Transition {
    pub fn new(property: TransitionProperty) -> Self {
        Self {
            property,
            duration: Duration::from_millis(300),
            easing: EasingFunction::EaseInOutQuad,
            delay: Duration::ZERO,
        }
    }
    
    pub fn duration(mut self, duration: Duration) -> Self {
        self.duration = duration;
        self
    }
    
    pub fn easing(mut self, easing: EasingFunction) -> Self {
        self.easing = easing;
        self
    }
    
    pub fn delay(mut self, delay: Duration) -> Self {
        self.delay = delay;
        self
    }
}

/// Component state snapshot
#[derive(Clone)]
pub struct ComponentState {
    pub id: ComponentId,
    pub properties: HashMap<String, AnimatableValue>,
}

/// Transition manager for components
pub struct TransitionManager {
    /// Active transitions by component ID
    transitions: HashMap<ComponentId, Vec<Transition>>,
    /// Animation manager
    animations: AnimationManager,
    /// Previous states for detecting changes
    previous_states: HashMap<ComponentId, ComponentState>,
}

impl TransitionManager {
    pub fn new() -> Self {
        Self {
            transitions: HashMap::new(),
            animations: AnimationManager::new(),
            previous_states: HashMap::new(),
        }
    }
    
    /// Register transitions for a component
    pub fn register_transitions(&mut self, component_id: ComponentId, transitions: Vec<Transition>) {
        self.transitions.insert(component_id, transitions);
    }
    
    /// Update component state and trigger transitions
    pub fn update_state(&mut self, component_id: ComponentId, new_state: ComponentState) {
        if let Some(prev_state) = self.previous_states.get(&component_id) {
            if let Some(transitions) = self.transitions.get(&component_id) {
                // Check for property changes
                for transition in transitions {
                    let prop_name = transition.property.as_string();
                    
                    // Handle "all" property
                    let properties_to_check = if transition.property == TransitionProperty::All {
                        new_state.properties.keys().cloned().collect()
                    } else {
                        vec![prop_name.clone()]
                    };
                    
                    for prop in properties_to_check {
                        if let (Some(old_value), Some(new_value)) = 
                            (prev_state.properties.get(&prop), new_state.properties.get(&prop)) {
                            
                            // Only animate if values differ
                            if !values_equal(old_value, new_value) {
                                let anim = Animation::new(
                                    format!("{}_{}", component_id, prop),
                                    old_value.clone(),
                                    new_value.clone(),
                                )
                                .duration(transition.duration)
                                .easing(transition.easing)
                                .delay(transition.delay);
                                
                                self.animations.add(anim);
                            }
                        }
                    }
                }
            }
        }
        
        // Store new state
        self.previous_states.insert(component_id, new_state);
    }
    
    /// Update all animations
    pub fn update(&mut self, delta: Duration) -> HashMap<ComponentId, HashMap<String, AnimatableValue>> {
        let values = self.animations.update(delta);
        
        // Group by component ID
        let mut component_values: HashMap<ComponentId, HashMap<String, AnimatableValue>> = HashMap::new();
        
        for (key, value) in values {
            if let Some((comp_id_str, prop_name)) = key.split_once('_') {
                if let Ok(comp_id) = comp_id_str.parse::<ComponentId>() {
                    component_values
                        .entry(comp_id)
                        .or_insert_with(HashMap::new)
                        .insert(prop_name.to_string(), value);
                }
            }
        }
        
        component_values
    }
}

/// Check if two animatable values are equal
fn values_equal(a: &AnimatableValue, b: &AnimatableValue) -> bool {
    match (a, b) {
        (AnimatableValue::Float(a), AnimatableValue::Float(b)) => (a - b).abs() < 0.001,
        (AnimatableValue::Vector2(a), AnimatableValue::Vector2(b)) => {
            (a.x - b.x).abs() < 0.001 && (a.y - b.y).abs() < 0.001
        }
        (AnimatableValue::Color(a), AnimatableValue::Color(b)) => {
            (a.r - b.r).abs() < 0.001 && 
            (a.g - b.g).abs() < 0.001 && 
            (a.b - b.b).abs() < 0.001 && 
            (a.a - b.a).abs() < 0.001
        }
        _ => false,
    }
}

/// Transition builder for easy creation
pub struct TransitionBuilder {
    transitions: Vec<Transition>,
}

impl TransitionBuilder {
    pub fn new() -> Self {
        Self {
            transitions: Vec::new(),
        }
    }
    
    /// Add a transition for all properties
    pub fn all(mut self, duration: Duration) -> Self {
        self.transitions.push(
            Transition::new(TransitionProperty::All)
                .duration(duration)
        );
        self
    }
    
    /// Add opacity transition
    pub fn opacity(mut self, duration: Duration) -> Self {
        self.transitions.push(
            Transition::new(TransitionProperty::Opacity)
                .duration(duration)
        );
        self
    }
    
    /// Add size transitions
    pub fn size(mut self, duration: Duration) -> Self {
        self.transitions.push(
            Transition::new(TransitionProperty::Width)
                .duration(duration)
        );
        self.transitions.push(
            Transition::new(TransitionProperty::Height)
                .duration(duration)
        );
        self
    }
    
    /// Add position transitions
    pub fn position(mut self, duration: Duration) -> Self {
        self.transitions.push(
            Transition::new(TransitionProperty::X)
                .duration(duration)
        );
        self.transitions.push(
            Transition::new(TransitionProperty::Y)
                .duration(duration)
        );
        self
    }
    
    /// Add transform transitions
    pub fn transform(mut self, duration: Duration) -> Self {
        self.transitions.extend(vec![
            Transition::new(TransitionProperty::Scale).duration(duration),
            Transition::new(TransitionProperty::Rotate).duration(duration),
            Transition::new(TransitionProperty::TranslateX).duration(duration),
            Transition::new(TransitionProperty::TranslateY).duration(duration),
        ]);
        self
    }
    
    /// Add color transitions
    pub fn colors(mut self, duration: Duration) -> Self {
        self.transitions.extend(vec![
            Transition::new(TransitionProperty::BackgroundColor).duration(duration),
            Transition::new(TransitionProperty::BorderColor).duration(duration),
        ]);
        self
    }
    
    /// Build the transitions
    pub fn build(self) -> Vec<Transition> {
        self.transitions
    }
}

/// Animation presets
pub mod presets {
    use super::*;
    
    /// Fade in animation
    pub fn fade_in(duration: Duration) -> Animation {
        Animation::new(
            "opacity",
            AnimatableValue::Float(0.0),
            AnimatableValue::Float(1.0),
        )
        .duration(duration)
        .easing(EasingFunction::EaseOutCubic)
    }
    
    /// Fade out animation
    pub fn fade_out(duration: Duration) -> Animation {
        Animation::new(
            "opacity",
            AnimatableValue::Float(1.0),
            AnimatableValue::Float(0.0),
        )
        .duration(duration)
        .easing(EasingFunction::EaseInCubic)
    }
    
    /// Slide in from direction
    pub fn slide_in(from: Direction, distance: f32, duration: Duration) -> Vec<Animation> {
        let (start_x, start_y) = match from {
            Direction::Left => (-distance, 0.0),
            Direction::Right => (distance, 0.0),
            Direction::Top => (0.0, -distance),
            Direction::Bottom => (0.0, distance),
        };
        
        vec![
            Animation::new(
                "translateX",
                AnimatableValue::Float(start_x),
                AnimatableValue::Float(0.0),
            )
            .duration(duration)
            .easing(EasingFunction::EaseOutCubic),
            
            Animation::new(
                "translateY",
                AnimatableValue::Float(start_y),
                AnimatableValue::Float(0.0),
            )
            .duration(duration)
            .easing(EasingFunction::EaseOutCubic),
            
            fade_in(duration),
        ]
    }
    
    /// Scale animation
    pub fn scale(from: f32, to: f32, duration: Duration) -> Animation {
        Animation::new(
            "scale",
            AnimatableValue::Float(from),
            AnimatableValue::Float(to),
        )
        .duration(duration)
        .easing(EasingFunction::EaseInOutQuad)
    }
    
    /// Bounce animation
    pub fn bounce(height: f32, duration: Duration) -> Animation {
        Animation::new(
            "translateY",
            AnimatableValue::Float(0.0),
            AnimatableValue::Float(-height),
        )
        .duration(duration)
        .easing(EasingFunction::EaseOutBounce)
        .alternate(true)
        .iterations(2)
    }
    
    /// Shake animation
    pub fn shake(intensity: f32, duration: Duration) -> Animation {
        Animation::new(
            "translateX",
            AnimatableValue::Float(-intensity),
            AnimatableValue::Float(intensity),
        )
        .duration(duration / 10)
        .easing(EasingFunction::Linear)
        .alternate(true)
        .iterations(10)
    }
    
    /// Pulse animation
    pub fn pulse(scale_factor: f32, duration: Duration) -> Animation {
        Animation::new(
            "scale",
            AnimatableValue::Float(1.0),
            AnimatableValue::Float(scale_factor),
        )
        .duration(duration)
        .easing(EasingFunction::EaseInOutSine)
        .alternate(true)
        .iterations(0) // Infinite
    }
    
    #[derive(Debug, Clone, Copy)]
    pub enum Direction {
        Left,
        Right,
        Top,
        Bottom,
    }
}