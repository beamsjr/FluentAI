// Animation engine for FluentAI UI

use web_time::Duration;
use std::collections::HashMap;
use cgmath::{Vector2, Vector3, Vector4};

/// Animation state
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AnimationState {
    /// Not started yet
    Idle,
    /// Currently running
    Running,
    /// Paused
    Paused,
    /// Completed
    Completed,
}

/// Easing functions for smooth animations
#[derive(Debug, Clone, Copy)]
pub enum EasingFunction {
    // Linear
    Linear,
    
    // Quad
    EaseInQuad,
    EaseOutQuad,
    EaseInOutQuad,
    
    // Cubic
    EaseInCubic,
    EaseOutCubic,
    EaseInOutCubic,
    
    // Quart
    EaseInQuart,
    EaseOutQuart,
    EaseInOutQuart,
    
    // Quint
    EaseInQuint,
    EaseOutQuint,
    EaseInOutQuint,
    
    // Sine
    EaseInSine,
    EaseOutSine,
    EaseInOutSine,
    
    // Expo
    EaseInExpo,
    EaseOutExpo,
    EaseInOutExpo,
    
    // Circ
    EaseInCirc,
    EaseOutCirc,
    EaseInOutCirc,
    
    // Elastic
    EaseInElastic,
    EaseOutElastic,
    EaseInOutElastic,
    
    // Back
    EaseInBack,
    EaseOutBack,
    EaseInOutBack,
    
    // Bounce
    EaseInBounce,
    EaseOutBounce,
    EaseInOutBounce,
    
    // Custom cubic bezier
    CubicBezier(f32, f32, f32, f32),
}

impl EasingFunction {
    /// Calculate easing value for time t (0.0 to 1.0)
    pub fn ease(&self, t: f32) -> f32 {
        let t = t.clamp(0.0, 1.0);
        
        match self {
            // Linear
            EasingFunction::Linear => t,
            
            // Quad
            EasingFunction::EaseInQuad => t * t,
            EasingFunction::EaseOutQuad => t * (2.0 - t),
            EasingFunction::EaseInOutQuad => {
                if t < 0.5 {
                    2.0 * t * t
                } else {
                    -1.0 + (4.0 - 2.0 * t) * t
                }
            }
            
            // Cubic
            EasingFunction::EaseInCubic => t * t * t,
            EasingFunction::EaseOutCubic => {
                let t = t - 1.0;
                t * t * t + 1.0
            }
            EasingFunction::EaseInOutCubic => {
                if t < 0.5 {
                    4.0 * t * t * t
                } else {
                    let t = 2.0 * t - 2.0;
                    1.0 + t * t * t / 2.0
                }
            }
            
            // Quart
            EasingFunction::EaseInQuart => t * t * t * t,
            EasingFunction::EaseOutQuart => {
                let t = t - 1.0;
                1.0 - t * t * t * t
            }
            EasingFunction::EaseInOutQuart => {
                if t < 0.5 {
                    8.0 * t * t * t * t
                } else {
                    let t = t - 1.0;
                    1.0 - 8.0 * t * t * t * t
                }
            }
            
            // Quint
            EasingFunction::EaseInQuint => t * t * t * t * t,
            EasingFunction::EaseOutQuint => {
                let t = t - 1.0;
                t * t * t * t * t + 1.0
            }
            EasingFunction::EaseInOutQuint => {
                if t < 0.5 {
                    16.0 * t * t * t * t * t
                } else {
                    let t = 2.0 * t - 2.0;
                    1.0 + t * t * t * t * t / 2.0
                }
            }
            
            // Sine
            EasingFunction::EaseInSine => 1.0 - ((t * std::f32::consts::PI / 2.0).cos()),
            EasingFunction::EaseOutSine => (t * std::f32::consts::PI / 2.0).sin(),
            EasingFunction::EaseInOutSine => -(((std::f32::consts::PI * t).cos() - 1.0) / 2.0),
            
            // Expo
            EasingFunction::EaseInExpo => {
                if t == 0.0 { 0.0 } else { 2.0_f32.powf(10.0 * t - 10.0) }
            }
            EasingFunction::EaseOutExpo => {
                if t == 1.0 { 1.0 } else { 1.0 - 2.0_f32.powf(-10.0 * t) }
            }
            EasingFunction::EaseInOutExpo => {
                if t == 0.0 {
                    0.0
                } else if t == 1.0 {
                    1.0
                } else if t < 0.5 {
                    2.0_f32.powf(20.0 * t - 10.0) / 2.0
                } else {
                    (2.0 - 2.0_f32.powf(-20.0 * t + 10.0)) / 2.0
                }
            }
            
            // Circ
            EasingFunction::EaseInCirc => 1.0 - (1.0 - t * t).sqrt(),
            EasingFunction::EaseOutCirc => (1.0 - (t - 1.0) * (t - 1.0)).sqrt(),
            EasingFunction::EaseInOutCirc => {
                if t < 0.5 {
                    (1.0 - (1.0 - 4.0 * t * t).sqrt()) / 2.0
                } else {
                    ((1.0 - (-2.0 * t + 2.0).powi(2)).sqrt() + 1.0) / 2.0
                }
            }
            
            // Elastic
            EasingFunction::EaseInElastic => {
                if t == 0.0 || t == 1.0 {
                    t
                } else {
                    let c4 = (2.0 * std::f32::consts::PI) / 3.0;
                    -2.0_f32.powf(10.0 * t - 10.0) * ((t * 10.0 - 10.75) * c4).sin()
                }
            }
            EasingFunction::EaseOutElastic => {
                if t == 0.0 || t == 1.0 {
                    t
                } else {
                    let c4 = (2.0 * std::f32::consts::PI) / 3.0;
                    2.0_f32.powf(-10.0 * t) * ((t * 10.0 - 0.75) * c4).sin() + 1.0
                }
            }
            EasingFunction::EaseInOutElastic => {
                if t == 0.0 || t == 1.0 {
                    t
                } else {
                    let c5 = (2.0 * std::f32::consts::PI) / 4.5;
                    if t < 0.5 {
                        -(2.0_f32.powf(20.0 * t - 10.0) * ((20.0 * t - 11.125) * c5).sin()) / 2.0
                    } else {
                        (2.0_f32.powf(-20.0 * t + 10.0) * ((20.0 * t - 11.125) * c5).sin()) / 2.0 + 1.0
                    }
                }
            }
            
            // Back
            EasingFunction::EaseInBack => {
                let c1 = 1.70158;
                let c3 = c1 + 1.0;
                c3 * t * t * t - c1 * t * t
            }
            EasingFunction::EaseOutBack => {
                let c1 = 1.70158;
                let c3 = c1 + 1.0;
                1.0 + c3 * (t - 1.0).powi(3) + c1 * (t - 1.0).powi(2)
            }
            EasingFunction::EaseInOutBack => {
                let c1 = 1.70158;
                let c2 = c1 * 1.525;
                
                if t < 0.5 {
                    ((2.0 * t).powi(2) * ((c2 + 1.0) * 2.0 * t - c2)) / 2.0
                } else {
                    ((2.0 * t - 2.0).powi(2) * ((c2 + 1.0) * (t * 2.0 - 2.0) + c2) + 2.0) / 2.0
                }
            }
            
            // Bounce
            EasingFunction::EaseInBounce => 1.0 - EasingFunction::EaseOutBounce.ease(1.0 - t),
            EasingFunction::EaseOutBounce => {
                let n1 = 7.5625;
                let d1 = 2.75;
                
                if t < 1.0 / d1 {
                    n1 * t * t
                } else if t < 2.0 / d1 {
                    let t = t - 1.5 / d1;
                    n1 * t * t + 0.75
                } else if t < 2.5 / d1 {
                    let t = t - 2.25 / d1;
                    n1 * t * t + 0.9375
                } else {
                    let t = t - 2.625 / d1;
                    n1 * t * t + 0.984375
                }
            }
            EasingFunction::EaseInOutBounce => {
                if t < 0.5 {
                    (1.0 - EasingFunction::EaseOutBounce.ease(1.0 - 2.0 * t)) / 2.0
                } else {
                    (1.0 + EasingFunction::EaseOutBounce.ease(2.0 * t - 1.0)) / 2.0
                }
            }
            
            // Cubic Bezier
            EasingFunction::CubicBezier(x1, y1, x2, y2) => {
                // Simplified cubic bezier calculation
                // For accurate implementation, use binary search or Newton's method
                let mut t_guess = t;
                for _ in 0..4 {
                    let x = 3.0 * t_guess * (1.0 - t_guess).powi(2) * x1
                        + 3.0 * t_guess.powi(2) * (1.0 - t_guess) * x2
                        + t_guess.powi(3);
                    let dx = 3.0 * (1.0 - t_guess).powi(2) * x1
                        + 6.0 * t_guess * (1.0 - t_guess) * (x2 - x1)
                        + 3.0 * t_guess.powi(2) * (1.0 - x2);
                    t_guess = t_guess - (x - t) / dx;
                    t_guess = t_guess.clamp(0.0, 1.0);
                }
                
                3.0 * t_guess * (1.0 - t_guess).powi(2) * y1
                    + 3.0 * t_guess.powi(2) * (1.0 - t_guess) * y2
                    + t_guess.powi(3)
            }
        }
    }
}

/// Animatable property types
#[derive(Debug, Clone)]
pub enum AnimatableValue {
    Float(f32),
    Vector2(Vector2<f32>),
    Vector3(Vector3<f32>),
    Vector4(Vector4<f32>),
    Color(crate::primitives::Color),
}

impl AnimatableValue {
    /// Interpolate between two values
    pub fn lerp(&self, other: &Self, t: f32) -> Self {
        match (self, other) {
            (AnimatableValue::Float(a), AnimatableValue::Float(b)) => {
                AnimatableValue::Float(a + (b - a) * t)
            }
            (AnimatableValue::Vector2(a), AnimatableValue::Vector2(b)) => {
                AnimatableValue::Vector2(Vector2::new(
                    a.x + (b.x - a.x) * t,
                    a.y + (b.y - a.y) * t,
                ))
            }
            (AnimatableValue::Vector3(a), AnimatableValue::Vector3(b)) => {
                AnimatableValue::Vector3(Vector3::new(
                    a.x + (b.x - a.x) * t,
                    a.y + (b.y - a.y) * t,
                    a.z + (b.z - a.z) * t,
                ))
            }
            (AnimatableValue::Vector4(a), AnimatableValue::Vector4(b)) => {
                AnimatableValue::Vector4(Vector4::new(
                    a.x + (b.x - a.x) * t,
                    a.y + (b.y - a.y) * t,
                    a.z + (b.z - a.z) * t,
                    a.w + (b.w - a.w) * t,
                ))
            }
            (AnimatableValue::Color(a), AnimatableValue::Color(b)) => {
                AnimatableValue::Color(crate::primitives::Color::new(
                    a.r + (b.r - a.r) * t,
                    a.g + (b.g - a.g) * t,
                    a.b + (b.b - a.b) * t,
                    a.a + (b.a - a.a) * t,
                ))
            }
            _ => self.clone(), // Type mismatch, return original
        }
    }
}

/// A single animation track
pub struct Animation {
    /// Unique animation ID
    pub id: String,
    /// Property being animated
    pub property: String,
    /// Start value
    pub from: AnimatableValue,
    /// End value
    pub to: AnimatableValue,
    /// Duration
    pub duration: Duration,
    /// Easing function
    pub easing: EasingFunction,
    /// Delay before starting
    pub delay: Duration,
    /// Number of iterations (0 = infinite)
    pub iterations: u32,
    /// Whether to alternate direction
    pub alternate: bool,
    /// Current state
    state: AnimationState,
    /// Elapsed time
    elapsed: Duration,
    /// Current iteration
    current_iteration: u32,
    /// Current direction
    forward: bool,
    /// Completion callback
    on_complete: Option<Box<dyn Fn() + Send + Sync>>,
}

impl Animation {
    /// Create a new animation
    pub fn new(property: impl Into<String>, from: AnimatableValue, to: AnimatableValue) -> Self {
        Self {
            id: format!("anim_{}", uuid::Uuid::new_v4()),
            property: property.into(),
            from,
            to,
            duration: Duration::from_millis(300),
            easing: EasingFunction::EaseInOutQuad,
            delay: Duration::ZERO,
            iterations: 1,
            alternate: false,
            state: AnimationState::Idle,
            elapsed: Duration::ZERO,
            current_iteration: 0,
            forward: true,
            on_complete: None,
        }
    }
    
    /// Set duration
    pub fn duration(mut self, duration: Duration) -> Self {
        self.duration = duration;
        self
    }
    
    /// Set easing function
    pub fn easing(mut self, easing: EasingFunction) -> Self {
        self.easing = easing;
        self
    }
    
    /// Set delay
    pub fn delay(mut self, delay: Duration) -> Self {
        self.delay = delay;
        self
    }
    
    /// Set iterations (0 = infinite)
    pub fn iterations(mut self, iterations: u32) -> Self {
        self.iterations = iterations;
        self
    }
    
    /// Set alternating direction
    pub fn alternate(mut self, alternate: bool) -> Self {
        self.alternate = alternate;
        self
    }
    
    /// Set completion callback
    pub fn on_complete<F>(mut self, callback: F) -> Self
    where
        F: Fn() + Send + Sync + 'static,
    {
        self.on_complete = Some(Box::new(callback));
        self
    }
    
    /// Start the animation
    pub fn start(&mut self) {
        self.state = AnimationState::Running;
        self.elapsed = Duration::ZERO;
        self.current_iteration = 0;
        self.forward = true;
    }
    
    /// Pause the animation
    pub fn pause(&mut self) {
        if self.state == AnimationState::Running {
            self.state = AnimationState::Paused;
        }
    }
    
    /// Resume the animation
    pub fn resume(&mut self) {
        if self.state == AnimationState::Paused {
            self.state = AnimationState::Running;
        }
    }
    
    /// Update the animation
    pub fn update(&mut self, delta: Duration) -> AnimatableValue {
        if self.state != AnimationState::Running {
            return if self.forward { self.from.clone() } else { self.to.clone() };
        }
        
        self.elapsed += delta;
        
        // Handle delay
        if self.elapsed < self.delay {
            return self.from.clone();
        }
        
        let animation_elapsed = self.elapsed - self.delay;
        let iteration_duration = if self.alternate {
            self.duration * 2
        } else {
            self.duration
        };
        
        // Check if we've completed an iteration
        if animation_elapsed >= iteration_duration {
            self.current_iteration += 1;
            
            if self.iterations > 0 && self.current_iteration >= self.iterations {
                self.state = AnimationState::Completed;
                if let Some(callback) = &self.on_complete {
                    callback();
                }
                return self.to.clone();
            }
            
            let remainder_nanos = animation_elapsed.as_nanos() % iteration_duration.as_nanos();
            self.elapsed = self.delay + Duration::from_nanos(remainder_nanos as u64);
        }
        
        // Calculate progress within current iteration
        let progress = if self.alternate {
            let half_duration = self.duration.as_secs_f32();
            let remainder_nanos = animation_elapsed.as_nanos() % iteration_duration.as_nanos();
            let elapsed_f32 = Duration::from_nanos(remainder_nanos as u64).as_secs_f32();
            
            if elapsed_f32 < half_duration {
                elapsed_f32 / half_duration
            } else {
                1.0 - (elapsed_f32 - half_duration) / half_duration
            }
        } else {
            {
                let remainder_nanos = animation_elapsed.as_nanos() % self.duration.as_nanos();
                Duration::from_nanos(remainder_nanos as u64).as_secs_f32() / self.duration.as_secs_f32()
            }
        };
        
        // Apply easing
        let eased_progress = self.easing.ease(progress);
        
        // Interpolate value
        self.from.lerp(&self.to, eased_progress)
    }
    
    /// Get current state
    pub fn state(&self) -> AnimationState {
        self.state
    }
}

impl Clone for Animation {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            property: self.property.clone(),
            from: self.from.clone(),
            to: self.to.clone(),
            duration: self.duration,
            easing: self.easing,
            delay: self.delay,
            iterations: self.iterations,
            alternate: self.alternate,
            state: self.state,
            elapsed: self.elapsed,
            current_iteration: self.current_iteration,
            forward: self.forward,
            on_complete: None, // Cannot clone closure
        }
    }
}

/// Animation manager for coordinating multiple animations
pub struct AnimationManager {
    animations: HashMap<String, Animation>,
    groups: HashMap<String, Vec<String>>,
}

impl AnimationManager {
    pub fn new() -> Self {
        Self {
            animations: HashMap::new(),
            groups: HashMap::new(),
        }
    }
    
    /// Add an animation
    pub fn add(&mut self, animation: Animation) -> String {
        let id = animation.id.clone();
        self.animations.insert(id.clone(), animation);
        id
    }
    
    /// Remove an animation
    pub fn remove(&mut self, id: &str) -> Option<Animation> {
        self.animations.remove(id)
    }
    
    /// Start an animation
    pub fn start(&mut self, id: &str) {
        if let Some(anim) = self.animations.get_mut(id) {
            anim.start();
        }
    }
    
    /// Start all animations in a group
    pub fn start_group(&mut self, group: &str) {
        if let Some(ids) = self.groups.get(group).cloned() {
            for id in ids {
                self.start(&id);
            }
        }
    }
    
    /// Update all animations
    pub fn update(&mut self, delta: Duration) -> HashMap<String, AnimatableValue> {
        let mut values = HashMap::new();
        
        for (id, anim) in &mut self.animations {
            let value = anim.update(delta);
            values.insert(anim.property.clone(), value);
        }
        
        // Remove completed animations
        self.animations.retain(|_, anim| anim.state() != AnimationState::Completed);
        
        values
    }
    
    /// Create an animation group
    pub fn create_group(&mut self, name: impl Into<String>, animation_ids: Vec<String>) {
        self.groups.insert(name.into(), animation_ids);
    }
}

// Add uuid dependency for animation IDs
use uuid;

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_linear_easing() {
        let ease = EasingFunction::Linear;
        assert_eq!(ease.ease(0.0), 0.0);
        assert_eq!(ease.ease(0.5), 0.5);
        assert_eq!(ease.ease(1.0), 1.0);
    }
    
    #[test]
    fn test_ease_in_quad() {
        let ease = EasingFunction::EaseInQuad;
        assert_eq!(ease.ease(0.0), 0.0);
        assert_eq!(ease.ease(0.5), 0.25);
        assert_eq!(ease.ease(1.0), 1.0);
    }
    
    #[test]
    fn test_animation_progress() {
        let mut anim = Animation::new(
            "opacity",
            AnimatableValue::Float(0.0),
            AnimatableValue::Float(1.0),
        )
        .duration(Duration::from_secs(1));
        
        anim.start();
        
        // At 0.5 seconds
        if let AnimatableValue::Float(val) = anim.update(Duration::from_millis(500)) {
            assert!((val - 0.5).abs() < 0.1); // Allow for easing
        }
        
        // At 1 second
        if let AnimatableValue::Float(val) = anim.update(Duration::from_millis(500)) {
            assert!((val - 1.0).abs() < 0.01);
        }
        
        assert_eq!(anim.state(), AnimationState::Completed);
    }
}