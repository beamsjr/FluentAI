// Animation and transitions demonstration

use fluentai_renderer::animation::{
    Animation, AnimatableValue, EasingFunction, AnimationManager
};
use fluentai_renderer::transitions::{
    Transition, TransitionProperty, TransitionBuilder, TransitionManager,
    ComponentState, presets
};
use fluentai_renderer::primitives::Color;
use cgmath::Vector2;
use std::time::Duration;
use std::collections::HashMap;

fn main() {
    println!("FluentAI Animation & Transitions Demo");
    println!("=====================================\n");
    
    // Demo 1: Basic animations
    demo_basic_animations();
    
    // Demo 2: Easing functions
    demo_easing_functions();
    
    // Demo 3: Component transitions
    demo_transitions();
    
    // Demo 4: Animation presets
    demo_animation_presets();
}

fn demo_basic_animations() {
    println!("Demo 1: Basic Animations");
    println!("------------------------");
    
    let mut manager = AnimationManager::new();
    
    // Create animations
    let fade_in = Animation::new(
        "opacity",
        AnimatableValue::Float(0.0),
        AnimatableValue::Float(1.0),
    )
    .duration(Duration::from_millis(500))
    .easing(EasingFunction::EaseOutQuad);
    
    let move_right = Animation::new(
        "x",
        AnimatableValue::Float(0.0),
        AnimatableValue::Float(200.0),
    )
    .duration(Duration::from_secs(1))
    .easing(EasingFunction::EaseInOutCubic)
    .delay(Duration::from_millis(200));
    
    let color_change = Animation::new(
        "color",
        AnimatableValue::Color(Color::new(1.0, 0.0, 0.0, 1.0)),
        AnimatableValue::Color(Color::new(0.0, 0.0, 1.0, 1.0)),
    )
    .duration(Duration::from_millis(800))
    .easing(EasingFunction::Linear);
    
    // Add animations
    let fade_id = manager.add(fade_in);
    let move_id = manager.add(move_right);
    let color_id = manager.add(color_change);
    
    println!("Created animations:");
    println!("- Fade in: 500ms with EaseOutQuad");
    println!("- Move right: 1s with EaseInOutCubic (200ms delay)");
    println!("- Color change: 800ms linear (red to blue)");
    
    // Start animations
    manager.start(&fade_id);
    manager.start(&move_id);
    manager.start(&color_id);
    
    // Simulate updates
    println!("\nAnimation progress:");
    for i in 0..5 {
        let values = manager.update(Duration::from_millis(250));
        
        println!("At {}ms:", i * 250);
        for (prop, value) in values {
            match value {
                AnimatableValue::Float(v) => println!("  {}: {:.2}", prop, v),
                AnimatableValue::Color(c) => {
                    println!("  {}: rgba({:.2}, {:.2}, {:.2}, {:.2})", 
                        prop, c.r, c.g, c.b, c.a);
                }
                _ => {}
            }
        }
    }
    
    println!();
}

fn demo_easing_functions() {
    println!("Demo 2: Easing Functions");
    println!("------------------------");
    
    let easings = vec![
        ("Linear", EasingFunction::Linear),
        ("EaseInQuad", EasingFunction::EaseInQuad),
        ("EaseOutQuad", EasingFunction::EaseOutQuad),
        ("EaseInOutQuad", EasingFunction::EaseInOutQuad),
        ("EaseInCubic", EasingFunction::EaseInCubic),
        ("EaseOutCubic", EasingFunction::EaseOutCubic),
        ("EaseInOutCubic", EasingFunction::EaseInOutCubic),
        ("EaseInElastic", EasingFunction::EaseInElastic),
        ("EaseOutElastic", EasingFunction::EaseOutElastic),
        ("EaseInBounce", EasingFunction::EaseInBounce),
        ("EaseOutBounce", EasingFunction::EaseOutBounce),
    ];
    
    println!("Easing function values at t=0.0, 0.25, 0.5, 0.75, 1.0:");
    println!("{:<16} {:>6} {:>6} {:>6} {:>6} {:>6}", 
        "Function", "0%", "25%", "50%", "75%", "100%");
    println!("{}", "-".repeat(50));
    
    for (name, easing) in easings {
        println!("{:<16} {:>6.3} {:>6.3} {:>6.3} {:>6.3} {:>6.3}",
            name,
            easing.ease(0.0),
            easing.ease(0.25),
            easing.ease(0.5),
            easing.ease(0.75),
            easing.ease(1.0),
        );
    }
    
    println!("\nCustom cubic-bezier(0.17, 0.67, 0.83, 0.67):");
    let custom = EasingFunction::CubicBezier(0.17, 0.67, 0.83, 0.67);
    println!("Values: {:.3}, {:.3}, {:.3}, {:.3}, {:.3}",
        custom.ease(0.0),
        custom.ease(0.25),
        custom.ease(0.5),
        custom.ease(0.75),
        custom.ease(1.0),
    );
    
    println!();
}

fn demo_transitions() {
    println!("Demo 3: Component Transitions");
    println!("-----------------------------");
    
    let mut transition_manager = TransitionManager::new();
    
    // Create transitions for a component
    let transitions = TransitionBuilder::new()
        .opacity(Duration::from_millis(300))
        .size(Duration::from_millis(400))
        .position(Duration::from_millis(500))
        .colors(Duration::from_millis(600))
        .build();
    
    let component_id = 12345;
    transition_manager.register_transitions(component_id, transitions);
    
    println!("Registered transitions for component {}:", component_id);
    println!("- Opacity: 300ms");
    println!("- Size (width/height): 400ms");
    println!("- Position (x/y): 500ms");
    println!("- Colors: 600ms");
    
    // Initial state
    let mut properties = HashMap::new();
    properties.insert("opacity".to_string(), AnimatableValue::Float(0.5));
    properties.insert("width".to_string(), AnimatableValue::Float(100.0));
    properties.insert("height".to_string(), AnimatableValue::Float(50.0));
    properties.insert("x".to_string(), AnimatableValue::Float(10.0));
    properties.insert("y".to_string(), AnimatableValue::Float(20.0));
    properties.insert("background-color".to_string(), 
        AnimatableValue::Color(Color::new(1.0, 0.0, 0.0, 1.0)));
    
    let initial_state = ComponentState {
        id: component_id,
        properties: properties.clone(),
    };
    
    transition_manager.update_state(component_id, initial_state);
    
    // Update to new state
    properties.insert("opacity".to_string(), AnimatableValue::Float(1.0));
    properties.insert("width".to_string(), AnimatableValue::Float(200.0));
    properties.insert("height".to_string(), AnimatableValue::Float(100.0));
    properties.insert("x".to_string(), AnimatableValue::Float(50.0));
    properties.insert("y".to_string(), AnimatableValue::Float(60.0));
    properties.insert("background-color".to_string(), 
        AnimatableValue::Color(Color::new(0.0, 1.0, 0.0, 1.0)));
    
    let new_state = ComponentState {
        id: component_id,
        properties,
    };
    
    println!("\nTriggering state change...");
    transition_manager.update_state(component_id, new_state);
    
    // Simulate updates
    println!("\nTransition progress:");
    for i in 0..7 {
        let component_values = transition_manager.update(Duration::from_millis(100));
        
        if let Some(values) = component_values.get(&component_id) {
            println!("At {}ms:", i * 100);
            for (prop, value) in values {
                match value {
                    AnimatableValue::Float(v) => println!("  {}: {:.2}", prop, v),
                    AnimatableValue::Color(c) => {
                        println!("  {}: rgba({:.2}, {:.2}, {:.2}, {:.2})", 
                            prop, c.r, c.g, c.b, c.a);
                    }
                    _ => {}
                }
            }
        }
    }
    
    println!();
}

fn demo_animation_presets() {
    println!("Demo 4: Animation Presets");
    println!("-------------------------");
    
    let mut manager = AnimationManager::new();
    
    // Fade in
    let fade_in = presets::fade_in(Duration::from_millis(500));
    manager.add(fade_in);
    
    // Slide in from left
    let slide_animations = presets::slide_in(
        presets::Direction::Left,
        100.0,
        Duration::from_millis(600)
    );
    for anim in slide_animations {
        manager.add(anim);
    }
    
    // Scale animation
    let scale = presets::scale(0.5, 1.5, Duration::from_millis(400));
    manager.add(scale);
    
    // Bounce animation
    let bounce = presets::bounce(50.0, Duration::from_millis(800));
    manager.add(bounce);
    
    // Shake animation
    let shake = presets::shake(5.0, Duration::from_millis(500));
    manager.add(shake);
    
    // Pulse animation
    let pulse = presets::pulse(1.2, Duration::from_millis(1000));
    manager.add(pulse);
    
    println!("Created preset animations:");
    println!("- Fade in (500ms)");
    println!("- Slide in from left (600ms)");
    println!("- Scale from 0.5 to 1.5 (400ms)");
    println!("- Bounce with 50px height (800ms)");
    println!("- Shake with 5px intensity (500ms)");
    println!("- Pulse to 1.2x scale (1000ms, infinite)");
    
    println!("\nAnimation characteristics:");
    println!("- Fade uses EaseOut for smooth appearance");
    println!("- Slide uses EaseOutCubic for deceleration");
    println!("- Bounce uses EaseOutBounce for realistic physics");
    println!("- Shake alternates direction for jitter effect");
    println!("- Pulse uses EaseInOutSine for smooth breathing");
}