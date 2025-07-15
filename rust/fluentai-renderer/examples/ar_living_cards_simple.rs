//! Simple AR Living Cards Demo
//! 
//! A simplified version to demonstrate the AR dashboard without WebGL complications
//! 
//! This example must be compiled for wasm32 target:
//! cargo build --example ar_living_cards_simple --target wasm32-unknown-unknown

#[cfg(target_arch = "wasm32")]
use fluentai_renderer::ar::{ARSession, TouchPhase, CardStatus, LivingCard};
#[cfg(target_arch = "wasm32")]
use fluentai_renderer::ar::ar_session::{ARFrame, ViewerPose};
#[cfg(target_arch = "wasm32")]
use glam::{Vec3, Vec2, Quat, Mat4};

#[cfg(target_arch = "wasm32")]
fn main() {
    println!("=== AR Living Cards Demo ===\n");
    
    // Create AR session
    let mut ar_session = ARSession::new();
    
    // Initialize session (would be async in real app)
    println!("Initializing AR session...");
    // ar_session.initialize().await.unwrap();
    
    // Simulate some interactions
    println!("\nSimulating card interactions:");
    
    // Simulate touch on a card
    let touch_pos = Vec2::new(100.0, 100.0);
    let world_pos = Vec3::new(-0.5, 0.0, -2.0);
    
    println!("1. Touch down on card at {:?}", world_pos);
    ar_session.handle_touch(0, touch_pos, world_pos, TouchPhase::Began);
    
    // Simulate drag
    let drag_pos = Vec3::new(0.0, 0.0, -2.0);
    println!("2. Dragging card to {:?}", drag_pos);
    ar_session.handle_touch(0, touch_pos + Vec2::new(50.0, 0.0), drag_pos, TouchPhase::Moved);
    
    // Simulate flick release
    println!("3. Flicking card with velocity");
    ar_session.handle_touch(0, touch_pos + Vec2::new(100.0, 0.0), drag_pos + Vec3::new(0.5, 0.0, 0.0), TouchPhase::Ended);
    
    // Process a frame
    let frame = ARFrame {
        viewer_pose: Some(ViewerPose {
            position: Vec3::new(0.0, 1.6, 0.0),
            rotation: Quat::IDENTITY,
            view_matrix: Mat4::IDENTITY,
            projection_matrix: Mat4::IDENTITY,
        }),
        planes: Vec::new(),
        hit_tests: Vec::new(),
        timestamp: 1000.0,
    };
    
    println!("\n4. Processing AR frame at time {}", frame.timestamp);
    ar_session.process_frame(frame).unwrap();
    
    // Get debug info
    let living_cards = ar_session.get_living_cards();
    let debug_info = living_cards.get_debug_info();
    
    println!("\n=== Debug Info ===");
    println!("Physics bodies: {}", debug_info.physics_bodies.len());
    println!("Collision bounds: {}", debug_info.collision_bounds.len());
    println!("Force vectors: {}", debug_info.force_vectors.len());
    println!("Column zones: {}", debug_info.column_zones.len());
    
    // Display card states
    println!("\n=== Card States ===");
    for (id, card) in living_cards.get_cards() {
        println!("Card '{}': {:?} at position {:?}", 
            card.title, 
            card.status, 
            card.position
        );
    }
    
    println!("\n=== Spatial Anchors ===");
    for (id, anchor) in ar_session.get_anchors() {
        println!("Anchor '{}': {:?} at {:?}", 
            id, 
            anchor.anchor_type, 
            anchor.position
        );
    }
    
    println!("\nDemo complete!");
}

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    eprintln!("This example must be compiled for wasm32 target:");
    eprintln!("cargo build --example ar_living_cards_simple --target wasm32-unknown-unknown");
}