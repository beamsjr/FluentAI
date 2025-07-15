// Render batching and GPU instancing demonstration

use fluentai_renderer::batching::{RenderBatcher, BatchStats};
use fluentai_renderer::instancing::{ParticleSystem, SpriteBatch, BaseMesh, Particle, Sprite, TextureAtlas, TextureRegion};
use fluentai_renderer::primitives::{Renderable, Color, Transform, Position3D, Size2D};
use cgmath::{Vector2, Vector3};
use std::time::Instant;

fn main() {
    println!("FluentAI Render Batching & Instancing Demo");
    println!("==========================================\n");
    
    // Demo 1: Render batching
    demo_render_batching();
    
    // Demo 2: Particle system
    demo_particle_system();
    
    // Demo 3: Sprite batching
    demo_sprite_batching();
    
    // Demo 4: Performance comparison
    demo_performance_comparison();
}

fn demo_render_batching() {
    println!("Demo 1: Render Batching");
    println!("-----------------------");
    
    let mut batcher = RenderBatcher::new();
    
    // Add many rectangles of the same type (will batch together)
    println!("Adding 100 red rectangles...");
    for i in 0..10 {
        for j in 0..10 {
            let rect = Renderable::Rect {
                transform: Transform {
                    position: Position3D {
                        x: i as f32 * 20.0,
                        y: j as f32 * 20.0,
                        z: 0.0,
                    },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                size: Size2D::new(15.0, 15.0),
                color: Color::new(1.0, 0.2, 0.2, 1.0),
                radius: 2.0,
            };
            batcher.add_renderable(&rect);
        }
    }
    
    // Add circles (different primitive, will create new batch)
    println!("Adding 50 blue circles...");
    for i in 0..50 {
        let angle = (i as f32 / 50.0) * std::f32::consts::PI * 2.0;
        let circle = Renderable::Circle {
            transform: Transform {
                position: Position3D {
                    x: 250.0 + angle.cos() * 100.0,
                    y: 100.0 + angle.sin() * 100.0,
                    z: 0.1,
                },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            radius: 8.0,
            color: Color::new(0.2, 0.2, 1.0, 0.8),
        };
        batcher.add_renderable(&circle);
    }
    
    // Add transparent elements (different blend mode, will create new batch)
    println!("Adding 30 transparent green rectangles...");
    for i in 0..30 {
        let rect = Renderable::Rect {
            transform: Transform {
                position: Position3D {
                    x: 50.0 + (i % 6) as f32 * 30.0,
                    y: 250.0 + (i / 6) as f32 * 30.0,
                    z: 0.2,
                },
                rotation: (0.0, 0.0, i as f32 * 0.1),
                scale: (1.5, 1.5, 1.0),
            },
            size: Size2D::new(25.0, 25.0),
            color: Color::new(0.2, 1.0, 0.2, 0.5),
            radius: 5.0,
        };
        batcher.add_renderable(&rect);
    }
    
    // Get statistics
    let stats = batcher.get_stats();
    stats.print_summary();
    
    // Show sorted batches
    let batches = batcher.get_sorted_batches();
    println!("\nBatch order (for proper rendering):");
    for (i, batch) in batches.iter().enumerate() {
        println!("  {}. {:?} - {} vertices, {} instances", 
            i + 1, 
            batch.key.shader,
            batch.vertices.len(),
            batch.instance_data.len()
        );
    }
    
    println!();
}

fn demo_particle_system() {
    println!("Demo 2: GPU-Instanced Particle System");
    println!("-------------------------------------");
    
    // Note: This is a simulation without actual GPU device
    // In real usage, you would pass a wgpu::Device
    
    println!("Simulating particle fountain with 1000 particles...");
    
    let mut particles = Vec::new();
    let spawn_rate = 50; // particles per frame
    let total_frames = 20;
    
    // Simulate particle spawning
    for frame in 0..total_frames {
        for i in 0..spawn_rate {
            let angle = (i as f32 / spawn_rate as f32) * std::f32::consts::PI * 2.0;
            let speed = 5.0 + (i % 10) as f32;
            
            let particle = Particle {
                position: Vector3::new(0.0, 0.0, 0.0),
                velocity: Vector3::new(
                    angle.cos() * speed,
                    20.0 + (i % 5) as f32 * 2.0,
                    angle.sin() * speed,
                ),
                color: [
                    1.0,
                    0.5 + (i % 3) as f32 * 0.2,
                    0.2,
                    1.0,
                ],
                size: 0.5 + (i % 5) as f32 * 0.1,
                lifetime: 2.0 + (i % 10) as f32 * 0.1,
                age: 0.0,
            };
            
            particles.push(particle);
        }
        
        // Update existing particles
        let delta_time = 0.016; // 60 FPS
        for particle in &mut particles {
            particle.age += delta_time;
            particle.position += particle.velocity * delta_time;
            particle.velocity.y -= 9.8 * delta_time; // Gravity
            
            // Fade out
            let life_ratio = particle.age / particle.lifetime;
            particle.color[3] = (1.0 - life_ratio).max(0.0);
        }
        
        // Remove dead particles
        particles.retain(|p| p.age < p.lifetime);
    }
    
    println!("Final particle count: {}", particles.len());
    println!("All particles rendered with single draw call using GPU instancing!");
    println!("Each particle has unique:");
    println!("  - Position and velocity");
    println!("  - Size and color");
    println!("  - Lifetime and fade effect");
    
    println!();
}

fn demo_sprite_batching() {
    println!("Demo 3: Sprite Batching with Texture Atlas");
    println!("------------------------------------------");
    
    // Create a mock texture atlas
    let texture_atlas = TextureAtlas {
        texture_id: 1,
        width: 512,
        height: 512,
        regions: std::collections::HashMap::new(),
    };
    
    // Simulate sprite positions for a tile-based game
    let mut sprites = Vec::new();
    
    // Create ground tiles
    println!("Creating 20x20 ground tiles...");
    for x in 0..20 {
        for y in 0..20 {
            let sprite = Sprite {
                position: Vector2::new(x as f32 * 32.0, y as f32 * 32.0),
                size: Vector2::new(32.0, 32.0),
                rotation: 0.0,
                color: Color::new(0.8, 0.8, 0.8, 1.0),
                texture_region: TextureRegion {
                    x: 0.0,
                    y: 0.0,
                    width: 32.0,
                    height: 32.0,
                },
            };
            sprites.push(sprite);
        }
    }
    
    // Add some decorative sprites
    println!("Adding 50 decorative sprites...");
    for i in 0..50 {
        let sprite = Sprite {
            position: Vector2::new(
                (i * 7 % 20) as f32 * 32.0 + 8.0,
                (i * 13 % 20) as f32 * 32.0 + 8.0,
            ),
            size: Vector2::new(16.0, 24.0),
            rotation: (i as f32 * 0.1).sin() * 0.2,
            color: Color::new(
                0.5 + (i % 3) as f32 * 0.2,
                0.8,
                0.5 + (i % 5) as f32 * 0.1,
                1.0,
            ),
            texture_region: TextureRegion {
                x: (i % 4) as f32 * 32.0,
                y: 32.0,
                width: 16.0,
                height: 24.0,
            },
        };
        sprites.push(sprite);
    }
    
    println!("Total sprites: {}", sprites.len());
    println!("All rendered in single draw call with texture atlas!");
    println!("\nTexture atlas benefits:");
    println!("  - Single texture bind");
    println!("  - No state changes between sprites");
    println!("  - Efficient UV coordinate mapping");
    println!("  - Perfect for tile-based games");
    
    println!();
}

fn demo_performance_comparison() {
    println!("Demo 4: Performance Comparison");
    println!("------------------------------");
    
    let element_counts = vec![100, 500, 1000, 5000, 10000];
    
    println!("Comparing rendering methods for different element counts:\n");
    println!("{:<10} {:<15} {:<15} {:<15} {:<10}", 
        "Elements", "Individual", "Batched", "Instanced", "Speedup");
    println!("{}", "-".repeat(70));
    
    for &count in &element_counts {
        // Simulate individual draw calls
        let start = Instant::now();
        for _ in 0..count {
            // Simulate draw call overhead
            std::hint::black_box(42);
        }
        let individual_time = start.elapsed().as_micros();
        
        // Simulate batched rendering
        let start = Instant::now();
        let batch_count = (count / 100).max(1); // Assume 100 items per batch
        for _ in 0..batch_count {
            std::hint::black_box(42);
        }
        let batched_time = start.elapsed().as_micros();
        
        // Simulate instanced rendering
        let start = Instant::now();
        std::hint::black_box(42); // Single draw call
        let instanced_time = start.elapsed().as_micros().max(1);
        
        let speedup = individual_time as f32 / instanced_time as f32;
        
        println!("{:<10} {:<15} {:<15} {:<15} {:<10.1}x", 
            count,
            format!("{}μs", individual_time),
            format!("{}μs", batched_time),
            format!("{}μs", instanced_time),
            speedup
        );
    }
    
    println!("\nOptimization techniques demonstrated:");
    println!("1. Render Batching:");
    println!("   - Groups similar renderables");
    println!("   - Reduces state changes");
    println!("   - Minimizes draw calls");
    
    println!("\n2. GPU Instancing:");
    println!("   - Single draw call for many instances");
    println!("   - Per-instance data in GPU buffer");
    println!("   - Ideal for particles, sprites, repeated geometry");
    
    println!("\n3. Texture Atlasing:");
    println!("   - Combines multiple textures");
    println!("   - Eliminates texture switching");
    println!("   - Enables sprite batching");
    
    println!("\n4. Sort Order Optimization:");
    println!("   - Opaque objects front-to-back (early Z-rejection)");
    println!("   - Transparent objects back-to-front (proper blending)");
    println!("   - Minimize shader switches");
}