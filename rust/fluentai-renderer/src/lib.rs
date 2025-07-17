//! FluentAI Renderer - High-performance 3D rendering engine for Continuum UI
//! 
//! This crate provides the core rendering infrastructure for the Continuum UI framework,
//! supporting both 2D and 3D content with WebGPU as the backend.

#![warn(missing_docs)]

pub mod scene;
#[cfg(not(target_arch = "wasm32"))]
pub mod renderer;
#[cfg(not(target_arch = "wasm32"))]
pub mod simple_renderer;
pub mod primitives;
pub mod pipeline;
pub mod bridge;
#[cfg(not(target_arch = "wasm32"))]
pub mod physics;
pub mod reactive;
pub mod events;
#[cfg(not(target_arch = "wasm32"))]
pub mod integration;
pub mod three_d;
#[cfg(not(target_arch = "wasm32"))]
pub mod text_renderer;
pub mod simple_text;
pub mod path;
pub mod gradient;
pub mod effects;
pub mod animation;
pub mod transitions;
#[cfg(not(target_arch = "wasm32"))]
pub mod batching;
#[cfg(not(target_arch = "wasm32"))]
pub mod instancing;
pub mod components;
pub mod reactive_v2;
// pub mod ui_builder; // TODO: Fix component dependencies for WASM build
#[cfg(not(target_arch = "wasm32"))]
pub mod platform;
#[cfg(not(target_arch = "wasm32"))]
pub mod devtools;

#[cfg(target_arch = "wasm32")]
pub mod ar;

#[cfg(target_arch = "wasm32")]
pub mod webgl_renderer;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(target_arch = "wasm32")]
pub mod demo_3d_wasm;

#[cfg(target_arch = "wasm32")]
pub mod physics_webpage_demo;

// Re-export for WASM bindings
#[cfg(target_arch = "wasm32")]
pub use demo_3d_wasm::FluentAI3DDemo;

#[cfg(target_arch = "wasm32")]
pub use physics_webpage_demo::PhysicsWebpageDemo;

#[cfg(target_arch = "wasm32")]
pub mod wasm_bindings;

// #[cfg(target_arch = "wasm32")]
// pub mod continuum_wasm; // TODO: Make integration module WASM-compatible

#[cfg(target_arch = "wasm32")]
pub mod continuum_3d_wasm;

#[cfg(target_arch = "wasm32")]
pub mod fluentai_vm_wasm;

#[cfg(target_arch = "wasm32")]
pub mod minimal_vm_wasm;

#[cfg(not(target_arch = "wasm32"))]
pub use renderer::Renderer;
#[cfg(not(target_arch = "wasm32"))]
pub use simple_renderer::SimpleRenderer;
pub use scene::{Scene, SceneNode};
pub use primitives::{Renderable, Color, Transform, Vertex};
pub use bridge::RendererBridge;
pub use three_d::{Scene3D, Camera3D, Light, Mesh3D, Material3D};

#[cfg(target_arch = "wasm32")]
pub use ar::{ARSession, LivingCard, CardStatus};

/// Initialize the renderer for WebAssembly targets
#[cfg(target_arch = "wasm32")]
pub fn init_wasm() {
    console_error_panic_hook::set_once();
    console_log::init_with_level(log::Level::Info).expect("Failed to initialize logger");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::{Color, Position2D};
    
    #[test]
    fn test_basic_color_creation() {
        let color = Color::new(1.0, 0.5, 0.25, 1.0);
        assert_eq!(color.r, 1.0);
        assert_eq!(color.g, 0.5);
        assert_eq!(color.b, 0.25);
        assert_eq!(color.a, 1.0);
    }
    
    #[test]
    fn test_position_creation() {
        let position = Position2D::new(10.0, 20.0);
        assert_eq!(position.x, 10.0);
        assert_eq!(position.y, 20.0);
    }
}