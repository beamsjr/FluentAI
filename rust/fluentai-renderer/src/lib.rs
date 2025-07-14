//! FluentAI Renderer - High-performance 3D rendering engine for Continuum UI
//! 
//! This crate provides the core rendering infrastructure for the Continuum UI framework,
//! supporting both 2D and 3D content with WebGPU as the backend.

#![warn(missing_docs)]

pub mod scene;
// pub mod renderer;  // Temporarily disabled while fixing wgpu issues
pub mod simple_renderer;
pub mod primitives;
pub mod pipeline;
pub mod bridge;
pub mod physics;
pub mod reactive;
pub mod events;
pub mod integration;
pub mod three_d;

#[cfg(target_arch = "wasm32")]
pub mod webgl_renderer;

#[cfg(target_arch = "wasm32")]
pub mod web;

#[cfg(target_arch = "wasm32")]
pub mod wasm_bindings;

#[cfg(target_arch = "wasm32")]
pub mod continuum_wasm;

#[cfg(target_arch = "wasm32")]
pub mod continuum_3d_wasm;

// pub use renderer::Renderer;
pub use simple_renderer::SimpleRenderer;
pub use scene::{Scene, SceneNode};
pub use primitives::{Renderable, Color, Transform};
pub use bridge::RendererBridge;
pub use three_d::{Scene3D, Camera3D, Light, Mesh3D, Material3D};

/// Initialize the renderer for WebAssembly targets
#[cfg(target_arch = "wasm32")]
pub fn init_wasm() {
    console_error_panic_hook::set_once();
    console_log::init_with_level(log::Level::Info).expect("Failed to initialize logger");
}