//! 3D rendering support for Continuum UI
//! 
//! This module provides 3D model loading, camera systems, lighting,
//! and spatial interaction capabilities.

pub mod gltf_loader;
pub mod camera;
pub mod lighting;
pub mod mesh;
pub mod material;
pub mod scene3d;

pub use camera::{Camera3D, CameraType};
pub use lighting::{Light, LightType};
pub use mesh::{Mesh3D, Vertex3D};
pub use material::{Material3D, MaterialType};
pub use scene3d::{Scene3D, Node3D};