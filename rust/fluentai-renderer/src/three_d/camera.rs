//! 3D camera system for perspective and orthographic projection

use glam::{Mat4, Vec3};
use serde::{Deserialize, Serialize};

/// Type of camera projection
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CameraType {
    /// Perspective projection (3D depth)
    Perspective {
        /// Field of view in radians
        fov: f32,
        /// Near clipping plane
        near: f32,
        /// Far clipping plane
        far: f32,
    },
    /// Orthographic projection (no perspective)
    Orthographic {
        /// Left boundary
        left: f32,
        /// Right boundary
        right: f32,
        /// Bottom boundary
        bottom: f32,
        /// Top boundary
        top: f32,
        /// Near clipping plane
        near: f32,
        /// Far clipping plane
        far: f32,
    },
}

/// 3D camera for viewing the scene
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Camera3D {
    /// Camera position in world space
    pub position: Vec3,
    /// Target position the camera is looking at
    pub target: Vec3,
    /// Up vector (usually Y-axis)
    pub up: Vec3,
    /// Type of projection
    pub projection: CameraType,
    /// Aspect ratio (width / height)
    pub aspect_ratio: f32,
}

impl Camera3D {
    /// Create a new perspective camera
    pub fn new_perspective(position: Vec3, target: Vec3, fov: f32, aspect_ratio: f32) -> Self {
        Self {
            position,
            target,
            up: Vec3::Y,
            projection: CameraType::Perspective {
                fov,
                near: 0.1,
                far: 1000.0,
            },
            aspect_ratio,
        }
    }
    
    /// Create a new orthographic camera
    pub fn new_orthographic(position: Vec3, target: Vec3, size: f32, aspect_ratio: f32) -> Self {
        let half_width = size * aspect_ratio * 0.5;
        let half_height = size * 0.5;
        
        Self {
            position,
            target,
            up: Vec3::Y,
            projection: CameraType::Orthographic {
                left: -half_width,
                right: half_width,
                bottom: -half_height,
                top: half_height,
                near: -100.0,
                far: 100.0,
            },
            aspect_ratio,
        }
    }
    
    /// Get the view matrix (world to camera space)
    pub fn view_matrix(&self) -> Mat4 {
        Mat4::look_at_rh(self.position, self.target, self.up)
    }
    
    /// Get the projection matrix
    pub fn projection_matrix(&self) -> Mat4 {
        match self.projection {
            CameraType::Perspective { fov, near, far } => {
                Mat4::perspective_rh(fov, self.aspect_ratio, near, far)
            }
            CameraType::Orthographic { left, right, bottom, top, near, far } => {
                Mat4::orthographic_rh(left, right, bottom, top, near, far)
            }
        }
    }
    
    /// Get the combined view-projection matrix
    pub fn view_projection_matrix(&self) -> Mat4 {
        self.projection_matrix() * self.view_matrix()
    }
    
    /// Update aspect ratio (e.g., when window resizes)
    pub fn set_aspect_ratio(&mut self, aspect_ratio: f32) {
        self.aspect_ratio = aspect_ratio;
    }
    
    /// Orbit around the target
    pub fn orbit(&mut self, yaw: f32, pitch: f32) {
        let distance = (self.position - self.target).length();
        
        // Calculate new position
        let rotation_yaw = Mat4::from_rotation_y(yaw);
        let rotation_pitch = Mat4::from_rotation_x(pitch);
        let rotation = rotation_yaw * rotation_pitch;
        
        let offset = self.position - self.target;
        let new_offset = rotation.transform_vector3(offset);
        self.position = self.target + new_offset.normalize() * distance;
    }
    
    /// Zoom in/out (move closer/farther from target)
    pub fn zoom(&mut self, delta: f32) {
        let direction = (self.target - self.position).normalize();
        self.position += direction * delta;
    }
}