//! Spatial anchoring for AR objects

use glam::{Vec3, Quat, Mat4};
use serde::{Serialize, Deserialize};

/// Types of spatial anchors
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum AnchorType {
    /// Anchored to a detected plane
    Plane,
    /// Anchored to a specific point
    Point,
    /// Anchored relative to user
    UserRelative,
    /// Anchored to an image marker
    Image,
}

/// A spatial anchor in AR space
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpatialAnchor {
    /// Unique identifier
    pub id: String,
    /// Type of anchor
    pub anchor_type: AnchorType,
    /// Position in world space
    pub position: Vec3,
    /// Rotation
    pub rotation: Quat,
    /// Scale
    pub scale: Vec3,
    /// Is this anchor active?
    pub is_active: bool,
    /// Confidence level (0.0 - 1.0)
    pub confidence: f32,
}

impl SpatialAnchor {
    /// Create a new spatial anchor
    pub fn new(id: String, anchor_type: AnchorType, position: Vec3) -> Self {
        Self {
            id,
            anchor_type,
            position,
            rotation: Quat::IDENTITY,
            scale: Vec3::ONE,
            is_active: true,
            confidence: 1.0,
        }
    }
    
    /// Get the transformation matrix
    pub fn transform(&self) -> Mat4 {
        Mat4::from_scale_rotation_translation(self.scale, self.rotation, self.position)
    }
    
    /// Update anchor from tracking data
    pub fn update_from_tracking(&mut self, position: Vec3, rotation: Quat, confidence: f32) {
        self.position = position;
        self.rotation = rotation;
        self.confidence = confidence;
        self.is_active = confidence > 0.5;
    }
}