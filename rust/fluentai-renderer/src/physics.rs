//! Physics-based layout system for Continuum UI
//! 
//! This module implements Phase 3's reactive physics engine using Rapier2D.
//! UI elements are represented as physics bodies that respond to forces and constraints.

use rapier2d::prelude::*;
use std::collections::HashMap;
use crate::primitives::{Position2D, Size2D};
use nalgebra::Vector2;

/// Physics-based layout engine for Continuum UI
pub struct PhysicsLayout {
    /// Rapier physics world
    physics_pipeline: PhysicsPipeline,
    gravity: Vector<Real>,
    integration_parameters: IntegrationParameters,
    island_manager: IslandManager,
    broad_phase: BroadPhase,
    narrow_phase: NarrowPhase,
    impulse_joint_set: ImpulseJointSet,
    multibody_joint_set: MultibodyJointSet,
    ccd_solver: CCDSolver,
    
    /// Physics bodies
    rigid_body_set: RigidBodySet,
    collider_set: ColliderSet,
    
    /// Mapping from element IDs to physics handles
    element_to_body: HashMap<String, RigidBodyHandle>,
    body_to_element: HashMap<RigidBodyHandle, String>,
    
    /// Event handling
    event_handler: ChannelEventCollector,
}

impl PhysicsLayout {
    /// Create a new physics-based layout engine
    pub fn new() -> Self {
        let gravity = vector![0.0, 0.0]; // No gravity for UI layouts
        let integration_parameters = IntegrationParameters::default();
        let physics_pipeline = PhysicsPipeline::new();
        let island_manager = IslandManager::new();
        let broad_phase = BroadPhase::new();
        let narrow_phase = NarrowPhase::new();
        let impulse_joint_set = ImpulseJointSet::new();
        let multibody_joint_set = MultibodyJointSet::new();
        let ccd_solver = CCDSolver::new();
        let rigid_body_set = RigidBodySet::new();
        let collider_set = ColliderSet::new();
        let (collision_send, _collision_recv) = crossbeam::channel::unbounded();
        let (contact_force_send, _contact_force_recv) = crossbeam::channel::unbounded();
        let event_handler = ChannelEventCollector::new(collision_send, contact_force_send);
        
        Self {
            physics_pipeline,
            gravity,
            integration_parameters,
            island_manager,
            broad_phase,
            narrow_phase,
            impulse_joint_set,
            multibody_joint_set,
            ccd_solver,
            rigid_body_set,
            collider_set,
            element_to_body: HashMap::new(),
            body_to_element: HashMap::new(),
            event_handler,
        }
    }
    
    /// Add a UI element as a physics body
    pub fn add_element(&mut self, id: String, position: Position2D, size: Size2D, is_static: bool) {
        // Create rigid body
        let rigid_body = if is_static {
            RigidBodyBuilder::fixed()
                .translation(vector![position.x, position.y])
                .build()
        } else {
            RigidBodyBuilder::dynamic()
                .translation(vector![position.x, position.y])
                .linear_damping(5.0) // High damping for UI elements
                .angular_damping(5.0)
                .build()
        };
        
        let handle = self.rigid_body_set.insert(rigid_body);
        
        // Create collider (rectangle shape)
        let collider = ColliderBuilder::cuboid(size.width / 2.0, size.height / 2.0)
            .restitution(0.0) // No bouncing for UI
            .friction(0.7)
            .build();
        
        self.collider_set.insert_with_parent(collider, handle, &mut self.rigid_body_set);
        
        // Store mapping
        self.element_to_body.insert(id.clone(), handle);
        self.body_to_element.insert(handle, id);
    }
    
    /// Add a constraint between two elements
    pub fn add_constraint(&mut self, element1: &str, element2: &str, constraint_type: ConstraintType) {
        if let (Some(&body1), Some(&body2)) = (
            self.element_to_body.get(element1),
            self.element_to_body.get(element2)
        ) {
            match constraint_type {
                ConstraintType::Distance(distance) => {
                    let joint = FixedJointBuilder::new()
                        .local_anchor1(point![0.0, 0.0])
                        .local_anchor2(point![distance, 0.0])
                        .build();
                    self.impulse_joint_set.insert(body1, body2, joint, true);
                }
                ConstraintType::Spring { stiffness: _, damping: _, rest_length } => {
                    // Rapier doesn't have spring joints, use a fixed joint with compliance
                    let joint = FixedJointBuilder::new()
                        .local_anchor1(point![0.0, 0.0])
                        .local_anchor2(point![rest_length, 0.0])
                        .build();
                    self.impulse_joint_set.insert(body1, body2, joint, true);
                }
                ConstraintType::Alignment(axis) => {
                    // Prismatic joint for alignment along an axis
                    let joint = PrismaticJointBuilder::new(nalgebra::Unit::new_normalize(axis))
                        .local_anchor1(point![0.0, 0.0])
                        .local_anchor2(point![0.0, 0.0])
                        .build();
                    self.impulse_joint_set.insert(body1, body2, joint, true);
                }
            }
        }
    }
    
    /// Apply force to an element (for animations/interactions)
    pub fn apply_force(&mut self, element_id: &str, force: Vector2<f32>) {
        if let Some(&handle) = self.element_to_body.get(element_id) {
            if let Some(body) = self.rigid_body_set.get_mut(handle) {
                body.add_force(force, true);
            }
        }
    }
    
    /// Update physics simulation
    pub fn step(&mut self) {
        self.physics_pipeline.step(
            &self.gravity,
            &self.integration_parameters,
            &mut self.island_manager,
            &mut self.broad_phase,
            &mut self.narrow_phase,
            &mut self.rigid_body_set,
            &mut self.collider_set,
            &mut self.impulse_joint_set,
            &mut self.multibody_joint_set,
            &mut self.ccd_solver,
            None,
            &(),
            &(),
        );
    }
    
    /// Get current position of an element after physics update
    pub fn get_element_position(&self, element_id: &str) -> Option<Position2D> {
        self.element_to_body.get(element_id)
            .and_then(|&handle| self.rigid_body_set.get(handle))
            .map(|body| {
                let pos = body.translation();
                Position2D { x: pos.x, y: pos.y }
            })
    }
    
    /// Get all element positions for rendering
    pub fn get_all_positions(&self) -> HashMap<String, Position2D> {
        self.element_to_body.iter()
            .filter_map(|(id, &handle)| {
                self.rigid_body_set.get(handle).map(|body| {
                    let pos = body.translation();
                    (id.clone(), Position2D::new(pos.x, pos.y))
                })
            })
            .collect()
    }
    
    /// Handle mouse/touch dragging
    pub fn start_drag(&mut self, element_id: &str, mouse_pos: Position2D) {
        if let Some(&handle) = self.element_to_body.get(element_id) {
            if let Some(body) = self.rigid_body_set.get_mut(handle) {
                body.set_body_type(RigidBodyType::KinematicPositionBased, true);
                body.set_next_kinematic_translation(vector![mouse_pos.x, mouse_pos.y]);
            }
        }
    }
    
    /// Update drag position
    pub fn update_drag(&mut self, element_id: &str, mouse_pos: Position2D) {
        if let Some(&handle) = self.element_to_body.get(element_id) {
            if let Some(body) = self.rigid_body_set.get_mut(handle) {
                body.set_next_kinematic_translation(vector![mouse_pos.x, mouse_pos.y]);
            }
        }
    }
    
    /// End dragging
    pub fn end_drag(&mut self, element_id: &str) {
        if let Some(&handle) = self.element_to_body.get(element_id) {
            if let Some(body) = self.rigid_body_set.get_mut(handle) {
                body.set_body_type(RigidBodyType::Dynamic, true);
            }
        }
    }
}

/// Types of constraints between UI elements
pub enum ConstraintType {
    /// Fixed distance between elements
    Distance(f32),
    /// Spring connection with stiffness and damping
    Spring { stiffness: f32, damping: f32, rest_length: f32 },
    /// Alignment along an axis
    Alignment(Vector2<f32>),
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_physics_layout_creation() {
        let mut layout = PhysicsLayout::new();
        
        // Add some UI elements
        layout.add_element("header".to_string(), Position2D::new(400.0, 50.0), Size2D::new(200.0, 50.0), true);
        layout.add_element("button1".to_string(), Position2D::new(100.0, 200.0), Size2D::new(100.0, 40.0), false);
        
        // Run a physics step
        layout.step();
        
        // Check positions are maintained
        let pos = layout.get_element_position("header").unwrap();
        assert_eq!(pos.x, 400.0);
        assert_eq!(pos.y, 50.0);
    }
}