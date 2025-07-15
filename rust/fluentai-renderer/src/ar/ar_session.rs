//! AR session management for WebXR

use crate::ar::{SpatialAnchor, AnchorType, GestureRecognizer, Gesture};
use crate::ar::living_cards::{LivingCardsSystem, LivingCard, CardStatus};
use glam::{Vec3, Vec2, Quat, Mat4};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

/// AR session state
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SessionState {
    /// Not initialized
    Uninitialized,
    /// Requesting session
    Requesting,
    /// Session active
    Active,
    /// Session ended
    Ended,
}

/// AR frame data
#[derive(Debug, Clone)]
pub struct ARFrame {
    /// Viewer pose (camera)
    pub viewer_pose: Option<ViewerPose>,
    /// Detected planes
    pub planes: Vec<DetectedPlane>,
    /// Hit test results
    pub hit_tests: Vec<HitTestResult>,
    /// Frame timestamp
    pub timestamp: f64,
}

/// Viewer pose data
#[derive(Debug, Clone)]
pub struct ViewerPose {
    /// Position in world space
    pub position: Vec3,
    /// Rotation
    pub rotation: Quat,
    /// View matrix
    pub view_matrix: Mat4,
    /// Projection matrix
    pub projection_matrix: Mat4,
}

/// Detected plane
#[derive(Debug, Clone)]
pub struct DetectedPlane {
    /// Plane ID
    pub id: String,
    /// Center position
    pub center: Vec3,
    /// Normal vector
    pub normal: Vec3,
    /// Plane extents
    pub extents: Vec3,
}

/// Hit test result
#[derive(Debug, Clone)]
pub struct HitTestResult {
    /// Hit position
    pub position: Vec3,
    /// Hit normal
    pub normal: Vec3,
    /// Distance from ray origin
    pub distance: f32,
}

/// AR session manager
pub struct ARSession {
    /// Session state
    state: SessionState,
    /// Spatial anchors
    anchors: HashMap<String, SpatialAnchor>,
    /// Gesture recognizer
    gesture_recognizer: GestureRecognizer,
    /// Living Cards system
    living_cards: LivingCardsSystem,
    /// Session configuration
    config: SessionConfig,
}

/// Session configuration
#[derive(Debug, Clone)]
pub struct SessionConfig {
    /// Enable plane detection
    pub plane_detection: bool,
    /// Enable image tracking
    pub image_tracking: bool,
    /// Enable hand tracking
    pub hand_tracking: bool,
    /// Enable hit testing
    pub hit_testing: bool,
}

impl Default for SessionConfig {
    fn default() -> Self {
        Self {
            plane_detection: true,
            image_tracking: false,
            hand_tracking: false,
            hit_testing: true,
        }
    }
}

impl ARSession {
    /// Create a new AR session
    pub fn new() -> Self {
        Self {
            state: SessionState::Uninitialized,
            anchors: HashMap::new(),
            gesture_recognizer: GestureRecognizer::new(),
            living_cards: LivingCardsSystem::new(),
            config: SessionConfig::default(),
        }
    }
    
    /// Initialize the AR session
    pub async fn initialize(&mut self) -> Result<(), JsValue> {
        self.state = SessionState::Requesting;
        
        // In a real implementation, this would request WebXR session
        // For now, we simulate initialization
        self.state = SessionState::Active;
        
        // Create initial anchors for Kanban columns
        self.create_column_anchors();
        
        // Add some demo cards
        self.add_demo_cards();
        
        Ok(())
    }
    
    /// Create spatial anchors for Kanban columns
    fn create_column_anchors(&mut self) {
        // Todo column anchor
        let todo_anchor = SpatialAnchor::new(
            "column_todo".to_string(),
            AnchorType::Plane,
            Vec3::new(-1.0, 0.0, -2.0),
        );
        self.anchors.insert(todo_anchor.id.clone(), todo_anchor);
        
        // In Progress column anchor
        let progress_anchor = SpatialAnchor::new(
            "column_progress".to_string(),
            AnchorType::Plane,
            Vec3::new(0.0, 0.0, -2.0),
        );
        self.anchors.insert(progress_anchor.id.clone(), progress_anchor);
        
        // Done column anchor
        let done_anchor = SpatialAnchor::new(
            "column_done".to_string(),
            AnchorType::Plane,
            Vec3::new(1.0, 0.0, -2.0),
        );
        self.anchors.insert(done_anchor.id.clone(), done_anchor);
    }
    
    /// Add demo cards
    fn add_demo_cards(&mut self) {
        // Todo cards
        self.living_cards.add_card(LivingCard::new(
            "task1".to_string(),
            "Setup project".to_string(),
            "Initialize the new AR dashboard project".to_string(),
            CardStatus::Todo,
        ));
        
        self.living_cards.add_card(LivingCard::new(
            "task2".to_string(),
            "Design UI".to_string(),
            "Create wireframes for the AR interface".to_string(),
            CardStatus::Todo,
        ));
        
        // In Progress cards
        let mut in_progress = LivingCard::new(
            "task3".to_string(),
            "Implement gestures".to_string(),
            "Add pinch, drag, and flick gestures".to_string(),
            CardStatus::InProgress,
        );
        in_progress.priority = 4;
        self.living_cards.add_card(in_progress);
        
        // Done cards
        self.living_cards.add_card(LivingCard::new(
            "task4".to_string(),
            "Research WebXR".to_string(),
            "Study WebXR API documentation".to_string(),
            CardStatus::Done,
        ));
    }
    
    /// Process AR frame
    pub fn process_frame(&mut self, frame: ARFrame) -> Result<(), JsValue> {
        if self.state != SessionState::Active {
            return Ok(());
        }
        
        // Update spatial anchors from tracking
        if let Some(pose) = &frame.viewer_pose {
            self.update_anchor_tracking(&pose);
        }
        
        // Update Living Cards physics
        self.living_cards.update(frame.timestamp as f32 / 1000.0);
        
        Ok(())
    }
    
    /// Handle touch/pointer input
    pub fn handle_touch(&mut self, touch_id: u32, screen_pos: Vec2, world_pos: Vec3, phase: TouchPhase) {
        match phase {
            TouchPhase::Began => {
                self.gesture_recognizer.touch_down(touch_id, screen_pos, world_pos);
                
                // Check if we hit a card
                if let Some(card_id) = self.hit_test_cards(world_pos) {
                    self.living_cards.start_drag(&card_id, world_pos);
                }
            }
            TouchPhase::Moved => {
                if let Some(gesture) = self.gesture_recognizer.touch_move(touch_id, screen_pos, world_pos) {
                    self.handle_gesture(gesture);
                }
                
                // Update drag if active
                if let Some(card_id) = self.get_dragging_card() {
                    self.living_cards.update_drag(&card_id, world_pos);
                }
            }
            TouchPhase::Ended => {
                if let Some(gesture) = self.gesture_recognizer.touch_up(touch_id) {
                    self.handle_gesture(gesture);
                }
                
                // End drag
                if let Some(card_id) = self.get_dragging_card() {
                    // Check if this was a flick
                    let flick_velocity = match self.gesture_recognizer.touch_up(touch_id) {
                        Some(Gesture::Flick { direction, velocity }) => {
                            Some(direction * velocity)
                        }
                        _ => None,
                    };
                    
                    self.living_cards.end_drag(&card_id, flick_velocity);
                }
            }
        }
        
        // Check for pinch gesture
        if let Some(gesture) = self.gesture_recognizer.check_pinch() {
            self.handle_gesture(gesture);
        }
    }
    
    /// Handle recognized gesture
    fn handle_gesture(&mut self, gesture: Gesture) {
        match gesture {
            Gesture::Tap => {
                // Single tap - select card
                web_sys::console::log_1(&"Tap detected".into());
            }
            Gesture::DoubleTap => {
                // Double tap - open card details
                web_sys::console::log_1(&"Double tap detected".into());
            }
            Gesture::LongPress => {
                // Long press - show context menu
                web_sys::console::log_1(&"Long press detected".into());
            }
            Gesture::Pinch { scale } => {
                // Pinch - scale the dashboard
                web_sys::console::log_1(&format!("Pinch scale: {}", scale).into());
            }
            _ => {}
        }
    }
    
    /// Hit test against cards
    fn hit_test_cards(&self, world_pos: Vec3) -> Option<String> {
        // Simple sphere hit test for now
        let hit_radius = 0.15; // 15cm
        
        for (id, card) in self.living_cards.get_cards() {
            let distance = (card.position - world_pos).length();
            if distance < hit_radius {
                return Some(id.clone());
            }
        }
        
        None
    }
    
    /// Get currently dragging card
    fn get_dragging_card(&self) -> Option<String> {
        for (id, card) in self.living_cards.get_cards() {
            if card.is_dragging {
                return Some(id.clone());
            }
        }
        None
    }
    
    /// Update anchor tracking
    fn update_anchor_tracking(&mut self, viewer_pose: &ViewerPose) {
        // Update anchor confidence based on viewer distance
        for anchor in self.anchors.values_mut() {
            let distance = (anchor.position - viewer_pose.position).length();
            
            // Confidence decreases with distance
            let max_distance = 5.0; // 5 meters
            let confidence = (1.0 - (distance / max_distance).min(1.0)).max(0.0);
            
            anchor.update_from_tracking(
                anchor.position,
                anchor.rotation,
                confidence,
            );
        }
    }
    
    /// Get all spatial anchors
    pub fn get_anchors(&self) -> &HashMap<String, SpatialAnchor> {
        &self.anchors
    }
    
    /// Get the Living Cards system
    pub fn get_living_cards(&mut self) -> &mut LivingCardsSystem {
        &mut self.living_cards
    }
    
    /// Get session state
    pub fn state(&self) -> SessionState {
        self.state
    }
    
    /// End the AR session
    pub fn end_session(&mut self) {
        self.state = SessionState::Ended;
    }
}

/// Touch phase
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TouchPhase {
    Began,
    Moved,
    Ended,
}