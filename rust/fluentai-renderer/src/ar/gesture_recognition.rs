//! Gesture recognition for AR interactions

use glam::{Vec3, Vec2};
use std::collections::VecDeque;
use std::time::Instant;

/// Recognized gesture types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Gesture {
    /// Single tap
    Tap,
    /// Double tap
    DoubleTap,
    /// Long press
    LongPress,
    /// Swipe gesture
    Swipe { direction: Vec2, velocity: f32 },
    /// Pinch gesture
    Pinch { scale: f32 },
    /// Drag gesture
    Drag { delta: Vec3 },
    /// Flick gesture (fast swipe)
    Flick { direction: Vec3, velocity: f32 },
}

/// Touch point data
#[derive(Debug, Clone)]
struct TouchPoint {
    id: u32,
    position: Vec2,
    world_position: Vec3,
    timestamp: Instant,
}

/// Gesture recognizer
pub struct GestureRecognizer {
    /// Active touch points
    touch_points: Vec<TouchPoint>,
    /// Touch history for gesture detection
    touch_history: VecDeque<TouchPoint>,
    /// Maximum history size
    max_history: usize,
    /// Gesture detection thresholds
    tap_threshold: f32,
    swipe_threshold: f32,
    long_press_duration: f32,
    flick_velocity_threshold: f32,
}

impl GestureRecognizer {
    /// Create a new gesture recognizer
    pub fn new() -> Self {
        Self {
            touch_points: Vec::new(),
            touch_history: VecDeque::new(),
            max_history: 20,
            tap_threshold: 0.05,        // 5cm movement threshold
            swipe_threshold: 0.1,       // 10cm for swipe
            long_press_duration: 0.5,   // 500ms
            flick_velocity_threshold: 1.0, // 1m/s
        }
    }
    
    /// Process a touch down event
    pub fn touch_down(&mut self, id: u32, screen_pos: Vec2, world_pos: Vec3) {
        let touch = TouchPoint {
            id,
            position: screen_pos,
            world_position: world_pos,
            timestamp: Instant::now(),
        };
        
        self.touch_points.push(touch.clone());
        self.add_to_history(touch);
    }
    
    /// Process a touch move event
    pub fn touch_move(&mut self, id: u32, screen_pos: Vec2, world_pos: Vec3) -> Option<Gesture> {
        if let Some(touch) = self.touch_points.iter_mut().find(|t| t.id == id) {
            let delta = world_pos - touch.world_position;
            touch.position = screen_pos;
            touch.world_position = world_pos;
            
            // Check for drag
            if delta.length() > 0.01 {
                return Some(Gesture::Drag { delta });
            }
        }
        None
    }
    
    /// Process a touch up event
    pub fn touch_up(&mut self, id: u32) -> Option<Gesture> {
        if let Some(index) = self.touch_points.iter().position(|t| t.id == id) {
            let touch = self.touch_points.remove(index);
            let duration = touch.timestamp.elapsed().as_secs_f32();
            
            // Get the starting position from history
            if let Some(start) = self.find_touch_start(id) {
                let movement = touch.world_position - start.world_position;
                let distance = movement.length();
                let velocity = distance / duration;
                
                // Check gesture types
                if distance < self.tap_threshold {
                    if duration > self.long_press_duration {
                        return Some(Gesture::LongPress);
                    } else {
                        // Check for double tap
                        if self.is_double_tap(&start) {
                            return Some(Gesture::DoubleTap);
                        } else {
                            return Some(Gesture::Tap);
                        }
                    }
                } else if velocity > self.flick_velocity_threshold {
                    // Flick gesture
                    return Some(Gesture::Flick {
                        direction: movement.normalize(),
                        velocity,
                    });
                } else if distance > self.swipe_threshold {
                    // Swipe gesture
                    let direction = Vec2::new(movement.x, movement.y).normalize();
                    return Some(Gesture::Swipe { direction, velocity });
                }
            }
        }
        None
    }
    
    /// Check for pinch gesture
    pub fn check_pinch(&self) -> Option<Gesture> {
        if self.touch_points.len() == 2 {
            let p1 = &self.touch_points[0];
            let p2 = &self.touch_points[1];
            
            // Find previous positions
            if let (Some(prev1), Some(prev2)) = (
                self.find_previous_position(p1.id),
                self.find_previous_position(p2.id),
            ) {
                let prev_distance = (prev1.world_position - prev2.world_position).length();
                let curr_distance = (p1.world_position - p2.world_position).length();
                
                if prev_distance > 0.0 {
                    let scale = curr_distance / prev_distance;
                    if (scale - 1.0).abs() > 0.05 {
                        return Some(Gesture::Pinch { scale });
                    }
                }
            }
        }
        None
    }
    
    /// Add touch point to history
    fn add_to_history(&mut self, touch: TouchPoint) {
        self.touch_history.push_back(touch);
        while self.touch_history.len() > self.max_history {
            self.touch_history.pop_front();
        }
    }
    
    /// Find the start of a touch
    fn find_touch_start(&self, id: u32) -> Option<&TouchPoint> {
        self.touch_history.iter().find(|t| t.id == id)
    }
    
    /// Find previous position
    fn find_previous_position(&self, id: u32) -> Option<&TouchPoint> {
        self.touch_history.iter().rev().find(|t| t.id == id)
    }
    
    /// Check if this is a double tap
    fn is_double_tap(&self, touch: &TouchPoint) -> bool {
        let now = touch.timestamp;
        
        // Look for a recent tap at similar position
        for historic in self.touch_history.iter().rev() {
            if historic.id != touch.id {
                let time_diff = now.duration_since(historic.timestamp).as_secs_f32();
                if time_diff < 0.3 && time_diff > 0.05 {
                    let distance = (touch.world_position - historic.world_position).length();
                    if distance < self.tap_threshold {
                        return true;
                    }
                }
            }
        }
        false
    }
}