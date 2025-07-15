//! Living Cards AR Dashboard Implementation

use crate::three_d::{Mesh3D, Material3D, Node3D};
use crate::reactive::{ReactiveEngine, StateField};
use crate::primitives::{Color, Position2D, Size2D};
use glam::{Vec3, Vec2, Mat4};
// use rapier2d::prelude::*; // TODO: Make physics WASM-compatible
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

// Mock physics for WASM
#[cfg(target_arch = "wasm32")]
struct PhysicsLayout;

#[cfg(target_arch = "wasm32")]
impl PhysicsLayout {
    fn new() -> Self { Self }
    fn add_element(&mut self, _id: String, _pos: Position2D, _size: Size2D, _is_static: bool) {}
    fn start_drag(&mut self, _id: &str, _pos: Position2D) {}
    fn update_drag(&mut self, _id: &str, _pos: Position2D) {}
    fn end_drag(&mut self, _id: &str) {}
    fn apply_force(&mut self, _id: &str, _force: nalgebra::Vector2<f32>) {}
    fn step(&mut self) {}
    fn get_all_positions(&self) -> HashMap<String, Position2D> { HashMap::new() }
}
use std::rc::Rc;
use serde::{Serialize, Deserialize};

/// Status of a task card
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CardStatus {
    /// To-do column
    Todo,
    /// In progress column
    InProgress,
    /// Done column
    Done,
}

impl CardStatus {
    /// Get the color for this status
    pub fn color(&self) -> Color {
        match self {
            CardStatus::Todo => Color::new(0.8, 0.3, 0.3, 0.9),       // Red
            CardStatus::InProgress => Color::new(0.8, 0.6, 0.2, 0.9), // Orange
            CardStatus::Done => Color::new(0.3, 0.8, 0.3, 0.9),       // Green
        }
    }
    
    /// Get the column position for this status
    pub fn column_position(&self) -> f32 {
        match self {
            CardStatus::Todo => -1.0,
            CardStatus::InProgress => 0.0,
            CardStatus::Done => 1.0,
        }
    }
}

/// A living card in the AR space
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LivingCard {
    /// Unique identifier
    pub id: String,
    /// Task title
    pub title: String,
    /// Task description
    pub description: String,
    /// Current status
    pub status: CardStatus,
    /// Priority (1-5)
    pub priority: u8,
    /// Position in 3D space
    pub position: Vec3,
    /// Velocity for physics
    pub velocity: Vec3,
    /// Size of the card
    pub size: Vec2,
    /// Is being dragged
    pub is_dragging: bool,
    /// Last interaction time
    pub last_interaction: f64,
}

impl LivingCard {
    /// Create a new card
    pub fn new(id: String, title: String, description: String, status: CardStatus) -> Self {
        Self {
            id,
            title,
            description,
            status,
            priority: 3,
            position: Vec3::new(status.column_position(), 0.0, 0.0),
            velocity: Vec3::ZERO,
            size: Vec2::new(0.3, 0.2), // 30cm x 20cm in AR space
            is_dragging: false,
            last_interaction: 0.0,
        }
    }
    
    /// Update card color based on status
    pub fn update_material(&self, material: &mut Material3D) {
        if let crate::three_d::material::MaterialType::PBR { ref mut base_color, .. } = material.material_type {
            *base_color = self.status.color();
        }
    }
    
    /// Apply a force to the card (for flicking)
    pub fn apply_impulse(&mut self, force: Vec3) {
        self.velocity += force;
    }
    
    /// Check if card is in a column zone
    pub fn check_column_snap(&mut self) -> bool {
        let threshold = 0.3; // 30cm from column center
        
        for status in [CardStatus::Todo, CardStatus::InProgress, CardStatus::Done] {
            let column_x = status.column_position();
            if (self.position.x - column_x).abs() < threshold {
                // Snap to column
                self.status = status;
                self.position.x = column_x;
                self.velocity.x *= 0.1; // Dampen horizontal velocity
                return true;
            }
        }
        false
    }
}

/// A Kanban column in AR space
#[derive(Debug, Clone)]
pub struct CardColumn {
    /// Column status
    pub status: CardStatus,
    /// Column position in AR
    pub position: Vec3,
    /// Column bounds
    pub width: f32,
    pub height: f32,
    /// Cards in this column
    pub cards: Vec<String>,
}

impl CardColumn {
    /// Create a new column
    pub fn new(status: CardStatus, position: Vec3) -> Self {
        Self {
            status,
            position,
            width: 0.4,  // 40cm wide
            height: 1.0, // 1m tall
            cards: Vec::new(),
        }
    }
    
    /// Check if a position is within column bounds
    pub fn contains(&self, pos: Vec3) -> bool {
        (pos.x - self.position.x).abs() < self.width / 2.0 &&
        (pos.y - self.position.y).abs() < self.height / 2.0
    }
}

/// The complete Living Cards system
pub struct LivingCardsSystem {
    /// All cards
    cards: HashMap<String, LivingCard>,
    /// Kanban columns
    pub columns: Vec<CardColumn>,
    /// Physics simulation
    physics: Arc<Mutex<PhysicsLayout>>,
    /// Reactive state
    reactive: Arc<Mutex<ReactiveEngine>>,
    /// Debug visualization enabled
    debug_physics: bool,
    /// Time accumulator for physics
    physics_accumulator: f32,
    /// Fixed physics timestep
    physics_timestep: f32,
}

impl LivingCardsSystem {
    /// Create a new Living Cards system
    pub fn new() -> Self {
        let physics = Arc::new(Mutex::new(PhysicsLayout::new()));
        let reactive = Arc::new(Mutex::new(ReactiveEngine::new()));
        
        // Create columns
        let columns = vec![
            CardColumn::new(CardStatus::Todo, Vec3::new(-1.0, 0.0, -1.0)),
            CardColumn::new(CardStatus::InProgress, Vec3::new(0.0, 0.0, -1.0)),
            CardColumn::new(CardStatus::Done, Vec3::new(1.0, 0.0, -1.0)),
        ];
        
        Self {
            cards: HashMap::new(),
            columns,
            physics,
            reactive,
            debug_physics: false,
            physics_accumulator: 0.0,
            physics_timestep: 1.0 / 60.0, // 60 Hz physics
        }
    }
    
    /// Add a new card
    pub fn add_card(&mut self, card: LivingCard) {
        // Add to physics
        let mut physics = self.physics.lock().unwrap();
        physics.add_element(
            card.id.clone(),
            Position2D::new(card.position.x, card.position.y),
            Size2D::new(card.size.x, card.size.y),
            false, // Dynamic body
        );
        
        // Add to reactive state
        let mut reactive = self.reactive.lock().unwrap();
        reactive.register_state_field(
            format!("card_{}_status", card.id),
            Some("string".to_string()),
            Some(serde_json::json!(card.status)),
        );
        
        // Store card
        self.cards.insert(card.id.clone(), card);
    }
    
    /// Start dragging a card
    pub fn start_drag(&mut self, card_id: &str, world_pos: Vec3) {
        if let Some(card) = self.cards.get_mut(card_id) {
            card.is_dragging = true;
            card.velocity = Vec3::ZERO;
            
            // Tell physics to start dragging
            let mut physics = self.physics.lock().unwrap();
            physics.start_drag(card_id, Position2D::new(world_pos.x, world_pos.y));
        }
    }
    
    /// Update drag position
    pub fn update_drag(&mut self, card_id: &str, world_pos: Vec3) {
        if let Some(card) = self.cards.get_mut(card_id) {
            if card.is_dragging {
                // Calculate velocity from movement
                let delta = world_pos - card.position;
                card.velocity = delta * 10.0; // Scale for momentum
                card.position = world_pos;
                
                // Update physics
                let mut physics = self.physics.lock().unwrap();
                physics.update_drag(card_id, Position2D::new(world_pos.x, world_pos.y));
            }
        }
    }
    
    /// End dragging (with optional flick)
    pub fn end_drag(&mut self, card_id: &str, flick_velocity: Option<Vec3>) {
        if let Some(card) = self.cards.get_mut(card_id) {
            card.is_dragging = false;
            
            // Apply flick velocity if provided
            if let Some(velocity) = flick_velocity {
                card.apply_impulse(velocity * 0.5); // Scale down for realism
            }
            
            // End physics drag
            let mut physics = self.physics.lock().unwrap();
            physics.end_drag(card_id);
            
            // Apply the velocity as a force
            if let Some(velocity) = flick_velocity {
                physics.apply_force(card_id, nalgebra::Vector2::new(velocity.x, velocity.y));
            }
        }
    }
    
    /// Update the system
    pub fn update(&mut self, delta_time: f32) {
        // Accumulate time
        self.physics_accumulator += delta_time;
        
        // Fixed timestep physics
        while self.physics_accumulator >= self.physics_timestep {
            // Step physics
            {
                let mut physics = self.physics.lock().unwrap();
                physics.step();
            }
            
            // Update card positions from physics
            self.update_card_positions();
            
            // Check column snapping
            for (_, card) in self.cards.iter_mut() {
                if !card.is_dragging {
                    card.check_column_snap();
                }
            }
            
            self.physics_accumulator -= self.physics_timestep;
        }
        
        // Update reactive state
        self.update_reactive_state();
    }
    
    /// Update card positions from physics
    fn update_card_positions(&mut self) {
        let physics = self.physics.lock().unwrap();
        let positions = physics.get_all_positions();
        
        for (id, pos) in positions {
            if let Some(card) = self.cards.get_mut(&id) {
                if !card.is_dragging {
                    card.position.x = pos.x;
                    card.position.y = pos.y;
                }
            }
        }
    }
    
    /// Update reactive state fields
    fn update_reactive_state(&mut self) {
        let reactive = self.reactive.lock().unwrap();
        
        for (id, card) in &self.cards {
            if let Some(field) = reactive.get_field(&format!("card_{}_status", id)) {
                field.set(serde_json::json!(card.status));
            }
        }
    }
    
    /// Get all cards
    pub fn get_cards(&self) -> &HashMap<String, LivingCard> {
        &self.cards
    }
    
    /// Get debug visualization data
    pub fn get_debug_info(&self) -> DebugInfo {
        DebugInfo {
            physics_bodies: self.get_physics_debug(),
            collision_bounds: self.get_collision_bounds(),
            force_vectors: self.get_force_vectors(),
            column_zones: self.get_column_zones(),
        }
    }
    
    fn get_physics_debug(&self) -> Vec<PhysicsDebugBody> {
        let physics = self.physics.lock().unwrap();
        let positions = physics.get_all_positions();
        
        positions.into_iter().map(|(id, pos)| {
            PhysicsDebugBody {
                id: id.clone(),
                position: Vec3::new(pos.x, pos.y, 0.0),
                velocity: self.cards.get(&id).map(|c| c.velocity).unwrap_or(Vec3::ZERO),
            }
        }).collect()
    }
    
    fn get_collision_bounds(&self) -> Vec<CollisionBound> {
        self.cards.values().map(|card| {
            CollisionBound {
                center: card.position,
                size: Vec3::new(card.size.x, card.size.y, 0.1),
            }
        }).collect()
    }
    
    fn get_force_vectors(&self) -> Vec<ForceVector> {
        self.cards.values()
            .filter(|c| c.velocity.length() > 0.01)
            .map(|card| {
                ForceVector {
                    origin: card.position,
                    direction: card.velocity.normalize(),
                    magnitude: card.velocity.length(),
                }
            }).collect()
    }
    
    fn get_column_zones(&self) -> Vec<ColumnZone> {
        self.columns.iter().map(|col| {
            ColumnZone {
                position: col.position,
                width: col.width,
                height: col.height,
                status: col.status,
            }
        }).collect()
    }
}

/// Debug visualization data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DebugInfo {
    pub physics_bodies: Vec<PhysicsDebugBody>,
    pub collision_bounds: Vec<CollisionBound>,
    pub force_vectors: Vec<ForceVector>,
    pub column_zones: Vec<ColumnZone>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhysicsDebugBody {
    pub id: String,
    pub position: Vec3,
    pub velocity: Vec3,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollisionBound {
    pub center: Vec3,
    pub size: Vec3,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForceVector {
    pub origin: Vec3,
    pub direction: Vec3,
    pub magnitude: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ColumnZone {
    pub position: Vec3,
    pub width: f32,
    pub height: f32,
    pub status: CardStatus,
}