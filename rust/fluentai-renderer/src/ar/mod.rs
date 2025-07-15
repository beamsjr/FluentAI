//! AR support for Continuum UI
//! 
//! This module provides WebXR integration, spatial tracking,
//! and AR-specific features for the Living Cards demo.

pub mod spatial_anchor;
pub mod gesture_recognition;
pub mod ar_session;
pub mod living_cards;
pub mod debug_overlay;

pub use spatial_anchor::{SpatialAnchor, AnchorType};
pub use gesture_recognition::{Gesture, GestureRecognizer};
pub use ar_session::{ARSession, ARFrame, TouchPhase};
pub use living_cards::{LivingCard, CardColumn, LivingCardsSystem, CardStatus};
pub use debug_overlay::{DebugOverlay, LineMesh};