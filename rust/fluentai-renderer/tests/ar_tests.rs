/// Tests for AR (Augmented Reality) functionality
#[cfg(target_arch = "wasm32")]
mod ar_tests {
    use fluentai_renderer::ar::{
        ARSession, ARFrame, TouchPhase,
        SpatialAnchor, AnchorType,
        Gesture, GestureRecognizer,
        LivingCard, CardColumn, LivingCardsSystem, CardStatus,
        DebugOverlay, LineMesh,
    };
    use fluentai_renderer::primitives::{Color, Point2D};
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn test_ar_session_creation() {
        let session = ARSession::new();
        assert_eq!(session.frame_count(), 0);
        assert!(!session.is_tracking());
    }

    #[wasm_bindgen_test]
    fn test_ar_session_update() {
        let mut session = ARSession::new();
        
        // Simulate frame update
        session.update(16.0); // 16ms delta time
        assert_eq!(session.frame_count(), 1);
        
        session.update(16.0);
        assert_eq!(session.frame_count(), 2);
    }

    #[wasm_bindgen_test]
    fn test_spatial_anchor_creation() {
        let position = Point2D::new(100.0, 200.0);
        let anchor = SpatialAnchor::new(position, AnchorType::Fixed);
        
        assert_eq!(anchor.position().x, 100.0);
        assert_eq!(anchor.position().y, 200.0);
        assert!(matches!(anchor.anchor_type(), AnchorType::Fixed));
    }

    #[wasm_bindgen_test]
    fn test_spatial_anchor_types() {
        let anchors = vec![
            SpatialAnchor::new(Point2D::new(0.0, 0.0), AnchorType::Fixed),
            SpatialAnchor::new(Point2D::new(50.0, 50.0), AnchorType::Floating),
            SpatialAnchor::new(Point2D::new(100.0, 100.0), AnchorType::Tracked),
        ];
        
        assert!(matches!(anchors[0].anchor_type(), AnchorType::Fixed));
        assert!(matches!(anchors[1].anchor_type(), AnchorType::Floating));
        assert!(matches!(anchors[2].anchor_type(), AnchorType::Tracked));
    }

    #[wasm_bindgen_test]
    fn test_gesture_recognizer_tap() {
        let mut recognizer = GestureRecognizer::new();
        
        // Simulate tap gesture
        let touch_point = Point2D::new(100.0, 100.0);
        recognizer.touch_began(0, touch_point, 0.0);
        recognizer.touch_ended(0, touch_point, 100.0); // 100ms later
        
        let gestures = recognizer.get_recognized_gestures();
        assert_eq!(gestures.len(), 1);
        
        match &gestures[0] {
            Gesture::Tap { position, .. } => {
                assert_eq!(position.x, 100.0);
                assert_eq!(position.y, 100.0);
            }
            _ => panic!("Expected tap gesture"),
        }
    }

    #[wasm_bindgen_test]
    fn test_gesture_recognizer_double_tap() {
        let mut recognizer = GestureRecognizer::new();
        let touch_point = Point2D::new(50.0, 50.0);
        
        // First tap
        recognizer.touch_began(0, touch_point, 0.0);
        recognizer.touch_ended(0, touch_point, 50.0);
        
        // Second tap within double tap threshold
        recognizer.touch_began(0, touch_point, 100.0);
        recognizer.touch_ended(0, touch_point, 150.0);
        
        let gestures = recognizer.get_recognized_gestures();
        
        // Should recognize double tap
        let has_double_tap = gestures.iter().any(|g| matches!(g, Gesture::DoubleTap { .. }));
        assert!(has_double_tap, "Should recognize double tap");
    }

    #[wasm_bindgen_test]
    fn test_gesture_recognizer_drag() {
        let mut recognizer = GestureRecognizer::new();
        
        let start = Point2D::new(100.0, 100.0);
        let end = Point2D::new(200.0, 150.0);
        
        recognizer.touch_began(0, start, 0.0);
        recognizer.touch_moved(0, Point2D::new(150.0, 125.0), 50.0);
        recognizer.touch_moved(0, end, 100.0);
        recognizer.touch_ended(0, end, 150.0);
        
        let gestures = recognizer.get_recognized_gestures();
        
        // Should have drag gestures
        let drag_count = gestures.iter().filter(|g| matches!(g, Gesture::Drag { .. })).count();
        assert!(drag_count > 0, "Should recognize drag gestures");
    }

    #[wasm_bindgen_test]
    fn test_living_card_creation() {
        let card = LivingCard::new(
            "test-card",
            "Test Task",
            CardStatus::Todo,
            Point2D::new(100.0, 100.0),
        );
        
        assert_eq!(card.id(), "test-card");
        assert_eq!(card.title(), "Test Task");
        assert!(matches!(card.status(), CardStatus::Todo));
        assert_eq!(card.position().x, 100.0);
        assert_eq!(card.position().y, 100.0);
    }

    #[wasm_bindgen_test]
    fn test_living_card_status_transitions() {
        let mut card = LivingCard::new(
            "card-1",
            "Task",
            CardStatus::Todo,
            Point2D::new(0.0, 0.0),
        );
        
        // Test status transitions
        card.set_status(CardStatus::InProgress);
        assert!(matches!(card.status(), CardStatus::InProgress));
        
        card.set_status(CardStatus::Done);
        assert!(matches!(card.status(), CardStatus::Done));
    }

    #[wasm_bindgen_test]
    fn test_card_column_creation() {
        let column = CardColumn::new(
            "todo-column",
            "To Do",
            CardStatus::Todo,
            Point2D::new(0.0, 0.0),
            200.0,
            Color::red(),
        );
        
        assert_eq!(column.id(), "todo-column");
        assert_eq!(column.title(), "To Do");
        assert!(matches!(column.status(), CardStatus::Todo));
        assert_eq!(column.width(), 200.0);
    }

    #[wasm_bindgen_test]
    fn test_living_cards_system() {
        let mut system = LivingCardsSystem::new();
        
        // Add columns
        system.add_column(CardColumn::new(
            "todo",
            "To Do",
            CardStatus::Todo,
            Point2D::new(0.0, 0.0),
            200.0,
            Color::red(),
        ));
        
        system.add_column(CardColumn::new(
            "in-progress",
            "In Progress",
            CardStatus::InProgress,
            Point2D::new(250.0, 0.0),
            200.0,
            Color::orange(),
        ));
        
        // Add cards
        system.add_card(LivingCard::new(
            "card-1",
            "Task 1",
            CardStatus::Todo,
            Point2D::new(50.0, 100.0),
        ));
        
        system.add_card(LivingCard::new(
            "card-2",
            "Task 2",
            CardStatus::InProgress,
            Point2D::new(300.0, 100.0),
        ));
        
        assert_eq!(system.card_count(), 2);
        assert_eq!(system.column_count(), 2);
    }

    #[wasm_bindgen_test]
    fn test_living_cards_physics_update() {
        let mut system = LivingCardsSystem::new();
        
        // Add a card with velocity
        let mut card = LivingCard::new(
            "moving-card",
            "Moving Task",
            CardStatus::Todo,
            Point2D::new(100.0, 100.0),
        );
        card.set_velocity(Point2D::new(10.0, 0.0)); // Moving right
        
        system.add_card(card);
        
        let initial_pos = system.get_card("moving-card").unwrap().position();
        
        // Update physics
        system.update_physics(0.1); // 100ms
        
        let new_pos = system.get_card("moving-card").unwrap().position();
        assert!(new_pos.x > initial_pos.x, "Card should move right");
    }

    #[wasm_bindgen_test]
    fn test_card_column_assignment() {
        let mut system = LivingCardsSystem::new();
        
        // Add columns
        system.add_column(CardColumn::new(
            "todo",
            "To Do",
            CardStatus::Todo,
            Point2D::new(0.0, 0.0),
            200.0,
            Color::red(),
        ));
        
        system.add_column(CardColumn::new(
            "done",
            "Done",
            CardStatus::Done,
            Point2D::new(400.0, 0.0),
            200.0,
            Color::green(),
        ));
        
        // Add card in todo column
        let mut card = LivingCard::new(
            "card-1",
            "Task",
            CardStatus::Todo,
            Point2D::new(100.0, 100.0),
        );
        
        system.add_card(card);
        
        // Move card to done column
        system.move_card_to_column("card-1", "done");
        
        let card = system.get_card("card-1").unwrap();
        assert!(matches!(card.status(), CardStatus::Done));
    }

    #[wasm_bindgen_test]
    fn test_debug_overlay_creation() {
        let overlay = DebugOverlay::new();
        assert!(!overlay.is_enabled());
        
        let mut overlay = DebugOverlay::new();
        overlay.set_enabled(true);
        assert!(overlay.is_enabled());
    }

    #[wasm_bindgen_test]
    fn test_debug_overlay_lines() {
        let mut overlay = DebugOverlay::new();
        overlay.set_enabled(true);
        
        // Add debug line
        overlay.add_line(
            Point2D::new(0.0, 0.0),
            Point2D::new(100.0, 100.0),
            Color::red(),
        );
        
        // Add debug circle
        overlay.add_circle(
            Point2D::new(50.0, 50.0),
            25.0,
            Color::green(),
        );
        
        let meshes = overlay.get_debug_meshes();
        assert!(meshes.len() >= 2, "Should have at least line and circle meshes");
    }

    #[wasm_bindgen_test]
    fn test_ar_frame_data() {
        let frame = ARFrame {
            timestamp: 1000.0,
            camera_transform: [1.0, 0.0, 0.0, 0.0,
                               0.0, 1.0, 0.0, 0.0,
                               0.0, 0.0, 1.0, 0.0,
                               0.0, 0.0, 0.0, 1.0],
            tracking_state: "normal",
            anchors: vec![],
        };
        
        assert_eq!(frame.timestamp, 1000.0);
        assert_eq!(frame.tracking_state, "normal");
        assert_eq!(frame.anchors.len(), 0);
    }

    #[wasm_bindgen_test]
    fn test_gesture_recognizer_pinch() {
        let mut recognizer = GestureRecognizer::new();
        
        // Simulate two-finger pinch
        let finger1_start = Point2D::new(100.0, 100.0);
        let finger2_start = Point2D::new(200.0, 100.0);
        
        recognizer.touch_began(0, finger1_start, 0.0);
        recognizer.touch_began(1, finger2_start, 10.0);
        
        // Move fingers closer (pinch in)
        recognizer.touch_moved(0, Point2D::new(125.0, 100.0), 50.0);
        recognizer.touch_moved(1, Point2D::new(175.0, 100.0), 50.0);
        
        let gestures = recognizer.get_recognized_gestures();
        let has_pinch = gestures.iter().any(|g| matches!(g, Gesture::Pinch { .. }));
        assert!(has_pinch, "Should recognize pinch gesture");
    }
}

// Non-WASM tests (for code that doesn't require browser environment)
#[cfg(not(target_arch = "wasm32"))]
mod ar_unit_tests {
    use super::*;
    
    #[test]
    fn test_card_status_display() {
        assert_eq!(format!("{:?}", CardStatus::Todo), "Todo");
        assert_eq!(format!("{:?}", CardStatus::InProgress), "InProgress");
        assert_eq!(format!("{:?}", CardStatus::Done), "Done");
    }
    
    #[test]
    fn test_anchor_type_properties() {
        assert!(AnchorType::Fixed.is_movable() == false);
        assert!(AnchorType::Floating.is_movable() == true);
        assert!(AnchorType::Tracked.is_trackable() == true);
    }
}

// Mock implementations for testing
#[cfg(target_arch = "wasm32")]
impl ARSession {
    fn frame_count(&self) -> u32 {
        // Mock implementation
        0
    }
    
    fn is_tracking(&self) -> bool {
        // Mock implementation
        false
    }
}

#[cfg(target_arch = "wasm32")]
impl LivingCardsSystem {
    fn card_count(&self) -> usize {
        // Mock implementation
        0
    }
    
    fn column_count(&self) -> usize {
        // Mock implementation
        0
    }
    
    fn get_card(&self, id: &str) -> Option<&LivingCard> {
        // Mock implementation
        None
    }
    
    fn move_card_to_column(&mut self, card_id: &str, column_id: &str) {
        // Mock implementation
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl AnchorType {
    fn is_movable(&self) -> bool {
        match self {
            AnchorType::Fixed => false,
            AnchorType::Floating => true,
            AnchorType::Tracked => true,
        }
    }
    
    fn is_trackable(&self) -> bool {
        matches!(self, AnchorType::Tracked)
    }
}