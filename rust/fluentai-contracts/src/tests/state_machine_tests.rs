//! Tests for state machine contracts

use crate::{
    contract::{ContractCondition, ContractKind},
    state_machine::*,
    temporal_dsl::*,
    TemporalFormula,
};
use fluentai_core::ast::NodeId;
use std::collections::{HashMap, HashSet};
use std::num::NonZeroU32;

#[test]
fn test_state_machine_creation() {
    let mut machine = StateMachine::new("vending_machine".to_string());

    // Add states
    machine.add_state(State {
        id: "idle".to_string(),
        name: "Idle".to_string(),
        is_initial: true,
        is_final: false,
        invariants: vec![ContractCondition::new(
            NodeId(NonZeroU32::new(1).unwrap()),
            ContractKind::Invariant,
        )
        .with_message("credit >= 0".to_string())],
        properties: HashMap::new(),
    });

    machine.add_state(State {
        id: "selecting".to_string(),
        name: "Selecting Product".to_string(),
        is_initial: false,
        is_final: false,
        invariants: vec![ContractCondition::new(
            NodeId(NonZeroU32::new(2).unwrap()),
            ContractKind::Invariant,
        )
        .with_message("credit > 0".to_string())],
        properties: HashMap::new(),
    });

    machine.add_state(State {
        id: "dispensing".to_string(),
        name: "Dispensing Product".to_string(),
        is_initial: false,
        is_final: false,
        invariants: vec![],
        properties: HashMap::new(),
    });

    // Add transitions
    let result = machine.add_transition(Transition {
        from: "idle".to_string(),
        to: "selecting".to_string(),
        event: "insert_coin".to_string(),
        guard: Some(
            ContractCondition::new(
                NodeId(NonZeroU32::new(3).unwrap()),
                ContractKind::Precondition,
            )
            .with_message("coin_value > 0".to_string()),
        ),
        actions: vec![TransitionAction::Assign(
            "credit".to_string(),
            serde_json::json!(1),
        )],
        postconditions: vec![ContractCondition::new(
            NodeId(NonZeroU32::new(4).unwrap()),
            ContractKind::Postcondition,
        )
        .with_message("credit == old_credit + coin_value".to_string())],
    });

    assert!(result.is_ok());

    // Test properties
    assert_eq!(machine.initial_states().len(), 1);
    assert_eq!(machine.initial_states()[0].id, "idle");
    assert!(machine.is_deterministic());
    assert_eq!(machine.find_deadlocks().len(), 2); // selecting and dispensing have no outgoing
}

#[test]
fn test_determinism_check() {
    let mut machine = StateMachine::new("non_det_machine".to_string());

    machine.add_state(State {
        id: "s1".to_string(),
        name: "State 1".to_string(),
        is_initial: true,
        is_final: false,
        invariants: vec![],
        properties: HashMap::new(),
    });

    machine.add_state(State {
        id: "s2".to_string(),
        name: "State 2".to_string(),
        is_initial: false,
        is_final: true,
        invariants: vec![],
        properties: HashMap::new(),
    });

    machine.add_state(State {
        id: "s3".to_string(),
        name: "State 3".to_string(),
        is_initial: false,
        is_final: true,
        invariants: vec![],
        properties: HashMap::new(),
    });

    // Add two transitions with same event from s1
    machine
        .add_transition(Transition {
            from: "s1".to_string(),
            to: "s2".to_string(),
            event: "event_a".to_string(),
            guard: None,
            actions: vec![],
            postconditions: vec![],
        })
        .unwrap();

    machine
        .add_transition(Transition {
            from: "s1".to_string(),
            to: "s3".to_string(),
            event: "event_a".to_string(), // Same event!
            guard: None,
            actions: vec![],
            postconditions: vec![],
        })
        .unwrap();

    // Machine should be non-deterministic
    assert!(!machine.is_deterministic());
}

#[test]
fn test_state_machine_contract() {
    let contract = StateMachineContract {
        name: "traffic_light_contract".to_string(),
        safety_properties: vec![SafetyProperty {
            name: "no_simultaneous_green".to_string(),
            forbidden_states: HashSet::new(),
            forbidden_predicates: vec![ContractCondition::new(
                NodeId(NonZeroU32::new(1).unwrap()),
                ContractKind::Invariant,
            )
            .with_message("north_green && east_green".to_string())],
            forbidden_sequences: vec![],
        }],
        liveness_properties: vec![LivenessProperty {
            name: "all_directions_get_green".to_string(),
            required_states: ["north_green", "east_green", "south_green", "west_green"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            required_predicates: vec![],
            response_properties: vec![],
        }],
        reachability: vec![],
        deadlock_free: true,
        deterministic: true,
    };

    assert_eq!(contract.safety_properties.len(), 1);
    assert_eq!(contract.liveness_properties.len(), 1);
}

#[test]
fn test_state_machine_builder() {
    let machine = StateMachineBuilder::new("built_machine".to_string())
        .state(State {
            id: "init".to_string(),
            name: "Initial".to_string(),
            is_initial: true,
            is_final: false,
            invariants: vec![],
            properties: HashMap::new(),
        })
        .state(State {
            id: "done".to_string(),
            name: "Done".to_string(),
            is_initial: false,
            is_final: true,
            invariants: vec![],
            properties: HashMap::new(),
        })
        .transition(Transition {
            from: "init".to_string(),
            to: "done".to_string(),
            event: "complete".to_string(),
            guard: None,
            actions: vec![],
            postconditions: vec![],
        })
        .unwrap()
        .invariant(ContractCondition::new(
            NodeId(NonZeroU32::new(1).unwrap()),
            ContractKind::Invariant,
        ))
        .build();

    assert_eq!(machine.states.len(), 2);
    assert_eq!(machine.transitions.len(), 1);
    assert_eq!(machine.global_invariants.len(), 1);
}

#[test]
fn test_state_machine_to_temporal() {
    let mut machine = StateMachine::new("simple_fsm".to_string());

    machine.add_state(State {
        id: "a".to_string(),
        name: "A".to_string(),
        is_initial: true,
        is_final: false,
        invariants: vec![],
        properties: HashMap::new(),
    });

    machine.add_state(State {
        id: "b".to_string(),
        name: "B".to_string(),
        is_initial: false,
        is_final: true,
        invariants: vec![],
        properties: HashMap::new(),
    });

    machine
        .add_transition(Transition {
            from: "a".to_string(),
            to: "b".to_string(),
            event: "go".to_string(),
            guard: None,
            actions: vec![],
            postconditions: vec![],
        })
        .unwrap();

    // Convert to temporal formula
    let temporal = machine.to_temporal_formula();

    // Should be a conjunction of initial state and transition constraints
    match temporal {
        TemporalFormula::And(formulas) => {
            assert!(!formulas.is_empty());
        }
        _ => panic!("Expected conjunction at top level"),
    }
}

#[test]
fn test_state_machine_verifier() {
    let mut verifier = StateMachineVerifier::new();

    // Create a simple machine
    let machine = StateMachineBuilder::new("test_machine".to_string())
        .state(State {
            id: "working".to_string(),
            name: "Working".to_string(),
            is_initial: true,
            is_final: false,
            invariants: vec![],
            properties: HashMap::new(),
        })
        .state(State {
            id: "error".to_string(),
            name: "Error".to_string(),
            is_initial: false,
            is_final: false,
            invariants: vec![],
            properties: HashMap::new(),
        })
        .build();

    // Create a contract
    let contract = StateMachineContract {
        name: "no_errors".to_string(),
        safety_properties: vec![SafetyProperty {
            name: "avoid_error_state".to_string(),
            forbidden_states: ["error".to_string()].into_iter().collect(),
            forbidden_predicates: vec![],
            forbidden_sequences: vec![],
        }],
        liveness_properties: vec![],
        reachability: vec![],
        deadlock_free: true,
        deterministic: true,
    };

    verifier.register_machine(machine);
    verifier.register_contract(contract);

    let result = verifier.verify_contract("test_machine", "no_errors");
    assert!(result.is_ok());

    // Verification result would depend on actual implementation
}

#[test]
fn test_fairness_constraints() {
    let machine = StateMachine {
        name: "fair_scheduler".to_string(),
        states: HashMap::new(),
        transitions: vec![],
        global_invariants: vec![],
        fairness_constraints: vec![
            FairnessConstraint::Weak("process1_scheduled".to_string()),
            FairnessConstraint::Strong("process2_scheduled".to_string()),
            FairnessConstraint::State("idle".to_string()),
        ],
        temporal_properties: vec![],
    };

    assert_eq!(machine.fairness_constraints.len(), 3);
}

#[test]
fn test_reachability_property() {
    let prop = ReachabilityProperty {
        name: "reach_done".to_string(),
        from: ["start".to_string()].into_iter().collect(),
        to: ["done".to_string()].into_iter().collect(),
        must_reach: true,
        within_steps: Some(10),
    };

    assert!(prop.must_reach);
    assert_eq!(prop.within_steps, Some(10));
}
