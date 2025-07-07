//! State machine contracts for FSM verification
//! 
//! This module provides support for specifying and verifying contracts
//! on finite state machines, including state invariants, transition guards,
//! and temporal properties over state sequences.

use std::collections::{HashMap, HashSet};
use std::num::NonZeroU32;
use fluentai_core::ast::NodeId;
use crate::{
    contract::{ContractCondition, ContractKind},
    temporal::TemporalFormula,
    temporal_dsl::*,
    errors::{ContractError, ContractResult},
};

/// State in a finite state machine
#[derive(Debug, Clone, PartialEq)]
pub struct State {
    /// Unique state identifier
    pub id: String,
    
    /// Human-readable name
    pub name: String,
    
    /// Whether this is an initial state
    pub is_initial: bool,
    
    /// Whether this is a final/accepting state
    pub is_final: bool,
    
    /// State invariants that must hold
    pub invariants: Vec<ContractCondition>,
    
    /// Properties of this state
    pub properties: HashMap<String, serde_json::Value>,
}

/// Transition between states
#[derive(Debug, Clone)]
pub struct Transition {
    /// Source state ID
    pub from: String,
    
    /// Target state ID
    pub to: String,
    
    /// Transition label/event
    pub event: String,
    
    /// Guard condition (must be true to take transition)
    pub guard: Option<ContractCondition>,
    
    /// Actions to perform on transition
    pub actions: Vec<TransitionAction>,
    
    /// Postconditions after transition
    pub postconditions: Vec<ContractCondition>,
}

/// Action performed during a transition
#[derive(Debug, Clone)]
pub enum TransitionAction {
    /// Assign a value to a variable
    Assign(String, serde_json::Value),
    
    /// Call a function
    Call(String, Vec<serde_json::Value>),
    
    /// Send an event
    Send(String),
    
    /// Custom action
    Custom(String, HashMap<String, serde_json::Value>),
}

/// Finite State Machine with contracts
#[derive(Debug)]
pub struct StateMachine {
    /// Machine name
    pub name: String,
    
    /// All states
    pub states: HashMap<String, State>,
    
    /// All transitions
    pub transitions: Vec<Transition>,
    
    /// Global invariants (must hold in all states)
    pub global_invariants: Vec<ContractCondition>,
    
    /// Fairness constraints
    pub fairness_constraints: Vec<FairnessConstraint>,
    
    /// Temporal properties
    pub temporal_properties: Vec<TemporalFormula>,
}

/// Fairness constraint for state machines
#[derive(Debug, Clone)]
pub enum FairnessConstraint {
    /// Weak fairness: if continuously enabled, eventually taken
    Weak(String), // transition event
    
    /// Strong fairness: if infinitely often enabled, eventually taken
    Strong(String), // transition event
    
    /// State fairness: state visited infinitely often
    State(String), // state id
}

/// Contract for state machine behavior
#[derive(Debug)]
pub struct StateMachineContract {
    /// Name of the contract
    pub name: String,
    
    /// Safety properties (bad states never reached)
    pub safety_properties: Vec<SafetyProperty>,
    
    /// Liveness properties (good states eventually reached)
    pub liveness_properties: Vec<LivenessProperty>,
    
    /// Reachability properties
    pub reachability: Vec<ReachabilityProperty>,
    
    /// Deadlock freedom
    pub deadlock_free: bool,
    
    /// Determinism requirement
    pub deterministic: bool,
}

/// Safety property for state machines
#[derive(Debug, Clone)]
pub struct SafetyProperty {
    /// Property name
    pub name: String,
    
    /// States that should never be reached
    pub forbidden_states: HashSet<String>,
    
    /// State predicates that should never hold
    pub forbidden_predicates: Vec<ContractCondition>,
    
    /// Sequences that should never occur
    pub forbidden_sequences: Vec<Vec<String>>,
}

/// Liveness property for state machines
#[derive(Debug, Clone)]
pub struct LivenessProperty {
    /// Property name
    pub name: String,
    
    /// States that must eventually be reached
    pub required_states: HashSet<String>,
    
    /// Predicates that must eventually hold
    pub required_predicates: Vec<ContractCondition>,
    
    /// Response property: if trigger then eventually response
    pub response_properties: Vec<(String, String)>, // (trigger_state, response_state)
}

/// Reachability property
#[derive(Debug, Clone)]
pub struct ReachabilityProperty {
    /// Property name
    pub name: String,
    
    /// Source states
    pub from: HashSet<String>,
    
    /// Target states
    pub to: HashSet<String>,
    
    /// Whether reachability is required or forbidden
    pub must_reach: bool,
    
    /// Maximum steps allowed
    pub within_steps: Option<usize>,
}

impl StateMachine {
    /// Create a new state machine
    pub fn new(name: String) -> Self {
        Self {
            name,
            states: HashMap::new(),
            transitions: Vec::new(),
            global_invariants: Vec::new(),
            fairness_constraints: Vec::new(),
            temporal_properties: Vec::new(),
        }
    }
    
    /// Add a state to the machine
    pub fn add_state(&mut self, state: State) {
        self.states.insert(state.id.clone(), state);
    }
    
    /// Add a transition
    pub fn add_transition(&mut self, transition: Transition) -> ContractResult<()> {
        // Verify source and target states exist
        if !self.states.contains_key(&transition.from) {
            return Err(ContractError::Other(
                format!("Source state '{}' not found", transition.from)
            ));
        }
        if !self.states.contains_key(&transition.to) {
            return Err(ContractError::Other(
                format!("Target state '{}' not found", transition.to)
            ));
        }
        
        self.transitions.push(transition);
        Ok(())
    }
    
    /// Get initial states
    pub fn initial_states(&self) -> Vec<&State> {
        self.states.values()
            .filter(|s| s.is_initial)
            .collect()
    }
    
    /// Get final states
    pub fn final_states(&self) -> Vec<&State> {
        self.states.values()
            .filter(|s| s.is_final)
            .collect()
    }
    
    /// Get transitions from a state
    pub fn transitions_from(&self, state_id: &str) -> Vec<&Transition> {
        self.transitions.iter()
            .filter(|t| t.from == state_id)
            .collect()
    }
    
    /// Check if machine is deterministic
    pub fn is_deterministic(&self) -> bool {
        // Check if any state has multiple transitions with same event
        for state_id in self.states.keys() {
            let transitions = self.transitions_from(state_id);
            let mut events = HashSet::new();
            
            for trans in transitions {
                if !events.insert(&trans.event) {
                    // Duplicate event from same state
                    return false;
                }
            }
        }
        
        true
    }
    
    /// Check for deadlock states
    pub fn find_deadlocks(&self) -> Vec<&State> {
        self.states.values()
            .filter(|state| {
                !state.is_final && self.transitions_from(&state.id).is_empty()
            })
            .collect()
    }
    
    /// Convert to temporal formula for verification
    pub fn to_temporal_formula(&self) -> TemporalFormula {
        let mut formulas = Vec::new();
        
        // Initial state constraint
        let init_states: Vec<_> = self.initial_states()
            .iter()
            .map(|s| atom(self.state_predicate(&s.id)))
            .collect();
        
        if !init_states.is_empty() {
            formulas.push(or(init_states));
        }
        
        // Transition relation
        for trans in &self.transitions {
            let trans_formula = self.transition_formula(trans);
            formulas.push(always(trans_formula));
        }
        
        // Global invariants
        for inv in &self.global_invariants {
            formulas.push(always(atom(inv.clone())));
        }
        
        // State invariants
        for state in self.states.values() {
            for inv in &state.invariants {
                let state_pred = self.state_predicate(&state.id);
                formulas.push(always(implies(
                    atom(state_pred),
                    atom(inv.clone())
                )));
            }
        }
        
        and(formulas)
    }
    
    /// Create predicate for being in a state
    fn state_predicate(&self, state_id: &str) -> ContractCondition {
        ContractCondition::new_with_message(
            NodeId(NonZeroU32::new(1).unwrap()), 
            ContractKind::Invariant,
            format!("in_state_{}", state_id)
        )
    }
    
    /// Create formula for a transition
    fn transition_formula(&self, trans: &Transition) -> TemporalFormula {
        let from_pred = self.state_predicate(&trans.from);
        let to_pred = self.state_predicate(&trans.to);
        let event_pred = ContractCondition::new_with_message(
            NodeId(NonZeroU32::new(1).unwrap()),
            ContractKind::Precondition,
            format!("event_{}", trans.event)
        );
        
        let mut ante_preds = vec![atom(from_pred), atom(event_pred)];
        
        // Add guard if present
        if let Some(guard) = &trans.guard {
            ante_preds.push(atom(guard.clone()));
        }
        
        // Create postconditions
        let mut post_preds = vec![atom(to_pred)];
        for post in &trans.postconditions {
            post_preds.push(atom(post.clone()));
        }
        
        implies(
            and(ante_preds),
            next(and(post_preds))
        )
    }
}

/// State machine contract verifier
pub struct StateMachineVerifier {
    /// Registered state machines
    machines: HashMap<String, StateMachine>,
    
    /// Registered contracts
    contracts: HashMap<String, StateMachineContract>,
}

impl StateMachineVerifier {
    pub fn new() -> Self {
        Self {
            machines: HashMap::new(),
            contracts: HashMap::new(),
        }
    }
    
    /// Register a state machine
    pub fn register_machine(&mut self, machine: StateMachine) {
        self.machines.insert(machine.name.clone(), machine);
    }
    
    /// Register a contract
    pub fn register_contract(&mut self, contract: StateMachineContract) {
        self.contracts.insert(contract.name.clone(), contract);
    }
    
    /// Verify a contract against a state machine
    pub fn verify_contract(
        &self,
        machine_name: &str,
        contract_name: &str,
    ) -> ContractResult<StateMachineVerificationResult> {
        let machine = self.machines.get(machine_name)
            .ok_or_else(|| ContractError::Other(format!("Machine '{}' not found", machine_name)))?;
        
        let contract = self.contracts.get(contract_name)
            .ok_or_else(|| ContractError::Other(format!("Contract '{}' not found", contract_name)))?;
        
        let mut violations = Vec::new();
        
        // Check safety properties
        for safety in &contract.safety_properties {
            if let Some(violation) = self.check_safety(machine, safety)? {
                violations.push(violation);
            }
        }
        
        // Check liveness properties
        for liveness in &contract.liveness_properties {
            if let Some(violation) = self.check_liveness(machine, liveness)? {
                violations.push(violation);
            }
        }
        
        // Check reachability
        for reach in &contract.reachability {
            if let Some(violation) = self.check_reachability(machine, reach)? {
                violations.push(violation);
            }
        }
        
        // Check deadlock freedom
        if contract.deadlock_free {
            let deadlocks = machine.find_deadlocks();
            if !deadlocks.is_empty() {
                violations.push(StateMachineViolation {
                    property: "deadlock_freedom".to_string(),
                    violation_type: ViolationType::DeadlockFound,
                    states: deadlocks.iter().map(|s| s.id.clone()).collect(),
                    trace: None,
                });
            }
        }
        
        // Check determinism
        if contract.deterministic && !machine.is_deterministic() {
            violations.push(StateMachineViolation {
                property: "determinism".to_string(),
                violation_type: ViolationType::NonDeterminism,
                states: vec![],
                trace: None,
            });
        }
        
        Ok(StateMachineVerificationResult {
            machine: machine_name.to_string(),
            contract: contract_name.to_string(),
            verified: violations.is_empty(),
            violations,
        })
    }
    
    /// Check safety property
    fn check_safety(
        &self,
        _machine: &StateMachine,
        _safety: &SafetyProperty,
    ) -> ContractResult<Option<StateMachineViolation>> {
        // Check if forbidden states are reachable
        // This would use model checking or graph analysis
        
        // Placeholder implementation
        Ok(None)
    }
    
    /// Check liveness property
    fn check_liveness(
        &self,
        _machine: &StateMachine,
        _liveness: &LivenessProperty,
    ) -> ContractResult<Option<StateMachineViolation>> {
        // Check if required states are eventually reached
        // This would use temporal model checking
        
        // Placeholder implementation
        Ok(None)
    }
    
    /// Check reachability property
    fn check_reachability(
        &self,
        _machine: &StateMachine,
        _reach: &ReachabilityProperty,
    ) -> ContractResult<Option<StateMachineViolation>> {
        // Use BFS/DFS to check reachability
        
        // Placeholder implementation
        Ok(None)
    }
}

/// Result of state machine verification
#[derive(Debug)]
pub struct StateMachineVerificationResult {
    /// Machine that was verified
    pub machine: String,
    
    /// Contract that was checked
    pub contract: String,
    
    /// Whether all properties hold
    pub verified: bool,
    
    /// Violations found
    pub violations: Vec<StateMachineViolation>,
}

/// Violation in state machine contract
#[derive(Debug)]
pub struct StateMachineViolation {
    /// Property that was violated
    pub property: String,
    
    /// Type of violation
    pub violation_type: ViolationType,
    
    /// States involved in violation
    pub states: Vec<String>,
    
    /// Counterexample trace
    pub trace: Option<Vec<String>>,
}

/// Types of state machine violations
#[derive(Debug)]
pub enum ViolationType {
    /// Forbidden state reached
    ForbiddenStateReached,
    
    /// Required state not reachable
    RequiredStateUnreachable,
    
    /// Deadlock found
    DeadlockFound,
    
    /// Non-determinism detected
    NonDeterminism,
    
    /// Invariant violated
    InvariantViolated,
    
    /// Temporal property failed
    TemporalPropertyFailed,
}

/// Builder for state machines
pub struct StateMachineBuilder {
    machine: StateMachine,
}

impl StateMachineBuilder {
    pub fn new(name: String) -> Self {
        Self {
            machine: StateMachine::new(name),
        }
    }
    
    /// Add a state
    pub fn state(mut self, state: State) -> Self {
        self.machine.add_state(state);
        self
    }
    
    /// Add a transition
    pub fn transition(mut self, transition: Transition) -> ContractResult<Self> {
        self.machine.add_transition(transition)?;
        Ok(self)
    }
    
    /// Add global invariant
    pub fn invariant(mut self, inv: ContractCondition) -> Self {
        self.machine.global_invariants.push(inv);
        self
    }
    
    /// Add temporal property
    pub fn temporal(mut self, prop: TemporalFormula) -> Self {
        self.machine.temporal_properties.push(prop);
        self
    }
    
    /// Build the state machine
    pub fn build(self) -> StateMachine {
        self.machine
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simple_state_machine() {
        let mut machine = StateMachine::new("traffic_light".to_string());
        
        // Add states
        machine.add_state(State {
            id: "red".to_string(),
            name: "Red Light".to_string(),
            is_initial: true,
            is_final: false,
            invariants: vec![],
            properties: HashMap::new(),
        });
        
        machine.add_state(State {
            id: "green".to_string(),
            name: "Green Light".to_string(),
            is_initial: false,
            is_final: false,
            invariants: vec![],
            properties: HashMap::new(),
        });
        
        // Add transition
        machine.add_transition(Transition {
            from: "red".to_string(),
            to: "green".to_string(),
            event: "timer".to_string(),
            guard: None,
            actions: vec![],
            postconditions: vec![],
        }).unwrap();
        
        assert_eq!(machine.initial_states().len(), 1);
        assert!(machine.is_deterministic());
        assert_eq!(machine.find_deadlocks().len(), 1); // green has no outgoing
    }
}