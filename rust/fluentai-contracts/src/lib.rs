//! FluentAi Contract System
//! 
//! This crate provides formal verification capabilities through
//! preconditions, postconditions, and invariants.

pub mod contract;
pub mod errors;
pub mod evaluator;
pub mod frame_conditions;
pub mod ghost_state;
pub mod incremental;
pub mod parallel_verification;
pub mod purity;
pub mod quantifiers;
pub mod runtime;
pub mod static_verification;
pub mod termination;
pub mod proof;
pub mod proof_generator;
pub mod vm_integration;
pub mod symbolic_execution;
pub mod simplify;
pub mod test_generation;
pub mod visualization;
pub mod symbolic_verification;
pub mod counterexample;
pub mod parallel_execution;
pub mod inheritance;
pub mod debugging;
pub mod temporal;
pub mod bounded_model_checking;
pub mod state_machine;

#[cfg(feature = "static")]
pub mod z3_converter;

#[cfg(feature = "static")]
pub mod symbolic_z3;

#[cfg(feature = "static")]
pub mod incremental_solver;

pub use contract::{Contract, ContractCondition, ContractKind};
pub use errors::{ContractError, ContractViolation};
pub use purity::PurityChecker;
pub use runtime::{RuntimeVerifier, VerificationContext};
pub use vm_integration::{ContractVM, ContractRegistry};
pub use symbolic_execution::{SymbolicExecutor, SymbolicValue, SymbolicState};
pub use symbolic_verification::{SymbolicContractVerifier, SymbolicVerificationResult};
pub use quantifiers::{QuantifierBuilder, QuantifierParser, QuantifiedExpression, QuantifierDomain};
pub use incremental::{IncrementalVerifier, DependencyAnalyzer, IncrementalStats};
pub use termination::{TerminationChecker, TerminationMeasure, TerminationResult, TerminationMeasureBuilder};
pub use parallel_verification::{
    ParallelVerifier, ParallelVerificationConfig, ParallelCoordinator, ParallelVerificationStats
};
pub use ghost_state::{
    GhostStateManager, GhostVariable, OldValue, GhostFunction, HistoryVariable, GhostStateBuilder
};
pub use frame_conditions::{
    FrameConditionManager, FrameCondition, FieldAccess, IndexAccess, IndexExpr, HeapRegion,
    Modification, FrameConditionBuilder
};

#[cfg(feature = "static")]
pub use static_verification::{StaticVerifier, VerificationResult};

#[cfg(feature = "proof")]
pub use proof::{ProofGenerator, ProofStrategy};

pub use proof_generator::{
    ProofGenerator as AdvancedProofGenerator, 
    Proof, ProofStep, Justification, ProofFormula, Lemma,
    ProofStrategy as AdvancedProofStrategy
};

pub use inheritance::{
    ContractHierarchy, ContractHierarchyBuilder, ContractInheritance,
    InheritanceType, ContractInterface, RefinementRule, RefinementType,
    InheritanceVerificationResult, InheritanceViolation, ViolationType,
    CompositionType
};

pub use debugging::{
    ContractDebugger, DebugInfo, TraceStep, EvalResult,
    ContractDebugRepl,
};

pub use temporal::{
    TemporalOperator, TemporalFormula, TemporalContract,
    TemporalState, ExecutionTrace, TemporalVerifier,
    TemporalVerificationConfig, TemporalVerificationResult,
    TemporalCounterexample, TemporalContractBuilder,
    temporal_dsl,
};

pub use bounded_model_checking::{
    BoundedModelChecker, BMCResult, BMCCounterexample,
    BMCState, BMCStats, BMCEncoder,
};

pub use state_machine::{
    State, Transition, TransitionAction, StateMachine,
    FairnessConstraint, StateMachineContract, SafetyProperty,
    LivenessProperty, ReachabilityProperty, StateMachineVerifier,
    StateMachineVerificationResult, StateMachineViolation,
    StateMachineBuilder,
};

#[cfg(test)]
mod tests;