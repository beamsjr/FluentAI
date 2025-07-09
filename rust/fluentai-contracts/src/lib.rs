//! FluentAi Contract System
//!
//! This crate provides formal verification capabilities through
//! preconditions, postconditions, and invariants.

pub mod contract;
pub mod counterexample;
pub mod errors;
pub mod evaluator;
pub mod frame_conditions;
pub mod ghost_state;
pub mod incremental;
pub mod inheritance;
pub mod parallel_execution;
pub mod parallel_verification;
pub mod proof;
pub mod proof_generator;
pub mod purity;
pub mod quantifiers;
pub mod runtime;
pub mod simplify;
pub mod static_verification;
pub mod symbolic_execution;
pub mod symbolic_verification;
pub mod termination;
pub mod test_generation;
pub mod visualization;
pub mod vm_integration;
// pub mod debugging; // TODO: Fix missing types
pub mod bounded_model_checking;
pub mod state_machine;
pub mod temporal;

#[cfg(feature = "static")]
pub mod z3_converter;

#[cfg(feature = "static")]
pub mod symbolic_z3;

#[cfg(feature = "static")]
pub mod incremental_solver;

pub use contract::{Contract, ContractCondition, ContractKind};
pub use errors::{ContractError, ContractViolation};
pub use frame_conditions::{
    FieldAccess, FrameCondition, FrameConditionBuilder, FrameConditionManager, HeapRegion,
    IndexAccess, IndexExpr, Modification,
};
pub use ghost_state::{
    GhostFunction, GhostStateBuilder, GhostStateManager, GhostVariable, HistoryVariable, OldValue,
};
pub use incremental::{DependencyAnalyzer, IncrementalStats, IncrementalVerifier};
pub use parallel_verification::{
    ParallelCoordinator, ParallelVerificationConfig, ParallelVerificationStats, ParallelVerifier,
};
pub use purity::PurityChecker;
pub use quantifiers::{
    QuantifiedExpression, QuantifierBuilder, QuantifierDomain, QuantifierParser,
};
pub use runtime::{RuntimeVerifier, VerificationContext};
pub use symbolic_execution::{SymbolicExecutor, SymbolicState, SymbolicValue};
pub use symbolic_verification::{SymbolicContractVerifier, SymbolicVerificationResult};
pub use termination::{
    TerminationChecker, TerminationMeasure, TerminationMeasureBuilder, TerminationResult,
};
pub use vm_integration::{ContractRegistry, ContractVM};

#[cfg(feature = "static")]
pub use static_verification::{StaticVerifier, VerificationResult};

#[cfg(feature = "proof")]
pub use proof::{ProofGenerator, ProofStrategy};

pub use proof_generator::{
    Justification, Lemma, Proof, ProofFormula, ProofGenerator as AdvancedProofGenerator, ProofStep,
    ProofStrategy as AdvancedProofStrategy,
};

pub use inheritance::{
    CompositionType, ContractHierarchy, ContractHierarchyBuilder, ContractInheritance,
    ContractInterface, InheritanceType, InheritanceVerificationResult, InheritanceViolation,
    RefinementRule, RefinementType, ViolationType,
};

// pub use debugging::{
//     ContractDebugger, DebugInfo, TraceStep, EvalResult,
//     ContractDebugRepl,
// };

pub use temporal::{
    temporal_dsl, ExecutionTrace, TemporalContract, TemporalContractBuilder,
    TemporalCounterexample, TemporalFormula, TemporalOperator, TemporalState,
    TemporalVerificationConfig, TemporalVerificationResult, TemporalVerifier,
};

pub use bounded_model_checking::{
    BMCCounterexample, BMCEncoder, BMCResult, BMCState, BMCStats, BoundedModelChecker,
};

pub use state_machine::{
    FairnessConstraint, LivenessProperty, ReachabilityProperty, SafetyProperty, State,
    StateMachine, StateMachineBuilder, StateMachineContract, StateMachineVerificationResult,
    StateMachineVerifier, StateMachineViolation, Transition, TransitionAction,
};

#[cfg(test)]
mod tests;
