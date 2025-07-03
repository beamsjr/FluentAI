//! ClaudeLang Contract System
//! 
//! This crate provides formal verification capabilities through
//! preconditions, postconditions, and invariants.

pub mod contract;
pub mod errors;
pub mod evaluator;
pub mod runtime;
pub mod static_verification;
pub mod proof;
pub mod proof_generator;
pub mod vm_integration;
pub mod symbolic_execution;
pub mod inheritance;

#[cfg(feature = "static")]
pub mod z3_converter;

#[cfg(feature = "static")]
pub mod symbolic_z3;

pub use contract::{Contract, ContractCondition, ContractKind};
pub use errors::{ContractError, ContractViolation};
pub use runtime::{RuntimeVerifier, VerificationContext};
pub use vm_integration::{ContractVM, ContractRegistry};
pub use symbolic_execution::{SymbolicExecutor, SymbolicValue, SymbolicState, SymbolicVerificationResult};

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

#[cfg(test)]
mod tests;