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
pub mod vm_integration;

pub use contract::{Contract, ContractCondition, ContractKind};
pub use errors::{ContractError, ContractViolation};
pub use runtime::{RuntimeVerifier, VerificationContext};
pub use vm_integration::{ContractVM, ContractRegistry};

#[cfg(feature = "static")]
pub use static_verification::{StaticVerifier, VerificationResult};

#[cfg(feature = "proof")]
pub use proof::{ProofGenerator, ProofStrategy};

#[cfg(test)]
mod tests;