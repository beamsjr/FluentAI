"""Contract verification system for ClaudeLang"""

from .verification import (
    ContractVerifier,
    ContractViolation,
    ContractViolationType,
    ContractContext,
    ContractMonitor,
    register_contract_predicates
)

from .proof_verification import (
    ContractProofVerifier,
    ContractProof,
    VerificationStrategy,
    verify_contract_statically
)

__all__ = [
    'ContractVerifier',
    'ContractViolation',
    'ContractViolationType',
    'ContractContext',
    'ContractMonitor',
    'register_contract_predicates',
    'ContractProofVerifier',
    'ContractProof',
    'VerificationStrategy',
    'verify_contract_statically'
]