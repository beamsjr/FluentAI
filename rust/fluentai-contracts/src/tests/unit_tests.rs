//! Simple unit tests for fluentai-contracts

use crate::{
    contract::{Contract, ContractCondition, ContractKind},
    errors::ContractError,
};
use fluentai_core::ast::NodeId;
use std::num::NonZeroU32;

#[cfg(test)]
mod contract_tests {
    use super::*;

    #[test]
    fn test_contract_creation() {
        let contract = Contract::new("test_func".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
        
        assert_eq!(contract.function_name, "test_func");
        assert!(contract.preconditions.is_empty());
        assert!(contract.postconditions.is_empty());
        assert!(contract.invariants.is_empty());
        assert_eq!(contract.complexity, None);
        assert!(!contract.pure);
        assert!(contract.frame_condition.is_none());
    }

    #[test]
    fn test_add_conditions() {
        let mut contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
        
        let pre = ContractCondition::new(NodeId(NonZeroU32::new(2).unwrap()), ContractKind::Precondition);
        let post = ContractCondition::new(NodeId(NonZeroU32::new(3).unwrap()), ContractKind::Postcondition);
        let inv = ContractCondition::new(NodeId(NonZeroU32::new(4).unwrap()), ContractKind::Invariant);
        
        contract.add_precondition(pre);
        contract.add_postcondition(post);
        contract.add_invariant(inv);
        
        assert_eq!(contract.preconditions.len(), 1);
        assert_eq!(contract.postconditions.len(), 1);
        assert_eq!(contract.invariants.len(), 1);
        assert!(contract.has_conditions());
    }

    #[test]
    fn test_contract_condition_builder() {
        let condition = ContractCondition::new(
            NodeId(NonZeroU32::new(1).unwrap()),
            ContractKind::Invariant
        )
        .with_message("Must maintain sorted order".to_string())
        .with_span((10, 20))
        .with_blame("caller".to_string());
        
        assert_eq!(condition.message, Some("Must maintain sorted order".to_string()));
        assert_eq!(condition.span, Some((10, 20)));
        assert_eq!(condition.blame_label, Some("caller".to_string()));
    }

    #[test]
    fn test_conditions_of_kind() {
        let mut contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
        
        contract.add_precondition(ContractCondition::new(
            NodeId(NonZeroU32::new(2).unwrap()), 
            ContractKind::Precondition
        ));
        contract.add_precondition(ContractCondition::new(
            NodeId(NonZeroU32::new(3).unwrap()), 
            ContractKind::Precondition
        ));
        contract.add_postcondition(ContractCondition::new(
            NodeId(NonZeroU32::new(4).unwrap()), 
            ContractKind::Postcondition
        ));
        
        assert_eq!(contract.conditions_of_kind(ContractKind::Precondition).len(), 2);
        assert_eq!(contract.conditions_of_kind(ContractKind::Postcondition).len(), 1);
        assert_eq!(contract.conditions_of_kind(ContractKind::Invariant).len(), 0);
    }

    #[test]
    fn test_contract_serialization() {
        let contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
        
        let json = serde_json::to_string(&contract).unwrap();
        let deserialized: Contract = serde_json::from_str(&json).unwrap();
        
        assert_eq!(contract.function_name, deserialized.function_name);
        assert_eq!(contract.node_id, deserialized.node_id);
    }

    #[test]
    fn test_contract_with_message() {
        let condition = ContractCondition::new_with_message(
            NodeId(NonZeroU32::new(1).unwrap()),
            ContractKind::Postcondition,
            "Result must be positive".to_string()
        );
        
        assert_eq!(condition.expression, NodeId(NonZeroU32::new(1).unwrap()));
        assert_eq!(condition.kind, ContractKind::Postcondition);
        assert_eq!(condition.message, Some("Result must be positive".to_string()));
    }

    #[test]
    fn test_pure_contract() {
        let mut contract = Contract::new("pure_func".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
        contract.pure = true;
        
        assert!(contract.pure);
    }

    #[test]
    fn test_contract_with_complexity() {
        let mut contract = Contract::new("sort".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
        contract.complexity = Some("O(n log n)".to_string());
        
        assert_eq!(contract.complexity, Some("O(n log n)".to_string()));
    }
}

#[cfg(test)]
mod error_tests {
    use super::*;

    #[test]
    fn test_contract_error_display() {
        let error = ContractError::VerificationError("Unable to prove postcondition".to_string());
        let display = format!("{}", error);
        
        assert!(display.contains("Verification error"));
        assert!(display.contains("Unable to prove postcondition"));
    }

    #[test]
    fn test_error_variants() {
        // Test available error variants
        let verification_err = ContractError::VerificationError("Test".to_string());
        let parse_err = ContractError::ParseError("Invalid syntax".to_string());
        let invalid_expr_err = ContractError::InvalidExpression("Bad expression".to_string());
        let solver_err = ContractError::SolverError("Z3 failed".to_string());
        let timeout_err = ContractError::Timeout(30);
        let not_impl_err = ContractError::NotImplemented("Feature X".to_string());
        let other_err = ContractError::Other("Generic error".to_string());
        
        assert!(format!("{}", verification_err).contains("Verification error"));
        assert!(format!("{}", parse_err).contains("parsing error"));
        assert!(format!("{}", invalid_expr_err).contains("Invalid contract expression"));
        assert!(format!("{}", solver_err).contains("SMT solver error"));
        assert!(format!("{}", timeout_err).contains("timeout"));
        assert!(format!("{}", not_impl_err).contains("not implemented"));
        assert!(format!("{}", other_err).contains("Generic error"));
    }
}