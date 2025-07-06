//! Comprehensive tests for contract error types

use super::*;
use fluentai_core::ast::NodeId;
use crate::contract::ContractKind;

// ===== ContractError Tests =====

#[test]
fn test_contract_error_violation() {
    let violation = ContractViolation::new(
        ContractKind::Precondition,
        Some("test_func".to_string()),
        "x must be positive".to_string(),
        NodeId::new(42).unwrap(),
    );
    
    let error = ContractError::Violation(violation);
    assert!(error.to_string().contains("Contract violation"));
}

#[test]
fn test_contract_error_verification() {
    let error = ContractError::VerificationError("Failed to verify invariant".to_string());
    assert_eq!(error.to_string(), "Verification error: Failed to verify invariant");
}

#[test]
fn test_contract_error_parse() {
    let error = ContractError::ParseError("Invalid contract syntax".to_string());
    assert_eq!(error.to_string(), "Contract parsing error: Invalid contract syntax");
}

#[test]
fn test_contract_error_invalid_expression() {
    let error = ContractError::InvalidExpression("Undefined variable 'x'".to_string());
    assert_eq!(error.to_string(), "Invalid contract expression: Undefined variable 'x'");
}

#[test]
fn test_contract_error_solver() {
    let error = ContractError::SolverError("Z3 solver timeout".to_string());
    assert_eq!(error.to_string(), "SMT solver error: Z3 solver timeout");
}

#[test]
fn test_contract_error_timeout() {
    let error = ContractError::Timeout(30);
    assert_eq!(error.to_string(), "Verification timeout after 30 seconds");
}

#[test]
fn test_contract_error_not_implemented() {
    let error = ContractError::NotImplemented("Quantifier elimination".to_string());
    assert_eq!(error.to_string(), "Feature not implemented: Quantifier elimination");
}

#[test]
fn test_contract_error_other() {
    let error = ContractError::Other("Custom error message".to_string());
    assert_eq!(error.to_string(), "Custom error message");
}

// ===== ContractViolation Tests =====

#[test]
fn test_precondition_violation_simple() {
    let violation = ContractViolation::Precondition {
        function: None,
        message: "Parameter must be non-null".to_string(),
        node_id: NodeId::new(1).unwrap(),
        span: None,
        blame_label: None,
    };
    
    assert_eq!(violation.to_string(), "Precondition violated: Parameter must be non-null");
}

#[test]
fn test_precondition_violation_with_function() {
    let violation = ContractViolation::Precondition {
        function: Some("divide".to_string()),
        message: "Divisor must be non-zero".to_string(),
        node_id: NodeId::new(2).unwrap(),
        span: None,
        blame_label: None,
    };
    
    assert!(violation.to_string().contains("in function 'divide'"));
    assert!(violation.to_string().contains("Divisor must be non-zero"));
}

#[test]
fn test_precondition_violation_with_blame() {
    let violation = ContractViolation::Precondition {
        function: Some("sqrt".to_string()),
        message: "Argument must be non-negative".to_string(),
        node_id: NodeId::new(3).unwrap(),
        span: Some((10, 20)),
        blame_label: Some("caller".to_string()),
    };
    
    assert!(violation.to_string().contains("[caller]"));
}

#[test]
fn test_postcondition_violation() {
    let violation = ContractViolation::Postcondition {
        function: Some("sort".to_string()),
        message: "Result must be sorted".to_string(),
        node_id: NodeId::new(4).unwrap(),
        span: None,
        blame_label: None,
    };
    
    assert!(violation.to_string().contains("Postcondition violated"));
    assert!(violation.to_string().contains("in function 'sort'"));
}

#[test]
fn test_invariant_violation() {
    let violation = ContractViolation::Invariant {
        function: None,
        message: "Balance must be non-negative".to_string(),
        node_id: NodeId::new(5).unwrap(),
        span: Some((30, 40)),
        blame_label: Some("withdraw".to_string()),
    };
    
    assert!(violation.to_string().contains("Invariant violated"));
    assert!(violation.to_string().contains("[withdraw]"));
}

#[test]
fn test_purity_violation() {
    let violation = ContractViolation::Purity {
        function: "compute".to_string(),
        message: "Function performs I/O operations".to_string(),
        node_id: NodeId::new(6).unwrap(),
        span: None,
        blame_label: None,
    };
    
    assert!(violation.to_string().contains("Purity violation"));
    assert!(violation.to_string().contains("in function 'compute'"));
}

// ===== ContractViolation constructor tests =====

#[test]
fn test_violation_new_precondition() {
    let violation = ContractViolation::new(
        ContractKind::Precondition,
        Some("test".to_string()),
        "Error message".to_string(),
        NodeId::new(10).unwrap(),
    );
    
    match violation {
        ContractViolation::Precondition { function, message, node_id, span, blame_label } => {
            assert_eq!(function, Some("test".to_string()));
            assert_eq!(message, "Error message");
            assert_eq!(node_id, NodeId::new(10).unwrap());
            assert!(span.is_none());
            assert!(blame_label.is_none());
        }
        _ => panic!("Expected Precondition variant"),
    }
}

#[test]
fn test_violation_new_postcondition() {
    let violation = ContractViolation::new(
        ContractKind::Postcondition,
        None,
        "Post error".to_string(),
        NodeId::new(20).unwrap(),
    );
    
    match violation {
        ContractViolation::Postcondition { .. } => {}
        _ => panic!("Expected Postcondition variant"),
    }
}

#[test]
fn test_violation_new_invariant() {
    let violation = ContractViolation::new(
        ContractKind::Invariant,
        Some("loop".to_string()),
        "Invariant error".to_string(),
        NodeId::new(30).unwrap(),
    );
    
    match violation {
        ContractViolation::Invariant { .. } => {}
        _ => panic!("Expected Invariant variant"),
    }
}

#[test]
fn test_violation_with_details() {
    let violation = ContractViolation::with_details(
        ContractKind::Precondition,
        Some("func".to_string()),
        "Detailed error".to_string(),
        NodeId::new(40).unwrap(),
        Some((100, 200)),
        Some("test_blame".to_string()),
    );
    
    assert_eq!(violation.span(), Some((100, 200)));
    assert_eq!(violation.blame_label(), Some("test_blame"));
}

// ===== Method tests =====

#[test]
fn test_with_span() {
    let violation = ContractViolation::new(
        ContractKind::Precondition,
        None,
        "Error".to_string(),
        NodeId::new(50).unwrap(),
    ).with_span((10, 20));
    
    assert_eq!(violation.span(), Some((10, 20)));
}

#[test]
fn test_with_blame() {
    let violation = ContractViolation::new(
        ContractKind::Postcondition,
        None,
        "Error".to_string(),
        NodeId::new(60).unwrap(),
    ).with_blame("caller_function".to_string());
    
    assert_eq!(violation.blame_label(), Some("caller_function"));
}

#[test]
fn test_node_id() {
    let violation = ContractViolation::Purity {
        function: "test".to_string(),
        message: "Not pure".to_string(),
        node_id: NodeId::new(70).unwrap(),
        span: None,
        blame_label: None,
    };
    
    assert_eq!(violation.node_id(), NodeId::new(70).unwrap());
}

#[test]
fn test_function_name() {
    let violation1 = ContractViolation::Precondition {
        function: Some("func1".to_string()),
        message: "Error".to_string(),
        node_id: NodeId::new(80).unwrap(),
        span: None,
        blame_label: None,
    };
    
    let violation2 = ContractViolation::Purity {
        function: "func2".to_string(),
        message: "Error".to_string(),
        node_id: NodeId::new(90).unwrap(),
        span: None,
        blame_label: None,
    };
    
    assert_eq!(violation1.function_name(), Some("func1"));
    assert_eq!(violation2.function_name(), Some("func2"));
}

#[test]
fn test_violation_chaining() {
    let violation = ContractViolation::new(
        ContractKind::Invariant,
        Some("process".to_string()),
        "State corrupted".to_string(),
        NodeId::new(100).unwrap(),
    )
    .with_span((50, 60))
    .with_blame("modifier".to_string());
    
    assert_eq!(violation.function_name(), Some("process"));
    assert_eq!(violation.span(), Some((50, 60)));
    assert_eq!(violation.blame_label(), Some("modifier"));
    assert_eq!(violation.node_id(), NodeId::new(100).unwrap());
}

// ===== Error conversion tests =====

#[test]
fn test_violation_into_contract_error() {
    let violation = ContractViolation::new(
        ContractKind::Precondition,
        None,
        "Test".to_string(),
        NodeId::new(110).unwrap(),
    );
    
    let error: ContractError = violation.into();
    match error {
        ContractError::Violation(_) => {}
        _ => panic!("Expected Violation variant"),
    }
}