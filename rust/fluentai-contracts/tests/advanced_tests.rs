//! Advanced integration tests for fluentai-contracts

use fluentai_contracts::{
    contract::{Contract, ContractCondition, ContractKind},
    errors::ContractError,
    frame_conditions::{FieldAccess, FrameCondition, HeapRegion, IndexExpr},
};
use fluentai_core::ast::NodeId;
use std::num::NonZeroU32;

#[test]
fn test_contract_with_frame_conditions() {
    let _contract = Contract::new("swap".to_string(), NodeId(NonZeroU32::new(1).unwrap()));

    // Create a frame condition that modifies two variables
    let mut frame = FrameCondition::new();
    frame.add_modifies_var("a".to_string());
    frame.add_modifies_var("b".to_string());

    // Note: The actual FrameCondition doesn't have a frame_condition field on Contract
    // We'll test the frame condition itself
    assert_eq!(frame.modifies.len(), 2);
    assert!(frame.modifies.contains("a"));
    assert!(frame.modifies.contains("b"));
}

#[test]
fn test_frame_condition_purity() {
    // Pure function - no modifications
    let pure_frame = FrameCondition::new();
    assert!(pure_frame.is_pure());

    // Impure function - modifies state
    let mut impure_frame = FrameCondition::new();
    impure_frame.add_modifies_var("x".to_string());
    assert!(!impure_frame.is_pure());
}

#[test]
fn test_frame_condition_field_access() {
    let mut frame = FrameCondition::new();

    // Add field modification
    frame.add_modifies_field("obj".to_string(), "value".to_string());

    assert_eq!(frame.modifies_fields.len(), 1);
    assert!(frame.modifies_fields.contains(&FieldAccess {
        object: "obj".to_string(),
        field: "value".to_string()
    }));
}

#[test]
fn test_frame_condition_index_access() {
    let mut frame = FrameCondition::new();

    // Add index modification
    frame.add_modifies_index("arr".to_string(), IndexExpr::Constant(0));
    frame.add_modifies_index("matrix".to_string(), IndexExpr::Variable("i".to_string()));
    frame.add_modifies_index("all_elements".to_string(), IndexExpr::All);

    assert_eq!(frame.modifies_indices.len(), 3);
}

#[test]
fn test_contract_with_multiple_conditions() {
    let mut contract = Contract::new(
        "safe_divide".to_string(),
        NodeId(NonZeroU32::new(1).unwrap()),
    );

    // Add multiple preconditions
    for i in 1..=3 {
        let condition = ContractCondition::new(
            NodeId(NonZeroU32::new(i).unwrap()),
            ContractKind::Precondition,
        )
        .with_message(format!("Precondition {}", i));
        contract.add_precondition(condition);
    }

    // Add multiple postconditions
    for i in 4..=5 {
        let condition = ContractCondition::new(
            NodeId(NonZeroU32::new(i).unwrap()),
            ContractKind::Postcondition,
        )
        .with_message(format!("Postcondition {}", i - 3));
        contract.add_postcondition(condition);
    }

    assert_eq!(contract.preconditions.len(), 3);
    assert_eq!(contract.postconditions.len(), 2);
    assert!(contract.has_conditions());
}

#[test]
fn test_contract_condition_with_span_and_blame() {
    let condition =
        ContractCondition::new(NodeId(NonZeroU32::new(1).unwrap()), ContractKind::Invariant)
            .with_message("Invariant must hold".to_string())
            .with_span((100, 200))
            .with_blame("implementation".to_string());

    assert_eq!(condition.span, Some((100, 200)));
    assert_eq!(condition.blame_label, Some("implementation".to_string()));
}

#[test]
fn test_contract_complexity_annotation() {
    let mut contract = Contract::new("quicksort".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.complexity = Some("O(n log n) average, O(n²) worst case".to_string());

    assert!(contract.complexity.is_some());
    assert!(contract.complexity.unwrap().contains("O(n log n)"));
}

#[test]
fn test_frame_condition_heap_regions() {
    let mut frame = FrameCondition::new();

    // Add heap region modifications
    frame.add_modifies_region(HeapRegion::Named("stack".to_string()));
    frame.add_modifies_region(HeapRegion::ReachableFrom("root".to_string()));
    frame.add_modifies_region(HeapRegion::Fresh);
    frame.add_modifies_region(HeapRegion::Global);

    assert_eq!(frame.modifies_regions.len(), 4);
    assert!(frame.modifies_regions.contains(&HeapRegion::Fresh));
    assert!(frame.modifies_regions.contains(&HeapRegion::Global));
}

#[test]
fn test_contract_error_timeout() {
    let timeout_error = ContractError::Timeout(60);
    let display = format!("{}", timeout_error);
    assert!(display.contains("60 seconds"));
}

#[test]
fn test_frame_condition_allocation_flags() {
    let mut frame = FrameCondition::new();

    // Test allocation flags
    assert!(!frame.may_allocate);
    assert!(!frame.may_deallocate);

    frame.may_allocate = true;
    frame.may_deallocate = true;

    assert!(frame.may_allocate);
    assert!(frame.may_deallocate);
    assert!(!frame.is_pure()); // Allocation makes it impure
}

#[test]
fn test_frame_condition_builder() {
    use fluentai_contracts::frame_conditions::FrameConditionBuilder;

    let frame = FrameConditionBuilder::new()
        .modifies_vars(vec!["x".to_string(), "y".to_string()])
        .modifies_fields(vec![("obj".to_string(), "field".to_string())])
        .allows_allocation()
        .build();

    assert!(frame.modifies.contains("x"));
    assert!(frame.modifies.contains("y"));
    assert_eq!(frame.modifies_fields.len(), 1);
    assert!(frame.may_allocate);
    assert!(!frame.is_pure());
}

#[test]
fn test_index_expr_variants() {
    // Test different index expression types
    let const_idx = IndexExpr::Constant(42);
    let var_idx = IndexExpr::Variable("i".to_string());
    let range_idx = IndexExpr::Range(
        Box::new(IndexExpr::Constant(0)),
        Box::new(IndexExpr::Constant(10)),
    );
    let all_idx = IndexExpr::All;

    match const_idx {
        IndexExpr::Constant(n) => assert_eq!(n, 42),
        _ => panic!("Expected constant"),
    }

    match var_idx {
        IndexExpr::Variable(v) => assert_eq!(v, "i"),
        _ => panic!("Expected variable"),
    }

    match &range_idx {
        IndexExpr::Range(start, end) => {
            assert!(matches!(start.as_ref(), IndexExpr::Constant(0)));
            assert!(matches!(end.as_ref(), IndexExpr::Constant(10)));
        }
        _ => panic!("Expected range"),
    }

    assert!(matches!(all_idx, IndexExpr::All));
}

#[test]
fn test_contract_all_condition_types() {
    let mut contract = Contract::new(
        "complete_func".to_string(),
        NodeId(NonZeroU32::new(1).unwrap()),
    );

    // Add all types of conditions
    contract.add_precondition(ContractCondition::new(
        NodeId(NonZeroU32::new(1).unwrap()),
        ContractKind::Precondition,
    ));

    contract.add_postcondition(ContractCondition::new(
        NodeId(NonZeroU32::new(2).unwrap()),
        ContractKind::Postcondition,
    ));

    contract.add_invariant(ContractCondition::new(
        NodeId(NonZeroU32::new(3).unwrap()),
        ContractKind::Invariant,
    ));

    // Test conditions_of_kind for each type
    assert_eq!(
        contract
            .conditions_of_kind(ContractKind::Precondition)
            .len(),
        1
    );
    assert_eq!(
        contract
            .conditions_of_kind(ContractKind::Postcondition)
            .len(),
        1
    );
    assert_eq!(
        contract.conditions_of_kind(ContractKind::Invariant).len(),
        1
    );

    assert!(contract.has_conditions());
}
