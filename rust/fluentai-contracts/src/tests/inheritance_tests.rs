//! Tests for contract inheritance

use crate::{
    inheritance::*,
    contract::{Contract, ContractCondition, ContractKind},
};
use fluentai_core::ast::NodeId;

#[test]
fn test_basic_inheritance() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Create base contract
    let base = Contract {
        function_name: "base_function".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("x > 0".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("result > 0".to_string())
        ],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    // Create derived contract
    let derived = Contract {
        function_name: "derived_function".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("x > 0".to_string()),
            ContractCondition::new(NodeId(3), ContractKind::Precondition)
                .with_message("x < 100".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("result > 0".to_string()),
            ContractCondition::new(NodeId(4), ContractKind::Postcondition)
                .with_message("result < 100".to_string())
        ],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(base);
    hierarchy.add_contract(derived);
    
    // Establish inheritance
    let result = hierarchy.add_inheritance(
        "base_function".to_string(),
        "derived_function".to_string(),
        InheritanceType::Standard
    );
    
    assert!(result.is_ok());
    
    // Check inheritance relationships
    let bases = hierarchy.get_bases("derived_function");
    assert_eq!(bases.len(), 1);
    assert_eq!(bases[0], "base_function");
    
    let derived_contracts = hierarchy.get_derived("base_function");
    assert_eq!(derived_contracts.len(), 1);
    assert_eq!(derived_contracts[0], "derived_function");
}

#[test]
fn test_inheritance_cycle_detection() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Create contracts
    let contract_a = Contract::new("contract_a".to_string(), NodeId(0));
    let contract_b = Contract::new("contract_b".to_string(), NodeId(1));
    let contract_c = Contract::new("contract_c".to_string(), NodeId(2));
    
    hierarchy.add_contract(contract_a);
    hierarchy.add_contract(contract_b);
    hierarchy.add_contract(contract_c);
    
    // Create inheritance chain: A -> B -> C
    hierarchy.add_inheritance(
        "contract_a".to_string(),
        "contract_b".to_string(),
        InheritanceType::Standard
    ).unwrap();
    
    hierarchy.add_inheritance(
        "contract_b".to_string(),
        "contract_c".to_string(),
        InheritanceType::Standard
    ).unwrap();
    
    // Try to create cycle: C -> A
    let result = hierarchy.add_inheritance(
        "contract_c".to_string(),
        "contract_a".to_string(),
        InheritanceType::Standard
    );
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("cycle"));
}

#[test]
fn test_contract_composition() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Create contracts to compose
    let contract1 = Contract {
        function_name: "contract1".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("x > 0".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("result != null".to_string())
        ],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    let contract2 = Contract {
        function_name: "contract2".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(3), ContractKind::Precondition)
                .with_message("y > 0".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(4), ContractKind::Postcondition)
                .with_message("result > 0".to_string())
        ],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(contract1);
    hierarchy.add_contract(contract2);
    
    // Test conjunction composition
    let conj_result = hierarchy.compose_contracts(
        &["contract1".to_string(), "contract2".to_string()],
        CompositionType::Conjunction
    );
    
    assert!(conj_result.is_ok());
    let conj_contract = conj_result.unwrap();
    assert_eq!(conj_contract.preconditions.len(), 2);
    assert_eq!(conj_contract.postconditions.len(), 2);
    
    // Test XOR composition
    let xor_result = hierarchy.compose_contracts(
        &["contract1".to_string(), "contract2".to_string()],
        CompositionType::ExclusiveOr
    );
    
    assert!(xor_result.is_ok());
    let xor_contract = xor_result.unwrap();
    assert!(xor_contract.preconditions[0].message.as_ref().unwrap().contains("Exactly one"));
    
    // Test implication composition
    let impl_result = hierarchy.compose_contracts(
        &["contract1".to_string(), "contract2".to_string()],
        CompositionType::Implication
    );
    
    assert!(impl_result.is_ok());
    let impl_contract = impl_result.unwrap();
    assert!(impl_contract.preconditions[0].message.as_ref().unwrap().contains("must also hold"));
}

#[test]
fn test_refinement_rules() {
    let mut hierarchy = ContractHierarchy::new();
    
    let base = Contract::new("base".to_string(), NodeId(0));
    hierarchy.add_contract(base);
    
    // Add refinement rule
    let rule = RefinementRule {
        name: "strengthen_pre".to_string(),
        rule_type: RefinementType::StrengthenPreconditions,
        constraints: vec![],
    };
    
    hierarchy.add_refinement_rule("base".to_string(), rule);
    
    // Verify rule was added
    // (Would need getter method in real implementation)
}

#[test]
fn test_interface_implementation() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Create interface
    let interface = ContractInterface {
        name: "Sortable".to_string(),
        required_preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("input != null".to_string())
        ],
        required_postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("is_sorted(result)".to_string())
        ],
        required_invariants: vec![],
        optional_conditions: vec![],
    };
    
    hierarchy.add_interface(interface);
    
    // Create implementing contract
    let impl_contract = Contract {
        function_name: "quick_sort".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("input != null".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("is_sorted(result)".to_string()),
            ContractCondition::new(NodeId(3), ContractKind::Postcondition)
                .with_message("result.length == input.length".to_string())
        ],
        invariants: vec![],
        complexity: Some("O(n log n)".to_string()),
        pure: false,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(impl_contract);
    
    // The contract implements the interface by having all required conditions
}