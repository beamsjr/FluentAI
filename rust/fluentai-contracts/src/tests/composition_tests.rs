//! Tests for contract composition with XOR and implication

use crate::{
    inheritance::*,
    contract::{Contract, ContractCondition, ContractKind},
};
use fluentai_core::ast::NodeId;

#[test]
fn test_xor_composition() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Create two contracts for XOR composition
    let contract_a = Contract {
        function_name: "process_type_a".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("is_type_a(input)".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("result.type == 'A'".to_string())
        ],
        invariants: vec![],
        complexity: Some("O(n)".to_string()),
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    let contract_b = Contract {
        function_name: "process_type_b".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(3), ContractKind::Precondition)
                .with_message("is_type_b(input)".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(4), ContractKind::Postcondition)
                .with_message("result.type == 'B'".to_string())
        ],
        invariants: vec![],
        complexity: Some("O(n^2)".to_string()),
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(contract_a);
    hierarchy.add_contract(contract_b);
    
    // Compose with XOR - exactly one must hold
    let xor_result = hierarchy.compose_contracts(
        &["process_type_a".to_string(), "process_type_b".to_string()],
        CompositionType::ExclusiveOr
    );
    
    assert!(xor_result.is_ok());
    let xor_contract = xor_result.unwrap();
    
    // Check the composed contract
    assert!(xor_contract.function_name.contains("xor"));
    assert_eq!(xor_contract.preconditions.len(), 1);
    assert!(xor_contract.preconditions[0].message.as_ref().unwrap().contains("Exactly one"));
    assert!(xor_contract.preconditions[0].blame_label.as_ref().unwrap() == "xor_composition");
}

#[test]
fn test_xor_composition_requires_two_contracts() {
    let mut hierarchy = ContractHierarchy::new();
    
    let contract = Contract::new("single".to_string(), NodeId(0));
    hierarchy.add_contract(contract);
    
    // XOR with single contract should fail
    let result = hierarchy.compose_contracts(
        &["single".to_string()],
        CompositionType::ExclusiveOr
    );
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("exactly 2"));
    
    // XOR with three contracts should also fail
    hierarchy.add_contract(Contract::new("second".to_string(), NodeId(1)));
    hierarchy.add_contract(Contract::new("third".to_string(), NodeId(2)));
    
    let result = hierarchy.compose_contracts(
        &["single".to_string(), "second".to_string(), "third".to_string()],
        CompositionType::ExclusiveOr
    );
    
    assert!(result.is_err());
}

#[test]
fn test_implication_composition() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Antecedent: validates input
    let validate_contract = Contract {
        function_name: "validate_input".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("input != null".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("is_valid(input)".to_string())
        ],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    // Consequent: processes validated input
    let process_contract = Contract {
        function_name: "process_validated".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(3), ContractKind::Precondition)
                .with_message("is_valid(input)".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(4), ContractKind::Postcondition)
                .with_message("result.processed == true".to_string())
        ],
        invariants: vec![],
        complexity: None,
        pure: false,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(validate_contract);
    hierarchy.add_contract(process_contract);
    
    // Compose with implication: if validation passes, processing must succeed
    let impl_result = hierarchy.compose_contracts(
        &["validate_input".to_string(), "process_validated".to_string()],
        CompositionType::Implication
    );
    
    assert!(impl_result.is_ok());
    let impl_contract = impl_result.unwrap();
    
    // Check composed contract
    assert!(impl_contract.function_name.contains("implies"));
    assert_eq!(impl_contract.preconditions.len(), 1);
    assert!(impl_contract.preconditions[0].message.as_ref().unwrap()
        .contains("must also hold"));
    assert_eq!(impl_contract.postconditions.len(), 1);
    
    // Pure should be false since consequent is not pure
    assert!(!impl_contract.pure);
}

#[test]
fn test_implication_empty_conditions() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Contract with no preconditions
    let no_pre = Contract {
        function_name: "no_preconditions".to_string(),
        preconditions: vec![],
        postconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Postcondition)
        ],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    // Contract with preconditions
    let with_pre = Contract {
        function_name: "with_preconditions".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Precondition)
        ],
        postconditions: vec![],
        invariants: vec![],
        complexity: None,
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(no_pre);
    hierarchy.add_contract(with_pre);
    
    // Implication with empty antecedent preconditions
    let result = hierarchy.compose_contracts(
        &["no_preconditions".to_string(), "with_preconditions".to_string()],
        CompositionType::Implication
    );
    
    assert!(result.is_ok());
    let contract = result.unwrap();
    
    // Should have no preconditions since antecedent has none
    assert_eq!(contract.preconditions.len(), 0);
}

#[test]
fn test_complex_composition_chain() {
    let mut hierarchy = ContractHierarchy::new();
    
    // Create base contracts
    for i in 1..=4 {
        let contract = Contract {
            function_name: format!("contract_{}", i),
            preconditions: vec![
                ContractCondition::new(NodeId(i * 2), ContractKind::Precondition)
                    .with_message(format!("pre_{}", i))
            ],
            postconditions: vec![
                ContractCondition::new(NodeId(i * 2 + 1), ContractKind::Postcondition)
                    .with_message(format!("post_{}", i))
            ],
            invariants: vec![],
            complexity: None,
            pure: true,
            frame_condition: None,
            node_id: NodeId(0),
        };
        hierarchy.add_contract(contract);
    }
    
    // First compose 1 and 2 with conjunction
    let conj_12 = hierarchy.compose_contracts(
        &["contract_1".to_string(), "contract_2".to_string()],
        CompositionType::Conjunction
    ).unwrap();
    
    assert_eq!(conj_12.preconditions.len(), 2);
    assert_eq!(conj_12.postconditions.len(), 2);
    
    // Then compose 3 and 4 with disjunction
    let disj_34 = hierarchy.compose_contracts(
        &["contract_3".to_string(), "contract_4".to_string()],
        CompositionType::Disjunction
    ).unwrap();
    
    // Disjunction creates empty conditions in this simplified implementation
    assert_eq!(disj_34.preconditions.len(), 0);
}

#[test]
fn test_sequential_composition() {
    let mut hierarchy = ContractHierarchy::new();
    
    // First step: parse input
    let parse = Contract {
        function_name: "parse".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(1), ContractKind::Precondition)
                .with_message("is_string(input)".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(2), ContractKind::Postcondition)
                .with_message("is_ast(result)".to_string())
        ],
        invariants: vec![],
        complexity: Some("O(n)".to_string()),
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    // Second step: optimize AST
    let optimize = Contract {
        function_name: "optimize".to_string(),
        preconditions: vec![
            ContractCondition::new(NodeId(3), ContractKind::Precondition)
                .with_message("is_ast(input)".to_string())
        ],
        postconditions: vec![
            ContractCondition::new(NodeId(4), ContractKind::Postcondition)
                .with_message("is_optimized_ast(result)".to_string())
        ],
        invariants: vec![],
        complexity: Some("O(n^2)".to_string()),
        pure: true,
        frame_condition: None,
        node_id: NodeId(0),
    };
    
    hierarchy.add_contract(parse);
    hierarchy.add_contract(optimize);
    
    // Sequential composition
    let seq_result = hierarchy.compose_contracts(
        &["parse".to_string(), "optimize".to_string()],
        CompositionType::Sequential
    );
    
    assert!(seq_result.is_ok());
    let seq_contract = seq_result.unwrap();
    
    // Sequential takes first's preconditions and last's postconditions
    assert_eq!(seq_contract.preconditions.len(), 1);
    assert_eq!(seq_contract.postconditions.len(), 1);
    assert_eq!(seq_contract.invariants.len(), 0);
}