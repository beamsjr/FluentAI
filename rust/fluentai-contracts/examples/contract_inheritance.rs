//! Example demonstrating contract inheritance and refinement
use fluentai_parser::parse_flc;


use fluentai_contracts::{
    CompositionType, Contract, ContractCondition, ContractHierarchy, ContractHierarchyBuilder,
    ContractInterface, ContractKind, InheritanceType, RefinementRule, RefinementType,
};
use fluentai_core::ast::NodeId;
use std::num::NonZero;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Basic Contract Inheritance
    println!("=== Example 1: Basic Contract Inheritance ===\n");
    {
        // Base contract for a sorting function
        let base_sort = Contract {
            function_name: "sort".to_string(),
            preconditions: vec![ContractCondition {
                expression: NodeId(NonZero::new(1).unwrap()), // Dummy node ID
                message: Some("Input must be a list".to_string()),
                kind: ContractKind::Precondition,
                span: None,
                blame_label: None,
            }],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(1).unwrap()),
                    message: Some("Output is sorted".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(2).unwrap()),
                    message: Some("Output has same elements as input".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
            ],
            invariants: vec![],
            complexity: Some("O(n log n)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        // Derived contract for a stable sort
        let stable_sort = Contract {
            function_name: "stable_sort".to_string(),
            preconditions: vec![ContractCondition {
                expression: NodeId(NonZero::new(1).unwrap()),
                message: Some("Input must be a list".to_string()),
                kind: ContractKind::Precondition,
                span: None,
                blame_label: None,
            }],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(1).unwrap()),
                    message: Some("Output is sorted".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(2).unwrap()),
                    message: Some("Output has same elements as input".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(3).unwrap()),
                    message: Some("Equal elements maintain relative order".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
            ],
            invariants: vec![],
            complexity: Some("O(n log n)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        let mut hierarchy = ContractHierarchy::new();
        hierarchy.add_contract(base_sort);
        hierarchy.add_contract(stable_sort);
        hierarchy.add_inheritance(
            "sort".to_string(),
            "stable_sort".to_string(),
            InheritanceType::Refinement,
        )?;

        println!("Created inheritance: stable_sort refines sort");
        println!("- Base 'sort' ensures: sorted output with same elements");
        println!("- Derived 'stable_sort' adds: stability guarantee");

        // Verify inheritance
        let graph = parse_flc("(define (dummy) 0)")?; // Dummy graph
        let result = hierarchy.verify_inheritance(&graph, "sort", "stable_sort")?;

        println!(
            "\nInheritance verification: {}",
            if result.verified {
                "PASSED ✓"
            } else {
                "FAILED ✗"
            }
        );

        if !result.violations.is_empty() {
            println!("Violations:");
            for violation in &result.violations {
                println!("  - {}", violation.message);
            }
        }
    }

    // Example 2: Contract Interfaces
    println!("\n\n=== Example 2: Contract Interfaces ===\n");
    {
        // Define a Collection interface
        let collection_interface = ContractInterface {
            name: "Collection".to_string(),
            required_preconditions: vec![],
            required_postconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(10).unwrap()),
                    message: Some("size() >= 0".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(11).unwrap()),
                    message: Some("empty() iff size() == 0".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
            ],
            required_invariants: vec![ContractCondition {
                expression: NodeId(NonZero::new(12).unwrap()),
                message: Some("Capacity >= size".to_string()),
                kind: ContractKind::Invariant,
                span: None,
                blame_label: None,
            }],
            optional_conditions: vec![ContractCondition {
                expression: NodeId(NonZero::new(13).unwrap()),
                message: Some("Thread-safe operations".to_string()),
                kind: ContractKind::Invariant,
                span: None,
                blame_label: None,
            }],
        };

        // List implementation
        let list_contract = Contract {
            function_name: "List".to_string(),
            preconditions: vec![],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(10).unwrap()),
                    message: Some("size() >= 0".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(11).unwrap()),
                    message: Some("empty() iff size() == 0".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
            ],
            invariants: vec![ContractCondition {
                expression: NodeId(NonZero::new(12).unwrap()),
                message: Some("Capacity >= size".to_string()),
                kind: ContractKind::Invariant,
                span: None,
                blame_label: None,
            }],
            complexity: Some("O(1) for size, O(n) for search".to_string()),
            pure: false,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        let mut hierarchy = ContractHierarchy::new();
        hierarchy.add_interface(collection_interface);
        hierarchy.add_contract(list_contract);

        println!("Defined Collection interface with requirements:");
        println!("- size() >= 0");
        println!("- empty() iff size() == 0");
        println!("- Invariant: Capacity >= size");
        println!("\nList implements Collection interface");
    }

    // Example 3: Contract Refinement with Rules
    println!("\n\n=== Example 3: Contract Refinement with Rules ===\n");
    {
        // Base binary search
        let binary_search = Contract {
            function_name: "binary_search".to_string(),
            preconditions: vec![ContractCondition {
                expression: NodeId(NonZero::new(20).unwrap()),
                message: Some("Array is sorted".to_string()),
                kind: ContractKind::Precondition,
                span: None,
                blame_label: None,
            }],
            postconditions: vec![ContractCondition {
                expression: NodeId(NonZero::new(21).unwrap()),
                message: Some("Returns index if found, -1 otherwise".to_string()),
                kind: ContractKind::Postcondition,
                span: None,
                blame_label: None,
            }],
            invariants: vec![],
            complexity: Some("O(log n)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        // Refined binary search with bounds checking
        let safe_binary_search = Contract {
            function_name: "safe_binary_search".to_string(),
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(20).unwrap()),
                    message: Some("Array is sorted".to_string()),
                    kind: ContractKind::Precondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(22).unwrap()),
                    message: Some("Array is non-null".to_string()),
                    kind: ContractKind::Precondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(23).unwrap()),
                    message: Some("Search bounds are valid".to_string()),
                    kind: ContractKind::Precondition,
                    span: None,
                    blame_label: None,
                },
            ],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(21).unwrap()),
                    message: Some("Returns index if found, -1 otherwise".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(24).unwrap()),
                    message: Some("No array access out of bounds".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
            ],
            invariants: vec![ContractCondition {
                expression: NodeId(NonZero::new(25).unwrap()),
                message: Some("Low <= high throughout execution".to_string()),
                kind: ContractKind::Invariant,
                span: None,
                blame_label: None,
            }],
            complexity: Some("O(log n)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        let mut hierarchy = ContractHierarchy::new();
        hierarchy.add_contract(binary_search);
        hierarchy.add_contract(safe_binary_search);

        // Add refinement rule
        let bounds_rule = RefinementRule {
            name: "bounds_checking".to_string(),
            rule_type: RefinementType::StrengthenPreconditions,
            constraints: vec![],
        };

        hierarchy.add_refinement_rule("safe_binary_search".to_string(), bounds_rule);

        hierarchy.add_inheritance(
            "binary_search".to_string(),
            "safe_binary_search".to_string(),
            InheritanceType::Refinement,
        )?;

        println!("Refinement: safe_binary_search refines binary_search");
        println!("Added preconditions:");
        println!("  - Array is non-null");
        println!("  - Search bounds are valid");
        println!("Added postcondition:");
        println!("  - No array access out of bounds");
        println!("Added invariant:");
        println!("  - Low <= high throughout execution");
    }

    // Example 4: Contract Composition
    println!("\n\n=== Example 4: Contract Composition ===\n");
    {
        // Contract for input validation
        let validate_input = Contract {
            function_name: "validate_input".to_string(),
            preconditions: vec![],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(30).unwrap()),
                    message: Some("Input is non-null".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(31).unwrap()),
                    message: Some("Input length > 0".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                },
            ],
            invariants: vec![],
            complexity: Some("O(1)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        // Contract for processing
        let process_data = Contract {
            function_name: "process_data".to_string(),
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(NonZero::new(30).unwrap()),
                    message: Some("Input is non-null".to_string()),
                    kind: ContractKind::Precondition,
                    span: None,
                    blame_label: None,
                },
                ContractCondition {
                    expression: NodeId(NonZero::new(31).unwrap()),
                    message: Some("Input length > 0".to_string()),
                    kind: ContractKind::Precondition,
                    span: None,
                    blame_label: None,
                },
            ],
            postconditions: vec![ContractCondition {
                expression: NodeId(NonZero::new(32).unwrap()),
                message: Some("Data is processed".to_string()),
                kind: ContractKind::Postcondition,
                span: None,
                blame_label: None,
            }],
            invariants: vec![],
            complexity: Some("O(n)".to_string()),
            pure: false,
            frame_condition: None,
            node_id: NodeId(NonZero::new(1).unwrap()),
        };

        let mut hierarchy = ContractHierarchy::new();
        hierarchy.add_contract(validate_input.clone());
        hierarchy.add_contract(process_data.clone());

        // Compose sequentially
        let composed = hierarchy.compose_contracts(
            &[validate_input.function_name, process_data.function_name],
            CompositionType::Sequential,
        )?;

        println!("Sequential composition: validate_input ; process_data");
        println!("Composed contract:");
        println!(
            "  Preconditions: {} (from first contract)",
            composed.preconditions.len()
        );
        println!(
            "  Postconditions: {} (from last contract)",
            composed.postconditions.len()
        );
        println!("  Combined complexity: O(1) + O(n) = O(n)");
    }

    // Example 5: Inheritance Hierarchy
    println!("\n\n=== Example 5: Inheritance Hierarchy ===\n");
    {
        let mut hierarchy = ContractHierarchyBuilder::new()
            .contract(Contract {
                function_name: "Animal".to_string(),
                preconditions: vec![],
                postconditions: vec![ContractCondition {
                    expression: NodeId(NonZero::new(40).unwrap()),
                    message: Some("Has name".to_string()),
                    kind: ContractKind::Postcondition,
                    span: None,
                    blame_label: None,
                }],
                invariants: vec![],
                complexity: None,
                pure: true,
                frame_condition: None,
                node_id: NodeId(NonZero::new(1).unwrap()),
            })
            .contract(Contract {
                function_name: "Mammal".to_string(),
                preconditions: vec![],
                postconditions: vec![
                    ContractCondition {
                        expression: NodeId(NonZero::new(40).unwrap()),
                        message: Some("Has name".to_string()),
                        kind: ContractKind::Postcondition,
                        span: None,
                        blame_label: None,
                    },
                    ContractCondition {
                        expression: NodeId(NonZero::new(41).unwrap()),
                        message: Some("Is warm-blooded".to_string()),
                        kind: ContractKind::Postcondition,
                        span: None,
                        blame_label: None,
                    },
                ],
                invariants: vec![],
                complexity: None,
                pure: true,
                frame_condition: None,
                node_id: NodeId(NonZero::new(1).unwrap()),
            })
            .contract(Contract {
                function_name: "Dog".to_string(),
                preconditions: vec![],
                postconditions: vec![
                    ContractCondition {
                        expression: NodeId(NonZero::new(40).unwrap()),
                        message: Some("Has name".to_string()),
                        kind: ContractKind::Postcondition,
                        span: None,
                        blame_label: None,
                    },
                    ContractCondition {
                        expression: NodeId(NonZero::new(41).unwrap()),
                        message: Some("Is warm-blooded".to_string()),
                        kind: ContractKind::Postcondition,
                        span: None,
                        blame_label: None,
                    },
                    ContractCondition {
                        expression: NodeId(NonZero::new(42).unwrap()),
                        message: Some("Can bark".to_string()),
                        kind: ContractKind::Postcondition,
                        span: None,
                        blame_label: None,
                    },
                ],
                invariants: vec![],
                complexity: None,
                pure: true,
                frame_condition: None,
                node_id: NodeId(NonZero::new(1).unwrap()),
            })
            .inherits(
                "Animal".to_string(),
                "Mammal".to_string(),
                InheritanceType::Standard,
            )
            .inherits(
                "Mammal".to_string(),
                "Dog".to_string(),
                InheritanceType::Standard,
            )
            .build();

        println!("Contract Inheritance Hierarchy:");
        println!("  Animal");
        println!("    └── Mammal");
        println!("          └── Dog");

        println!("\nBase contracts for Dog: {:?}", hierarchy.get_bases("Dog"));
        println!(
            "Derived contracts from Animal: {:?}",
            hierarchy.get_derived("Animal")
        );

        // Test cycle detection
        match hierarchy.add_inheritance(
            "Dog".to_string(),
            "Animal".to_string(),
            InheritanceType::Standard,
        ) {
            Ok(_) => println!("\nERROR: Cycle not detected!"),
            Err(e) => println!("\nCorrectly rejected cycle: {}", e),
        }
    }

    Ok(())
}
