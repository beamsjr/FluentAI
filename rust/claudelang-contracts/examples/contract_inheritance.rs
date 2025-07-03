//! Example demonstrating contract inheritance and refinement

use claudelang_contracts::{
    Contract, ContractCondition, ContractKind,
    ContractHierarchy, ContractHierarchyBuilder, ContractInterface,
    InheritanceType, RefinementRule, RefinementType, CompositionType,
};
use claudelang_parser::parse;
use claudelang_core::ast::NodeId;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Basic Contract Inheritance
    println!("=== Example 1: Basic Contract Inheritance ===\n");
    {
        // Base contract for a sorting function
        let base_sort = Contract {
            function_name: "sort".to_string(),
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(0), // Dummy node ID
                    message: Some("Input must be a list".to_string()),
                    kind: ContractKind::Precondition,
                }
            ],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(1),
                    message: Some("Output is sorted".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(2),
                    message: Some("Output has same elements as input".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: Some("O(n log n)".to_string()),
            pure: true,
            node_id: NodeId(0),
        };
        
        // Derived contract for a stable sort
        let stable_sort = Contract {
            function_name: "stable_sort".to_string(),
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(0),
                    message: Some("Input must be a list".to_string()),
                    kind: ContractKind::Precondition,
                }
            ],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(1),
                    message: Some("Output is sorted".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(2),
                    message: Some("Output has same elements as input".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(3),
                    message: Some("Equal elements maintain relative order".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: Some("O(n log n)".to_string()),
            pure: true,
            node_id: NodeId(0),
        };
        
        let mut hierarchy = ContractHierarchy::new();
        hierarchy.add_contract(base_sort);
        hierarchy.add_contract(stable_sort);
        hierarchy.add_inheritance(
            "sort".to_string(),
            "stable_sort".to_string(),
            InheritanceType::Refinement
        )?;
        
        println!("Created inheritance: stable_sort refines sort");
        println!("- Base 'sort' ensures: sorted output with same elements");
        println!("- Derived 'stable_sort' adds: stability guarantee");
        
        // Verify inheritance
        let graph = parse("(define (dummy) 0)")?; // Dummy graph
        let result = hierarchy.verify_inheritance(&graph, "sort", "stable_sort")?;
        
        println!("\nInheritance verification: {}", 
            if result.verified { "PASSED ✓" } else { "FAILED ✗" }
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
                    expression: NodeId(10),
                    message: Some("size() >= 0".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(11),
                    message: Some("empty() iff size() == 0".to_string()),
                    kind: ContractKind::Postcondition,
                },
            ],
            required_invariants: vec![
                ContractCondition {
                    expression: NodeId(12),
                    message: Some("Capacity >= size".to_string()),
                    kind: ContractKind::Invariant,
                }
            ],
            optional_conditions: vec![
                ContractCondition {
                    expression: NodeId(13),
                    message: Some("Thread-safe operations".to_string()),
                    kind: ContractKind::Invariant,
                }
            ],
        };
        
        // List implementation
        let list_contract = Contract {
            function_name: "List".to_string(),
            preconditions: vec![],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(10),
                    message: Some("size() >= 0".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(11),
                    message: Some("empty() iff size() == 0".to_string()),
                    kind: ContractKind::Postcondition,
                },
            ],
            invariants: vec![
                ContractCondition {
                    expression: NodeId(12),
                    message: Some("Capacity >= size".to_string()),
                    kind: ContractKind::Invariant,
                }
            ],
            complexity: Some("O(1) for size, O(n) for search".to_string()),
            pure: false,
            node_id: NodeId(0),
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
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(20),
                    message: Some("Array is sorted".to_string()),
                    kind: ContractKind::Precondition,
                }
            ],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(21),
                    message: Some("Returns index if found, -1 otherwise".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: Some("O(log n)".to_string()),
            pure: true,
            node_id: NodeId(0),
        };
        
        // Refined binary search with bounds checking
        let safe_binary_search = Contract {
            function_name: "safe_binary_search".to_string(),
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(20),
                    message: Some("Array is sorted".to_string()),
                    kind: ContractKind::Precondition,
                },
                ContractCondition {
                    expression: NodeId(22),
                    message: Some("Array is non-null".to_string()),
                    kind: ContractKind::Precondition,
                },
                ContractCondition {
                    expression: NodeId(23),
                    message: Some("Search bounds are valid".to_string()),
                    kind: ContractKind::Precondition,
                }
            ],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(21),
                    message: Some("Returns index if found, -1 otherwise".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(24),
                    message: Some("No array access out of bounds".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![
                ContractCondition {
                    expression: NodeId(25),
                    message: Some("Low <= high throughout execution".to_string()),
                    kind: ContractKind::Invariant,
                }
            ],
            complexity: Some("O(log n)".to_string()),
            pure: true,
            node_id: NodeId(0),
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
            InheritanceType::Refinement
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
                    expression: NodeId(30),
                    message: Some("Input is non-null".to_string()),
                    kind: ContractKind::Postcondition,
                },
                ContractCondition {
                    expression: NodeId(31),
                    message: Some("Input length > 0".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: Some("O(1)".to_string()),
            pure: true,
            node_id: NodeId(0),
        };
        
        // Contract for processing
        let process_data = Contract {
            function_name: "process_data".to_string(),
            preconditions: vec![
                ContractCondition {
                    expression: NodeId(30),
                    message: Some("Input is non-null".to_string()),
                    kind: ContractKind::Precondition,
                },
                ContractCondition {
                    expression: NodeId(31),
                    message: Some("Input length > 0".to_string()),
                    kind: ContractKind::Precondition,
                }
            ],
            postconditions: vec![
                ContractCondition {
                    expression: NodeId(32),
                    message: Some("Data is processed".to_string()),
                    kind: ContractKind::Postcondition,
                }
            ],
            invariants: vec![],
            complexity: Some("O(n)".to_string()),
            pure: false,
            node_id: NodeId(0),
        };
        
        let mut hierarchy = ContractHierarchy::new();
        hierarchy.add_contract(validate_input.clone());
        hierarchy.add_contract(process_data.clone());
        
        // Compose sequentially
        let composed = hierarchy.compose_contracts(
            &[validate_input.function_name, process_data.function_name],
            CompositionType::Sequential
        )?;
        
        println!("Sequential composition: validate_input ; process_data");
        println!("Composed contract:");
        println!("  Preconditions: {} (from first contract)", 
            composed.preconditions.len());
        println!("  Postconditions: {} (from last contract)", 
            composed.postconditions.len());
        println!("  Combined complexity: O(1) + O(n) = O(n)");
    }
    
    // Example 5: Inheritance Hierarchy
    println!("\n\n=== Example 5: Inheritance Hierarchy ===\n");
    {
        let mut hierarchy = ContractHierarchyBuilder::new()
            .contract(Contract {
                function_name: "Animal".to_string(),
                preconditions: vec![],
                postconditions: vec![
                    ContractCondition {
                        expression: NodeId(40),
                        message: Some("Has name".to_string()),
                        kind: ContractKind::Postcondition,
                    }
                ],
                invariants: vec![],
                complexity: None,
                pure: true,
                node_id: NodeId(0),
            })
            .contract(Contract {
                function_name: "Mammal".to_string(),
                preconditions: vec![],
                postconditions: vec![
                    ContractCondition {
                        expression: NodeId(40),
                        message: Some("Has name".to_string()),
                        kind: ContractKind::Postcondition,
                    },
                    ContractCondition {
                        expression: NodeId(41),
                        message: Some("Is warm-blooded".to_string()),
                        kind: ContractKind::Postcondition,
                    }
                ],
                invariants: vec![],
                complexity: None,
                pure: true,
                node_id: NodeId(0),
            })
            .contract(Contract {
                function_name: "Dog".to_string(),
                preconditions: vec![],
                postconditions: vec![
                    ContractCondition {
                        expression: NodeId(40),
                        message: Some("Has name".to_string()),
                        kind: ContractKind::Postcondition,
                    },
                    ContractCondition {
                        expression: NodeId(41),
                        message: Some("Is warm-blooded".to_string()),
                        kind: ContractKind::Postcondition,
                    },
                    ContractCondition {
                        expression: NodeId(42),
                        message: Some("Can bark".to_string()),
                        kind: ContractKind::Postcondition,
                    }
                ],
                invariants: vec![],
                complexity: None,
                pure: true,
                node_id: NodeId(0),
            })
            .inherits("Animal".to_string(), "Mammal".to_string(), InheritanceType::Standard)
            .inherits("Mammal".to_string(), "Dog".to_string(), InheritanceType::Standard)
            .build();
        
        println!("Contract Inheritance Hierarchy:");
        println!("  Animal");
        println!("    └── Mammal");
        println!("          └── Dog");
        
        println!("\nBase contracts for Dog: {:?}", hierarchy.get_bases("Dog"));
        println!("Derived contracts from Animal: {:?}", hierarchy.get_derived("Animal"));
        
        // Test cycle detection
        match hierarchy.add_inheritance("Dog".to_string(), "Animal".to_string(), InheritanceType::Standard) {
            Ok(_) => println!("\nERROR: Cycle not detected!"),
            Err(e) => println!("\nCorrectly rejected cycle: {}", e),
        }
    }
    
    Ok(())
}