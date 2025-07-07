//! Demonstrates quantifiers in contract specifications

use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
    quantifiers::{QuantifierBuilder, QuantifierDomain},
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use std::num::NonZeroU32;

#[cfg(feature = "static")]
use z3::{Config, Context, Solver};
#[cfg(feature = "static")]
use fluentai_contracts::{
    z3_converter::{Z3Converter, Z3Sort, Z3Expr},
    static_verification::StaticVerifier,
};

fn main() {
    println!("=== Quantifier Contracts Demo ===\n");
    
    // Example 1: Array all positive
    println!("Example 1: Array with all positive elements");
    demo_all_positive();
    
    // Example 2: Sorted array
    println!("\nExample 2: Sorted array property");
    demo_sorted_array();
    
    // Example 3: Existence property
    println!("\nExample 3: Existence of element");
    demo_exists_element();
    
    // Example 4: Bounded array
    println!("\nExample 4: Bounded array values");
    demo_bounded_array();
}

/// Demo: forall i in [0, length(arr)), arr[i] > 0
fn demo_all_positive() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    // Build the array reference
    let arr = builder.graph.add_node(Node::Variable { name: "arr".to_string() }).expect("Failed to add node");
    
    // Build: forall i in [0, length(arr)), arr[i] > 0
    let contract_expr = builder.forall(
        vec![("i", QuantifierDomain::ListIndices(arr))],
        |builder, vars| {
            let i = vars["i"];
            let zero = builder.graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
            
            // arr[i]
            let nth_op = builder.graph.add_node(Node::Variable { name: "nth".to_string() }).expect("Failed to add node");
            let arr_i = builder.graph.add_node(Node::Application {
                function: nth_op,
                args: vec![arr, i],
            }).expect("Failed to add node");
            
            // arr[i] > 0
            let gt_op = builder.graph.add_node(Node::Variable { name: ">".to_string() }).expect("Failed to add node");
            builder.graph.add_node(Node::Application {
                function: gt_op,
                args: vec![arr_i, zero],
            }).expect("Failed to add node")
        }
    );
    
    // Create contract
    let mut contract = Contract::new("all_positive".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.add_postcondition(ContractCondition {
        expression: contract_expr,
        message: Some("All array elements must be positive".to_string()),
        kind: ContractKind::Postcondition,
        span: None,
        blame_label: Some("postcondition".to_string()),
    });
    
    println!("Contract: forall i in indices(arr), arr[i] > 0");
    
    #[cfg(feature = "static")]
    verify_contract(&builder.graph, &contract);
}

/// Demo: forall i in [0, length(arr)-1), arr[i] <= arr[i+1]
fn demo_sorted_array() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    let arr = builder.graph.add_node(Node::Variable { name: "arr".to_string() }).expect("Failed to add node");
    
    // Build the sorted property
    let contract_expr = builder.forall(
        vec![("i", QuantifierDomain::ListIndices(arr))],
        |builder, vars| {
            let i = vars["i"];
            let one = builder.graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
            
            // i + 1
            let plus_op = builder.graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
            let i_plus_1 = builder.graph.add_node(Node::Application {
                function: plus_op,
                args: vec![i, one],
            }).expect("Failed to add node");
            
            // length(arr)
            let length_op = builder.graph.add_node(Node::Variable { name: "length".to_string() }).expect("Failed to add node");
            let arr_length = builder.graph.add_node(Node::Application {
                function: length_op,
                args: vec![arr],
            }).expect("Failed to add node");
            
            // i + 1 < length(arr)
            let lt_op = builder.graph.add_node(Node::Variable { name: "<".to_string() }).expect("Failed to add node");
            let valid_index = builder.graph.add_node(Node::Application {
                function: lt_op,
                args: vec![i_plus_1, arr_length],
            }).expect("Failed to add node");
            
            // arr[i]
            let nth_op = builder.graph.add_node(Node::Variable { name: "nth".to_string() }).expect("Failed to add node");
            let arr_i = builder.graph.add_node(Node::Application {
                function: nth_op,
                args: vec![arr, i],
            }).expect("Failed to add node");
            
            // arr[i+1]
            let arr_i_plus_1 = builder.graph.add_node(Node::Application {
                function: nth_op,
                args: vec![arr, i_plus_1],
            }).expect("Failed to add node");
            
            // arr[i] <= arr[i+1]
            let le_op = builder.graph.add_node(Node::Variable { name: "<=".to_string() }).expect("Failed to add node");
            let sorted_pair = builder.graph.add_node(Node::Application {
                function: le_op,
                args: vec![arr_i, arr_i_plus_1],
            }).expect("Failed to add node");
            
            // implies(i + 1 < length(arr), arr[i] <= arr[i+1])
            let implies_op = builder.graph.add_node(Node::Variable { name: "implies".to_string() }).expect("Failed to add node");
            builder.graph.add_node(Node::Application {
                function: implies_op,
                args: vec![valid_index, sorted_pair],
            }).expect("Failed to add node")
        }
    );
    
    let mut contract = Contract::new("is_sorted".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.add_postcondition(ContractCondition::new(contract_expr, ContractKind::Postcondition)
        .with_blame("Array must be sorted in ascending order".to_string()));
    
    println!("Contract: forall i, (i + 1 < length(arr)) => (arr[i] <= arr[i+1])");
    
    #[cfg(feature = "static")]
    verify_contract(&builder.graph, &contract);
}

/// Demo: exists i in indices(arr), arr[i] = target
fn demo_exists_element() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    let arr = builder.graph.add_node(Node::Variable { name: "arr".to_string() }).expect("Failed to add node");
    let target = builder.graph.add_node(Node::Variable { name: "target".to_string() }).expect("Failed to add node");
    
    // Build: exists i in indices(arr), arr[i] = target
    let contract_expr = builder.exists(
        vec![("i", QuantifierDomain::ListIndices(arr))],
        |builder, vars| {
            let i = vars["i"];
            
            // arr[i]
            let nth_op = builder.graph.add_node(Node::Variable { name: "nth".to_string() }).expect("Failed to add node");
            let arr_i = builder.graph.add_node(Node::Application {
                function: nth_op,
                args: vec![arr, i],
            }).expect("Failed to add node");
            
            // arr[i] = target
            let eq_op = builder.graph.add_node(Node::Variable { name: "=".to_string() }).expect("Failed to add node");
            builder.graph.add_node(Node::Application {
                function: eq_op,
                args: vec![arr_i, target],
            }).expect("Failed to add node")
        }
    );
    
    let mut contract = Contract::new("contains".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.add_postcondition(ContractCondition {
        expression: contract_expr,
        message: Some("Target element must exist in array".to_string()),
        kind: ContractKind::Postcondition,
        span: None,
        blame_label: Some("postcondition".to_string()),
    });
    
    println!("Contract: exists i in indices(arr), arr[i] = target");
    
    #[cfg(feature = "static")]
    verify_contract(&builder.graph, &contract);
}

/// Demo: forall x in arr, min <= x <= max
fn demo_bounded_array() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    let arr = builder.graph.add_node(Node::Variable { name: "arr".to_string() }).expect("Failed to add node");
    let min_val = builder.graph.add_node(Node::Variable { name: "min".to_string() }).expect("Failed to add node");
    let max_val = builder.graph.add_node(Node::Variable { name: "max".to_string() }).expect("Failed to add node");
    
    // Build: forall x in arr, min <= x <= max
    let contract_expr = builder.forall(
        vec![("x", QuantifierDomain::ListElements(arr))],
        |builder, vars| {
            let x = vars["x"];
            
            // min <= x
            let ge_op = builder.graph.add_node(Node::Variable { name: ">=".to_string() }).expect("Failed to add node");
            let x_ge_min = builder.graph.add_node(Node::Application {
                function: ge_op,
                args: vec![x, min_val],
            }).expect("Failed to add node");
            
            // x <= max
            let le_op = builder.graph.add_node(Node::Variable { name: "<=".to_string() }).expect("Failed to add node");
            let x_le_max = builder.graph.add_node(Node::Application {
                function: le_op,
                args: vec![x, max_val],
            }).expect("Failed to add node");
            
            // min <= x and x <= max
            let and_op = builder.graph.add_node(Node::Variable { name: "and".to_string() }).expect("Failed to add node");
            builder.graph.add_node(Node::Application {
                function: and_op,
                args: vec![x_ge_min, x_le_max],
            }).expect("Failed to add node")
        }
    );
    
    let mut contract = Contract::new("bounded_values".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.add_precondition(ContractCondition {
        expression: contract_expr,
        message: Some("All array values must be within bounds".to_string()),
        kind: ContractKind::Precondition,
        span: None,
        blame_label: Some("precondition".to_string()),
    });
    
    println!("Contract: forall x in arr, min <= x <= max");
    
    #[cfg(feature = "static")]
    verify_contract(&builder.graph, &contract);
}

#[cfg(feature = "static")]
fn verify_contract(graph: &Graph, contract: &Contract) {
    let config = Config::new();
    let context = Context::new(&config);
    let mut converter = Z3Converter::new(&context, graph);
    
    // Declare some variables for demonstration
    converter.declare_var("arr", Z3Sort::Int); // Simplified - array as int
    converter.declare_var("target", Z3Sort::Int);
    converter.declare_var("min", Z3Sort::Int);
    converter.declare_var("max", Z3Sort::Int);
    
    println!("  [Note: Z3 verification of quantifiers is simplified in this demo]");
    
    // In a real implementation, we would:
    // 1. Use Z3's array theory for proper array modeling
    // 2. Add axioms about array operations
    // 3. Verify the actual contract conditions
}

#[cfg(not(feature = "static"))]
fn verify_contract(_graph: &Graph, _contract: &Contract) {
    println!("  [Static verification requires 'static' feature]");
}