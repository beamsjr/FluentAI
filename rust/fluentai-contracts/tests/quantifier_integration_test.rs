//! Integration tests for quantifier support

use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
    QuantifierBuilder, QuantifierDomain,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use std::num::NonZeroU32;

#[test]
fn test_forall_quantifier() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    // Create array variable
    let arr = builder.graph.add_node(Node::Variable { name: "arr".to_string() });
    
    // Build: forall i in indices(arr), arr[i] > 0
    let quantified = builder.forall(
        vec![("i", QuantifierDomain::ListIndices(arr))],
        |builder, vars| {
            let i = vars["i"];
            let zero = builder.graph.add_node(Node::Literal(Literal::Integer(0)));
            
            // nth(arr, i)
            let nth_op = builder.graph.add_node(Node::Variable { name: "nth".to_string() });
            let arr_i = builder.graph.add_node(Node::Application {
                function: nth_op,
                args: vec![arr, i],
            });
            
            // arr[i] > 0
            let gt_op = builder.graph.add_node(Node::Variable { name: ">".to_string() });
            builder.graph.add_node(Node::Application {
                function: gt_op,
                args: vec![arr_i, zero],
            })
        }
    );
    
    // Create contract with the quantified expression
    let mut contract = Contract::new(
        "all_positive".to_string(),
        NodeId(NonZeroU32::new(1).unwrap())
    );
    
    contract.add_postcondition(
        ContractCondition::new(quantified, ContractKind::Postcondition)
            .with_message("All elements must be positive".to_string())
    );
    
    assert_eq!(contract.postconditions.len(), 1);
    assert_eq!(contract.postconditions[0].message.as_ref().unwrap(), "All elements must be positive");
}

#[test]
fn test_exists_quantifier() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    // Create variables
    let lst = builder.graph.add_node(Node::Variable { name: "lst".to_string() });
    let target = builder.graph.add_node(Node::Variable { name: "target".to_string() });
    
    // Build: exists x in lst, x = target
    let quantified = builder.exists(
        vec![("x", QuantifierDomain::ListElements(lst))],
        |builder, vars| {
            let x = vars["x"];
            let eq_op = builder.graph.add_node(Node::Variable { name: "=".to_string() });
            builder.graph.add_node(Node::Application {
                function: eq_op,
                args: vec![x, target],
            })
        }
    );
    
    // Create contract
    let mut contract = Contract::new(
        "contains".to_string(),
        NodeId(NonZeroU32::new(1).unwrap())
    );
    
    contract.add_precondition(
        ContractCondition::new(quantified, ContractKind::Precondition)
            .with_blame("Target must exist in list".to_string())
    );
    
    assert_eq!(contract.preconditions.len(), 1);
}

#[test]
fn test_nested_quantifiers() {
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    // Build: forall x in [0, 10], exists y in [0, 10], y > x
    let quantified = builder.forall(
        vec![("x", QuantifierDomain::IntRange(0, 10))],
        |outer_builder, outer_vars| {
            let x = outer_vars["x"];
            
            outer_builder.exists(
                vec![("y", QuantifierDomain::IntRange(0, 10))],
                move |inner_builder, inner_vars| {
                    let y = inner_vars["y"];
                    let gt_op = inner_builder.graph.add_node(Node::Variable { name: ">".to_string() });
                    inner_builder.graph.add_node(Node::Application {
                        function: gt_op,
                        args: vec![y, x],
                    })
                }
            )
        }
    );
    
    // Verify the expression was created
    assert_ne!(quantified, NodeId(NonZeroU32::new(1).unwrap()));
}