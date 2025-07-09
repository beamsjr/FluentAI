//! Demonstrates termination checking for recursive contracts

use fluentai_contracts::{
    Contract, ContractCondition, ContractKind, TerminationChecker, TerminationMeasureBuilder,
    TerminationResult,
};
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use std::num::NonZeroU32;

fn main() {
    println!("=== Termination Checking Demo ===\n");

    // Example 1: Non-recursive function
    println!("Example 1: Non-recursive function (always terminates)");
    demo_non_recursive();

    // Example 2: Simple recursive function with decreasing parameter
    println!("\nExample 2: Factorial with decreasing parameter");
    demo_factorial();

    // Example 3: List recursion
    println!("\nExample 3: List length with structural recursion");
    demo_list_length();

    // Example 4: Complex recursion with termination measure
    println!("\nExample 4: Ackermann function with lexicographic measure");
    demo_ackermann();
}

fn demo_non_recursive() {
    let mut graph = Graph::new();

    // Simple add function: (+ x y)
    let x = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let y = graph
        .add_node(Node::Variable {
            name: "y".to_string(),
        })
        .expect("Failed to add node");
    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let body = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![x, y],
        })
        .expect("Failed to add node");

    let mut contract = Contract::new("add".to_string(), body);
    // function_name is already set in Contract::new

    let mut checker = TerminationChecker::new(&graph);
    match checker.analyze_contract(&contract) {
        Ok(result) => print_result(&result),
        Err(e) => println!("  Error: {}", e),
    }
}

fn demo_factorial() {
    let mut graph = Graph::new();

    // Factorial function with recursive structure
    // (if (= n 0) 1 (* n (factorial (- n 1))))
    let n = graph
        .add_node(Node::Variable {
            name: "n".to_string(),
        })
        .expect("Failed to add node");
    let zero = graph
        .add_node(Node::Literal(Literal::Integer(0)))
        .expect("Failed to add node");
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");

    // Base case: (= n 0)
    let eq = graph
        .add_node(Node::Variable {
            name: "=".to_string(),
        })
        .expect("Failed to add node");
    let base_cond = graph
        .add_node(Node::Application {
            function: eq,
            args: vec![n, zero],
        })
        .expect("Failed to add node");

    // Recursive case: (* n (factorial (- n 1)))
    let minus = graph
        .add_node(Node::Variable {
            name: "-".to_string(),
        })
        .expect("Failed to add node");
    let n_minus_1 = graph
        .add_node(Node::Application {
            function: minus,
            args: vec![n, one],
        })
        .expect("Failed to add node");

    let factorial_ref = graph
        .add_node(Node::Variable {
            name: "factorial".to_string(),
        })
        .expect("Failed to add node");
    let recursive_call = graph
        .add_node(Node::Application {
            function: factorial_ref,
            args: vec![n_minus_1],
        })
        .expect("Failed to add node");

    let times = graph
        .add_node(Node::Variable {
            name: "*".to_string(),
        })
        .expect("Failed to add node");
    let recursive_case = graph
        .add_node(Node::Application {
            function: times,
            args: vec![n, recursive_call],
        })
        .expect("Failed to add node");

    let body = graph
        .add_node(Node::If {
            condition: base_cond,
            then_branch: one,
            else_branch: recursive_case,
        })
        .expect("Failed to add node");

    let mut contract = Contract::new("factorial".to_string(), body);
    // function_name is already set in Contract::new

    // Add precondition: n >= 0
    let ge = graph
        .add_node(Node::Variable {
            name: ">=".to_string(),
        })
        .expect("Failed to add node");
    let precond = graph
        .add_node(Node::Application {
            function: ge,
            args: vec![n, zero],
        })
        .expect("Failed to add node");
    contract.add_precondition(
        ContractCondition::new(precond, ContractKind::Precondition)
            .with_blame("n must be non-negative".to_string()),
    );

    // Create termination measure: n decreases
    let measure = {
        let mut measure_builder = TerminationMeasureBuilder::new(&mut graph);
        measure_builder.numeric_measure("n")
    };

    // Create termination checker and add measure
    let mut checker = TerminationChecker::new(&graph);
    checker.add_termination_measure("factorial".to_string(), measure);

    match checker.analyze_contract(&contract) {
        Ok(result) => print_result(&result),
        Err(e) => println!("  Error: {}", e),
    }
}

fn demo_list_length() {
    let mut graph = Graph::new();

    // List length with structural recursion
    // (if (null? lst) 0 (+ 1 (length (cdr lst))))
    let lst = graph
        .add_node(Node::Variable {
            name: "lst".to_string(),
        })
        .expect("Failed to add node");
    let zero = graph
        .add_node(Node::Literal(Literal::Integer(0)))
        .expect("Failed to add node");
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");

    // Base case: (null? lst)
    let null_pred = graph
        .add_node(Node::Variable {
            name: "null?".to_string(),
        })
        .expect("Failed to add node");
    let base_cond = graph
        .add_node(Node::Application {
            function: null_pred,
            args: vec![lst],
        })
        .expect("Failed to add node");

    // Recursive case: (+ 1 (length (cdr lst)))
    let cdr_fn = graph
        .add_node(Node::Variable {
            name: "cdr".to_string(),
        })
        .expect("Failed to add node");
    let cdr_lst = graph
        .add_node(Node::Application {
            function: cdr_fn,
            args: vec![lst],
        })
        .expect("Failed to add node");

    let length_ref = graph
        .add_node(Node::Variable {
            name: "length".to_string(),
        })
        .expect("Failed to add node");
    let recursive_call = graph
        .add_node(Node::Application {
            function: length_ref,
            args: vec![cdr_lst],
        })
        .expect("Failed to add node");

    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let recursive_case = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![one, recursive_call],
        })
        .expect("Failed to add node");

    let body = graph
        .add_node(Node::If {
            condition: base_cond,
            then_branch: zero,
            else_branch: recursive_case,
        })
        .expect("Failed to add node");

    let mut contract = Contract::new("length".to_string(), body);
    // function_name is already set in Contract::new

    // Create termination checker with list measure
    let measure = {
        let mut measure_builder = TerminationMeasureBuilder::new(&mut graph);
        measure_builder.list_length_measure("lst")
    };
    let mut checker = TerminationChecker::new(&graph);
    checker.add_termination_measure("length".to_string(), measure);

    match checker.analyze_contract(&contract) {
        Ok(result) => print_result(&result),
        Err(e) => println!("  Error: {}", e),
    }
}

fn demo_ackermann() {
    let mut graph = Graph::new();

    // Ackermann function - complex recursion requiring lexicographic ordering
    // This is a simplified representation
    let m = graph
        .add_node(Node::Variable {
            name: "m".to_string(),
        })
        .expect("Failed to add node");
    let n = graph
        .add_node(Node::Variable {
            name: "n".to_string(),
        })
        .expect("Failed to add node");

    // For demonstration, create a simple body
    let body = graph
        .add_node(Node::Variable {
            name: "ackermann-body".to_string(),
        })
        .expect("Failed to add node");

    let mut contract = Contract::new("ackermann".to_string(), body);
    // function_name is already set in Contract::new

    // Create lexicographic termination measure (m, n)
    let measure = {
        let mut measure_builder = TerminationMeasureBuilder::new(&mut graph);
        measure_builder.lexicographic_measure(vec![("m", m), ("n", n)])
    };
    let mut checker = TerminationChecker::new(&graph);
    checker.add_termination_measure("ackermann".to_string(), measure);

    println!("  Ackermann function requires lexicographic termination measure (m, n)");
    println!("  where (m', n') < (m, n) in lexicographic ordering");

    match checker.analyze_contract(&contract) {
        Ok(result) => print_result(&result),
        Err(e) => println!("  Error: {}", e),
    }
}

fn print_result(result: &TerminationResult) {
    match result {
        TerminationResult::Terminates { reason } => {
            println!("  ✅ Terminates: {}", reason);
        }
        TerminationResult::MayNotTerminate { reason } => {
            println!("  ⚠️  May not terminate: {}", reason);
        }
        TerminationResult::Unknown { reason } => {
            println!("  ❓ Unknown: {}", reason);
        }
    }
}
