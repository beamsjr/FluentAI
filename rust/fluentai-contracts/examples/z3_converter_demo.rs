//! Demonstrates the expanded Z3 converter with various operators

#[cfg(feature = "static")]
use fluentai_contracts::{
    z3_converter::{Z3Converter, Z3Expr, Z3Sort},
    Contract, ContractCondition, ContractKind,
};
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
#[cfg(feature = "static")]
use z3::{Config, Context, Solver};

#[cfg(feature = "static")]
fn main() {
    println!("=== Z3 Converter Demo ===\n");

    // Create Z3 context
    let config = Config::new();
    let context = Context::new(&config);

    // Create a sample AST graph
    let mut graph = Graph::new();

    // Example 1: Arithmetic with modulo
    println!("Example 1: Verify (x mod 2 = 0) => even?(x)");
    {
        let x = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .expect("Failed to add node");
        let two = graph
            .add_node(Node::Literal(Literal::Integer(2)))
            .expect("Failed to add node");
        let zero = graph
            .add_node(Node::Literal(Literal::Integer(0)))
            .expect("Failed to add node");

        // x mod 2
        let mod_op = graph
            .add_node(Node::Variable {
                name: "mod".to_string(),
            })
            .expect("Failed to add node");
        let x_mod_2 = graph
            .add_node(Node::Application {
                function: mod_op,
                args: vec![x, two],
            })
            .expect("Failed to add node");

        // x mod 2 = 0
        let eq_op = graph
            .add_node(Node::Variable {
                name: "=".to_string(),
            })
            .expect("Failed to add node");
        let is_even_manual = graph
            .add_node(Node::Application {
                function: eq_op,
                args: vec![x_mod_2, zero],
            })
            .expect("Failed to add node");

        // even?(x)
        let even_pred = graph
            .add_node(Node::Variable {
                name: "even?".to_string(),
            })
            .expect("Failed to add node");
        let is_even_builtin = graph
            .add_node(Node::Application {
                function: even_pred,
                args: vec![x],
            })
            .expect("Failed to add node");

        // (x mod 2 = 0) => even?(x)
        let implies_op = graph
            .add_node(Node::Variable {
                name: "implies".to_string(),
            })
            .expect("Failed to add node");
        let implication = graph
            .add_node(Node::Application {
                function: implies_op,
                args: vec![is_even_manual, is_even_builtin],
            })
            .expect("Failed to add node");

        let mut converter = Z3Converter::new(&context, &graph);
        converter.declare_var("x", Z3Sort::Int);

        match converter.convert_node(implication) {
            Ok(Z3Expr::Bool(formula)) => {
                let solver = Solver::new(&context);
                solver.assert(&formula.not()); // Try to find counterexample

                match solver.check() {
                    z3::SatResult::Unsat => println!("✓ Verified: (x mod 2 = 0) => even?(x)"),
                    z3::SatResult::Sat => println!("✗ Found counterexample"),
                    z3::SatResult::Unknown => println!("? Unknown result"),
                }
            }
            _ => println!("Error converting to Z3"),
        }
    }

    println!("\nExample 2: Min/Max properties");
    {
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

        // min(x, y)
        let min_op = graph
            .add_node(Node::Variable {
                name: "min".to_string(),
            })
            .expect("Failed to add node");
        let min_xy = graph
            .add_node(Node::Application {
                function: min_op,
                args: vec![x, y],
            })
            .expect("Failed to add node");

        // min(x, y) <= x
        let le_op = graph
            .add_node(Node::Variable {
                name: "<=".to_string(),
            })
            .expect("Failed to add node");
        let min_le_x = graph
            .add_node(Node::Application {
                function: le_op,
                args: vec![min_xy, x],
            })
            .expect("Failed to add node");

        let mut converter = Z3Converter::new(&context, &graph);
        converter.declare_var("x", Z3Sort::Int);
        converter.declare_var("y", Z3Sort::Int);

        match converter.convert_node(min_le_x) {
            Ok(Z3Expr::Bool(formula)) => {
                let solver = Solver::new(&context);
                solver.assert(&formula.not()); // Try to find counterexample

                match solver.check() {
                    z3::SatResult::Unsat => println!("✓ Verified: min(x, y) <= x"),
                    z3::SatResult::Sat => println!("✗ Found counterexample"),
                    z3::SatResult::Unknown => println!("? Unknown result"),
                }
            }
            _ => println!("Error converting to Z3"),
        }
    }

    println!("\nExample 3: Absolute value properties");
    {
        let x = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .expect("Failed to add node");
        let zero = graph
            .add_node(Node::Literal(Literal::Integer(0)))
            .expect("Failed to add node");

        // abs(x)
        let abs_op = graph
            .add_node(Node::Variable {
                name: "abs".to_string(),
            })
            .expect("Failed to add node");
        let abs_x = graph
            .add_node(Node::Application {
                function: abs_op,
                args: vec![x],
            })
            .expect("Failed to add node");

        // abs(x) >= 0
        let ge_op = graph
            .add_node(Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");
        let abs_non_negative = graph
            .add_node(Node::Application {
                function: ge_op,
                args: vec![abs_x, zero],
            })
            .expect("Failed to add node");

        let mut converter = Z3Converter::new(&context, &graph);
        converter.declare_var("x", Z3Sort::Int);

        match converter.convert_node(abs_non_negative) {
            Ok(Z3Expr::Bool(formula)) => {
                let solver = Solver::new(&context);
                solver.assert(&formula.not()); // Try to find counterexample

                match solver.check() {
                    z3::SatResult::Unsat => println!("✓ Verified: abs(x) >= 0"),
                    z3::SatResult::Sat => println!("✗ Found counterexample"),
                    z3::SatResult::Unknown => println!("? Unknown result"),
                }
            }
            _ => println!("Error converting to Z3"),
        }
    }

    println!("\nExample 4: Complex logical formula");
    {
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
        let zero = graph
            .add_node(Node::Literal(Literal::Integer(0)))
            .expect("Failed to add node");

        // positive?(x)
        let pos_pred = graph
            .add_node(Node::Variable {
                name: "positive?".to_string(),
            })
            .expect("Failed to add node");
        let x_positive = graph
            .add_node(Node::Application {
                function: pos_pred,
                args: vec![x],
            })
            .expect("Failed to add node");

        // negative?(y)
        let neg_pred = graph
            .add_node(Node::Variable {
                name: "negative?".to_string(),
            })
            .expect("Failed to add node");
        let y_negative = graph
            .add_node(Node::Application {
                function: neg_pred,
                args: vec![y],
            })
            .expect("Failed to add node");

        // x + y
        let plus_op = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .expect("Failed to add node");
        let x_plus_y = graph
            .add_node(Node::Application {
                function: plus_op,
                args: vec![x, y],
            })
            .expect("Failed to add node");

        // abs(x) > abs(y)
        let abs_op = graph
            .add_node(Node::Variable {
                name: "abs".to_string(),
            })
            .expect("Failed to add node");
        let abs_x = graph
            .add_node(Node::Application {
                function: abs_op,
                args: vec![x],
            })
            .expect("Failed to add node");
        let abs_y = graph
            .add_node(Node::Application {
                function: abs_op,
                args: vec![y],
            })
            .expect("Failed to add node");
        let gt_op = graph
            .add_node(Node::Variable {
                name: ">".to_string(),
            })
            .expect("Failed to add node");
        let abs_x_gt_abs_y = graph
            .add_node(Node::Application {
                function: gt_op,
                args: vec![abs_x, abs_y],
            })
            .expect("Failed to add node");

        // x + y > 0
        let sum_positive = graph
            .add_node(Node::Application {
                function: gt_op.clone(),
                args: vec![x_plus_y, zero],
            })
            .expect("Failed to add node");

        // (positive?(x) and negative?(y) and abs(x) > abs(y)) => (x + y > 0)
        let and_op = graph
            .add_node(Node::Variable {
                name: "and".to_string(),
            })
            .expect("Failed to add node");
        let cond1 = graph
            .add_node(Node::Application {
                function: and_op,
                args: vec![x_positive, y_negative],
            })
            .expect("Failed to add node");
        let cond2 = graph
            .add_node(Node::Application {
                function: and_op,
                args: vec![cond1, abs_x_gt_abs_y],
            })
            .expect("Failed to add node");
        let implies_op = graph
            .add_node(Node::Variable {
                name: "implies".to_string(),
            })
            .expect("Failed to add node");
        let theorem = graph
            .add_node(Node::Application {
                function: implies_op,
                args: vec![cond2, sum_positive],
            })
            .expect("Failed to add node");

        let mut converter = Z3Converter::new(&context, &graph);
        converter.declare_var("x", Z3Sort::Int);
        converter.declare_var("y", Z3Sort::Int);

        match converter.convert_node(theorem) {
            Ok(Z3Expr::Bool(formula)) => {
                let solver = Solver::new(&context);
                solver.assert(&formula.not()); // Try to find counterexample

                match solver.check() {
                    z3::SatResult::Unsat => {
                        println!(
                            "✓ Verified: (positive?(x) ∧ negative?(y) ∧ |x| > |y|) => (x + y > 0)"
                        )
                    }
                    z3::SatResult::Sat => println!("✗ Found counterexample"),
                    z3::SatResult::Unknown => println!("? Unknown result"),
                }
            }
            _ => println!("Error converting to Z3"),
        }
    }
}

#[cfg(not(feature = "static"))]
fn main() {
    println!("This example requires the 'static' feature to be enabled");
    println!("Run with: cargo run --example z3_converter_demo --features static");
}
