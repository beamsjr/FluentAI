//! Example demonstrating proof generation for contracts
use fluentai_parser::parse_flc;


use fluentai_contracts::{
    AdvancedProofGenerator, AdvancedProofStrategy, Contract, ContractCondition, ContractKind,
    Lemma, ProofFormula,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Direct proof using symbolic execution
    println!("=== Example 1: Direct Proof for Absolute Value ===\n");
    {
        let program = r#"
            (define (abs x)
              (if (< x 0)
                  (- 0 x)
                  x))
        "#;

        let mut graph = parse_flc(program)?;

        // Create a contract: result >= 0
        let ge_fn = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");
        let result_var = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "result".to_string(),
            })
            .expect("Failed to add node");
        let zero = graph
            .add_node(fluentai_core::ast::Node::Literal(
                fluentai_core::ast::Literal::Integer(0),
            ))
            .expect("Failed to add node");

        let postcond_expr = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: ge_fn,
                args: vec![result_var, zero],
            })
            .expect("Failed to add node");

        let contract = Contract {
            function_name: "abs".to_string(),
            preconditions: vec![],
            postconditions: vec![ContractCondition {
                expression: postcond_expr,
                message: Some("Result must be non-negative".to_string()),
                kind: ContractKind::Postcondition,
                span: None,
                blame_label: None,
            }],
            invariants: vec![],
            complexity: None,
            pure: true,
            frame_condition: None,
            node_id: postcond_expr, // dummy
        };

        let generator = AdvancedProofGenerator::new();
        let proof =
            generator.generate_proof(&graph, &contract, Some(AdvancedProofStrategy::Direct))?;

        println!("{}", proof.format());
    }

    // Example 2: Proof by induction
    println!("\n=== Example 2: Induction Proof for Factorial ===\n");
    {
        let program = r#"
            (define (factorial n)
              (if (= n 0)
                  1
                  (* n (factorial (- n 1)))))
        "#;

        let mut graph = parse_flc(program)?;

        // Contract: factorial(n) >= 1 for all n >= 0
        let ge_fn = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");
        let result_var = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "result".to_string(),
            })
            .expect("Failed to add node");
        let one = graph
            .add_node(fluentai_core::ast::Node::Literal(
                fluentai_core::ast::Literal::Integer(1),
            ))
            .expect("Failed to add node");

        let postcond_expr = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: ge_fn,
                args: vec![result_var, one],
            })
            .expect("Failed to add node");

        // Precondition: n >= 0
        let n_var = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "n".to_string(),
            })
            .expect("Failed to add node");
        let zero = graph
            .add_node(fluentai_core::ast::Node::Literal(
                fluentai_core::ast::Literal::Integer(0),
            ))
            .expect("Failed to add node");
        let ge_fn2 = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");

        let precond_expr = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: ge_fn2,
                args: vec![n_var, zero],
            })
            .expect("Failed to add node");

        let contract = Contract {
            function_name: "factorial".to_string(),
            preconditions: vec![ContractCondition {
                expression: precond_expr,
                message: Some("n must be non-negative".to_string()),
                kind: ContractKind::Precondition,
                span: None,
                blame_label: None,
            }],
            postconditions: vec![ContractCondition {
                expression: postcond_expr,
                message: Some("Result must be positive".to_string()),
                kind: ContractKind::Postcondition,
                span: None,
                blame_label: None,
            }],
            invariants: vec![],
            complexity: Some("O(n)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: postcond_expr,
        };

        let generator = AdvancedProofGenerator::new();
        let proof = generator.generate_proof(
            &graph,
            &contract,
            Some(AdvancedProofStrategy::Induction {
                induction_var: Some("n".to_string()),
            }),
        )?;

        println!("{}", proof.format());
    }

    // Example 3: Bounded Model Checking
    println!("\n=== Example 3: Bounded Model Checking ===\n");
    {
        let program = r#"
            (define (sum-to-n n)
              (if (= n 0)
                  0
                  (+ n (sum-to-n (- n 1)))))
        "#;

        let mut graph = parse_flc(program)?;

        // Contract: sum-to-n(n) = n*(n+1)/2
        // For simplicity, we'll check sum-to-n(n) >= 0
        let ge_fn = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");
        let result_var = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "result".to_string(),
            })
            .expect("Failed to add node");
        let zero = graph
            .add_node(fluentai_core::ast::Node::Literal(
                fluentai_core::ast::Literal::Integer(0),
            ))
            .expect("Failed to add node");

        let postcond_expr = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: ge_fn,
                args: vec![result_var, zero],
            })
            .expect("Failed to add node");

        let contract = Contract {
            function_name: "sum-to-n".to_string(),
            preconditions: vec![],
            postconditions: vec![ContractCondition {
                expression: postcond_expr,
                message: Some("Sum must be non-negative".to_string()),
                kind: ContractKind::Postcondition,
                span: None,
                blame_label: None,
            }],
            invariants: vec![],
            complexity: Some("O(n)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: postcond_expr,
        };

        let generator = AdvancedProofGenerator::new();
        let proof = generator.generate_proof(
            &graph,
            &contract,
            Some(AdvancedProofStrategy::BoundedModelChecking { bound: 5 }),
        )?;

        println!("{}", proof.format());
    }

    // Example 4: Using lemmas
    println!("\n=== Example 4: Proof with Lemmas ===\n");
    {
        // Create a lemma about positive multiplication
        let lemma = Lemma {
            name: "positive_mult".to_string(),
            statement: ProofFormula::ForAll {
                var: "x".to_string(),
                body: Box::new(ProofFormula::ForAll {
                    var: "y".to_string(),
                    body: Box::new(ProofFormula::Implies(
                        Box::new(ProofFormula::And(
                            Box::new(ProofFormula::LessThan(
                                Box::new(ProofFormula::Atom("0".to_string())),
                                Box::new(ProofFormula::Atom("x".to_string())),
                            )),
                            Box::new(ProofFormula::LessThan(
                                Box::new(ProofFormula::Atom("0".to_string())),
                                Box::new(ProofFormula::Atom("y".to_string())),
                            )),
                        )),
                        Box::new(ProofFormula::LessThan(
                            Box::new(ProofFormula::Atom("0".to_string())),
                            Box::new(ProofFormula::Apply {
                                func: "*".to_string(),
                                args: vec![
                                    ProofFormula::Atom("x".to_string()),
                                    ProofFormula::Atom("y".to_string()),
                                ],
                            }),
                        )),
                    )),
                }),
            },
            proof: None, // Could provide actual proof
        };

        let mut generator = AdvancedProofGenerator::new();
        generator.add_lemma(lemma);

        println!("Added lemma: ∀x,y. (x > 0 ∧ y > 0) → (x * y > 0)");
        println!("This lemma can now be used in subsequent proofs.");
    }

    // Example 5: Automated proof (if Z3 is available)
    println!("\n=== Example 5: Automated Proof ===\n");
    {
        let program = r#"
            (define (max a b)
              (if (> a b) a b))
        "#;

        let mut graph = parse_flc(program)?;

        // Contract: result >= a AND result >= b
        let ge_fn1 = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");
        let ge_fn2 = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: ">=".to_string(),
            })
            .expect("Failed to add node");
        let and_fn = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "and".to_string(),
            })
            .expect("Failed to add node");
        let result_var1 = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "result".to_string(),
            })
            .expect("Failed to add node");
        let result_var2 = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "result".to_string(),
            })
            .expect("Failed to add node");
        let a_var = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "a".to_string(),
            })
            .expect("Failed to add node");
        let b_var = graph
            .add_node(fluentai_core::ast::Node::Variable {
                name: "b".to_string(),
            })
            .expect("Failed to add node");

        let cond1 = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: ge_fn1,
                args: vec![result_var1, a_var],
            })
            .expect("Failed to add node");

        let cond2 = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: ge_fn2,
                args: vec![result_var2, b_var],
            })
            .expect("Failed to add node");

        let postcond_expr = graph
            .add_node(fluentai_core::ast::Node::Application {
                function: and_fn,
                args: vec![cond1, cond2],
            })
            .expect("Failed to add node");

        let contract = Contract {
            function_name: "max".to_string(),
            preconditions: vec![],
            postconditions: vec![ContractCondition {
                expression: postcond_expr,
                message: Some("Result must be >= both arguments".to_string()),
                kind: ContractKind::Postcondition,
                span: None,
                blame_label: None,
            }],
            invariants: vec![],
            complexity: Some("O(1)".to_string()),
            pure: true,
            frame_condition: None,
            node_id: postcond_expr,
        };

        let generator = AdvancedProofGenerator::new();
        let proof =
            generator.generate_proof(&graph, &contract, Some(AdvancedProofStrategy::Automated))?;

        println!("{}", proof.format());
    }

    Ok(())
}
