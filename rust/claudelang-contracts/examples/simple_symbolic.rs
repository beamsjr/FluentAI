//! Simple example demonstrating symbolic execution

use claudelang_parser::parse;
use claudelang_contracts::{SymbolicExecutor, SymbolicValue};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Simple arithmetic function with branches
    println!("=== Example 1: Absolute Value Function ===");
    {
        let program = r#"
            (define (abs x)
              (if (< x 0)
                  (- 0 x)
                  x))
        "#;
        
        let graph = parse(program)?;
        
        // Create symbolic executor
        let executor = SymbolicExecutor::new();
        
        // Execute symbolically
        let states = executor.execute_function_by_name(&graph, "abs")?;
        
        println!("Explored {} execution paths", states.len());
        
        for (i, state) in states.iter().enumerate() {
            println!("\nPath {}:", i + 1);
            println!("  Path constraints:");
            for constraint in &state.path_constraints {
                println!("    {:?} should be {}", constraint.constraint, constraint.expected);
            }
            println!("  Final bindings:");
            for (var, value) in &state.bindings {
                println!("    {} = {:?}", var, value);
            }
        }
    }
    
    // Example 2: Function with multiple branches
    println!("\n\n=== Example 2: Sign Function ===");
    {
        let program = r#"
            (define (sign x)
              (if (< x 0)
                  -1
                  (if (> x 0)
                      1
                      0)))
        "#;
        
        let graph = parse(program)?;
        let executor = SymbolicExecutor::new();
        let states = executor.execute_function_by_name(&graph, "sign")?;
        
        println!("Explored {} execution paths", states.len());
        
        for (i, state) in states.iter().enumerate() {
            println!("\nPath {}:", i + 1);
            println!("  Path constraints:");
            for constraint in &state.path_constraints {
                match &constraint.constraint {
                    SymbolicValue::BinOp { op, left, right } => {
                        println!("    {:?} {} {:?} should be {}", left, op, right, constraint.expected);
                    }
                    _ => println!("    {:?} should be {}", constraint.constraint, constraint.expected),
                }
            }
        }
    }
    
    // Example 3: Max function
    println!("\n\n=== Example 3: Max Function ===");
    {
        let program = r#"
            (define (max a b)
              (if (> a b)
                  a
                  b))
        "#;
        
        let graph = parse(program)?;
        let executor = SymbolicExecutor::new();
        let states = executor.execute_function_by_name(&graph, "max")?;
        
        println!("Explored {} execution paths", states.len());
        
        for (i, state) in states.iter().enumerate() {
            println!("\nPath {}:", i + 1);
            
            // Show constraints
            for constraint in &state.path_constraints {
                match &constraint.constraint {
                    SymbolicValue::BinOp { op, left, right } => {
                        let left_str = match left.as_ref() {
                            SymbolicValue::Symbolic(s) => s.clone(),
                            _ => format!("{:?}", left),
                        };
                        let right_str = match right.as_ref() {
                            SymbolicValue::Symbolic(s) => s.clone(),
                            _ => format!("{:?}", right),
                        };
                        println!("  Constraint: {} {} {} = {}", left_str, op, right_str, constraint.expected);
                    }
                    _ => println!("  Constraint: {:?} = {}", constraint.constraint, constraint.expected),
                }
            }
        }
    }
    
    Ok(())
}