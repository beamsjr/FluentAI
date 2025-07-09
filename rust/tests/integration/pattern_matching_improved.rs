//! Demonstrates the improved pattern matching APIs

use fluentai_core::ast::{Graph, Node, Literal, Pattern};
use fluentai_vm::{compiler::Compiler, Value, VM};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

fn main() -> Result<()> {
    println!("=== Pattern Matching API Improvements Demo ===\n");
    
    // Example 1: Simple value matching
    println!("Example 1: Simple value matching");
    simple_value_matching()?;
    
    // Example 2: List pattern matching
    println!("\nExample 2: List pattern matching");
    list_pattern_matching()?;
    
    // Example 3: Complex pattern matching
    println!("\nExample 3: Complex pattern matching");
    complex_pattern_matching()?;
    
    Ok(())
}

fn simple_value_matching() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a value to match
    let value = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    
    // Create result expressions
    let r1 = graph.add_node(Node::Literal(Literal::String("one".to_string())))?;
    let r2 = graph.add_node(Node::Literal(Literal::String("two".to_string())))?;
    let r3 = graph.add_node(Node::Literal(Literal::String("three".to_string())))?;
    let default = graph.add_node(Node::Literal(Literal::String("other".to_string())))?;
    
    // OLD WAY: Manual construction
    println!("  Old way - manual construction:");
    let _old_match = graph.add_node(Node::Match {
        expr: value,
        branches: vec![
            (Pattern::Literal(Literal::Integer(1)), r1),
            (Pattern::Literal(Literal::Integer(2)), r2),
            (Pattern::Literal(Literal::Integer(3)), r3),
            (Pattern::Wildcard, default),
        ],
    })?;
    println!("    Required: Multiple Pattern::Literal constructions");
    println!("    Required: Manual Pattern::Wildcard for default");
    
    // NEW WAY: Using match builder
    println!("\n  New way - using match builder:");
    let new_match = graph.build_match()
        .expr(value)
        .int_case(1, r1)
        .int_case(2, r2)
        .int_case(3, r3)
        .default(default)
        .build()?;
    println!("    Simple method calls with clear intent");
    
    // Or using the helper function
    println!("\n  Even simpler - using match_value helper:");
    let helper_match = graph.match_value(
        value,
        vec![
            (Literal::Integer(1), r1),
            (Literal::Integer(2), r2),
            (Literal::Integer(3), r3),
        ],
        default,
    )?;
    println!("    Just provide a vector of cases!");
    
    // Run one of them to verify it works
    graph.root_id = Some(new_match);
    let result = compile_and_run(&graph)?;
    println!("\n  Result: {:?}", result);
    
    Ok(())
}

fn list_pattern_matching() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a list to match
    let one = graph.add_node(Node::Literal(Literal::Integer(1)))?;
    let two = graph.add_node(Node::Literal(Literal::Integer(2)))?;
    let three = graph.add_node(Node::Literal(Literal::Integer(3)))?;
    let list = graph.add_node(Node::List(vec![one, two, three]))?;
    
    let empty_result = graph.add_node(Node::Literal(Literal::String("empty".to_string())))?;
    
    // OLD WAY: Complex constructor patterns
    println!("  Old way - manual constructor patterns:");
    let head_var = graph.add_node(Node::Variable { name: "head".to_string() })?;
    let _old_match = graph.add_node(Node::Match {
        expr: list,
        branches: vec![
            (Pattern::Constructor {
                name: "nil".to_string(),
                patterns: vec![],
            }, empty_result),
            (Pattern::Constructor {
                name: "cons".to_string(),
                patterns: vec![
                    Pattern::Variable("head".to_string()),
                    Pattern::Variable("tail".to_string()),
                ],
            }, head_var),
        ],
    })?;
    println!("    Required: Manual Pattern::Constructor creation");
    println!("    Required: String allocation for pattern names");
    println!("    Required: Vector construction for sub-patterns");
    
    // NEW WAY: Using pattern builders
    println!("\n  New way - using pattern builders:");
    let new_match = graph.build_match()
        .expr(list)
        .branch(Pattern::nil(), empty_result)
        .branch(Pattern::cons(Pattern::var("head"), Pattern::var("tail")), head_var)
        .build()?;
    println!("    Clean Pattern::nil() and Pattern::cons() methods");
    println!("    Readable Pattern::var() for variables");
    
    // EVEN NEWER WAY: Using match_list helper
    println!("\n  Even simpler - using match_list helper:");
    let helper_match = graph.match_list(
        list,
        empty_result,
        |_g, head, _tail| Ok(head),
    )?;
    println!("    Just provide empty case and a closure for cons case!");
    
    // Run one of them
    graph.root_id = Some(helper_match);
    let result = compile_and_run(&graph)?;
    println!("\n  Result (first element): {:?}", result);
    
    Ok(())
}

fn complex_pattern_matching() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a tagged value
    let x_val = graph.add_node(Node::Literal(Literal::Integer(10)))?;
    let y_val = graph.add_node(Node::Literal(Literal::Integer(20)))?;
    let point_ctor = graph.add_node(Node::Variable { name: "Point".to_string() })?;
    let point = graph.add_node(Node::Application {
        function: point_ctor,
        args: vec![x_val, y_val],
    })?;
    
    // Results
    let origin = graph.add_node(Node::Literal(Literal::String("origin".to_string())))?;
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() })?;
    let other = graph.add_node(Node::Literal(Literal::String("other point".to_string())))?;
    
    // Using the new API for complex patterns
    println!("  Complex pattern matching with custom constructors:");
    let match_node = graph.build_match()
        .expr(point)
        .branch(
            Pattern::constructor("Point", vec![Pattern::int(0), Pattern::int(0)]),
            origin,
        )
        .branch(
            Pattern::constructor("Point", vec![Pattern::var("x"), Pattern::int(0)]),
            x_var,
        )
        .default(other)
        .build()?;
    println!("    Easy to build complex constructor patterns");
    println!("    Pattern::constructor() handles the details");
    
    graph.root_id = Some(match_node);
    
    Ok(())
}

fn compile_and_run(graph: &Graph) -> Result<Value> {
    let compiler = Compiler::new();
    let bytecode = compiler.compile(graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}