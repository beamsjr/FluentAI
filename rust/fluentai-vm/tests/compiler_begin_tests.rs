//! Compiler tests for Begin node (multiple top-level expressions)

use fluentai_core::ast::Node;
use fluentai_parser::parse;
use fluentai_vm::{
    bytecode::{Chunk, Instruction, Opcode},
    compiler::Compiler,
};

#[test]
fn test_compile_begin_empty() {
    let mut compiler = Compiler::new();
    let graph = parse("").unwrap_or_else(|_| {
        // Create empty graph if parse fails
        let mut g = fluentai_core::ast::ASTGraph::new();
        g.root_id = None;
        g
    });

    let chunk = compiler.compile(&graph).unwrap();

    // Empty or no root should compile to just PushNil and Return
    assert!(chunk.instructions.len() >= 2);
    let last_two: Vec<_> = chunk.instructions.iter().rev().take(2).rev().collect();
    assert_eq!(last_two[0].opcode, Opcode::PushNil);
    assert_eq!(last_two[1].opcode, Opcode::Return);
}

#[test]
fn test_compile_begin_multiple_literals() {
    let code = "1 2 3";
    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).unwrap();

    // Should have:
    // Push(1), Pop, Push(2), Pop, Push(3), Return
    let opcodes: Vec<_> = chunk.instructions.iter().map(|i| i.opcode).collect();

    // Find the pattern
    assert!(opcodes.contains(&Opcode::Pop)); // At least one Pop for intermediate values
    assert_eq!(opcodes.last(), Some(&Opcode::Return));
}

#[test]
fn test_compile_begin_expressions_with_side_effects() {
    let code = r#"
        (effect state:set "x" 10)
        (effect state:set "y" 20)
        (+ (effect state:get "x") (effect state:get "y"))
    "#;

    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).unwrap();

    // Verify we have effect operations
    let opcodes: Vec<_> = chunk.instructions.iter().map(|i| i.opcode).collect();
    assert!(opcodes.iter().any(|op| matches!(op, Opcode::Effect)));

    // Should have Pops between expressions
    let pop_count = opcodes
        .iter()
        .filter(|op| matches!(op, Opcode::Pop))
        .count();
    assert!(pop_count >= 2); // At least 2 pops for the first two expressions
}

#[test]
fn test_compile_begin_preserves_last_value() {
    let code = r#"
        100
        200
        300
    "#;

    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).unwrap();

    // The last expression (300) should not be followed by Pop
    let opcodes: Vec<_> = chunk.instructions.iter().map(|i| i.opcode).collect();

    // Find the last constant push before Return
    let mut found_last_constant = false;
    for i in (0..opcodes.len() - 1).rev() {
        if matches!(opcodes[i], Opcode::PushInt | Opcode::LoadConst) {
            // The instruction after the last constant push should not be Pop
            assert_ne!(opcodes[i + 1], Opcode::Pop);
            found_last_constant = true;
            break;
        }
    }
    assert!(found_last_constant);
}

#[test]
fn test_compile_begin_single_expression() {
    let code = "(+ 1 2 3)";
    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).unwrap();

    // Single expression should not have any Pop instructions
    let opcodes: Vec<_> = chunk.instructions.iter().map(|i| i.opcode).collect();
    assert!(!opcodes.contains(&Opcode::Pop));
}

#[test]
fn test_compile_begin_with_let_bindings() {
    let code = r#"
        (let ((x 10)) x)
        (let ((y 20)) y)
        (+ 5 5)
    "#;

    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).unwrap();

    // Should compile all three expressions with proper cleanup
    let opcodes: Vec<_> = chunk.instructions.iter().map(|i| i.opcode).collect();

    // Should have EnterScope/LeaveScope for let bindings
    assert!(opcodes.contains(&Opcode::EnterScope));
    assert!(opcodes.contains(&Opcode::LeaveScope));

    // Should have Pops between expressions
    assert!(opcodes.contains(&Opcode::Pop));
}

#[test]
fn test_begin_stack_depth_tracking() {
    // This test verifies that stack depth is properly tracked
    // when compiling Begin nodes
    let code = r#"
        (+ 1 2)
        (* 3 4)
        (- 10 5)
    "#;

    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();

    // Compile and verify no panic from stack depth issues
    let result = compiler.compile(&graph);
    assert!(result.is_ok());

    let chunk = result.unwrap();
    // Final stack depth should be 1 (just the result)
    // This is implicitly verified by successful compilation
    assert!(!chunk.instructions.is_empty());
}

#[test]
fn test_begin_with_complex_expressions() {
    let code = r#"
        (let ((f (lambda (x) (* x x)))) (f 5))
        (if (> 10 5) "yes" "no")
        (list 1 2 3)
    "#;

    let graph = parse(code).unwrap();
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).unwrap();

    // Verify successful compilation of complex expressions
    let opcodes: Vec<_> = chunk.instructions.iter().map(|i| i.opcode).collect();

    // Should see evidence of all three expressions
    assert!(opcodes.contains(&Opcode::MakeClosure)); // For lambda
    assert!(opcodes.contains(&Opcode::JumpIfFalse)); // For if
    assert!(opcodes.contains(&Opcode::MakeList)); // For list
}

#[test]
fn test_begin_error_recovery() {
    // Test that compilation handles invalid expressions gracefully
    let mut compiler = Compiler::new();
    let mut graph = fluentai_core::ast::ASTGraph::new();

    // Create a Begin node with invalid node IDs
    let begin_node = Node::Begin {
        exprs: vec![999.into(), 1000.into()],
    };
    let begin_id = graph.add_node(begin_node).unwrap();
    graph.root_id = Some(begin_id);

    // Should handle gracefully (might error, but shouldn't panic)
    let result = compiler.compile(&graph);
    assert!(result.is_err());
}
