use fluentai_parser::parse_flc;
use fluentai_optimizer::OptimizationLevel;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, Opcode};

#[test]
fn test_optimizer_let_closure_simple() {
    // Simple test: let binding with lambda that captures the variable
    let source = r#"
        let ch = channel();
        (() => send(ch, 42))
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Compile without optimization
    let mut options = CompilerOptions::default();
    options.optimization_level = OptimizationLevel::None;
    let compiler = Compiler::with_options(options);
    let bytecode_no_opt = compiler.compile(&graph).unwrap();
    
    // Compile with optimization
    let compiler_opt = Compiler::new();
    let bytecode_opt = compiler_opt.compile(&graph).unwrap();
    
    println!("\n=== Simple let closure test ===");
    println!("Without optimization:");
    for (i, chunk) in bytecode_no_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    println!("\nWith optimization:");
    for (i, chunk) in bytecode_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    // Check that Channel instruction exists
    let main_chunk_opt = &bytecode_opt.chunks[0];
    assert!(
        main_chunk_opt.instructions.iter().any(|i| i.opcode == Opcode::Channel),
        "Channel instruction was removed by optimizer"
    );
    
    // Check that MakeClosure is used (not MakeFunc)
    assert!(
        main_chunk_opt.instructions.iter().any(|i| i.opcode == Opcode::MakeClosure),
        "MakeClosure was replaced with MakeFunc by optimizer"
    );
}

#[test]
fn test_optimizer_nested_let_closure() {
    // More complex: nested let with closure capturing outer variable
    // The lambda should NOT be inlined because it's not immediately applied
    let source = r#"
        let x = 10;
        let f = () => x + 1;
        f  // Return the function itself, not apply it
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Compile with optimization
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    println!("\n=== Nested let closure test ===");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    // The lambda should capture x, so it should use MakeClosure
    let main_chunk = &bytecode.chunks[0];
    assert!(
        main_chunk.instructions.iter().any(|i| i.opcode == Opcode::MakeClosure),
        "Lambda capturing outer variable should use MakeClosure"
    );
}

#[test]
fn test_optimizer_spawn_closure() {
    // Test the exact pattern from the bug report
    // Note: This test captures a channel in a spawned closure
    let source = r#"
        let ch = channel();
        let lambda = () => send(ch, 42);
        lambda
    "#;
    
    let graph = parse_flc(source).map_err(|e| {
        println!("Parse error: {:?}", e);
        e
    }).unwrap();
    
    // Compile without optimization
    let mut options = CompilerOptions::default();
    options.optimization_level = OptimizationLevel::None;
    let compiler_no_opt = Compiler::with_options(options);
    let bytecode_no_opt = compiler_no_opt.compile(&graph).unwrap();
    
    // Compile with optimization
    let compiler_opt = Compiler::new();
    let bytecode_opt = compiler_opt.compile(&graph).unwrap();
    
    println!("\n=== Spawn closure test ===");
    println!("Without optimization:");
    for (i, chunk) in bytecode_no_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    println!("\nWith optimization:");
    for (i, chunk) in bytecode_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    // Verify the bytecode structure
    let main_chunk_opt = &bytecode_opt.chunks[0];
    
    // 1. Channel should be created
    assert!(
        main_chunk_opt.instructions.iter().any(|i| i.opcode == Opcode::Channel),
        "Channel instruction missing"
    );
    
    // 2. Should use MakeClosure for the lambda
    assert!(
        main_chunk_opt.instructions.iter().any(|i| i.opcode == Opcode::MakeClosure),
        "Should use MakeClosure for lambda with captured variable"
    );
    
    // 3. Lambda chunk should use LoadCaptured for the channel
    let mut found_lambda_with_captured = false;
    for i in 1..bytecode_opt.chunks.len() {
        let chunk = &bytecode_opt.chunks[i];
        if chunk.instructions.iter().any(|instr| instr.opcode == Opcode::LoadCaptured) {
            found_lambda_with_captured = true;
            break;
        }
    }
    assert!(
        found_lambda_with_captured,
        "Lambda should use LoadCaptured for captured channel variable"
    );
}