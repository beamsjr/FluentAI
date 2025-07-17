//! Tests for JIT closure creation and captured variables

use fluentai_jit::JitCompiler;
use fluentai_core::value::Value;
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};

#[test]
fn test_simple_closure_creation() {
    let mut bytecode = Bytecode::new();
    
    // Main chunk
    let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
    
    // Push a value to capture
    let captured_val = main_chunk.add_constant(Value::Integer(42));
    main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, captured_val));
    
    // Create closure that captures one value
    // Pack chunk_id (1) and capture_count (1) into arg
    let closure_arg = (1 << 16) | 1;
    main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeClosure, closure_arg as u32));
    
    // Return the closure
    main_chunk.add_instruction(Instruction::new(Opcode::Return));
    
    // Closure chunk (won't be executed in this test)
    let mut closure_chunk = BytecodeChunk::new(Some("closure".to_string()));
    closure_chunk.add_instruction(Instruction::with_arg(Opcode::LoadCaptured, 0));
    closure_chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(main_chunk);
    bytecode.chunks.push(closure_chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Check that we got a function value
    match result {
        Value::Function { chunk_id, env } => {
            assert_eq!(chunk_id, 1);
            assert_eq!(env.len(), 1);
            assert_eq!(env[0], Value::Integer(42));
        }
        _ => panic!("Expected function value, got {:?}", result),
    }
}

#[test]
fn test_closure_with_multiple_captures() {
    let mut bytecode = Bytecode::new();
    
    // Main chunk
    let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
    
    // Push values to capture
    let val1 = main_chunk.add_constant(Value::Integer(10));
    main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, val1));
    
    let val2 = main_chunk.add_constant(Value::String("hello".to_string()));
    main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, val2));
    
    let val3 = main_chunk.add_constant(Value::Float(3.14));
    main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, val3));
    
    // Create closure that captures three values
    let closure_arg = (1 << 16) | 3;
    main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeClosure, closure_arg as u32));
    
    // Return the closure
    main_chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(main_chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Check that we got a function value with correct captures
    match result {
        Value::Function { chunk_id, env } => {
            assert_eq!(chunk_id, 1);
            assert_eq!(env.len(), 3);
            assert_eq!(env[0], Value::Integer(10));
            assert_eq!(env[1], Value::String("hello".to_string()));
            assert_eq!(env[2], Value::Float(3.14));
        }
        _ => panic!("Expected function value, got {:?}", result),
    }
}

#[test]
fn test_closure_no_captures() {
    let mut bytecode = Bytecode::new();
    
    // Main chunk
    let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
    
    // Create closure with no captures
    let closure_arg = (1 << 16) | 0;
    main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeClosure, closure_arg as u32));
    
    // Return the closure
    main_chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(main_chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Check that we got a function value with no captures
    match result {
        Value::Function { chunk_id, env } => {
            assert_eq!(chunk_id, 1);
            assert_eq!(env.len(), 0);
        }
        _ => panic!("Expected function value, got {:?}", result),
    }
}