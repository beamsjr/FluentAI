//! Tests for JIT channel operations using bytecode

use fluentai_jit::JitCompiler;
use fluentai_core::value::Value;
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};

#[test]
fn test_make_channel() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create a channel
    chunk.add_instruction(Instruction::new(Opcode::Channel));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Check that we got a channel value
    match result {
        Value::Channel(id) => {
            // Channel ID should be non-zero
            assert!(id > 0);
        }
        _ => panic!("Expected channel value, got {:?}", result),
    }
}

#[test]
fn test_channel_send() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create a channel
    chunk.add_instruction(Instruction::new(Opcode::Channel));
    
    // Push a value to send
    let val = chunk.add_constant(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val));
    
    // Send the value to the channel
    chunk.add_instruction(Instruction::new(Opcode::Send));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // The current placeholder implementation returns nil on success
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_channel_receive() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create a channel
    chunk.add_instruction(Instruction::new(Opcode::Channel));
    
    // Try to receive from it
    chunk.add_instruction(Instruction::new(Opcode::Receive));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // The current placeholder implementation returns an error
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "RuntimeError");
            assert!(message.contains("Channel operations not yet fully implemented"));
        }
        _ => panic!("Expected error for receive on placeholder channel"),
    }
}

#[test]
fn test_send_type_error() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push non-channel value
    let val = chunk.add_constant(Value::Integer(123));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val));
    
    // Push value to send
    let msg = chunk.add_constant(Value::String("hello".to_string()));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, msg));
    
    // Try to send
    chunk.add_instruction(Instruction::new(Opcode::Send));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "TypeError");
            assert!(message.contains("Channel send requires a channel"));
        }
        _ => panic!("Expected type error for send to non-channel"),
    }
}

#[test]
fn test_receive_type_error() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push non-channel value
    let val = chunk.add_constant(Value::String("not a channel".to_string()));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val));
    
    // Try to receive from it
    chunk.add_instruction(Instruction::new(Opcode::Receive));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "TypeError");
            assert!(message.contains("Channel receive requires a channel"));
        }
        _ => panic!("Expected type error for receive from non-channel"),
    }
}

#[test]
fn test_multiple_channels() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create first channel
    chunk.add_instruction(Instruction::new(Opcode::Channel));
    
    // Create second channel
    chunk.add_instruction(Instruction::new(Opcode::Channel));
    
    // Return the second channel
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Check that we got a channel value
    match result {
        Value::Channel(id) => {
            // Second channel should have ID > 1
            assert!(id > 1);
        }
        _ => panic!("Expected channel value, got {:?}", result),
    }
}