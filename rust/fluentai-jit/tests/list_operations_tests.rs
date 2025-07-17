//! Tests for JIT list operations using bytecode

use fluentai_jit::JitCompiler;
use fluentai_core::value::Value;
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};

#[test]
fn test_make_list_empty() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create empty list
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 0));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 0);
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_make_list_with_items() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push items for list
    let val1 = chunk.add_constant(Value::Integer(10));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val1));
    
    let val2 = chunk.add_constant(Value::Integer(20));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val2));
    
    let val3 = chunk.add_constant(Value::Integer(30));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val3));
    
    // Create list with 3 items
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 3));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(10));
            assert_eq!(items[1], Value::Integer(20));
            assert_eq!(items[2], Value::Integer(30));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_list_len() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create a list with 3 items
    let val1 = chunk.add_constant(Value::Integer(1));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val1));
    
    let val2 = chunk.add_constant(Value::Integer(2));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val2));
    
    let val3 = chunk.add_constant(Value::Integer(3));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val3));
    
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 3));
    
    // Get list length
    chunk.add_instruction(Instruction::new(Opcode::ListLen));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    assert_eq!(result, Value::Integer(3));
}

#[test]
fn test_list_empty() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create empty list
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 0));
    
    // Check if empty
    chunk.add_instruction(Instruction::new(Opcode::ListEmpty));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    assert_eq!(result, Value::Integer(1)); // True
}

#[test]
fn test_list_not_empty() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create list with one item
    let val = chunk.add_constant(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val));
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 1));
    
    // Check if empty
    chunk.add_instruction(Instruction::new(Opcode::ListEmpty));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    assert_eq!(result, Value::Integer(0)); // False
}

#[test]
fn test_list_head() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create list with items
    let val1 = chunk.add_constant(Value::Integer(100));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val1));
    
    let val2 = chunk.add_constant(Value::Integer(200));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val2));
    
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 2));
    
    // Get head
    chunk.add_instruction(Instruction::new(Opcode::ListHead));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    assert_eq!(result, Value::Integer(100));
}

#[test]
fn test_list_tail() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create list with items
    let val1 = chunk.add_constant(Value::Integer(1));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val1));
    
    let val2 = chunk.add_constant(Value::Integer(2));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val2));
    
    let val3 = chunk.add_constant(Value::Integer(3));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val3));
    
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 3));
    
    // Get tail
    chunk.add_instruction(Instruction::new(Opcode::ListTail));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Integer(2));
            assert_eq!(items[1], Value::Integer(3));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_list_cons() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push element to prepend first
    let new_elem = chunk.add_constant(Value::Integer(1));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, new_elem));
    
    // Create initial list
    let val1 = chunk.add_constant(Value::Integer(2));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val1));
    
    let val2 = chunk.add_constant(Value::Integer(3));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val2));
    
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 2));
    
    // Cons - expects [head, list] with list on top
    chunk.add_instruction(Instruction::new(Opcode::ListCons));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(1));
            assert_eq!(items[1], Value::Integer(2));
            assert_eq!(items[2], Value::Integer(3));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_list_mixed_types() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push different types
    let int_val = chunk.add_constant(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, int_val));
    
    let str_val = chunk.add_constant(Value::String("hello".to_string()));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, str_val));
    
    let float_val = chunk.add_constant(Value::Float(3.14));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, float_val));
    
    // Create list
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 3));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(42));
            assert_eq!(items[1], Value::String("hello".to_string()));
            assert_eq!(items[2], Value::Float(3.14));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}