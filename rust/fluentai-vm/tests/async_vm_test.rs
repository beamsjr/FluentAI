//! Test async VM functionality

use fluentai_vm::{Bytecode, VM};
use fluentai_vm::async_vm::AsyncVM;
use fluentai_vm::bytecode::{BytecodeChunk, Instruction, Opcode};
use fluentai_core::value::Value;

#[tokio::test]
async fn test_async_vm_basic_execution() {
    // Create bytecode that pushes and returns a value
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push value 42
    chunk.constants.push(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Create VM and async wrapper
    let vm = VM::new(bytecode);
    let mut async_vm = AsyncVM::new(vm);
    
    // Run asynchronously
    let result = async_vm.run().await.expect("VM execution failed");
    
    assert_eq!(result, Value::Integer(42));
}

#[tokio::test]
async fn test_async_vm_simple_operations() {
    // Create bytecode that does simple operations
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push two values and add them
    chunk.constants.push(Value::Integer(10));
    chunk.constants.push(Value::Integer(20));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
    chunk.add_instruction(Instruction::new(Opcode::Add));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Create VM and async wrapper
    let vm = VM::new(bytecode);
    let mut async_vm = AsyncVM::new(vm);
    
    // Run asynchronously
    let result = async_vm.run().await.expect("VM execution failed");
    
    assert_eq!(result, Value::Integer(30));
}

#[tokio::test]
async fn test_async_vm_promise_operations() {
    // Create bytecode that spawns a task and awaits it
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test_main".to_string()));
    
    // Create a simple function that returns 456
    let mut func_chunk = BytecodeChunk::new(Some("spawned_func".to_string()));
    func_chunk.constants.push(Value::Integer(456));
    func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    func_chunk.add_instruction(Instruction::new(Opcode::Return));
    let func_chunk_id = 1;
    
    // Main chunk: create closure and spawn it
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeClosure, func_chunk_id << 16));
    chunk.add_instruction(Instruction::new(Opcode::Spawn));
    
    // Await the promise
    chunk.add_instruction(Instruction::new(Opcode::Await));
    
    // Return the result
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.chunks.push(func_chunk);
    bytecode.main_chunk = 0;
    
    // Create VM and async wrapper
    let vm = VM::new(bytecode);
    let mut async_vm = AsyncVM::new(vm);
    
    // Run asynchronously
    let result = async_vm.run().await.expect("VM execution failed");
    
    // For now, spawn returns a placeholder promise, so we expect Nil
    assert_eq!(result, Value::Nil);
}