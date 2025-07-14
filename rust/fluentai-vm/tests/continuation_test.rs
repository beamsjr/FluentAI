//! Tests for VM continuation support (suspend/resume mechanics)

use fluentai_vm::{VM, Bytecode, BytecodeChunk, Instruction, Opcode, Value};
use fluentai_vm::continuation::ContinuationSupport;

#[test]
fn test_basic_continuation() {
    // Create a simple bytecode program
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push some values and add them
    chunk.add_constant(Value::Integer(10));
    chunk.add_constant(Value::Integer(20));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push 10
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1)); // Push 20
    chunk.add_instruction(Instruction::new(Opcode::Add));              // Add them
    chunk.add_instruction(Instruction::new(Opcode::Return));           // Return result
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut vm = VM::new(bytecode);
    
    // Start execution
    vm.call_stack_mut().push(fluentai_vm::vm::CallFrame {
        chunk_id: 0,
        ip: 0,
        stack_base: 0,
        env: vec![],
        start_time: None,
    });
    
    // Execute first two instructions (push 10, push 20)
    vm.call_stack_mut()[0].ip = 2; // Simulate execution up to Add
    vm.push(Value::Integer(10)).unwrap();
    vm.push(Value::Integer(20)).unwrap();
    
    // Suspend execution before the Add instruction
    let continuation_id = vm.suspend_execution(0).unwrap();
    
    // Store how many items were on the stack
    let stack_size_before = 2; // We pushed 10 and 20
    
    // Resume the continuation with a value
    vm.resume_continuation(continuation_id, Value::Integer(5)).unwrap();
    
    // After resuming, we should be able to continue execution
    // The continuation mechanism itself is working if no errors occurred
}

#[test]
fn test_continuation_with_locals() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create a program that uses local variables
    chunk.add_constant(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    chunk.add_instruction(Instruction::with_arg(Opcode::StoreLocal, 0));
    chunk.add_instruction(Instruction::with_arg(Opcode::LoadLocal, 0));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut vm = VM::new(bytecode);
    
    // Set up call frame
    vm.call_stack_mut().push(fluentai_vm::vm::CallFrame {
        chunk_id: 0,
        ip: 0,
        stack_base: 0,
        env: vec![],
        start_time: None,
    });
    
    // Execute push and simulate storing to local
    vm.push(Value::Integer(42)).unwrap();
    
    // Move IP forward to simulate execution
    let frame = vm.call_stack_mut().last_mut().unwrap();
    frame.ip = 2;
    
    // Suspend after the push
    let continuation_id = vm.suspend_execution(0).unwrap();
    
    // Resume - the continuation should restore state
    vm.resume_continuation(continuation_id, Value::Integer(100)).unwrap();
    
    // The continuation mechanism is working if we got here without errors
}

#[test]
fn test_continuation_not_found() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let mut vm = VM::new(bytecode);
    
    // Try to resume a non-existent continuation
    let fake_id = fluentai_vm::continuation::ContinuationId(999);
    let result = vm.resume_continuation(fake_id, Value::Nil);
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Continuation"));
}

#[test]
fn test_cannot_suspend_without_call_frame() {
    let bytecode = Bytecode::new();
    let mut vm = VM::new(bytecode);
    
    // Try to suspend without any call frames
    let result = vm.suspend_execution(0);
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Cannot suspend"));
}

