//! Tests for JIT function call dispatch

use fluentai_jit::JitCompiler;
use fluentai_core::value::Value;

// Skip complex stdlib test for now

#[test]
fn test_simple_native_function() {
    use fluentai_core::value::Value;
    use std::sync::Arc;
    
    // Create a simple native function
    let add_one_fn = Arc::new(|args: &[Value]| -> Result<Value, fluentai_core::value::ValueError> {
        match &args[0] {
            Value::Integer(n) => Ok(Value::Integer(n + 1)),
            _ => Err(fluentai_core::value::ValueError::InvalidOperation("Expected integer".to_string())),
        }
    }) as Arc<dyn Fn(&[Value]) -> Result<Value, fluentai_core::value::ValueError> + Send + Sync>;
    
    // Create bytecode that calls this function
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push argument first (it will be under the function on the stack)
    let arg_idx = chunk.add_constant(Value::Integer(41));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, arg_idx));
    
    // Push the function last (it will be on top of the stack)
    let func_idx = chunk.add_constant(Value::NativeFunction {
        name: "add_one".to_string(),
        arity: 1,
        function: add_one_fn,
    });
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, func_idx));
    
    // Call function with 1 argument
    chunk.add_instruction(Instruction::with_arg(Opcode::Call, 1));
    
    // Return the result
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_function_call_type_error() {
    // Test calling a non-function value
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push a non-function value (integer)
    let val_idx = chunk.add_constant(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, val_idx));
    
    // Try to call it with 0 arguments
    chunk.add_instruction(Instruction::with_arg(Opcode::Call, 0));
    
    // Return the result
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "TypeError");
            assert!(message.contains("Cannot call non-function value"));
        }
        _ => panic!("Expected type error when calling non-function"),
    }
}

#[test]
fn test_native_function_with_multiple_args() {
    use fluentai_core::value::Value;
    use std::sync::Arc;
    
    // Create a native function that adds two numbers
    let add_fn = Arc::new(|args: &[Value]| -> Result<Value, fluentai_core::value::ValueError> {
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            _ => Err(fluentai_core::value::ValueError::InvalidOperation("Expected two integers".to_string())),
        }
    }) as Arc<dyn Fn(&[Value]) -> Result<Value, fluentai_core::value::ValueError> + Send + Sync>;
    
    // Create bytecode that calls this function
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push arguments first (they will be under the function on the stack)
    let arg1_idx = chunk.add_constant(Value::Integer(30));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, arg1_idx));
    
    let arg2_idx = chunk.add_constant(Value::Integer(12));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, arg2_idx));
    
    // Push the function last (it will be on top of the stack)
    let func_idx = chunk.add_constant(Value::NativeFunction {
        name: "add".to_string(),
        arity: 2,
        function: add_fn,
    });
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, func_idx));
    
    // Call function with 2 arguments
    chunk.add_instruction(Instruction::with_arg(Opcode::Call, 2));
    
    // Return the result
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_native_function_error_handling() {
    use fluentai_core::value::Value;
    use std::sync::Arc;
    
    // Create a native function that only accepts positive integers
    let sqrt_int_fn = Arc::new(|args: &[Value]| -> Result<Value, fluentai_core::value::ValueError> {
        match &args[0] {
            Value::Integer(n) if n >= &0 => {
                let sqrt = (*n as f64).sqrt() as i64;
                Ok(Value::Integer(sqrt))
            }
            Value::Integer(_) => Err(fluentai_core::value::ValueError::InvalidOperation("Cannot take square root of negative number".to_string())),
            _ => Err(fluentai_core::value::ValueError::InvalidOperation("Expected integer".to_string())),
        }
    }) as Arc<dyn Fn(&[Value]) -> Result<Value, fluentai_core::value::ValueError> + Send + Sync>;
    
    // Create bytecode that calls this function with invalid input
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push negative argument first
    let arg_idx = chunk.add_constant(Value::Integer(-4));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, arg_idx));
    
    // Push the function last
    let func_idx = chunk.add_constant(Value::NativeFunction {
        name: "sqrt_int".to_string(),
        arity: 1,
        function: sqrt_int_fn,
    });
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, func_idx));
    
    // Call function
    chunk.add_instruction(Instruction::with_arg(Opcode::Call, 1));
    
    // Return the result
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Run through JIT
    let mut jit = JitCompiler::new().unwrap();
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "Native");
            assert!(message.contains("Cannot take square root of negative number"));
        }
        _ => panic!("Expected error for negative square root"),
    }
}