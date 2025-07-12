//! Example of using the async VM for non-blocking execution

use fluentai_vm::{Bytecode, VM};
use fluentai_vm::async_vm::AsyncVM;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create a simple bytecode program
    let bytecode = create_example_bytecode();
    
    // Create a regular VM
    let vm = VM::new(bytecode);
    
    // Wrap it in an async VM
    let mut async_vm = AsyncVM::new(vm);
    
    // Run asynchronously
    let result = async_vm.run().await?;
    
    println!("Execution result: {:?}", result);
    
    Ok(())
}

fn create_example_bytecode() -> Bytecode {
    use fluentai_bytecode::{BytecodeChunk, Instruction, Opcode};
    use fluentai_core::value::Value;
    
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("example".to_string()));
    
    // Push some values
    chunk.constants.push(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    
    // Return the value
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    bytecode
}