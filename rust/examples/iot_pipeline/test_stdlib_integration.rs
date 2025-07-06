use fluentai_vm::{VM, VMBuilder};
use fluentai_vm::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode, Value};
use fluentai_vm::compiler::Compiler;
use fluentai_parser::parser::Parser;
use fluentai_stdlib::init_stdlib;

fn main() {
    // Test 1: Check if stdlib is properly initialized
    println!("=== Test 1: Stdlib Registry ===");
    let stdlib = init_stdlib();
    println!("Stdlib contains 'map': {}", stdlib.contains("map"));
    println!("Stdlib contains 'filter': {}", stdlib.contains("filter"));
    println!("Stdlib contains 'fold': {}", stdlib.contains("fold"));
    
    // Test 2: Compile and run a simple map example
    println!("\n=== Test 2: Simple Map Example ===");
    let code = r#"
        let double = fn x -> x * 2;
        let nums = [1, 2, 3, 4, 5];
        map(double, nums)
    "#;
    
    match compile_and_run(code) {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 3: Test LoadGlobal behavior
    println!("\n=== Test 3: LoadGlobal Behavior ===");
    test_load_global();
    
    // Test 4: Direct VM test with manual bytecode
    println!("\n=== Test 4: Direct VM Test ===");
    test_direct_vm();
}

fn compile_and_run(code: &str) -> anyhow::Result<Value> {
    // Parse
    let mut parser = Parser::new(code);
    let graph = parser.parse()?;
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Run
    let mut vm = VM::new(bytecode);
    vm.run()?;
    
    // Get result
    Ok(vm.pop()?)
}

fn test_load_global() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Add "map" as a constant
    let map_idx = chunk.add_constant(Value::String("map".to_string()));
    
    // LoadGlobal instruction
    chunk.add_instruction(Instruction::with_arg(Opcode::LoadGlobal, map_idx as u32));
    chunk.add_instruction(Instruction::new(Opcode::Halt));
    
    let chunk_id = bytecode.add_chunk(chunk);
    bytecode.main_chunk = chunk_id;
    
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(_) => {
            match vm.pop() {
                Ok(value) => println!("LoadGlobal 'map' returned: {:?}", value),
                Err(e) => println!("Error popping result: {}", e),
            }
        }
        Err(e) => println!("Error running VM: {}", e),
    }
}

fn test_direct_vm() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Create a simple function that doubles a number
    let func_chunk_id = {
        let mut func_chunk = BytecodeChunk::new(Some("double".to_string()));
        func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load parameter
        func_chunk.add_instruction(Instruction::with_arg(Opcode::PushInt2, 0)); // Push 2
        func_chunk.add_instruction(Instruction::new(Opcode::Mul)); // Multiply
        func_chunk.add_instruction(Instruction::new(Opcode::Return));
        bytecode.add_chunk(func_chunk)
    };
    
    // Main chunk
    // Push the function
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeFunc, func_chunk_id as u32));
    
    // Push a list [1, 2, 3]
    chunk.add_instruction(Instruction::with_arg(Opcode::PushInt1, 0));
    chunk.add_instruction(Instruction::with_arg(Opcode::PushInt2, 0));
    let three_idx = chunk.add_constant(Value::Int(3));
    chunk.add_instruction(Instruction::with_arg(Opcode::PushConst, three_idx as u32));
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 3));
    
    // Load "map" function
    let map_idx = chunk.add_constant(Value::String("map".to_string()));
    chunk.add_instruction(Instruction::with_arg(Opcode::LoadGlobal, map_idx as u32));
    
    // Call map with 2 arguments (function, list)
    chunk.add_instruction(Instruction::new(Opcode::Swap)); // Swap to get map on top
    chunk.add_instruction(Instruction::with_arg(Opcode::Call, 2));
    
    chunk.add_instruction(Instruction::new(Opcode::Halt));
    
    let chunk_id = bytecode.add_chunk(chunk);
    bytecode.main_chunk = chunk_id;
    
    let mut vm = VM::new(bytecode);
    vm.enable_trace(); // Enable tracing to see what's happening
    
    match vm.run() {
        Ok(_) => {
            match vm.pop() {
                Ok(value) => println!("Direct VM test result: {:?}", value),
                Err(e) => println!("Error popping result: {}", e),
            }
        }
        Err(e) => println!("Error running VM: {}", e),
    }
}