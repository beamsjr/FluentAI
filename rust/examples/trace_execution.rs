use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};
use fluentai_bytecode::Opcode;

fn main() {
    let source = r#"
// Simple add function
private function add(x: int, y: int) -> int {
    x + y
}

// Test
add(5, 3)
    "#;
    
    // Parse
    let graph = match parse_flc(source) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            return;
        }
    };
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = match compiler.compile(&graph) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Compile error: {:?}", e);
            return;
        }
    };
    
    // Create VM and trace execution
    let mut vm = VM::new(bytecode);
    
    println!("=== EXECUTION TRACE ===");
    
    // Execute step by step
    let mut step = 0;
    loop {
        // Get current instruction
        let chunk_id = vm.current_chunk();
        let ip = vm.get_ip();
        let chunk = &vm.bytecode().chunks[chunk_id];
        
        if ip >= chunk.instructions.len() {
            println!("IP out of bounds!");
            break;
        }
        
        let instruction = &chunk.instructions[ip];
        
        // Print state before execution
        println!("\nStep {}: Chunk {} IP {}", step, chunk_id, ip);
        println!("  Instruction: {:?}", instruction);
        println!("  Stack before: {:?}", vm.debug_stack());
        
        // Special handling for debugging specific operations
        match instruction.opcode {
            Opcode::Call => {
                println!("  Call frame before: chunk={} ip={} stack_base={}", 
                    vm.current_frame().chunk_id,
                    vm.current_frame().ip,
                    vm.current_frame().stack_base
                );
            }
            Opcode::LoadLocal0 | Opcode::LoadLocal1 => {
                let frame = vm.current_frame();
                println!("  Current frame stack_base: {}", frame.stack_base);
                println!("  Stack length: {}", vm.stack_len());
            }
            _ => {}
        }
        
        // Execute one instruction
        match vm.step() {
            Ok(done) => {
                if done {
                    println!("\nExecution completed!");
                    println!("Final result: {:?}", vm.pop());
                    break;
                }
            }
            Err(e) => {
                println!("\nError at step {}: {:?}", step, e);
                break;
            }
        }
        
        step += 1;
        if step > 50 {
            println!("\nStopping after 50 steps to prevent infinite loop");
            break;
        }
    }
}