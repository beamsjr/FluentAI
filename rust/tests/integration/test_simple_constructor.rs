use anyhow::Result;
use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<()> {
    // Test creating a constructor
    let code = "(Cons 1 2)";
    println!("Testing: {}", code);
    
    let graph = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    println!("Result: {:?}", result);
    
    // Test pattern matching
    let code2 = "(match (Cons 1 2) ((Cons x y) x) (_ 0))";
    println!("\nTesting: {}", code2);
    
    let graph2 = parse(code2)?;
    let compiler2 = Compiler::new();
    let bytecode2 = compiler2.compile(&graph2)?;
    
    println!("\nDisassembly:");
    for (i, chunk) in bytecode2.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {:04}: {:?}", j, instr);
        }
    }
    
    let mut vm2 = VM::new(bytecode2);
    match vm2.run() {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}