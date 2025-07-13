use anyhow::Result;
use fluentai_vm::{VM, Compiler};

fn test_case(name: &str, code: &str) -> Result<()> {
    println!("\n=== Testing: {} ===", name);
    println!("Code: {}", code);
    
    let graph = parse(code)?;
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Print disassembly to see which opcodes are used
    println!("\nDisassembly:");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}:", i);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {:04}: {:?}", j, instr);
        }
    }
    
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    println!("Result: {:?}", result);
    
    Ok(())
}

fn main() -> Result<()> {
    println!("Testing fast local variable access opcodes");
    
    // Test LoadLocal0
    test_case("LoadLocal0", "(let ((a 10)) a)")?;
    
    // Test LoadLocal0 and LoadLocal1
    test_case("LoadLocal0-1", "(let ((a 10) (b 20)) (+ a b))")?;
    
    // Test LoadLocal0, LoadLocal1, LoadLocal2
    test_case("LoadLocal0-2", "(let ((a 10) (b 20) (c 30)) (+ (+ a b) c))")?;
    
    // Test LoadLocal0, LoadLocal1, LoadLocal2, LoadLocal3
    test_case("LoadLocal0-3", "(let ((a 10) (b 20) (c 30) (d 40)) (+ (+ (+ a b) c) d))")?;
    
    // Test mix of fast and regular locals (index 4 should use regular Load)
    test_case("Mixed locals", "(let ((a 1) (b 2) (c 3) (d 4) (e 5)) (+ (+ (+ (+ a b) c) d) e))")?;
    
    // Test in lambda - parameters should also use fast locals
    test_case("Lambda params", "((lambda (x y z w) (+ (+ (+ x y) z) w)) 10 20 30 40)")?;
    
    // Test nested scopes
    test_case("Nested scopes", 
        "(let ((a 1)) (let ((b 2)) (let ((c 3)) (+ (+ a b) c))))")?;
    
    println!("\nAll tests completed!");
    Ok(())
}