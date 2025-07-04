use anyhow::Result;
use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler};

fn test_case(name: &str, code: &str) -> Result<()> {
    println!("\n=== Testing: {} ===", name);
    println!("Code: {}", code);
    
    let graph = parse(code)?;
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Print disassembly to see the generated code
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
    println!("Testing constructor patterns in FluentAi");
    
    // Test creating a simple tagged value
    test_case("Simple constructor", "(Cons 1 2)")?;
    
    // Test pattern matching on constructors
    test_case("Pattern matching", 
        "(let ((pair (Pair \"hello\" 42))) (match pair ((Pair a b) (+ b 1)) (_ 0)))")?;
    
    // Test nested constructors
    test_case("Nested constructors", 
        "(let ((tree (Node (Leaf 1) (Leaf 2)))) (match tree ((Node (Leaf x) (Leaf y)) (+ x y)) (_ 0)))")?;
    
    // Test constructor with no arguments
    test_case("Nullary constructor", "(None)")?;
    
    // Test option-like pattern
    test_case("Option pattern", 
        "(let ((maybe-value (Some 10))) (match maybe-value ((Some x) (* x 2)) ((None) 0)))")?;
    
    // Test multiple constructors in match
    test_case("Result pattern", 
        "(let ((result (Ok 42))) (match result ((Ok value) value) ((Error msg) -1)))")?;
    
    // Test constructor equality
    test_case("Constructor equality", 
        "(let ((a (Point 1 2)) (b (Point 1 2))) (= a b))")?;
    
    println!("\nAll constructor tests completed!");
    Ok(())
}