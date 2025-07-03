use fluentai_parser::parse;
use fluentai_vm::compiler::Compiler;

fn main() -> anyhow::Result<()> {
    // Test free variable detection in nested lambdas
    let code = r#"
        (let ((x 10))
          (lambda (n)
            (lambda (m) (+ (+ n m) x))))
    "#;
    
    println!("Testing free variable detection");
    println!("Code: {}", code);
    
    let ast = parse(code)?;
    println!("AST: {:#?}", ast);
    
    // We can't easily test free variable detection without modifying the compiler
    // But we can compile and see what happens
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nBytecode:");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}: {}", i, chunk.name.as_ref().unwrap_or(&"unnamed".to_string()));
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, inst);
        }
    }
    
    // Check if x is being captured
    println!("\nAnalysis:");
    println!("- Outer lambda should capture x");
    println!("- Inner lambda should capture n and have access to x from outer lambda");
    
    Ok(())
}