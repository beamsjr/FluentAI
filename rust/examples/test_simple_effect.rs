use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime, handlers::*};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("=== Simple Effect Test ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Register IO handler
    context.register_handler(Arc::new(IOHandler::new()));
    
    // Try different syntaxes
    let test_cases = vec![
        ("Colon syntax", r#"(effect io:println "Hello")"#),
        ("Space syntax", r#"(effect io println "Hello")"#),
        ("String syntax", r#"(effect "io" "println" "Hello")"#),
    ];
    
    for (name, code) in test_cases {
        println!("Test: {}", name);
        println!("Code: {}", code);
        
        match parse(code) {
            Ok(ast) => {
                println!("  ✓ Parsed successfully");
                
                let compiler = Compiler::new();
                match compiler.compile(&ast) {
                    Ok(bytecode) => {
                        println!("  ✓ Compiled successfully");
                        
                        let mut vm = VM::new(bytecode);
                        vm.set_effect_context(context.clone());
                        vm.set_effect_runtime(runtime.clone());
                        
                        match vm.run() {
                            Ok(result) => println!("  ✓ Result: {:?}", result),
                            Err(e) => println!("  ✗ Runtime error: {}", e),
                        }
                    }
                    Err(e) => println!("  ✗ Compile error: {}", e),
                }
            }
            Err(e) => println!("  ✗ Parse error: {}", e),
        }
        println!();
    }
    
    Ok(())
}