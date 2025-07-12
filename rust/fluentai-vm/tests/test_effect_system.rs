// Simple test to check if async/await is actually implemented in FluentAI

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_core::value::Value;
use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parse;
use fluentai_vm::{Compiler, CompilerOptions, VM, VMBuilder};

fn main() -> Result<()> {
    println!("Testing FluentAI async/await functionality...\n");

    // Test 1: Basic async
    println!("Test 1: Basic async");
    let result = run_code("(async 42)")?;
    println!("Result: {:?}\n", result);

    // Test 2: Channel creation
    println!("Test 2: Channel creation");
    let result = run_code("(chan)")?;
    println!("Result: {:?}\n", result);

    // Test 3: Spawn
    println!("Test 3: Spawn");
    let result = run_code("(spawn (lambda () 42))")?;
    println!("Result: {:?}\n", result);

    // Test 4: Print-line
    println!("Test 4: Print-line");
    let result = run_code("(print-line \"Hello from FluentAI\")")?;
    println!("Result: {:?}\n", result);

    // Test 5: Let binding with channel
    println!("Test 5: Let binding with channel");
    let result = run_code("(let ((ch (chan))) ch)")?;
    println!("Result: {:?}\n", result);

    // Test 6: Send and receive
    println!("Test 6: Send and receive (if channels work)");
    let code = r#"
        (let ((ch (chan)))
          (do
            (send! ch "test message")
            (recv! ch)))
    "#;
    match run_code(code) {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {}", e),
    }

    Ok(())
}

fn run_code(code: &str) -> Result<Value> {
    // Parse the code
    let graph = parse(code)?;
    
    // Compile it
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM using builder pattern
    let mut vm = VMBuilder::new()
        .with_bytecode(bytecode)
        .build()?;
    
    // Run the code
    Ok(vm.run()?)
}