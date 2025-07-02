//! Test I/O sandboxing with effect system

use anyhow::Result;
use claudelang_parser::parse;
use claudelang_vm::{Compiler, VM};
use claudelang_stdlib::io_effects::{set_io_context, IOEffectContext, LoggingIOHandler};
use std::sync::Arc;

fn test_sandboxed_io(expr: &str, desc: &str) -> Result<()> {
    println!("\n{}: {}", desc, expr);
    
    // Set up sandboxed I/O
    let handler = Arc::new(LoggingIOHandler::new());
    let io_context = IOEffectContext {
        io_allowed: true,
        allowed_paths: None,
        read_only: false,
        io_handler: Some(handler.clone()),
    };
    set_io_context(io_context);
    
    // Compile and run
    let graph = parse(expr)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Show logged operations
    println!("Logged I/O operations:");
    for op in handler.get_log() {
        println!("  - {}", op);
    }
    
    Ok(())
}

fn test_restricted_io(expr: &str, desc: &str) -> Result<()> {
    println!("\n{}: {}", desc, expr);
    
    // Set up restricted I/O (no I/O allowed)
    let io_context = IOEffectContext {
        io_allowed: false,
        allowed_paths: None,
        read_only: false,
        io_handler: None,
    };
    set_io_context(io_context);
    
    // Compile and run
    let graph = parse(expr)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error (expected): {}", e),
    }
    
    Ok(())
}

fn test_path_restricted_io(expr: &str, desc: &str, allowed_paths: Vec<String>) -> Result<()> {
    println!("\n{}: {}", desc, expr);
    println!("Allowed paths: {:?}", allowed_paths);
    
    // Set up path-restricted I/O
    let io_context = IOEffectContext {
        io_allowed: true,
        allowed_paths: Some(allowed_paths),
        read_only: false,
        io_handler: None,
    };
    set_io_context(io_context);
    
    // Compile and run
    let graph = parse(expr)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}

fn main() -> Result<()> {
    println!("Testing I/O Sandboxing in ClaudeLang");
    println!("====================================");
    
    // Test with sandboxed I/O handler
    test_sandboxed_io(
        "(print-line \"Hello from sandbox!\")",
        "Sandboxed print"
    )?;
    
    test_sandboxed_io(
        "(file-read \"test.txt\")",
        "Sandboxed file read (should fail)"
    )?;
    
    // Test with I/O disabled
    test_restricted_io(
        "(print-line \"This should fail\")",
        "I/O disabled print"
    )?;
    
    // Test with path restrictions
    test_path_restricted_io(
        "(file-read \"/tmp/allowed.txt\")",
        "Path restricted read (allowed)",
        vec!["/tmp".to_string()]
    )?;
    
    test_path_restricted_io(
        "(file-read \"/etc/passwd\")",
        "Path restricted read (denied)",
        vec!["/tmp".to_string()]
    )?;
    
    // Test read-only mode
    println!("\nTesting read-only mode:");
    let io_context = IOEffectContext {
        io_allowed: true,
        allowed_paths: None,
        read_only: true,
        io_handler: None,
    };
    set_io_context(io_context);
    
    let graph = parse("(file-write \"/tmp/test.txt\" \"content\")")?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(_) => println!("Write succeeded (unexpected)"),
        Err(e) => println!("Write failed as expected: {}", e),
    }
    
    // Reset to default
    set_io_context(IOEffectContext::default());
    
    println!("\nAll sandboxing tests completed!");
    Ok(())
}