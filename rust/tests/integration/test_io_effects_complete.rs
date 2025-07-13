//! Complete test of I/O effects integration

use anyhow::Result;
use fluentai_vm::{Compiler, VM};
use fluentai_stdlib::io_effects::{set_io_context, IOEffectContext, LoggingIOHandler};
use std::sync::Arc;
use std::fs;
use std::path::Path;

fn main() -> Result<()> {
    println!("Testing Complete I/O Effects Integration");
    println!("=======================================\n");
    
    // Create a test directory
    let test_dir = "/tmp/fluentai_io_test";
    if Path::new(test_dir).exists() {
        fs::remove_dir_all(test_dir)?;
    }
    fs::create_dir_all(test_dir)?;
    
    // Test 1: Normal I/O operations (allowed)
    println!("Test 1: Normal I/O operations");
    println!("-----------------------------");
    set_io_context(IOEffectContext::default());
    
    test_expr(
        "(file-write \"/tmp/fluentai_io_test/test.txt\" \"Hello, FluentAi!\")",
        "Write file"
    )?;
    
    test_expr(
        "(file-read \"/tmp/fluentai_io_test/test.txt\")",
        "Read file"
    )?;
    
    test_expr(
        "(file-append \"/tmp/fluentai_io_test/test.txt\" \"\\nAppended line\")",
        "Append to file"
    )?;
    
    test_expr(
        "(file-read \"/tmp/fluentai_io_test/test.txt\")",
        "Read after append"
    )?;
    
    test_expr(
        "(file-exists? \"/tmp/fluentai_io_test/test.txt\")",
        "Check file exists"
    )?;
    
    test_expr(
        "(dir-list \"/tmp/fluentai_io_test\")",
        "List directory"
    )?;
    
    test_expr(
        "(current-directory)",
        "Get current directory"
    )?;
    
    // Test 2: Sandboxed I/O with logging
    println!("\nTest 2: Sandboxed I/O with logging");
    println!("----------------------------------");
    let handler = Arc::new(LoggingIOHandler::new());
    set_io_context(IOEffectContext {
        io_allowed: true,
        allowed_paths: None,
        read_only: false,
        io_handler: Some(handler.clone()),
    });
    
    test_expr("(print \"Hello\")", "Sandboxed print")?;
    test_expr("(print-line \"World\")", "Sandboxed print-line")?;
    test_expr("(file-read \"secret.txt\")", "Sandboxed file read (should fail)")?;
    
    println!("\nLogged operations:");
    for op in handler.get_log() {
        println!("  - {}", op);
    }
    
    // Test 3: Path-restricted I/O
    println!("\nTest 3: Path-restricted I/O");
    println!("----------------------------");
    set_io_context(IOEffectContext {
        io_allowed: true,
        allowed_paths: Some(vec!["/tmp/fluentai_io_test".to_string()]),
        read_only: false,
        io_handler: None,
    });
    
    test_expr(
        "(file-write \"/tmp/fluentai_io_test/allowed.txt\" \"This is allowed\")",
        "Write to allowed path"
    )?;
    
    test_expr(
        "(file-write \"/etc/passwd\" \"This should fail\")",
        "Write to restricted path (should fail)"
    )?;
    
    // Test 4: Read-only mode
    println!("\nTest 4: Read-only mode");
    println!("----------------------");
    set_io_context(IOEffectContext {
        io_allowed: true,
        allowed_paths: None,
        read_only: true,
        io_handler: None,
    });
    
    test_expr(
        "(file-read \"/tmp/fluentai_io_test/test.txt\")",
        "Read in read-only mode"
    )?;
    
    test_expr(
        "(file-write \"/tmp/fluentai_io_test/readonly.txt\" \"Should fail\")",
        "Write in read-only mode (should fail)"
    )?;
    
    test_expr(
        "(file-delete \"/tmp/fluentai_io_test/test.txt\")",
        "Delete in read-only mode (should fail)"
    )?;
    
    // Test 5: I/O disabled
    println!("\nTest 5: I/O disabled");
    println!("--------------------");
    set_io_context(IOEffectContext {
        io_allowed: false,
        allowed_paths: None,
        read_only: false,
        io_handler: None,
    });
    
    test_expr("(print-line \"Should fail\")", "Print with I/O disabled")?;
    test_expr("(read-line)", "Read line with I/O disabled")?;
    
    // Test 6: JSON operations (always allowed)
    println!("\nTest 6: JSON operations");
    println!("-----------------------");
    set_io_context(IOEffectContext::default());
    
    test_expr(
        "(json-stringify (list 1 2 3))",
        "Stringify list"
    )?;
    
    test_expr(
        "(json-parse \"[1,2,3]\")",
        "Parse JSON array"
    )?;
    
    test_expr(
        "(json-stringify (map-set (map-set (make-map) \"name\" \"Alice\") \"age\" 30))",
        "Stringify map"
    )?;
    
    test_expr(
        "(json-parse \"{\\\"name\\\":\\\"Bob\\\",\\\"age\\\":25}\")",
        "Parse JSON object"
    )?;
    
    // Cleanup
    fs::remove_dir_all(test_dir)?;
    
    println!("\nAll tests completed!");
    Ok(())
}

fn test_expr(expr: &str, desc: &str) -> Result<()> {
    println!("\n{}: {}", desc, expr);
    
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