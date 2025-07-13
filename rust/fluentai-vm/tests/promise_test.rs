use fluentai_vm::{Compiler, Value, VM};
use fluentai_parser::parse_flc;
use anyhow::Result;

#[test]
fn test_promise_new() -> Result<()> {
    let source = r#"
        let p = promise_new(() => 42);
        p
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    let result = vm.run()?;
    
    // Check that we get a promise value
    match result {
        Value::Promise(_) => Ok(()),
        _ => panic!("Expected promise value, got {:?}", result)
    }
}

#[test]
#[ignore] // Async execution not fully implemented yet
fn test_promise_await() -> Result<()> {
    let source = r#"
        let p = promise_new(() => 42);
        await p
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    
    let runtime = tokio::runtime::Runtime::new()?;
    
    runtime.block_on(async {
        let vm = VM::new(bytecode);
        let mut async_vm = fluentai_vm::async_vm::AsyncVM::new(vm);
        
        let result = async_vm.run().await?;
        
        match result {
            Value::Integer(42) => Ok(()),
            _ => panic!("Expected integer 42, got {:?}", result)
        }
    })
}

#[test]
fn test_promise_all() -> Result<()> {
    let source = r#"
        let p1 = promise_new(() => 1);
        let p2 = promise_new(() => 2);
        let p3 = promise_new(() => 3);
        promise_all([p1, p2, p3])
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    let result = vm.run()?;
    
    // Check that we get a promise value
    match result {
        Value::Promise(_) => Ok(()),
        _ => panic!("Expected promise value, got {:?}", result)
    }
}

#[test]
fn test_promise_race() -> Result<()> {
    let source = r#"
        let p1 = promise_new(() => {
            // This would take longer
            1
        });
        let p2 = promise_new(() => {
            // This resolves immediately
            2
        });
        promise_race([p1, p2])
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    let result = vm.run()?;
    
    // Check that we get a promise value
    match result {
        Value::Promise(_) => Ok(()),
        _ => panic!("Expected promise value, got {:?}", result)
    }
}

#[test]
fn test_promise_with_timeout() -> Result<()> {
    let source = r#"
        let p = promise_new(() => {
            // Simulate long operation
            42
        });
        with_timeout(p, 1000)
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    let result = vm.run()?;
    
    // Check that we get a promise value
    match result {
        Value::Promise(_) => Ok(()),
        _ => panic!("Expected promise value, got {:?}", result)
    }
}

#[test]
fn test_promise_all_empty_list() -> Result<()> {
    let source = r#"
        promise_all([])
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    let result = vm.run()?;
    
    // Check that we get a promise value
    match result {
        Value::Promise(_) => Ok(()),
        _ => panic!("Expected promise value, got {:?}", result)
    }
}

#[test]
fn test_promise_race_empty_list() -> Result<()> {
    let source = r#"
        promise_race([])
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Should error on empty list
    match vm.run() {
        Err(e) => {
            assert!(e.to_string().contains("at least one promise"));
            Ok(())
        }
        Ok(result) => panic!("Expected error for empty promise list, got {:?}", result)
    }
}

#[test]
fn test_promise_all_with_non_promise() -> Result<()> {
    let source = r#"
        let p1 = promise_new(() => 1);
        promise_all([p1, 42])
    "#;
    
    let graph = parse_flc(source)?;
    let bytecode = Compiler::new().compile(&graph)?;
    let mut vm = VM::new(bytecode);
    
    // Should error on non-promise value
    match vm.run() {
        Err(e) => {
            assert!(e.to_string().contains("promise"));
            Ok(())
        }
        Ok(result) => panic!("Expected error for non-promise value, got {:?}", result)
    }
}