use fluentai_vm::compiler::{Compiler, CompilerOptions};
use fluentai_vm::error::VMError;
use fluentai_vm::VM;
use fluentai_core::value::Value;
use fluentai_optimizer::OptimizationLevel;

fn compile_and_run(code: &str) -> Result<Value, VMError> {
    // Parse the code to get an AST
    let ast = parse(code).map_err(|e| VMError::RuntimeError {
        message: format!("Parse error: {:?}", e),
        stack_trace: None,
    })?;
    
    // Compile the AST to bytecode
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&ast).map_err(|e| VMError::RuntimeError {
        message: format!("Compile error: {:?}", e),
        stack_trace: None,
    })?;
    
    // Run the bytecode
    let mut vm = VM::new(bytecode);
    vm.run()
}

#[test]
fn test_deep_nested_try_catch() {
    // Test multiple nested try-catch blocks to ensure stack management works correctly
    let result = compile_and_run(
        r#"
        {
            let ch1 = Channel.new();
            let ch2 = Channel.new();
            let ch3 = Channel.new();
            
            ch1.send("level1");
            ch2.send("level2");
            ch3.send("level3");
            
            try {
                let x = ch1.receive();
                try {
                    let y = ch2.receive();
                    try {
                        let z = ch3.receive();
                        throw "deep error";
                    } catch (e1) {
                        x + y + z
                    }
                } catch (e2) {
                    x + y + "caught2"
                }
            } catch (e3) {
                x + "caught3"
            }
        }
        "#
    );
    
    match result {
        Ok(Value::String(s)) => {
            // Should be able to access variables from outer scopes
            assert!(s.contains("level1"));
        }
        Ok(val) => panic!("Expected string result, got: {:?}", val),
        Err(e) => panic!("Test failed with error: {:?}", e),
    }
}

#[test]
fn test_many_locals_in_try_catch() {
    // Test with many local variables to ensure bounds checking works
    let result = compile_and_run(
        r#"
        {
            let ch = Channel.new();
            let v1 = 1;
            let v2 = 2;
            let v3 = 3;
            let v4 = 4;
            let v5 = 5;
            let v6 = 6;
            let v7 = 7;
            let v8 = 8;
            let v9 = 9;
            let v10 = 10;
            
            ch.send("test");
            
            try {
                let data = ch.receive();
                throw "error";
            } catch (e) {
                v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10
            }
        }
        "#
    );
    
    match result {
        Ok(Value::Integer(n)) => {
            assert_eq!(n, 55); // Sum of 1+2+...+10
        }
        Ok(val) => panic!("Expected integer result, got: {:?}", val),
        Err(e) => panic!("Test failed with error: {:?}", e),
    }
}

#[test]
fn test_exception_with_finally_and_locals() {
    // Test that locals are preserved across both catch and finally blocks
    let result = compile_and_run(
        r#"
        {
            let ch = Channel.new();
            let result_ch = Channel.new();
            
            ch.send("data");
            result_ch.send("final");
            
            try {
                let x = ch.receive();
                throw "error";
            } catch (e) {
                let y = result_ch.receive();
                y
            } finally {
                result_ch.receive()
            }
        }
        "#
    );
    
    match result {
        Ok(Value::String(s)) => {
            assert_eq!(s, "final");
        }
        Ok(val) => panic!("Expected string result, got: {:?}", val),
        Err(e) => panic!("Test failed with error: {:?}", e),
    }
}

#[test]
fn test_empty_try_catch() {
    // Test edge case with no locals in try-catch
    let result = compile_and_run(
        r#"
        try {
            throw "error";
        } catch (e) {
            "caught"
        }
        "#
    );
    
    match result {
        Ok(Value::String(s)) => {
            assert_eq!(s, "caught");
        }
        Ok(val) => panic!("Expected string result, got: {:?}", val),
        Err(e) => panic!("Test failed with error: {:?}", e),
    }
}

#[test]
fn test_exception_in_nested_let() {
    // Test that nested let bindings work correctly with exception handling
    let result = compile_and_run(
        r#"
        {
            let ch = Channel.new();
            
            ch.send("outer");
            
            try {
                let outer = ch.receive();
                let inner = "inner";
                let nested = "nested";
                throw "error";
            } catch (e) {
                ch.receive()
            }
        }
        "#
    );
    
    match result {
        Ok(Value::String(s)) => {
            assert_eq!(s, "outer");
        }
        Ok(val) => panic!("Expected string result, got: {:?}", val),
        Err(e) => panic!("Test failed with error: {:?}", e),
    }
}