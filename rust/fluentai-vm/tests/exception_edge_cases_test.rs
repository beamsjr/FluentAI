use fluentai_vm::compiler::{Compiler, CompilerOptions};
use fluentai_vm::error::VMError;
use fluentai_vm::VM;
use fluentai_core::value::Value;
use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parser::Parser;

fn compile_and_run(code: &str) -> Result<Value, VMError> {
    // Parse the code to get an AST
    let mut parser = Parser::new(code);
    let ast = parser.parse().map_err(|e| VMError::RuntimeError {
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
        (let ((ch1 (chan))
              (ch2 (chan))
              (ch3 (chan)))
          (begin
            (send! ch1 "level1")
            (send! ch2 "level2")
            (send! ch3 "level3")
            (try
              (let ((x (recv! ch1)))
                (try
                  (let ((y (recv! ch2)))
                    (try
                      (let ((z (recv! ch3)))
                        (throw "deep error"))
                      (catch (e1)
                        (+ x y z))))
                  (catch (e2)
                    (+ x y "caught2"))))
              (catch (e3)
                (+ x "caught3")))))
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
        (let ((ch (chan))
              (v1 1) (v2 2) (v3 3) (v4 4) (v5 5)
              (v6 6) (v7 7) (v8 8) (v9 9) (v10 10))
          (begin
            (send! ch "test")
            (try
              (let ((data (recv! ch)))
                (throw "error"))
              (catch (e)
                (+ v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)))))
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
        (let ((ch (chan))
              (result-ch (chan)))
          (begin
            (send! ch "data")
            (send! result-ch "final")
            (try
              (let ((x (recv! ch)))
                (throw "error"))
              (catch (e)
                (let ((y (recv! result-ch)))
                  y))
              (finally
                (recv! result-ch)))))
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
        (try
          (throw "error")
          (catch (e)
            "caught"))
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
        (let ((ch (chan)))
          (begin
            (send! ch "outer")
            (try
              (let ((outer (recv! ch)))
                (let ((inner "inner"))
                  (let ((nested "nested"))
                    (throw "error"))))
              (catch (e)
                (recv! ch)))))
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