use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM};

fn main() {
    // Test different effect syntaxes
    let test_cases = vec![
        // The colon syntax as shown in tests
        r#"(effect error:raise "test error")"#,
        
        // What might be the actual syntax based on VM implementation
        r#"(effect "error" "raise" "test error")"#,
        r#"(effect "Error" "raise" "test error")"#,
        
        // Try with handler
        r#"
        (handler
            ((error (lambda (err) (concat "caught: " err))))
            (effect error:raise "test"))
        "#,
        
        // Try direct syntax
        r#"
        (handler
            ((error (lambda (err) (concat "caught: " err))))
            (effect "Error" "raise" "test"))
        "#,
    ];
    
    for (i, code) in test_cases.iter().enumerate() {
        println!("\n=== Test case {} ===", i + 1);
        println!("Code: {}", code);
        
        match parse(code) {
            Ok(graph) => {
                println!("✓ Parsed successfully");
                
                match Compiler::new().compile(&graph) {
                    Ok(bytecode) => {
                        println!("✓ Compiled successfully");
                        
                        let mut vm = VM::new(bytecode);
                        match vm.run() {
                            Ok(result) => {
                                println!("✓ Executed successfully");
                                println!("Result: {:?}", result);
                            }
                            Err(e) => {
                                println!("✗ Execution failed: {:?}", e);
                            }
                        }
                    }
                    Err(e) => {
                        println!("✗ Compilation failed: {:?}", e);
                    }
                }
            }
            Err(e) => {
                println!("✗ Parse failed: {:?}", e);
            }
        }
    }
}