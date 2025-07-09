use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

fn main() {
    let code = r#"
(define lst (list 1 2 3))
(match lst
  (Nil "empty")
  ((Cons x xs) x))
"#;
    
    println!("Testing Cons/Nil pattern matching:");
    println!("{}", code);
    
    match parse(code) {
        Ok(graph) => {
            println!("✓ Parse successful!");
            
            let options = CompilerOptions {
                optimization_level: OptimizationLevel::None,
                ..Default::default()
            };
            let compiler = Compiler::with_options(options);
            
            match compiler.compile(&graph) {
                Ok(bytecode) => {
                    println!("✓ Compilation successful!");
                    
                    let mut vm = VM::new(bytecode);
                    match vm.run() {
                        Ok(result) => {
                            println!("✓ Execution successful!");
                            println!("Result: {:?}", result);
                            
                            // The result should be 1 (the head of the list)
                            if let fluentai_vm::Value::Integer(1) = result {
                                println!("✓ Correct result! Pattern matching extracted the head of the list.");
                            } else {
                                println!("✗ Unexpected result. Expected Integer(1), got {:?}", result);
                            }
                        }
                        Err(e) => println!("✗ Runtime error: {:?}", e),
                    }
                }
                Err(e) => println!("✗ Compilation error: {:?}", e),
            }
        }
        Err(e) => println!("✗ Parse error: {:?}", e),
    }
}