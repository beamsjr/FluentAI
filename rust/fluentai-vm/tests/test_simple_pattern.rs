use fluentai_parser::parse;
use fluentai_vm::{VM, Compiler, CompilerOptions};
use fluentai_optimizer::OptimizationLevel;

#[test]
fn test_simple_or_pattern() {
    // Test simple or pattern
    let source = r#"
        (match 2
            ((or 1 2 3) "small")
            (_ "large"))
    "#;
    
    println!("Parsing source: {}", source);
    
    match parse(source) {
        Ok(graph) => {
            println!("Parse successful!");
            println!("Graph: {:?}", graph);
            
            let options = CompilerOptions {
                optimization_level: OptimizationLevel::None,
                debug_info: true,
            };
            
            let compiler = Compiler::with_options(options);
            match compiler.compile(&graph) {
                Ok(bytecode) => {
                    println!("Compilation successful!");
                    let mut vm = VM::new(bytecode);
                    
                    // For now, skip stdlib registration since we're just testing patterns
                    // The VM should handle basic operations internally
                    
                    match vm.run() {
                        Ok(value) => {
                            println!("Result: {:?}", value);
                        }
                        Err(e) => {
                            println!("VM error: {:?}", e);
                        }
                    }
                }
                Err(e) => {
                    println!("Compilation error: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}