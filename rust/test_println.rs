use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

fn main() {
    println!("Testing print-line function:");
    let code = r#"
(print-line "Hello")
(print-line "World")
(print-line (+ 1 2 3))
"#;
    
    // Parse
    let graph = parse(code).expect("Parse failed");
    
    // Compile
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).expect("Compile failed");
    
    // Run
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {:?}", e),
    }
}