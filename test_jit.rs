use claudelang_parser::parse;
use claudelang_vm::compiler::Compiler;
use claudelang_jit::JitCompiler;

fn main() {
    println!("Testing JIT compiler on current platform...");
    println!("Architecture: {}", std::env::consts::ARCH);
    
    match JitCompiler::new() {
        Ok(mut jit) => {
            println!("✓ JIT compiler created successfully");
            
            let source = "(+ 1 2)";
            let ast = parse(source).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            
            match jit.compile(&bytecode, 0) {
                Ok(func) => {
                    println!("✓ JIT compilation successful");
                    let result = func();
                    println!("✓ JIT execution result: {}", result);
                },
                Err(e) => {
                    println!("✗ JIT compilation failed: {}", e);
                }
            }
        },
        Err(e) => {
            println!("✗ Failed to create JIT compiler: {}", e);
        }
    }
}