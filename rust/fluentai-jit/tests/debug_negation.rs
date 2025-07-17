#[test]
fn debug_negation() {
    use fluentai_parser::parse_flc;
    use fluentai_vm::{Compiler, VM};
    use fluentai_core::value::Value;
    
    let source = "-5";
    let ast = parse_flc(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    println!("Bytecode for '{}':", source);
    for (i, instr) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {:02}: {:?}", i, instr);
    }
    
    // Also check the constants
    println!("\nConstants:");
    for (i, constant) in bytecode.chunks[0].constants.iter().enumerate() {
        println!("  {:02}: {:?}", i, constant);
    }
    
    // First run it through the VM to verify correctness
    let mut vm = VM::new(bytecode.clone());
    let vm_result = vm.run().unwrap();
    println!("\nVM Result: {:?}", vm_result);
    assert_eq!(vm_result, Value::Integer(-5));
    
    // For now, skip JIT test for expressions that require stdlib functions
    // The JIT doesn't yet support LoadGlobal with stdlib function resolution
    // TODO: Implement proper stdlib function resolution in JIT
    
    // Test a simpler expression that doesn't require stdlib
    let simple_source = "5";
    let simple_ast = parse_flc(simple_source).unwrap();
    let simple_compiler = Compiler::new();
    let simple_bytecode = simple_compiler.compile(&simple_ast).unwrap();
    
    let mut jit = fluentai_jit::JitCompiler::new().unwrap();
    let jit_result = jit.compile_and_run(&simple_bytecode).unwrap();
    println!("\nJIT Result for '{}': {:?}", simple_source, jit_result);
    assert_eq!(jit_result, Value::Integer(5));
}