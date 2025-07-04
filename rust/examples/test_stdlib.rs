//! Test the FluentAi standard library integration

use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::Compiler};

fn main() -> anyhow::Result<()> {
    println!("Testing FluentAi Standard Library Integration");
    println!("===========================================\n");
    
    // Test 1: Core functions
    test_expr("(length [1 2 3 4 5])", "Core: length function")?;
    test_expr("(append [1 2 3] 4)", "Core: append function")?;
    test_expr("(reverse [1 2 3 4 5])", "Core: reverse function")?;
    test_expr("(range 5)", "Core: range function")?;
    test_expr("(max 1 5 3 2 4)", "Core: max function")?;
    test_expr("(min 5 3 1 4 2)", "Core: min function")?;
    test_expr("(abs -42)", "Core: abs function")?;
    
    // Test 2: String functions
    test_expr("(string-length \"hello world\")", "String: length")?;
    test_expr("(string-concat \"hello\" \" \" \"world\")", "String: concat")?;
    test_expr("(string-upcase \"hello\")", "String: uppercase")?;
    test_expr("(string-split \"hello,world\" \",\")", "String: split")?;
    test_expr("(string-join \" \" [\"hello\" \"world\"])", "String: join")?;
    
    // Test 3: Math functions
    test_expr("(pi)", "Math: pi constant")?;
    test_expr("(sqrt 16)", "Math: square root")?;
    test_expr("(pow 2 8)", "Math: power")?;
    test_expr("(round 3.7)", "Math: round")?;
    test_expr("(floor 3.7)", "Math: floor")?;
    test_expr("(ceil 3.2)", "Math: ceil")?;
    
    // Test 4: Type predicates
    test_expr("(int? 42)", "Type: integer predicate")?;
    test_expr("(string? \"hello\")", "Type: string predicate")?;
    test_expr("(list? [1 2 3])", "Type: list predicate")?;
    test_expr("(number? 3.14)", "Type: number predicate")?;
    
    // Test 5: Collections
    test_expr("(list-slice [1 2 3 4 5] 1 4)", "Collections: list slice")?;
    test_expr("(list-unique [1 2 2 3 3 3 4])", "Collections: unique")?;
    test_expr("(dict-new \"a\" 1 \"b\" 2)", "Collections: create dictionary")?;
    
    println!("\nAll tests passed!");
    Ok(())
}

fn test_expr(expr: &str, desc: &str) -> anyhow::Result<()> {
    print!("{}: ", desc);
    
    // Parse the expression
    let graph = parse(expr)?;
    
    // Compile to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM and run
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    println!("{} = {}", expr, result);
    Ok(())
}