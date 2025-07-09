use fluentai_vm::{Value, VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

fn compile_and_run(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let graph = fluentai_parser::parse(code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

#[test]
fn test_define_simple_value() {
    let code = r#"
        (begin
            (define x 42)
            x)
    "#;
    
    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_define_function() {
    let code = r#"
        (begin
            (define add (lambda (x y) (+ x y)))
            (add 10 20))
    "#;
    
    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(30));
}

#[test]
fn test_define_nested_syntax() {
    let code = r#"
        (begin
            (define (square x) (* x x))
            (square 5))
    "#;
    
    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(25));
}

#[test]
fn test_multiple_defines() {
    let code = r#"
        (begin
            (define x 10)
            (define y 20)
            (define sum (+ x y))
            sum)
    "#;
    
    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(30));
}

#[test]
fn test_define_returns_nil() {
    let code = r#"(define x 42)"#;
    
    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_define_function_recursive() {
    let code = r#"
        (begin
            (define (factorial n)
                (if (= n 0)
                    1
                    (* n (factorial (- n 1)))))
            (factorial 5))
    "#;
    
    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(120));
}

#[test]
fn test_top_level_define() {
    // Test that define works at the top level without explicit begin
    let graph = fluentai_parser::parse(r#"
        (define x 10)
        (define y 20)
        (+ x y)
    "#).unwrap();
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    assert_eq!(result, Value::Integer(30));
}