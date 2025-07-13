use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parse_flc;
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    Value, VM,
};

fn compile_and_run(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let graph = fluentai_parser::parse_flc(code)?;
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
private function get_x() {
    42
}
get_x()
    "#;

    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_define_function() {
    let code = r#"
private function add(x, y) {
    x + y
}
add(10, 20)
    "#;

    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(30));
}

#[test]
fn test_define_nested_syntax() {
    let code = r#"
private function square(x) {
    x * x
}
square(5)
    "#;

    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(25));
}

#[test]
fn test_multiple_defines() {
    let code = r#"
private function get_x() { 10 }
private function get_y() { 20 }
private function get_sum() { get_x() + get_y() }
get_sum()
    "#;

    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(30));
}

#[test]
fn test_define_returns_nil() {
    let code = r#"private function x() { 42 }"#;

    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_define_function_recursive() {
    let code = r#"
private function factorial(n) {
    if (n == 0) {
        1
    } else {
        n * factorial(n - 1)
    }
}
factorial(5)
    "#;

    let result = compile_and_run(code).unwrap();
    assert_eq!(result, Value::Integer(120));
}

#[test]
fn test_top_level_define() {
    // Test that define works at the top level without explicit begin
    let graph = fluentai_parser::parse_flc(
        r#"
private function x() { 10 }
private function y() { 20 }
x() + y()
    "#,
    )
    .unwrap();

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
