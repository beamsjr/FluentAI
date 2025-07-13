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
fn test_cons_pattern_uppercase() {
    let code = r#"
        (match (list 1 2 3)
          (Nil "empty")
          ((Cons x xs) x))
    "#;

    let result = compile_and_run(code).expect("Should run successfully");
    assert_eq!(result, Value::Integer(1), "Should extract head of list");
}

#[test]
#[ignore = "Lowercase cons pattern not supported in parentheses - parser limitation"]
fn test_cons_pattern_lowercase() {
    let code = r#"
        (match (list 1 2 3)
          (nil "empty")
          ((cons x xs) x))
    "#;

    let result = compile_and_run(code).expect("Should run successfully");
    assert_eq!(result, Value::Integer(1), "Should extract head of list");
}

#[test]
fn test_nil_pattern_uppercase() {
    let code = r#"
        (match (list)
          (Nil "empty list")
          ((Cons x xs) "not empty"))
    "#;

    let result = compile_and_run(code).expect("Should run successfully");
    assert_eq!(
        result,
        Value::String("empty list".to_string()),
        "Should match empty list"
    );
}

#[test]
#[ignore = "Lowercase nil/cons patterns not supported in parentheses - parser limitation"]
fn test_nil_pattern_lowercase() {
    let code = r#"
        (match (list)
          (nil "empty list")
          ((cons x xs) "not empty"))
    "#;

    let result = compile_and_run(code).expect("Should run successfully");
    assert_eq!(
        result,
        Value::String("empty list".to_string()),
        "Should match empty list"
    );
}

#[test]
fn test_cons_extract_both_values() {
    let code = r#"
        (match (list 1 2 3)
          (Nil 0)
          ((Cons x xs) (+ x (length xs))))
    "#;

    let result = compile_and_run(code).expect("Should run successfully");
    assert_eq!(
        result,
        Value::Integer(3),
        "Should compute 1 + length([2, 3])"
    );
}
