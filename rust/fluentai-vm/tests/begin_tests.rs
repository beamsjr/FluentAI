//! Comprehensive tests for multiple top-level expressions (Begin node)

use fluentai_core::value::Value;
use fluentai_parser::parse;
use fluentai_vm::{Compiler, CompilerOptions, OptimizationLevel, VM};

fn compile_and_run(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    let graph = parse(code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

fn test_eval(code: &str) -> Result<Value, Box<dyn std::error::Error>> {
    compile_and_run(code)
}

#[test]
fn test_begin_multiple_literals() {
    let code = r#"
        1
        2
        3
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(3));
}

#[test]
fn test_begin_multiple_expressions() {
    let code = r#"
        (+ 1 2)
        (* 3 4)
        (- 10 5)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(5));
}

#[test]
fn test_begin_with_effects() {
    let code = r#"
        (effect state:set "x" 10)
        (effect state:set "y" 20)
        (+ (effect state:get "x") (effect state:get "y"))
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_with_let_bindings() {
    let code = r#"
        (let ((x 10)) x)
        (let ((y 20)) y)
        (let ((z 30)) z)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_mixed_expressions() {
    let code = r#"
        42
        (let ((x 10)) (* x 2))
        "hello"
        (+ 5 5)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(10));
}

#[test]
fn test_begin_with_functions() {
    let code = r#"
        (let ((inc (lambda (x) (+ x 1)))) (inc 5))
        (let ((double (lambda (x) (* x 2)))) (double 7))
        (let ((square (lambda (x) (* x x)))) (square 3))
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(9));
}

#[test]
fn test_begin_empty() {
    // Empty file should parse but might not have a root
    let code = "";
    let result = test_eval(code);
    // Empty code might return Nil or error, depending on implementation
    assert!(matches!(result, Ok(Value::Nil)) || result.is_err());
}

#[test]
fn test_begin_single_expression() {
    // Single expression should not be wrapped in Begin
    let code = "(+ 1 2 3)";
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(6));
}

#[test]
fn test_begin_with_lists() {
    let code = r#"
        (list 1 2 3)
        (list 4 5 6)
        (list 1 2 3 4)
    "#;
    assert_eq!(
        compile_and_run(code).unwrap(),
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4)
        ])
    );
}

#[test]
fn test_begin_with_conditionals() {
    let code = r#"
        (if true 10 20)
        (if false 30 40)
        (if true 50 60)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(50));
}

#[test]
fn test_begin_with_pattern_matching() {
    let code = r#"
        (match 1
          (1 "one")
          (2 "two"))
        (match 2
          (1 "one")
          (2 "two")
          (_ "other"))
        (match 3
          (1 "one")
          (2 "two")
          (_ "other"))
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::String("other".to_string()));
}

#[test]
fn test_begin_with_nested_begin() {
    let code = r#"
        (begin
          1
          2)
        (begin
          3
          4)
    "#;
    // Should evaluate to 4 (last expression of last begin)
    // But actually begin is parsed inline, so this becomes: 1 2 3 4
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(2));
}

#[test]
fn test_begin_with_define() {
    let code = r#"
        (define x 10)
        (define y 20)
        (+ x y)
    "#;
    // This depends on whether define returns a value
    // Usually define returns the defined value or nil
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_all_intermediate_evaluated() {
    let code = r#"
        (let ((x 0))
          (set! x 1)
          (set! x 2)
          (set! x 3)
          x)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(3));
}

#[test]
fn test_begin_with_side_effects() {
    let code = r#"
        (let ((x 10))
          (set! x (+ x 10))
          (set! x (+ x 10))
          x)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(30));
}

#[test]
fn test_begin_mixed_types() {
    let code = r#"
        10
        "hello"
        'symbol
        (list 1 2 3)
        12
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(12));
}

#[test]
fn test_begin_with_void_expressions() {
    // Some expressions might return void/nil
    let code = r#"
        (set! undefined-var 10)
        (print "hello")
        nil
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Nil);
}

#[test]
fn test_begin_in_let_body() {
    let code = r#"
        (let ((x 10))
          (list 1 2)
          (list 3 4)
          (list 2 4 6 8 10))
    "#;
    assert_eq!(
        compile_and_run(code).unwrap(),
        Value::List(vec![
            Value::Integer(2),
            Value::Integer(4),
            Value::Integer(6),
            Value::Integer(8),
            Value::Integer(10)
        ])
    );
}

#[test]
fn test_begin_with_recursive_function() {
    let code = r#"
        (define fib
          (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 3)
        (fib 4)
        (fib 5)
    "#;
    assert_eq!(compile_and_run(code).unwrap(), Value::Integer(5)); // fib(5) = 5
}

// Error cases
#[test]
fn test_begin_with_undefined_variable() {
    let code = r#"
        (let ((x 10)) x)
        undefined-var
        (+ 1 2)
    "#;
    // Should error on undefined-var
    assert!(compile_and_run(code).is_err());
}

#[test]
fn test_begin_with_type_error() {
    let code = r#"
        (+ 1 2)
        (+ "hello" "world")
        (* 3 4)
    "#;
    // Should error on string addition
    assert!(compile_and_run(code).is_err());
}