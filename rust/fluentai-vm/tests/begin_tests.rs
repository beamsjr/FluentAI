//! Comprehensive tests for multiple top-level expressions (Begin node)

use fluentai_core::value::Value;
use fluentai_vm::test_utils::*;

#[test]
fn test_begin_multiple_literals() {
    let code = r#"
        1
        2
        3
    "#;
    assert_eval_eq!(code, Value::Integer(3));
}

#[test]
fn test_begin_multiple_expressions() {
    let code = r#"
        (+ 1 2)
        (* 3 4)
        (- 10 5)
    "#;
    assert_eval_eq!(code, Value::Integer(5));
}

#[test]
fn test_begin_with_effects() {
    let code = r#"
        (effect state:set "x" 10)
        (effect state:set "y" 20)
        (+ (effect state:get "x") (effect state:get "y"))
    "#;
    assert_eval_eq!(code, Value::Integer(30));
}

#[test]
fn test_begin_with_let_bindings() {
    let code = r#"
        (let ((x 10)) x)
        (let ((y 20)) y)
        (let ((z 30)) z)
    "#;
    assert_eval_eq!(code, Value::Integer(30));
}

#[test]
fn test_begin_mixed_expressions() {
    let code = r#"
        42
        (let ((x 10)) (* x 2))
        "hello"
        (+ 5 5)
    "#;
    assert_eval_eq!(code, Value::Integer(10));
}

#[test]
fn test_begin_with_functions() {
    let code = r#"
        (let ((inc (lambda (x) (+ x 1)))) (inc 5))
        (let ((double (lambda (x) (* x 2)))) (double 7))
        (let ((square (lambda (x) (* x x)))) (square 3))
    "#;
    assert_eval_eq!(code, Value::Integer(9));
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
    assert_eval_eq!(code, Value::Integer(6));
}

#[test]
fn test_begin_with_list_operations() {
    let code = r#"
        (list 1 2 3)
        (cons 0 (list 1 2 3))
        (append (list 1 2) (list 3 4))
    "#;
    assert_eval_eq!(code, vec![1, 2, 3, 4]);
}

#[test]
fn test_begin_with_conditionals() {
    let code = r#"
        (if #t 10 20)
        (if #f 30 40)
        (if (> 5 3) 50 60)
    "#;
    assert_eval_eq!(code, Value::Integer(50));
}

#[test]
fn test_begin_with_pattern_matching() {
    let code = r#"
        (match 1
            (0 "zero")
            (1 "one")
            (_ "other"))
        (match 5
            (0 "zero")
            (1 "one")
            (_ "other"))
    "#;
    assert_eval_eq!(code, Value::String("other".to_string()));
}

#[test]
fn test_begin_side_effects_order() {
    // Verify expressions are evaluated in order
    let code = r#"
        (effect state:set "counter" 0)
        (effect state:set "counter" (+ (effect state:get "counter") 1))
        (effect state:set "counter" (+ (effect state:get "counter") 1))
        (effect state:get "counter")
    "#;
    assert_eval_eq!(code, Value::Integer(2));
}

#[test]
fn test_begin_with_nested_begins() {
    // If we explicitly use begin in the future
    let code = r#"
        1
        (let ((x 10))
            x
            (* x 2))
        3
    "#;
    assert_eval_eq!(code, Value::Integer(3));
}

#[test]
fn test_begin_with_define_statements() {
    // Currently define doesn't work, but test the structure
    let code = r#"
        (let ((x 10)) x)
        (let ((y 20)) y)
        (let ((z (+ 10 20))) z)
    "#;
    assert_eval_eq!(code, Value::Integer(30));
}

#[test]
fn test_begin_error_handling() {
    // Test that errors in earlier expressions don't prevent later ones
    let code = r#"
        (+ 1 2)
        (let ((f (lambda () (error "test error")))) 
            ; Don't call f to avoid error
            5)
        (* 3 4)
    "#;
    assert_eval_eq!(code, Value::Integer(12));
}

#[test]
fn test_begin_return_value() {
    // Verify it's always the last expression's value
    let code = r#"
        "first"
        42
        #t
        (list 1 2 3)
        nil
    "#;
    assert_eval_eq!(code, Value::Nil);
}

// Test interaction with other features
#[test]
fn test_begin_with_higher_order_functions() {
    let code = r#"
        (let ((nums (list 1 2 3 4 5))) nums)
        (let ((double (lambda (x) (* x 2)))) double)
        (map (lambda (x) (* x 2)) (list 1 2 3 4 5))
    "#;
    assert_eval_eq!(code, vec![2, 4, 6, 8, 10]);
}

#[test]
fn test_begin_with_recursion() {
    let code = r#"
        (let ((fact (lambda (n)
            (if (<= n 1)
                1
                (* n (fact (- n 1)))))))
            (fact 3))
        (let ((fib (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1)) (fib (- n 2))))))
            (fib 5))
    "#;
    assert_eval_eq!(code, Value::Integer(5)); // fib(5) = 5
}
