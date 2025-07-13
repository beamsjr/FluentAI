//! Integration tests for the interpreter

use fluentai_interpreter::{ExecutionMode, Interpreter, InterpreterOptions};
use fluentai_parser::parse_flc;

#[test]
fn test_basic_arithmetic() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    let test_cases = vec![
        ("1 + 2", 3),
        ("3 * 4", 12),
        ("10 - 3", 7),
        ("20 / 4", 5),
    ];

    for (expr, expected) in test_cases {
        let graph = parse_flc(expr).unwrap();
        let result = interpreter.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(expected), "Failed for: {}", expr);
    }
}

#[test]
fn test_boolean_operations() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    let test_cases = vec![
        ("5 > 3", true),
        ("2 < 4", true),
        ("3 == 3", true),
        ("2 > 4", false),
        ("5 < 3", false),
        ("3 == 4", false),
    ];

    for (expr, expected) in test_cases {
        let graph = parse_flc(expr).unwrap();
        let result = interpreter.interpret(&graph).unwrap();
        assert_eq!(result.to_boolean(), Some(expected), "Failed for: {}", expr);
    }
}

#[test]
fn test_let_bindings() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    let test_cases = vec![
        ("let x = 10; x", 10),
        ("let x = 5; x + 3", 8),
        ("let x = 5; let y = 10; x * y", 50),
        ("let x = 5; let y = 10; x + y", 15),
    ];

    for (expr, expected) in test_cases {
        let graph = parse_flc(expr).unwrap();
        let result = interpreter.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(expected), "Failed for: {}", expr);
    }
}

#[test]
fn test_lambda_functions() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    let test_cases = vec![
        ("((x) => x)(42)", 42),
        ("((x) => x * 2)(5)", 10),
        ("((x, y) => x + y)(3, 4)", 7),
        ("let f = (x) => x * x; f(7)", 49),
    ];

    for (expr, expected) in test_cases {
        let graph = parse_flc(expr).unwrap();
        let result = interpreter.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(expected), "Failed for: {}", expr);
    }
}

#[test]
fn test_if_expressions() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    let test_cases = vec![
        ("if (5 > 3) { 1 } else { 2 }", 1),
        ("if (5 < 3) { 1 } else { 2 }", 2),
        ("if (3 == 3) { 10 } else { 20 }", 10),
    ];

    for (expr, expected) in test_cases {
        let graph = parse_flc(expr).unwrap();
        let result = interpreter.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(expected), "Failed for: {}", expr);
    }
}

#[test]
fn test_list_operations() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    // Test list creation
    let graph = parse_flc("[1, 2, 3]").unwrap();
    let result = interpreter.interpret(&graph).unwrap();
    assert!(result.to_list().is_some());
    assert_eq!(result.to_list().unwrap().len(), 3);

    // Test list functions
    let graph = parse_flc("list(1, 2, 3)").unwrap();
    let result = interpreter.interpret(&graph).unwrap();
    assert!(result.to_list().is_some());
    assert_eq!(result.to_list().unwrap().len(), 3);

    // Test cons
    let graph = parse_flc("cons(0, list(1, 2))").unwrap();
    let result = interpreter.interpret(&graph).unwrap();
    let list = result.to_list().unwrap();
    assert_eq!(list.len(), 3);
    assert_eq!(list[0].to_integer(), Some(0));

    // Test car
    let graph = parse_flc("car(list(1, 2, 3))").unwrap();
    let result = interpreter.interpret(&graph).unwrap();
    assert_eq!(result.to_integer(), Some(1));

    // Test cdr
    let graph = parse_flc("cdr(list(1, 2, 3))").unwrap();
    let result = interpreter.interpret(&graph).unwrap();
    let list = result.to_list().unwrap();
    assert_eq!(list.len(), 2);
    assert_eq!(list[0].to_integer(), Some(2));
}

#[test]
fn test_execution_modes() {
    let mut options = InterpreterOptions::default();
    options.mode = ExecutionMode::TreeWalking;
    let mut interpreter = Interpreter::new(options);

    let graph = parse_flc("1 + 2").unwrap();
    let result = interpreter.interpret(&graph).unwrap();
    assert_eq!(result.to_integer(), Some(3));
}

#[test]
fn test_error_handling() {
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    // Test undefined variable
    let graph = parse_flc("x").unwrap();
    let result = interpreter.interpret(&graph);
    assert!(result.is_err());

    // Test arity error - cons with only one argument
    let graph = parse_flc("cons(1)").unwrap();
    let result = interpreter.interpret(&graph);
    assert!(result.is_err());

    // Test division by zero
    let graph = parse_flc("10 / 0").unwrap();
    let result = interpreter.interpret(&graph);
    assert!(result.is_err());
}
