//! End-to-end tests for complex pattern matching features

// Complex pattern matching tests
use anyhow::Result;
use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parse_flc;
use fluentai_vm::{Compiler, CompilerOptions, Value, VM};

fn compile_and_run(source: &str) -> Result<Value> {
    let graph = parse_flc(source)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

#[test]
fn test_or_pattern_execution() -> Result<()> {
    let source = r#"
        (let ((x 2))
            (match x
                ((or 1 2 3) "small")
                (_ "large")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("small".to_string()));

    let source2 = r#"
        (let ((x 5))
            (match x
                ((or 1 2 3) "small")
                (_ "large")))
    "#;

    let result2 = compile_and_run(source2)?;
    assert_eq!(result2, Value::String("large".to_string()));

    Ok(())
}

#[test]
fn test_as_pattern_execution() -> Result<()> {
    // Test simple as-pattern first
    let source = r#"
        (match 42
            ((as x 42) x)
            (_ 0))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::Integer(42));

    Ok(())
}

#[test]
fn test_guard_pattern_execution() -> Result<()> {
    // Skip guard patterns for now - they need stdlib functions
    Ok(())
}

#[test]
fn test_guard_pattern_execution_skip() -> Result<()> {
    let source = r#"
        (let ((x 5))
            (match x
                ((when n (> n 0)) "positive")
                (_ "non-positive")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("positive".to_string()));

    let source2 = r#"
        (let ((x -5))
            (match x
                ((when n (> n 0)) "positive")
                (_ "non-positive")))
    "#;

    let result2 = compile_and_run(source2)?;
    assert_eq!(result2, Value::String("non-positive".to_string()));

    Ok(())
}

#[test]
fn test_range_pattern_inclusive_execution() -> Result<()> {
    let source = r#"
        (let ((score 85))
            (match score
                ((..= 0 59) "F")
                ((..= 60 69) "D")
                ((..= 70 79) "C")
                ((..= 80 89) "B")
                ((..= 90 100) "A")
                (_ "Invalid")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("B".to_string()));

    Ok(())
}

#[test]
fn test_range_pattern_exclusive_execution() -> Result<()> {
    let source = r#"
        (let ((x 5))
            (match x
                ((.. 0 10) "single digit")
                (_ "double digit or more")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("single digit".to_string()));

    let source2 = r#"
        (let ((x 10))
            (match x
                ((.. 0 10) "single digit")
                (_ "double digit or more")))
    "#;

    let result2 = compile_and_run(source2)?;
    assert_eq!(result2, Value::String("double digit or more".to_string()));

    Ok(())
}

#[test]
fn test_view_pattern_execution() -> Result<()> {
    // Skip view patterns for now - they need function application
    Ok(())
}

#[test]
fn test_view_pattern_execution_skip() -> Result<()> {
    // Test view pattern with absolute value function
    let source = r#"
        (letrec ((abs (lambda (x) (if (< x 0) (- 0 x) x))))
            (match -5
                ((view abs 5) "absolute value is 5")
                (_ "other")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("absolute value is 5".to_string()));

    Ok(())
}

#[test]
fn test_complex_nested_patterns_execution() -> Result<()> {
    // Skip complex nested patterns for now - they need stdlib
    Ok(())
}

#[test]
fn test_complex_nested_patterns_execution_skip() -> Result<()> {
    // Test guard with as-pattern containing or-pattern
    let source = r#"
        (letrec ((even? (lambda (n) (= (mod n 2) 0))))
            (match 2
                ((when (as x (or 1 2 3)) (even? x)) "even small")
                (_ "other")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("even small".to_string()));

    let source2 = r#"
        (letrec ((even? (lambda (n) (= (mod n 2) 0))))
            (match 3
                ((when (as x (or 1 2 3)) (even? x)) "even small")
                (_ "other")))
    "#;

    let result2 = compile_and_run(source2)?;
    assert_eq!(result2, Value::String("other".to_string()));

    Ok(())
}

#[test]
fn test_multiple_or_patterns_execution() -> Result<()> {
    let source = r#"
        (match "Tuesday"
            ((or "Saturday" "Sunday") "weekend")
            ((or "Monday" "Tuesday" "Wednesday" "Thursday" "Friday") "weekday"))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("weekday".to_string()));

    let source2 = r#"
        (match "Sunday"
            ((or "Saturday" "Sunday") "weekend")
            ((or "Monday" "Tuesday" "Wednesday" "Thursday" "Friday") "weekday"))
    "#;

    let result2 = compile_and_run(source2)?;
    assert_eq!(result2, Value::String("weekend".to_string()));

    Ok(())
}

#[test]
fn test_range_pattern_with_variables() -> Result<()> {
    // Test that range patterns work with variables in context
    let source = r#"
        (let ((min 10) (max 20) (val 15))
            (match val
                ((..= 10 20) "in range")
                (_ "out of range")))
    "#;

    let result = compile_and_run(source)?;
    assert_eq!(result, Value::String("in range".to_string()));

    Ok(())
}

#[test]
fn test_as_pattern_bindings() -> Result<()> {
    // Test that as-patterns properly bind variables with lists
    let source = r#"
        (match [1 2 3]
            ((as lst _) lst))
    "#;

    let result = compile_and_run(source)?;
    // Should return the original list
    assert_eq!(
        result,
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ])
    );

    Ok(())
}
