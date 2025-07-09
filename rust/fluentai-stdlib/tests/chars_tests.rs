//! Integration tests for character functions

use fluentai_stdlib::{init_stdlib, value::Value};

#[test]
fn test_char_predicates() {
    let registry = init_stdlib();

    // Test char-alphabetic?
    let alphabetic = registry.get("char-alphabetic?").unwrap();
    assert_eq!(
        alphabetic.call(&[Value::String("A".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        alphabetic.call(&[Value::String("é".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        alphabetic.call(&[Value::String("3".to_string())]).unwrap(),
        Value::Boolean(false)
    );

    // Test char-numeric?
    let numeric = registry.get("char-numeric?").unwrap();
    assert_eq!(
        numeric.call(&[Value::String("5".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        numeric.call(&[Value::String("⅝".to_string())]).unwrap(), // Unicode numeric
        Value::Boolean(true)
    );
    assert_eq!(
        numeric.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(false)
    );

    // Test char-whitespace?
    let whitespace = registry.get("char-whitespace?").unwrap();
    assert_eq!(
        whitespace.call(&[Value::String(" ".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        whitespace.call(&[Value::String("\n".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        whitespace
            .call(&[Value::String("\u{00A0}".to_string())])
            .unwrap(), // Non-breaking space
        Value::Boolean(true)
    );

    // Test char-upper-case? and char-lower-case?
    let upper = registry.get("char-upper-case?").unwrap();
    let lower = registry.get("char-lower-case?").unwrap();

    assert_eq!(
        upper.call(&[Value::String("A".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        upper.call(&[Value::String("Ñ".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        lower.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        lower.call(&[Value::String("ñ".to_string())]).unwrap(),
        Value::Boolean(true)
    );
}

#[test]
fn test_char_case_conversion() {
    let registry = init_stdlib();

    let upcase = registry.get("char-upcase").unwrap();
    let downcase = registry.get("char-downcase").unwrap();

    // Basic case conversion
    assert_eq!(
        upcase.call(&[Value::String("a".to_string())]).unwrap(),
        Value::String("A".to_string())
    );
    assert_eq!(
        downcase.call(&[Value::String("A".to_string())]).unwrap(),
        Value::String("a".to_string())
    );

    // Unicode case conversion
    assert_eq!(
        upcase.call(&[Value::String("ñ".to_string())]).unwrap(),
        Value::String("Ñ".to_string())
    );

    // Special case: German eszett expands to two characters
    assert_eq!(
        upcase.call(&[Value::String("ß".to_string())]).unwrap(),
        Value::String("SS".to_string())
    );
}

#[test]
fn test_char_comparisons() {
    let registry = init_stdlib();

    // Test char=?
    let char_eq = registry.get("char=?").unwrap();
    assert_eq!(
        char_eq
            .call(&[
                Value::String("a".to_string()),
                Value::String("a".to_string())
            ])
            .unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        char_eq
            .call(&[
                Value::String("a".to_string()),
                Value::String("A".to_string())
            ])
            .unwrap(),
        Value::Boolean(false)
    );

    // Test char<?
    let char_lt = registry.get("char<?").unwrap();
    assert_eq!(
        char_lt
            .call(&[
                Value::String("a".to_string()),
                Value::String("b".to_string())
            ])
            .unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        char_lt
            .call(&[
                Value::String("A".to_string()),
                Value::String("a".to_string())
            ])
            .unwrap(),
        Value::Boolean(true) // 'A' < 'a' in Unicode
    );

    // Test char>?
    let char_gt = registry.get("char>?").unwrap();
    assert_eq!(
        char_gt
            .call(&[
                Value::String("z".to_string()),
                Value::String("a".to_string())
            ])
            .unwrap(),
        Value::Boolean(true)
    );
}

#[test]
fn test_char_case_insensitive_comparisons() {
    let registry = init_stdlib();

    // Test char-ci=?
    let char_ci_eq = registry.get("char-ci=?").unwrap();
    assert_eq!(
        char_ci_eq
            .call(&[
                Value::String("a".to_string()),
                Value::String("A".to_string())
            ])
            .unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        char_ci_eq
            .call(&[
                Value::String("a".to_string()),
                Value::String("b".to_string())
            ])
            .unwrap(),
        Value::Boolean(false)
    );

    // Test char-ci<?
    let char_ci_lt = registry.get("char-ci<?").unwrap();
    assert_eq!(
        char_ci_lt
            .call(&[
                Value::String("a".to_string()),
                Value::String("B".to_string())
            ])
            .unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        char_ci_lt
            .call(&[
                Value::String("A".to_string()),
                Value::String("b".to_string())
            ])
            .unwrap(),
        Value::Boolean(true)
    );
}

#[test]
fn test_special_characters() {
    let registry = init_stdlib();

    // Test control characters
    let control = registry.get("char-control?").unwrap();
    assert_eq!(
        control.call(&[Value::String("\n".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        control.call(&[Value::String("\t".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        control.call(&[Value::String("\x1B".to_string())]).unwrap(), // ESC
        Value::Boolean(true)
    );

    // Test ASCII
    let ascii = registry.get("char-ascii?").unwrap();
    assert_eq!(
        ascii.call(&[Value::String("A".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        ascii.call(&[Value::String("€".to_string())]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        ascii.call(&[Value::String("🎉".to_string())]).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_error_handling() {
    let registry = init_stdlib();
    let alphabetic = registry.get("char-alphabetic?").unwrap();

    // Empty string
    assert!(alphabetic.call(&[Value::String("".to_string())]).is_err());

    // Multi-character string
    assert!(alphabetic
        .call(&[Value::String("abc".to_string())])
        .is_err());

    // Non-string argument
    assert!(alphabetic.call(&[Value::Integer(65)]).is_err());
    assert!(alphabetic.call(&[Value::Boolean(true)]).is_err());

    // Wrong number of arguments
    assert!(alphabetic.call(&[]).is_err());
    assert!(alphabetic
        .call(&[
            Value::String("a".to_string()),
            Value::String("b".to_string())
        ])
        .is_err());
}
