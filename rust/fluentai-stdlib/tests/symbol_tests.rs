//! Integration tests for symbol operations

use fluentai_stdlib::{init_stdlib, value::Value};

#[test]
fn test_symbol_to_string() {
    let registry = init_stdlib();
    let symbol_to_string = registry.get("symbol->string").unwrap();

    // Currently, symbols are represented as strings in the VM
    // So this is essentially an identity function
    let result = symbol_to_string
        .call(&[Value::String("my-symbol".to_string())])
        .unwrap();
    assert_eq!(result, Value::String("my-symbol".to_string()));

    // Test with various symbol-like strings
    let result = symbol_to_string
        .call(&[Value::String("+".to_string())])
        .unwrap();
    assert_eq!(result, Value::String("+".to_string()));

    let result = symbol_to_string
        .call(&[Value::String("list->string".to_string())])
        .unwrap();
    assert_eq!(result, Value::String("list->string".to_string()));

    let result = symbol_to_string
        .call(&[Value::String("foo-bar-baz".to_string())])
        .unwrap();
    assert_eq!(result, Value::String("foo-bar-baz".to_string()));
}

#[test]
fn test_symbol_to_string_errors() {
    let registry = init_stdlib();
    let symbol_to_string = registry.get("symbol->string").unwrap();

    // Non-string values should error
    assert!(symbol_to_string.call(&[Value::Integer(42)]).is_err());
    assert!(symbol_to_string.call(&[Value::Boolean(true)]).is_err());
    assert!(symbol_to_string.call(&[Value::List(vec![])]).is_err());

    // Wrong number of arguments
    assert!(symbol_to_string.call(&[]).is_err());
    assert!(symbol_to_string
        .call(&[
            Value::String("sym1".to_string()),
            Value::String("sym2".to_string())
        ])
        .is_err());
}

#[test]
fn test_symbol_string_distinction() {
    // This test documents the current limitation where symbols and strings
    // are not distinguished at runtime in the VM

    let registry = init_stdlib();
    let symbol_to_string = registry.get("symbol->string").unwrap();

    // In a proper implementation, this would only work with symbols,
    // not arbitrary strings. But currently, any string works.
    let string_with_spaces = "this is a string, not a symbol";
    let result = symbol_to_string
        .call(&[Value::String(string_with_spaces.to_string())])
        .unwrap();
    assert_eq!(result, Value::String(string_with_spaces.to_string()));

    // Document that this is a known limitation
    // TODO: When proper symbol support is added to the VM, this test should
    // verify that strings with spaces are rejected as invalid symbols
}
