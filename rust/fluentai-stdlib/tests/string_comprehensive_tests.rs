//! Comprehensive tests for string functions to improve coverage

use fluentai_stdlib::value::Value;
use fluentai_stdlib::init_stdlib;

#[test]
fn test_string_case_functions() {
    let stdlib = init_stdlib();
    
    // Test string-upcase with mixed content
    let upcase = stdlib.get("string-upcase").unwrap();
    assert_eq!(
        upcase.call(&[Value::String("Hello World 123!".to_string())]).unwrap(),
        Value::String("HELLO WORLD 123!".to_string())
    );
    assert_eq!(
        upcase.call(&[Value::String("ALREADY UPPER".to_string())]).unwrap(),
        Value::String("ALREADY UPPER".to_string())
    );
    assert_eq!(
        upcase.call(&[Value::String("".to_string())]).unwrap(),
        Value::String("".to_string())
    );
    
    // Test string-downcase with mixed content
    let downcase = stdlib.get("string-downcase").unwrap();
    assert_eq!(
        downcase.call(&[Value::String("Hello World 123!".to_string())]).unwrap(),
        Value::String("hello world 123!".to_string())
    );
    assert_eq!(
        downcase.call(&[Value::String("already lower".to_string())]).unwrap(),
        Value::String("already lower".to_string())
    );
    
    // Test string-capitalize with various inputs
    let capitalize = stdlib.get("string-capitalize").unwrap();
    assert_eq!(
        capitalize.call(&[Value::String("hello world".to_string())]).unwrap(),
        Value::String("Hello world".to_string())
    );
    assert_eq!(
        capitalize.call(&[Value::String("HELLO WORLD".to_string())]).unwrap(),
        Value::String("Hello world".to_string())
    );
    assert_eq!(
        capitalize.call(&[Value::String("123abc".to_string())]).unwrap(),
        Value::String("123abc".to_string())
    );
    assert_eq!(
        capitalize.call(&[Value::String("".to_string())]).unwrap(),
        Value::String("".to_string())
    );
    assert_eq!(
        capitalize.call(&[Value::String("a".to_string())]).unwrap(),
        Value::String("A".to_string())
    );
}

#[test]
#[ignore = "TODO: Implement string-pad-left, string-pad-right, string-pad-center functions"]
fn test_string_padding() {
    let stdlib = init_stdlib();
    
    // Test string-pad-left
    let pad_left = stdlib.get("string-pad-left").unwrap();
    assert_eq!(
        pad_left.call(&[Value::String("test".to_string()), Value::Integer(8)]).unwrap(),
        Value::String("    test".to_string())
    );
    assert_eq!(
        pad_left.call(&[Value::String("test".to_string()), Value::Integer(4)]).unwrap(),
        Value::String("test".to_string())  // No padding needed
    );
    assert_eq!(
        pad_left.call(&[Value::String("test".to_string()), Value::Integer(2)]).unwrap(),
        Value::String("test".to_string())  // String already longer
    );
    assert_eq!(
        pad_left.call(&[Value::String("".to_string()), Value::Integer(3)]).unwrap(),
        Value::String("   ".to_string())
    );
    
    // Test string-pad-right
    let pad_right = stdlib.get("string-pad-right").unwrap();
    assert_eq!(
        pad_right.call(&[Value::String("test".to_string()), Value::Integer(8)]).unwrap(),
        Value::String("test    ".to_string())
    );
    assert_eq!(
        pad_right.call(&[Value::String("test".to_string()), Value::Integer(4)]).unwrap(),
        Value::String("test".to_string())  // No padding needed
    );
    
    // Test string-pad-center
    let pad_center = stdlib.get("string-pad-center").unwrap();
    assert_eq!(
        pad_center.call(&[Value::String("test".to_string()), Value::Integer(8)]).unwrap(),
        Value::String("  test  ".to_string())
    );
    assert_eq!(
        pad_center.call(&[Value::String("test".to_string()), Value::Integer(9)]).unwrap(),
        Value::String("  test   ".to_string())  // Odd padding goes to the right
    );
    assert_eq!(
        pad_center.call(&[Value::String("test".to_string()), Value::Integer(4)]).unwrap(),
        Value::String("test".to_string())  // No padding needed
    );
}

#[test]
#[ignore = "TODO: Implement string-repeat function"]
fn test_string_repeat() {
    let stdlib = init_stdlib();
    
    let repeat = stdlib.get("string-repeat").unwrap();
    assert_eq!(
        repeat.call(&[Value::String("ab".to_string()), Value::Integer(3)]).unwrap(),
        Value::String("ababab".to_string())
    );
    assert_eq!(
        repeat.call(&[Value::String("test".to_string()), Value::Integer(0)]).unwrap(),
        Value::String("".to_string())
    );
    assert_eq!(
        repeat.call(&[Value::String("x".to_string()), Value::Integer(5)]).unwrap(),
        Value::String("xxxxx".to_string())
    );
    assert_eq!(
        repeat.call(&[Value::String("".to_string()), Value::Integer(10)]).unwrap(),
        Value::String("".to_string())
    );
}

#[test]
#[ignore = "TODO: Implement string-reverse function"]
fn test_string_reverse() {
    let stdlib = init_stdlib();
    
    let reverse = stdlib.get("string-reverse").unwrap();
    assert_eq!(
        reverse.call(&[Value::String("hello".to_string())]).unwrap(),
        Value::String("olleh".to_string())
    );
    assert_eq!(
        reverse.call(&[Value::String("12345".to_string())]).unwrap(),
        Value::String("54321".to_string())
    );
    assert_eq!(
        reverse.call(&[Value::String("a".to_string())]).unwrap(),
        Value::String("a".to_string())
    );
    assert_eq!(
        reverse.call(&[Value::String("".to_string())]).unwrap(),
        Value::String("".to_string())
    );
    // Test with unicode
    assert_eq!(
        reverse.call(&[Value::String("café".to_string())]).unwrap(),
        Value::String("éfac".to_string())
    );
}

#[test]
#[ignore = "TODO: Implement string-index-of function"]
fn test_string_index_of() {
    let stdlib = init_stdlib();
    
    let index_of = stdlib.get("string-index-of").unwrap();
    
    // Basic search
    assert_eq!(
        index_of.call(&[
            Value::String("hello world".to_string()),
            Value::String("world".to_string())
        ]).unwrap(),
        Value::Integer(6)
    );
    
    // Search at beginning
    assert_eq!(
        index_of.call(&[
            Value::String("hello world".to_string()),
            Value::String("hello".to_string())
        ]).unwrap(),
        Value::Integer(0)
    );
    
    // Not found
    assert_eq!(
        index_of.call(&[
            Value::String("hello world".to_string()),
            Value::String("xyz".to_string())
        ]).unwrap(),
        Value::Integer(-1)
    );
    
    // Empty needle
    assert_eq!(
        index_of.call(&[
            Value::String("hello".to_string()),
            Value::String("".to_string())
        ]).unwrap(),
        Value::Integer(0)
    );
    
    // Empty haystack
    assert_eq!(
        index_of.call(&[
            Value::String("".to_string()),
            Value::String("test".to_string())
        ]).unwrap(),
        Value::Integer(-1)
    );
    
    // Multiple occurrences (finds first)
    assert_eq!(
        index_of.call(&[
            Value::String("abcabc".to_string()),
            Value::String("abc".to_string())
        ]).unwrap(),
        Value::Integer(0)
    );
}

#[test]
#[ignore = "TODO: Implement string-format function"]
fn test_string_format() {
    let stdlib = init_stdlib();
    
    let format = stdlib.get("string-format").unwrap();
    
    // Test various format specifiers
    assert_eq!(
        format.call(&[
            Value::String("Hello, {}!".to_string()),
            Value::String("World".to_string())
        ]).unwrap(),
        Value::String("Hello, World!".to_string())
    );
    
    // Multiple placeholders
    assert_eq!(
        format.call(&[
            Value::String("{} + {} = {}".to_string()),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(5)
        ]).unwrap(),
        Value::String("2 + 3 = 5".to_string())
    );
    
    // No placeholders
    assert_eq!(
        format.call(&[
            Value::String("No placeholders".to_string()),
            Value::String("ignored".to_string())
        ]).unwrap(),
        Value::String("No placeholders".to_string())
    );
    
    // More args than placeholders
    assert_eq!(
        format.call(&[
            Value::String("Only {}".to_string()),
            Value::String("one".to_string()),
            Value::String("extra".to_string())
        ]).unwrap(),
        Value::String("Only one".to_string())
    );
    
    // Fewer args than placeholders
    assert_eq!(
        format.call(&[
            Value::String("{} and {}".to_string()),
            Value::String("one".to_string())
        ]).unwrap(),
        Value::String("one and {}".to_string())
    );
}

#[test]
#[ignore = "TODO: Implement symbol->string function"]
fn test_string_conversion_functions() {
    let stdlib = init_stdlib();
    
    // Test number->string
    let num_to_str = stdlib.get("number->string").unwrap();
    assert_eq!(
        num_to_str.call(&[Value::Integer(42)]).unwrap(),
        Value::String("42".to_string())
    );
    assert_eq!(
        num_to_str.call(&[Value::Float(3.14)]).unwrap(),
        Value::String("3.14".to_string())
    );
    assert_eq!(
        num_to_str.call(&[Value::Float(-123.456)]).unwrap(),
        Value::String("-123.456".to_string())
    );
    
    // Test symbol->string
    let sym_to_str = stdlib.get("symbol->string").unwrap();
    assert_eq!(
        sym_to_str.call(&[Value::String("my-symbol".to_string())]).unwrap(),
        Value::String("my-symbol".to_string())
    );
    
    // Test list->string
    let list_to_str = stdlib.get("list->string").unwrap();
    assert_eq!(
        list_to_str.call(&[Value::List(vec![
            Value::String("H".to_string()),
            Value::String("e".to_string()),
            Value::String("l".to_string()),
            Value::String("l".to_string()),
            Value::String("o".to_string())
        ])]).unwrap(),
        Value::String("Hello".to_string())
    );
    
    // Empty list
    assert_eq!(
        list_to_str.call(&[Value::List(vec![])]).unwrap(),
        Value::String("".to_string())
    );
    
    // Test string->list
    let str_to_list = stdlib.get("string->list").unwrap();
    let result = str_to_list.call(&[Value::String("Hello".to_string())]).unwrap();
    match result {
        Value::List(chars) => {
            assert_eq!(chars.len(), 5);
            assert_eq!(chars[0], Value::String("H".to_string()));
            assert_eq!(chars[1], Value::String("e".to_string()));
            assert_eq!(chars[4], Value::String("o".to_string()));
        }
        _ => panic!("Expected list"),
    }
}

#[test]
#[ignore = "TODO: Implement char predicate functions"]
fn test_char_predicates() {
    let stdlib = init_stdlib();
    
    // Test char-alphabetic?
    let alphabetic = stdlib.get("char-alphabetic?").unwrap();
    assert_eq!(
        alphabetic.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        alphabetic.call(&[Value::String("Z".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        alphabetic.call(&[Value::String("5".to_string())]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        alphabetic.call(&[Value::String(" ".to_string())]).unwrap(),
        Value::Boolean(false)
    );
    
    // Test char-numeric?
    let numeric = stdlib.get("char-numeric?").unwrap();
    assert_eq!(
        numeric.call(&[Value::String("0".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        numeric.call(&[Value::String("9".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        numeric.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(false)
    );
    
    // Test char-whitespace?
    let whitespace = stdlib.get("char-whitespace?").unwrap();
    assert_eq!(
        whitespace.call(&[Value::String(" ".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        whitespace.call(&[Value::String("\t".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        whitespace.call(&[Value::String("\n".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        whitespace.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(false)
    );
    
    // Test char-upper-case? and char-lower-case?
    let upper = stdlib.get("char-upper-case?").unwrap();
    let lower = stdlib.get("char-lower-case?").unwrap();
    
    assert_eq!(
        upper.call(&[Value::String("A".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        upper.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        lower.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        lower.call(&[Value::String("A".to_string())]).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
#[ignore = "TODO: Implement missing string functions for error tests"]
fn test_error_handling() {
    let stdlib = init_stdlib();
    
    // Test functions with wrong argument types
    let upcase = stdlib.get("string-upcase").unwrap();
    assert!(upcase.call(&[Value::Integer(42)]).is_err());
    
    // Test functions with wrong number of arguments
    assert!(upcase.call(&[]).is_err());
    assert!(upcase.call(&[Value::String("a".to_string()), Value::String("b".to_string())]).is_err());
    
    // Test char functions with non-single-char strings
    let char_upper = stdlib.get("char-upper-case?").unwrap();
    assert!(char_upper.call(&[Value::String("ab".to_string())]).is_err());
    assert!(char_upper.call(&[Value::String("".to_string())]).is_err());
    
    // Test negative repeat count
    let repeat = stdlib.get("string-repeat").unwrap();
    assert!(repeat.call(&[Value::String("x".to_string()), Value::Integer(-1)]).is_err());
}