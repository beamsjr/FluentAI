//! Tests for string stdlib functions

use fluentai_stdlib::init_stdlib;
use fluentai_stdlib::value::Value;

#[test]
fn test_string_length() {
    let stdlib = init_stdlib();
    let string_length = stdlib.get("string-length").unwrap();

    assert_eq!(
        string_length
            .call(&[Value::String("hello".to_string())])
            .unwrap(),
        Value::Integer(5)
    );

    assert_eq!(
        string_length
            .call(&[Value::String("".to_string())])
            .unwrap(),
        Value::Integer(0)
    );

    assert_eq!(
        string_length
            .call(&[Value::String("こんにちは".to_string())])
            .unwrap(),
        Value::Integer(5) // 5 characters, not bytes
    );
}

#[test]
fn test_string_concat() {
    let stdlib = init_stdlib();
    let string_concat = stdlib.get("string-concat").unwrap();

    let result = string_concat
        .call(&[
            Value::String("hello".to_string()),
            Value::String(" ".to_string()),
            Value::String("world".to_string()),
        ])
        .unwrap();

    assert_eq!(result, Value::String("hello world".to_string()));

    // Test single string
    let result = string_concat
        .call(&[Value::String("alone".to_string())])
        .unwrap();
    assert_eq!(result, Value::String("alone".to_string()));
}

#[test]
fn test_string_upcase_downcase() {
    let stdlib = init_stdlib();
    let upcase = stdlib.get("string-upcase").unwrap();
    let downcase = stdlib.get("string-downcase").unwrap();

    assert_eq!(
        upcase.call(&[Value::String("hello".to_string())]).unwrap(),
        Value::String("HELLO".to_string())
    );

    assert_eq!(
        downcase
            .call(&[Value::String("HELLO".to_string())])
            .unwrap(),
        Value::String("hello".to_string())
    );

    assert_eq!(
        upcase
            .call(&[Value::String("Hello World!".to_string())])
            .unwrap(),
        Value::String("HELLO WORLD!".to_string())
    );
}

#[test]
fn test_string_capitalize() {
    let stdlib = init_stdlib();
    let capitalize = stdlib.get("string-capitalize").unwrap();

    assert_eq!(
        capitalize
            .call(&[Value::String("hello".to_string())])
            .unwrap(),
        Value::String("Hello".to_string())
    );

    assert_eq!(
        capitalize
            .call(&[Value::String("HELLO".to_string())])
            .unwrap(),
        Value::String("Hello".to_string())
    );

    assert_eq!(
        capitalize.call(&[Value::String("".to_string())]).unwrap(),
        Value::String("".to_string())
    );
}

#[test]
fn test_string_split() {
    let stdlib = init_stdlib();
    let split = stdlib.get("string-split").unwrap();

    let result = split
        .call(&[
            Value::String("hello,world,test".to_string()),
            Value::String(",".to_string()),
        ])
        .unwrap();

    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::String("hello".to_string()));
            assert_eq!(items[1], Value::String("world".to_string()));
            assert_eq!(items[2], Value::String("test".to_string()));
        }
        _ => panic!("Expected list"),
    }

    // Test split with empty delimiter (split into chars)
    let result = split
        .call(&[
            Value::String("abc".to_string()),
            Value::String("".to_string()),
        ])
        .unwrap();

    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::String("a".to_string()));
            assert_eq!(items[1], Value::String("b".to_string()));
            assert_eq!(items[2], Value::String("c".to_string()));
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_string_join() {
    let stdlib = init_stdlib();
    let join = stdlib.get("string-join").unwrap();

    let result = join
        .call(&[
            Value::String(" ".to_string()),
            Value::List(vec![
                Value::String("hello".to_string()),
                Value::String("world".to_string()),
            ]),
        ])
        .unwrap();

    assert_eq!(result, Value::String("hello world".to_string()));

    // Test join with comma
    let result = join
        .call(&[
            Value::String(", ".to_string()),
            Value::List(vec![
                Value::String("apple".to_string()),
                Value::String("banana".to_string()),
                Value::String("cherry".to_string()),
            ]),
        ])
        .unwrap();

    assert_eq!(result, Value::String("apple, banana, cherry".to_string()));
}

#[test]
fn test_string_trim() {
    let stdlib = init_stdlib();
    let trim = stdlib.get("string-trim").unwrap();

    assert_eq!(
        trim.call(&[Value::String("  hello  ".to_string())])
            .unwrap(),
        Value::String("hello".to_string())
    );

    assert_eq!(
        trim.call(&[Value::String("\t\nhello\r\n".to_string())])
            .unwrap(),
        Value::String("hello".to_string())
    );

    assert_eq!(
        trim.call(&[Value::String("hello".to_string())]).unwrap(),
        Value::String("hello".to_string())
    );
}

#[test]
fn test_string_contains() {
    let stdlib = init_stdlib();
    let contains = stdlib.get("string-contains?").unwrap();

    assert_eq!(
        contains
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("world".to_string()),
            ])
            .unwrap(),
        Value::Boolean(true)
    );

    assert_eq!(
        contains
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("foo".to_string()),
            ])
            .unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_string_starts_ends_with() {
    let stdlib = init_stdlib();
    let starts_with = stdlib.get("string-starts-with?").unwrap();
    let ends_with = stdlib.get("string-ends-with?").unwrap();

    assert_eq!(
        starts_with
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("hello".to_string()),
            ])
            .unwrap(),
        Value::Boolean(true)
    );

    assert_eq!(
        starts_with
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("world".to_string()),
            ])
            .unwrap(),
        Value::Boolean(false)
    );

    assert_eq!(
        ends_with
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("world".to_string()),
            ])
            .unwrap(),
        Value::Boolean(true)
    );

    assert_eq!(
        ends_with
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("hello".to_string()),
            ])
            .unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_string_replace() {
    let stdlib = init_stdlib();
    let replace = stdlib.get("string-replace").unwrap();

    assert_eq!(
        replace
            .call(&[
                Value::String("hello world".to_string()),
                Value::String("world".to_string()),
                Value::String("universe".to_string()),
            ])
            .unwrap(),
        Value::String("hello universe".to_string())
    );

    assert_eq!(
        replace
            .call(&[
                Value::String("foo bar foo baz".to_string()),
                Value::String("foo".to_string()),
                Value::String("qux".to_string()),
            ])
            .unwrap(),
        Value::String("qux bar qux baz".to_string())
    );
}

#[test]
fn test_substring() {
    let stdlib = init_stdlib();
    let substring = stdlib.get("substring").unwrap();

    // Test with start index only
    assert_eq!(
        substring
            .call(&[Value::String("hello world".to_string()), Value::Integer(6),])
            .unwrap(),
        Value::String("world".to_string())
    );

    // Test with start and end index
    assert_eq!(
        substring
            .call(&[
                Value::String("hello world".to_string()),
                Value::Integer(0),
                Value::Integer(5),
            ])
            .unwrap(),
        Value::String("hello".to_string())
    );

    assert_eq!(
        substring
            .call(&[
                Value::String("hello world".to_string()),
                Value::Integer(6),
                Value::Integer(11),
            ])
            .unwrap(),
        Value::String("world".to_string())
    );
}

#[test]
fn test_string_ref() {
    let stdlib = init_stdlib();
    let string_ref = stdlib.get("string-ref").unwrap();

    assert_eq!(
        string_ref
            .call(&[Value::String("hello".to_string()), Value::Integer(0),])
            .unwrap(),
        Value::String("h".to_string())
    );

    assert_eq!(
        string_ref
            .call(&[Value::String("hello".to_string()), Value::Integer(4),])
            .unwrap(),
        Value::String("o".to_string())
    );

    // Test out of bounds
    assert!(string_ref
        .call(&[Value::String("hello".to_string()), Value::Integer(5),])
        .is_err());
}

#[test]
fn test_string_conversions() {
    let stdlib = init_stdlib();
    let string_to_list = stdlib.get("string->list").unwrap();
    let list_to_string = stdlib.get("list->string").unwrap();
    let string_to_number = stdlib.get("string->number").unwrap();
    let number_to_string = stdlib.get("number->string").unwrap();

    // Test string->list
    let result = string_to_list
        .call(&[Value::String("abc".to_string())])
        .unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::String("a".to_string()));
            assert_eq!(items[1], Value::String("b".to_string()));
            assert_eq!(items[2], Value::String("c".to_string()));
        }
        _ => panic!("Expected list"),
    }

    // Test list->string
    let result = list_to_string
        .call(&[Value::List(vec![
            Value::String("a".to_string()),
            Value::String("b".to_string()),
            Value::String("c".to_string()),
        ])])
        .unwrap();
    assert_eq!(result, Value::String("abc".to_string()));

    // Test string->number
    assert_eq!(
        string_to_number
            .call(&[Value::String("42".to_string())])
            .unwrap(),
        Value::Integer(42)
    );

    assert_eq!(
        string_to_number
            .call(&[Value::String("3.14".to_string())])
            .unwrap(),
        Value::Float(3.14)
    );

    assert!(string_to_number
        .call(&[Value::String("not a number".to_string())])
        .is_err());

    // Test number->string
    assert_eq!(
        number_to_string.call(&[Value::Integer(42)]).unwrap(),
        Value::String("42".to_string())
    );

    assert_eq!(
        number_to_string.call(&[Value::Float(3.14)]).unwrap(),
        Value::String("3.14".to_string())
    );
}

#[test]
fn test_char_conversions() {
    let stdlib = init_stdlib();
    let char_to_int = stdlib.get("char->int").unwrap();
    let int_to_char = stdlib.get("int->char").unwrap();

    // Test char->int
    assert_eq!(
        char_to_int.call(&[Value::String("A".to_string())]).unwrap(),
        Value::Integer(65)
    );

    assert_eq!(
        char_to_int.call(&[Value::String("a".to_string())]).unwrap(),
        Value::Integer(97)
    );

    assert_eq!(
        char_to_int.call(&[Value::String("0".to_string())]).unwrap(),
        Value::Integer(48)
    );

    // Test int->char
    assert_eq!(
        int_to_char.call(&[Value::Integer(65)]).unwrap(),
        Value::String("A".to_string())
    );

    assert_eq!(
        int_to_char.call(&[Value::Integer(97)]).unwrap(),
        Value::String("a".to_string())
    );

    assert_eq!(
        int_to_char.call(&[Value::Integer(48)]).unwrap(),
        Value::String("0".to_string())
    );

    // Test invalid unicode code point
    assert!(int_to_char.call(&[Value::Integer(-1)]).is_err());
    assert!(int_to_char.call(&[Value::Integer(0x110000)]).is_err());
}
