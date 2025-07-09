//! Tests for I/O operations

use fluentai_stdlib::io_effects::*;
use fluentai_stdlib::value::Value;
use std::env::temp_dir;
use std::fs;
use std::path::PathBuf;

/// Helper to create a temporary test directory
fn test_dir() -> PathBuf {
    let mut path = temp_dir();
    path.push(format!("fluentai_test_{}", std::process::id()));
    fs::create_dir_all(&path).expect("Failed to create test dir");
    path
}

#[test]
fn test_file_write_and_read() {
    let dir_path = test_dir();
    let file_path = dir_path.join("test.txt");
    let file_path_str = file_path.to_str().unwrap();

    // Write to file
    let content = "Hello, World!";
    let result = file_write_with_effects(&[
        Value::String(file_path_str.to_string()),
        Value::String(content.to_string()),
    ]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);

    // Read from file
    let result = file_read_with_effects(&[Value::String(file_path_str.to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::String(content.to_string()));
}

#[test]
fn test_file_append() {
    let dir_path = test_dir();
    let file_path = dir_path.join("append.txt");
    let file_path_str = file_path.to_str().unwrap();

    // Write initial content
    file_write_with_effects(&[
        Value::String(file_path_str.to_string()),
        Value::String("Hello".to_string()),
    ])
    .unwrap();

    // Append more content
    file_append_with_effects(&[
        Value::String(file_path_str.to_string()),
        Value::String(", World!".to_string()),
    ])
    .unwrap();

    // Read and verify
    let result = file_read_with_effects(&[Value::String(file_path_str.to_string())]).unwrap();
    assert_eq!(result, Value::String("Hello, World!".to_string()));
}

#[test]
fn test_file_exists() {
    let dir_path = test_dir();
    let file_path = dir_path.join("exists.txt");
    let file_path_str = file_path.to_str().unwrap();

    // Check non-existent file
    let result = file_exists_with_effects(&[Value::String(file_path_str.to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(false));

    // Create file
    file_write_with_effects(&[
        Value::String(file_path_str.to_string()),
        Value::String("test".to_string()),
    ])
    .unwrap();

    // Check existing file
    let result = file_exists_with_effects(&[Value::String(file_path_str.to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_file_delete() {
    let dir_path = test_dir();
    let file_path = dir_path.join("delete.txt");
    let file_path_str = file_path.to_str().unwrap();

    // Create file
    file_write_with_effects(&[
        Value::String(file_path_str.to_string()),
        Value::String("to be deleted".to_string()),
    ])
    .unwrap();

    // Verify it exists
    assert!(file_path.exists());

    // Delete file
    let result = file_delete_with_effects(&[Value::String(file_path_str.to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);

    // Verify it's gone
    assert!(!file_path.exists());
}

#[test]
fn test_directory_operations() {
    let dir_path = test_dir();
    let new_dir = dir_path.join("subdir");
    let new_dir_str = new_dir.to_str().unwrap();

    // Create directory
    let result = dir_create_with_effects(&[Value::String(new_dir_str.to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);

    // Create some files in directory
    file_write_with_effects(&[
        Value::String(new_dir.join("file1.txt").to_str().unwrap().to_string()),
        Value::String("content1".to_string()),
    ])
    .unwrap();
    file_write_with_effects(&[
        Value::String(new_dir.join("file2.txt").to_str().unwrap().to_string()),
        Value::String("content2".to_string()),
    ])
    .unwrap();

    // List directory
    let result = dir_list_with_effects(&[Value::String(new_dir_str.to_string())]);
    assert!(result.is_ok());
    if let Value::List(entries) = result.unwrap() {
        assert_eq!(entries.len(), 2);
        let entry_names: Vec<String> = entries
            .into_iter()
            .filter_map(|v| {
                if let Value::String(s) = v {
                    Some(s)
                } else {
                    None
                }
            })
            .collect();
        assert!(entry_names.contains(&"file1.txt".to_string()));
        assert!(entry_names.contains(&"file2.txt".to_string()));
    } else {
        panic!("Expected list of directory entries");
    }
}

#[test]
fn test_current_directory() {
    let result = current_directory_with_effects(&[]);
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), Value::String(_)));
}

#[test]
fn test_error_handling() {
    // Test reading non-existent file
    let result =
        file_read_with_effects(&[Value::String("/definitely/does/not/exist.txt".to_string())]);
    assert!(result.is_err());

    // Test writing to invalid path
    let result = file_write_with_effects(&[
        Value::String("/root/cannot/write/here.txt".to_string()),
        Value::String("content".to_string()),
    ]);
    assert!(result.is_err());

    // Test invalid arguments
    let result = file_read_with_effects(&[Value::Integer(42)]);
    assert!(result.is_err());

    let result = file_write_with_effects(&[Value::String("file.txt".to_string())]);
    assert!(result.is_err());
}

#[test]
fn test_print_operations() {
    // Test print with various value types
    let test_values = vec![
        Value::String("Hello".to_string()),
        Value::Integer(42),
        Value::Float(3.14),
        Value::Boolean(true),
        Value::Nil,
        Value::List(vec![Value::Integer(1), Value::Integer(2)]),
    ];

    for value in test_values {
        // These operations write to stdout, so we just verify they don't panic
        let result = print_with_effects(&[value.clone()]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);

        let result = print_line_with_effects(&[value.clone()]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);
    }

    // Test print with multiple values
    let result = print_with_effects(&[
        Value::String("Hello".to_string()),
        Value::String(" ".to_string()),
        Value::String("World".to_string()),
    ]);
    assert!(result.is_ok());
}

// JSON operations test removed - json_parse and json_stringify are private functions
