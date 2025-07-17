//! Tests for set operations using the new Value::Set type

use fluentai_core::value::Value;
use fluentai_stdlib::collections;
use fluentai_stdlib::registry::{StdlibRegistry, FunctionImpl};

// Helper function to call stdlib functions
fn call_func(registry: &StdlibRegistry, name: &str, args: &[Value]) -> Value {
    let func = registry.get(name).unwrap();
    match &func.func {
        FunctionImpl::Simple(f) => f(args).unwrap(),
        _ => panic!("Expected simple function"),
    }
}

#[test]
fn test_set_new() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    // Empty set
    let result = call_func(&registry, "set-new", &[]);
    match result {
        Value::Set(items) => assert_eq!(items.len(), 0),
        _ => panic!("Expected Set, got {:?}", result),
    }
    
    // Set with duplicates - should be unique
    let result = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(1),
        Value::Integer(3),
    ]);
    
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 3);
            assert!(items.contains(&Value::Integer(1)));
            assert!(items.contains(&Value::Integer(2)));
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set, got {:?}", result),
    }
}

#[test]
fn test_set_add() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    // Create a set with some items
    let set = call_func(&registry, "set-new", &[Value::Integer(1), Value::Integer(2)]);
    
    // Add a new item
    let result = call_func(&registry, "set-add", &[set.clone(), Value::Integer(3)]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 3);
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set"),
    }
    
    // Add a duplicate item - should not increase size
    let result = call_func(&registry, "set-add", &[set, Value::Integer(1)]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 2);
        }
        _ => panic!("Expected Set"),
    }
}

#[test]
fn test_set_remove() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    let set = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    
    // Remove an existing item
    let result = call_func(&registry, "set-remove", &[set.clone(), Value::Integer(2)]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 2);
            assert!(!items.contains(&Value::Integer(2)));
            assert!(items.contains(&Value::Integer(1)));
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set"),
    }
    
    // Remove a non-existing item - should not change
    let result = call_func(&registry, "set-remove", &[set, Value::Integer(4)]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 3);
        }
        _ => panic!("Expected Set"),
    }
}

#[test]
fn test_set_contains() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    let set = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::String("hello".to_string()),
        Value::Boolean(true),
    ]);
    
    // Check existing items
    assert_eq!(
        call_func(&registry, "set-contains?", &[set.clone(), Value::Integer(1)]),
        Value::Boolean(true)
    );
    assert_eq!(
        call_func(&registry, "set-contains?", &[set.clone(), Value::String("hello".to_string())]),
        Value::Boolean(true)
    );
    
    // Check non-existing items
    assert_eq!(
        call_func(&registry, "set-contains?", &[set.clone(), Value::Integer(2)]),
        Value::Boolean(false)
    );
    assert_eq!(
        call_func(&registry, "set-contains?", &[set, Value::String("world".to_string())]),
        Value::Boolean(false)
    );
}

#[test]
fn test_set_union() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    let set1 = call_func(&registry, "set-new", &[Value::Integer(1), Value::Integer(2)]);
    let set2 = call_func(&registry, "set-new", &[Value::Integer(2), Value::Integer(3)]);
    let set3 = call_func(&registry, "set-new", &[Value::Integer(3), Value::Integer(4)]);
    
    // Union of two sets
    let result = call_func(&registry, "set-union", &[set1.clone(), set2.clone()]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 3);
            assert!(items.contains(&Value::Integer(1)));
            assert!(items.contains(&Value::Integer(2)));
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set"),
    }
    
    // Union of multiple sets
    let result = call_func(&registry, "set-union", &[set1, set2, set3]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 4);
            for i in 1..=4 {
                assert!(items.contains(&Value::Integer(i)));
            }
        }
        _ => panic!("Expected Set"),
    }
}

#[test]
fn test_set_intersection() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    let set1 = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    
    let set2 = call_func(&registry, "set-new", &[
        Value::Integer(2),
        Value::Integer(3),
        Value::Integer(4),
    ]);
    
    let set3 = call_func(&registry, "set-new", &[
        Value::Integer(3),
        Value::Integer(4),
        Value::Integer(5),
    ]);
    
    // Intersection of two sets
    let result = call_func(&registry, "set-intersection", &[set1.clone(), set2.clone()]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 2);
            assert!(items.contains(&Value::Integer(2)));
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set"),
    }
    
    // Intersection of three sets
    let result = call_func(&registry, "set-intersection", &[set1, set2, set3]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 1);
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set"),
    }
}

#[test]
fn test_set_difference() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    let set1 = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    
    let set2 = call_func(&registry, "set-new", &[
        Value::Integer(2),
        Value::Integer(3),
        Value::Integer(4),
    ]);
    
    // set1 - set2
    let result = call_func(&registry, "set-difference", &[set1.clone(), set2.clone()]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 1);
            assert!(items.contains(&Value::Integer(1)));
        }
        _ => panic!("Expected Set"),
    }
    
    // set2 - set1
    let result = call_func(&registry, "set-difference", &[set2, set1]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 1);
            assert!(items.contains(&Value::Integer(4)));
        }
        _ => panic!("Expected Set"),
    }
}

#[test]
fn test_set_to_list() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    let set = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::String("hello".to_string()),
        Value::Boolean(true),
    ]);
    
    let result = call_func(&registry, "set->list", &[set]);
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert!(items.contains(&Value::Integer(1)));
            assert!(items.contains(&Value::String("hello".to_string())));
            assert!(items.contains(&Value::Boolean(true)));
        }
        _ => panic!("Expected List"),
    }
}

#[test]
fn test_list_to_set() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    // List with duplicates
    let list = Value::List(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(1),
        Value::Integer(3),
        Value::Integer(2),
    ]);
    
    let result = call_func(&registry, "list->set", &[list]);
    match result {
        Value::Set(items) => {
            assert_eq!(items.len(), 3);
            assert!(items.contains(&Value::Integer(1)));
            assert!(items.contains(&Value::Integer(2)));
            assert!(items.contains(&Value::Integer(3)));
        }
        _ => panic!("Expected Set"),
    }
}

#[test]
fn test_set_size() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    // Empty set
    let empty = call_func(&registry, "set-new", &[]);
    assert_eq!(
        call_func(&registry, "set-size", &[empty]),
        Value::Integer(0)
    );
    
    // Set with items
    let set = call_func(&registry, "set-new", &[
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    assert_eq!(
        call_func(&registry, "set-size", &[set]),
        Value::Integer(3)
    );
}

#[test]
fn test_set_with_complex_types() {
    let mut registry = StdlibRegistry::new();
    collections::register(&mut registry);
    
    // Set with lists as elements
    let list1 = Value::List(vec![Value::Integer(1), Value::Integer(2)]);
    let list2 = Value::List(vec![Value::Integer(3), Value::Integer(4)]);
    let list3 = Value::List(vec![Value::Integer(1), Value::Integer(2)]); // Same as list1
    
    let set = call_func(&registry, "set-new", &[list1.clone(), list2.clone(), list3]);
    
    match &set {
        Value::Set(items) => {
            // Should only have 2 unique lists
            assert_eq!(items.len(), 2);
        }
        _ => panic!("Expected Set"),
    }
    
    // Check contains with complex type
    assert_eq!(
        call_func(&registry, "set-contains?", &[set.clone(), list1]),
        Value::Boolean(true)
    );
    
    let list4 = Value::List(vec![Value::Integer(5), Value::Integer(6)]);
    assert_eq!(
        call_func(&registry, "set-contains?", &[set, list4]),
        Value::Boolean(false)
    );
}