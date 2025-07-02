//! Tests for collections stdlib functions

use claudelang_stdlib::value::Value;
use claudelang_stdlib::init_stdlib;
use std::collections::HashMap;

#[test]
fn test_list_operations() {
    let stdlib = init_stdlib();
    
    // Test cons
    let cons = stdlib.get("cons").unwrap();
    let result = cons.call(&[
        Value::Int(1),
        Value::List(vec![Value::Int(2), Value::Int(3)])
    ]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Int(1));
            assert_eq!(items[1], Value::Int(2));
            assert_eq!(items[2], Value::Int(3));
        }
        _ => panic!("Expected list"),
    }
    
    // Test car
    let car = stdlib.get("car").unwrap();
    let list = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    assert_eq!(
        car.call(&[list.clone()]).unwrap(),
        Value::Int(1)
    );
    
    // Test car on empty list
    assert!(car.call(&[Value::List(vec![])]).is_err());
    
    // Test cdr
    let cdr = stdlib.get("cdr").unwrap();
    let result = cdr.call(&[list.clone()]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Int(2));
            assert_eq!(items[1], Value::Int(3));
        }
        _ => panic!("Expected list"),
    }
    
    // Test cdr on empty list
    assert!(cdr.call(&[Value::List(vec![])]).is_err());
    
    // Test null?
    let null = stdlib.get("null?").unwrap();
    assert_eq!(
        null.call(&[Value::List(vec![])]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        null.call(&[Value::List(vec![Value::Int(1)])]).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn test_list_utilities() {
    let stdlib = init_stdlib();
    
    // Test list-ref
    let list_ref = stdlib.get("list-ref").unwrap();
    let list = Value::List(vec![
        Value::String("a".to_string()),
        Value::String("b".to_string()),
        Value::String("c".to_string()),
    ]);
    
    assert_eq!(
        list_ref.call(&[list.clone(), Value::Int(0)]).unwrap(),
        Value::String("a".to_string())
    );
    assert_eq!(
        list_ref.call(&[list.clone(), Value::Int(2)]).unwrap(),
        Value::String("c".to_string())
    );
    
    // Test out of bounds
    assert!(list_ref.call(&[list.clone(), Value::Int(3)]).is_err());
    assert!(list_ref.call(&[list.clone(), Value::Int(-1)]).is_err());
    
    // Test take
    let take = stdlib.get("take").unwrap();
    let result = take.call(&[Value::Int(2), list.clone()]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("a".to_string()));
            assert_eq!(items[1], Value::String("b".to_string()));
        }
        _ => panic!("Expected list"),
    }
    
    // Test take more than available
    let result = take.call(&[Value::Int(5), list.clone()]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
        }
        _ => panic!("Expected list"),
    }
    
    // Test drop
    let drop = stdlib.get("drop").unwrap();
    let result = drop.call(&[Value::Int(1), list.clone()]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("b".to_string()));
            assert_eq!(items[1], Value::String("c".to_string()));
        }
        _ => panic!("Expected list"),
    }
    
    // Test zip
    let zip = stdlib.get("zip").unwrap();
    let list1 = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    let list2 = Value::List(vec![
        Value::String("a".to_string()),
        Value::String("b".to_string()),
        Value::String("c".to_string()),
    ]);
    
    let result = zip.call(&[list1, list2]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            
            match &items[0] {
                Value::List(pair) => {
                    assert_eq!(pair.len(), 2);
                    assert_eq!(pair[0], Value::Int(1));
                    assert_eq!(pair[1], Value::String("a".to_string()));
                }
                _ => panic!("Expected pair"),
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_flatten() {
    let stdlib = init_stdlib();
    let flatten = stdlib.get("flatten").unwrap();
    
    // Test simple flatten
    let nested = Value::List(vec![
        Value::List(vec![Value::Int(1), Value::Int(2)]),
        Value::List(vec![Value::Int(3), Value::Int(4)]),
        Value::List(vec![Value::Int(5)]),
    ]);
    
    let result = flatten.call(&[nested]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 5);
            assert_eq!(items, vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4),
                Value::Int(5),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test with empty lists
    let with_empty = Value::List(vec![
        Value::List(vec![Value::Int(1)]),
        Value::List(vec![]),
        Value::List(vec![Value::Int(2), Value::Int(3)]),
    ]);
    
    let result = flatten.call(&[with_empty]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items, vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
            ]);
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_map_operations() {
    let stdlib = init_stdlib();
    
    // Test make-map
    let make_map = stdlib.get("make-map").unwrap();
    let result = make_map.call(&[]).unwrap();
    match result {
        Value::Map(map) => {
            assert!(map.is_empty());
        }
        _ => panic!("Expected map"),
    }
    
    // Test map-set
    let map_set = stdlib.get("map-set").unwrap();
    let mut map = HashMap::new();
    let map_val = Value::Map(map.clone());
    
    let result = map_set.call(&[
        map_val,
        Value::String("key1".to_string()),
        Value::Int(42)
    ]).unwrap();
    
    match result {
        Value::Map(new_map) => {
            assert_eq!(new_map.len(), 1);
            assert_eq!(new_map.get("key1"), Some(&Value::Int(42)));
        }
        _ => panic!("Expected map"),
    }
    
    // Test map-get
    let map_get = stdlib.get("map-get").unwrap();
    map.insert("key1".to_string(), Value::Int(42));
    map.insert("key2".to_string(), Value::String("hello".to_string()));
    let map_val = Value::Map(map.clone());
    
    assert_eq!(
        map_get.call(&[
            map_val.clone(),
            Value::String("key1".to_string())
        ]).unwrap(),
        Value::Int(42)
    );
    
    assert_eq!(
        map_get.call(&[
            map_val.clone(),
            Value::String("key2".to_string())
        ]).unwrap(),
        Value::String("hello".to_string())
    );
    
    assert_eq!(
        map_get.call(&[
            map_val.clone(),
            Value::String("nonexistent".to_string())
        ]).unwrap(),
        Value::Nil
    );
}

#[test]
fn test_map_utilities() {
    let stdlib = init_stdlib();
    
    let mut map = HashMap::new();
    map.insert("a".to_string(), Value::Int(1));
    map.insert("b".to_string(), Value::Int(2));
    map.insert("c".to_string(), Value::Int(3));
    let map_val = Value::Map(map);
    
    // Test map-has?
    let map_has = stdlib.get("map-has?").unwrap();
    assert_eq!(
        map_has.call(&[
            map_val.clone(),
            Value::String("a".to_string())
        ]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        map_has.call(&[
            map_val.clone(),
            Value::String("d".to_string())
        ]).unwrap(),
        Value::Bool(false)
    );
    
    // Test map-remove
    let map_remove = stdlib.get("map-remove").unwrap();
    let result = map_remove.call(&[
        map_val.clone(),
        Value::String("b".to_string())
    ]).unwrap();
    
    match result {
        Value::Map(new_map) => {
            assert_eq!(new_map.len(), 2);
            assert!(!new_map.contains_key("b"));
            assert_eq!(new_map.get("a"), Some(&Value::Int(1)));
            assert_eq!(new_map.get("c"), Some(&Value::Int(3)));
        }
        _ => panic!("Expected map"),
    }
    
    // Test map-keys
    let map_keys = stdlib.get("map-keys").unwrap();
    let result = map_keys.call(&[map_val.clone()]).unwrap();
    
    match result {
        Value::List(keys) => {
            assert_eq!(keys.len(), 3);
            let key_strs: Vec<String> = keys.iter().map(|v| {
                match v {
                    Value::String(s) => s.clone(),
                    _ => panic!("Expected string key"),
                }
            }).collect();
            assert!(key_strs.contains(&"a".to_string()));
            assert!(key_strs.contains(&"b".to_string()));
            assert!(key_strs.contains(&"c".to_string()));
        }
        _ => panic!("Expected list"),
    }
    
    // Test map-values
    let map_values = stdlib.get("map-values").unwrap();
    let result = map_values.call(&[map_val.clone()]).unwrap();
    
    match result {
        Value::List(values) => {
            assert_eq!(values.len(), 3);
            assert!(values.contains(&Value::Int(1)));
            assert!(values.contains(&Value::Int(2)));
            assert!(values.contains(&Value::Int(3)));
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_map_merge() {
    let stdlib = init_stdlib();
    let map_merge = stdlib.get("map-merge").unwrap();
    
    let mut map1 = HashMap::new();
    map1.insert("a".to_string(), Value::Int(1));
    map1.insert("b".to_string(), Value::Int(2));
    
    let mut map2 = HashMap::new();
    map2.insert("b".to_string(), Value::Int(20)); // Override
    map2.insert("c".to_string(), Value::Int(3));
    
    let result = map_merge.call(&[
        Value::Map(map1),
        Value::Map(map2)
    ]).unwrap();
    
    match result {
        Value::Map(merged) => {
            assert_eq!(merged.len(), 3);
            assert_eq!(merged.get("a"), Some(&Value::Int(1)));
            assert_eq!(merged.get("b"), Some(&Value::Int(20))); // map2 value wins
            assert_eq!(merged.get("c"), Some(&Value::Int(3)));
        }
        _ => panic!("Expected map"),
    }
}

#[test]
fn test_tagged_values() {
    let stdlib = init_stdlib();
    
    // Test make-tagged
    let make_tagged = stdlib.get("make-tagged").unwrap();
    let result = make_tagged.call(&[
        Value::String("Point".to_string()),
        Value::Int(10),
        Value::Int(20)
    ]).unwrap();
    
    match result {
        Value::Tagged { tag, values } => {
            assert_eq!(tag, "Point");
            assert_eq!(values.len(), 2);
            assert_eq!(values[0], Value::Int(10));
            assert_eq!(values[1], Value::Int(20));
        }
        _ => panic!("Expected tagged value"),
    }
    
    // Test tagged?
    let tagged_pred = stdlib.get("tagged?").unwrap();
    let point = Value::Tagged {
        tag: "Point".to_string(),
        values: vec![Value::Int(10), Value::Int(20)]
    };
    
    assert_eq!(
        tagged_pred.call(&[point.clone()]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        tagged_pred.call(&[Value::Int(42)]).unwrap(),
        Value::Bool(false)
    );
    
    // Test tagged-tag
    let tagged_tag = stdlib.get("tagged-tag").unwrap();
    assert_eq!(
        tagged_tag.call(&[point.clone()]).unwrap(),
        Value::String("Point".to_string())
    );
    
    // Test tagged-values
    let tagged_values = stdlib.get("tagged-values").unwrap();
    let result = tagged_values.call(&[point.clone()]).unwrap();
    match result {
        Value::List(values) => {
            assert_eq!(values.len(), 2);
            assert_eq!(values[0], Value::Int(10));
            assert_eq!(values[1], Value::Int(20));
        }
        _ => panic!("Expected list"),
    }
}