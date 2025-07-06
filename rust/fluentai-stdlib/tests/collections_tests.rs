//! Tests for collections stdlib functions

use fluentai_stdlib::value::Value;
use fluentai_stdlib::init_stdlib;
use rustc_hash::FxHashMap;

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
    let mut map = FxHashMap::default();
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
    
    let mut map = FxHashMap::default();
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
    
    let mut map1 = FxHashMap::default();
    map1.insert("a".to_string(), Value::Int(1));
    map1.insert("b".to_string(), Value::Int(2));
    
    let mut map2 = FxHashMap::default();
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

#[test]
fn test_list_slice() {
    let stdlib = init_stdlib();
    let list_slice = stdlib.get("list-slice").unwrap();
    
    let list = Value::List(vec![
        Value::Int(0),
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(4),
        Value::Int(5),
    ]);
    
    // Test normal slice
    let result = list_slice.call(&[
        list.clone(),
        Value::Int(1),
        Value::Int(4),
    ]).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items, vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test slice from beginning
    let result = list_slice.call(&[
        list.clone(),
        Value::Int(0),
        Value::Int(3),
    ]).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items, vec![
                Value::Int(0),
                Value::Int(1),
                Value::Int(2),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test slice to end
    let result = list_slice.call(&[
        list.clone(),
        Value::Int(3),
        Value::Int(6),
    ]).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items, vec![
                Value::Int(3),
                Value::Int(4),
                Value::Int(5),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test empty slice
    let result = list_slice.call(&[
        list.clone(),
        Value::Int(2),
        Value::Int(2),
    ]).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 0);
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_list_sort() {
    let stdlib = init_stdlib();
    let list_sort = stdlib.get("list-sort").unwrap();
    
    // Test sorting integers
    let unsorted = Value::List(vec![
        Value::Int(3),
        Value::Int(1),
        Value::Int(4),
        Value::Int(1),
        Value::Int(5),
        Value::Int(9),
        Value::Int(2),
    ]);
    
    let result = list_sort.call(&[unsorted]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items, vec![
                Value::Int(1),
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4),
                Value::Int(5),
                Value::Int(9),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test sorting strings
    let unsorted = Value::List(vec![
        Value::String("banana".to_string()),
        Value::String("apple".to_string()),
        Value::String("cherry".to_string()),
        Value::String("apricot".to_string()),
    ]);
    
    let result = list_sort.call(&[unsorted]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items, vec![
                Value::String("apple".to_string()),
                Value::String("apricot".to_string()),
                Value::String("banana".to_string()),
                Value::String("cherry".to_string()),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test empty list
    let result = list_sort.call(&[Value::List(vec![])]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 0);
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_list_unique() {
    let stdlib = init_stdlib();
    let list_unique = stdlib.get("list-unique").unwrap();
    
    // Test removing duplicates
    let with_dups = Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(2),
        Value::Int(1),
        Value::Int(4),
        Value::Int(3),
    ]);
    
    let result = list_unique.call(&[with_dups]).unwrap();
    match result {
        Value::List(items) => {
            // Should preserve order of first occurrence
            assert_eq!(items, vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test already unique list
    let unique = Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]);
    
    let result = list_unique.call(&[unique]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items, vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
            ]);
        }
        _ => panic!("Expected list"),
    }
    
    // Test empty list
    let result = list_unique.call(&[Value::List(vec![])]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 0);
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_set_operations() {
    let stdlib = init_stdlib();
    
    // Test set-new
    let set_new = stdlib.get("set-new").unwrap();
    let result = set_new.call(&[]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 0);
        }
        _ => panic!("Expected list"),
    }
    
    // Test set-add
    let set_add = stdlib.get("set-add").unwrap();
    let empty_set = Value::List(vec![]);
    
    let result = set_add.call(&[empty_set, Value::Int(42)]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 1);
            assert_eq!(items[0], Value::Int(42));
        }
        _ => panic!("Expected list"),
    }
    
    // Test set-contains?
    let set_contains = stdlib.get("set-contains?").unwrap();
    let set_with_values = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    
    assert_eq!(
        set_contains.call(&[set_with_values.clone(), Value::Int(2)]).unwrap(),
        Value::Bool(true)
    );
    assert_eq!(
        set_contains.call(&[set_with_values.clone(), Value::Int(4)]).unwrap(),
        Value::Bool(false)
    );
    
    // Test set->list (identity function for our implementation)
    let set_to_list = stdlib.get("set->list").unwrap();
    let result = set_to_list.call(&[set_with_values.clone()]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert!(items.contains(&Value::Int(1)));
            assert!(items.contains(&Value::Int(2)));
            assert!(items.contains(&Value::Int(3)));
        }
        _ => panic!("Expected list"),
    }
    
    // Test list->set
    let list_to_set = stdlib.get("list->set").unwrap();
    let list_with_dups = Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(2),
        Value::Int(3),
        Value::Int(1),
    ]);
    
    let result = list_to_set.call(&[list_with_dups]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3); // duplicates removed
            assert!(items.contains(&Value::Int(1)));
            assert!(items.contains(&Value::Int(2)));
            assert!(items.contains(&Value::Int(3)));
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_dict_operations() {
    let stdlib = init_stdlib();
    
    // Test dict-new
    let dict_new = stdlib.get("dict-new").unwrap();
    let result = dict_new.call(&[]).unwrap();
    match result {
        Value::Map(map) => {
            assert!(map.is_empty());
        }
        _ => panic!("Expected map"),
    }
    
    // Test dict-set
    let dict_set = stdlib.get("dict-set").unwrap();
    let empty_dict = Value::Map(FxHashMap::default());
    
    let result = dict_set.call(&[
        empty_dict,
        Value::String("key".to_string()),
        Value::Int(42),
    ]).unwrap();
    
    match result {
        Value::Map(map) => {
            assert_eq!(map.len(), 1);
            assert_eq!(map.get("key"), Some(&Value::Int(42)));
        }
        _ => panic!("Expected map"),
    }
    
    // Test dict-get
    let dict_get = stdlib.get("dict-get").unwrap();
    let mut map = FxHashMap::default();
    map.insert("a".to_string(), Value::Int(1));
    map.insert("b".to_string(), Value::Int(2));
    let dict = Value::Map(map);
    
    assert_eq!(
        dict_get.call(&[dict.clone(), Value::String("a".to_string())]).unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        dict_get.call(&[dict.clone(), Value::String("c".to_string())]).unwrap(),
        Value::Nil
    );
    
    // Test dict->list
    let dict_to_list = stdlib.get("dict->list").unwrap();
    let result = dict_to_list.call(&[dict.clone()]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            // Each item should be a [key, value] pair
            for item in items {
                match item {
                    Value::List(pair) => {
                        assert_eq!(pair.len(), 2);
                        match &pair[0] {
                            Value::String(key) => {
                                assert!(key == "a" || key == "b");
                            }
                            _ => panic!("Expected string key"),
                        }
                    }
                    _ => panic!("Expected pair"),
                }
            }
        }
        _ => panic!("Expected list"),
    }
}
