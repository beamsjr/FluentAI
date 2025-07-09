//! Collection and data structure operations

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

/// Register all collection functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // List operations
        StdlibFunction::pure("list-slice", list_slice, 2, Some(3), "Extract a slice from a list"),
        StdlibFunction::pure("list-flatten", list_flatten, 1, Some(1), "Flatten nested lists"),
        StdlibFunction::pure("list-zip", list_zip, 2, None, "Zip multiple lists together"),
        StdlibFunction::pure("list-partition", list_partition, 2, Some(2), "Partition list by predicate"),
        StdlibFunction::pure("list-sort", list_sort, 1, Some(2), "Sort a list"),
        StdlibFunction::pure("list-unique", list_unique, 1, Some(1), "Remove duplicates from list"),
        
        // Set operations
        StdlibFunction::pure("set-new", set_new, 0, None, "Create a new set"),
        StdlibFunction::pure("set-add", set_add, 2, Some(2), "Add element to set"),
        StdlibFunction::pure("set-remove", set_remove, 2, Some(2), "Remove element from set"),
        StdlibFunction::pure("set-contains?", set_contains, 2, Some(2), "Check if set contains element"),
        StdlibFunction::pure("set-union", set_union, 2, None, "Union of sets"),
        StdlibFunction::pure("set-intersection", set_intersection, 2, None, "Intersection of sets"),
        StdlibFunction::pure("set-difference", set_difference, 2, Some(2), "Difference of sets"),
        StdlibFunction::pure("set->list", set_to_list, 1, Some(1), "Convert set to list"),
        StdlibFunction::pure("list->set", list_to_set, 1, Some(1), "Convert list to set"),
        
        // Map/Dictionary operations
        StdlibFunction::pure("dict-new", dict_new, 0, None, "Create a new dictionary"),
        StdlibFunction::pure("dict-get", dict_get, 2, Some(3), "Get value from dictionary"),
        StdlibFunction::pure("dict-set", dict_set, 3, Some(3), "Set value in dictionary"),
        StdlibFunction::pure("dict-remove", dict_remove, 2, Some(2), "Remove key from dictionary"),
        StdlibFunction::pure("dict-contains?", dict_contains, 2, Some(2), "Check if dictionary contains key"),
        StdlibFunction::pure("dict-keys", dict_keys, 1, Some(1), "Get all keys from dictionary"),
        StdlibFunction::pure("dict-values", dict_values, 1, Some(1), "Get all values from dictionary"),
        StdlibFunction::pure("dict-merge", dict_merge, 2, None, "Merge dictionaries"),
        StdlibFunction::pure("dict->list", dict_to_list, 1, Some(1), "Convert dictionary to association list"),
        StdlibFunction::pure("list->dict", list_to_dict, 1, Some(1), "Convert association list to dictionary"),
    ]);
}

// List operations

fn list_slice(args: &[Value]) -> Result<Value> {
    let list = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("list-slice: expected list")),
    };
    
    let start = match &args[1] {
        Value::Integer(i) => *i as usize,
        _ => return Err(anyhow!("list-slice: expected integer start index")),
    };
    
    let end = if args.len() > 2 {
        match &args[2] {
            Value::Integer(i) => *i as usize,
            _ => return Err(anyhow!("list-slice: expected integer end index")),
        }
    } else {
        list.len()
    };
    
    if start > list.len() || end > list.len() || start > end {
        return Err(anyhow!("list-slice: invalid indices"));
    }
    
    Ok(Value::List(list[start..end].to_vec()))
}

fn list_flatten(args: &[Value]) -> Result<Value> {
    fn flatten_helper(value: &Value, result: &mut Vec<Value>) {
        match value {
            Value::List(items) => {
                for item in items {
                    flatten_helper(item, result);
                }
            }
            _ => result.push(value.clone()),
        }
    }
    
    match &args[0] {
        Value::List(_) => {
            let mut result = Vec::new();
            flatten_helper(&args[0], &mut result);
            Ok(Value::List(result))
        }
        _ => Err(anyhow!("list-flatten: expected list")),
    }
}

fn list_zip(args: &[Value]) -> Result<Value> {
    let mut lists = Vec::new();
    
    for arg in args {
        match arg {
            Value::List(items) => lists.push(items),
            _ => return Err(anyhow!("list-zip: expected lists")),
        }
    }
    
    if lists.is_empty() {
        return Ok(Value::List(vec![]));
    }
    
    let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
    let mut result = Vec::new();
    
    for i in 0..min_len {
        let tuple: Vec<Value> = lists.iter().map(|list| list[i].clone()).collect();
        result.push(Value::List(tuple));
    }
    
    Ok(Value::List(result))
}

fn list_partition(_args: &[Value]) -> Result<Value> {
    // This requires VM integration to call the predicate function
    Err(anyhow!("list-partition: VM integration required for function application"))
}

fn list_sort(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut sorted = items.clone();
            
            // Basic sorting for numbers and strings
            sorted.sort_by(|a, b| {
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => x.cmp(y),
                    (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::String(x), Value::String(y)) => x.cmp(y),
                    _ => std::cmp::Ordering::Equal,
                }
            });
            
            Ok(Value::List(sorted))
        }
        _ => Err(anyhow!("list-sort: expected list")),
    }
}

fn list_unique(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut seen = FxHashSet::default();
            let mut result = Vec::new();
            
            for item in items {
                // Simple uniqueness check - would need proper equality for complex types
                let key = format!("{:?}", item);
                if seen.insert(key) {
                    result.push(item.clone());
                }
            }
            
            Ok(Value::List(result))
        }
        _ => Err(anyhow!("list-unique: expected list")),
    }
}

// Set operations (using List as backing for now)

fn set_new(args: &[Value]) -> Result<Value> {
    let items: Vec<Value> = args.to_vec();
    let mut unique = Vec::new();
    let mut seen = FxHashSet::default();
    
    for item in items {
        let key = format!("{:?}", item);
        if seen.insert(key) {
            unique.push(item);
        }
    }
    
    Ok(Value::List(unique))
}

fn set_add(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut result = items.clone();
            let new_item = &args[1];
            
            // Check if already present
            let key = format!("{:?}", new_item);
            let exists = items.iter().any(|item| format!("{:?}", item) == key);
            
            if !exists {
                result.push(new_item.clone());
            }
            
            Ok(Value::List(result))
        }
        _ => Err(anyhow!("set-add: expected set (list)")),
    }
}

fn set_remove(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let remove_key = format!("{:?}", &args[1]);
            let result: Vec<Value> = items.iter()
                .filter(|item| format!("{:?}", item) != remove_key)
                .cloned()
                .collect();
            
            Ok(Value::List(result))
        }
        _ => Err(anyhow!("set-remove: expected set (list)")),
    }
}

fn set_contains(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let search_key = format!("{:?}", &args[1]);
            let exists = items.iter().any(|item| format!("{:?}", item) == search_key);
            Ok(Value::Boolean(exists))
        }
        _ => Err(anyhow!("set-contains?: expected set (list)")),
    }
}

fn set_union(args: &[Value]) -> Result<Value> {
    let mut result_items = Vec::new();
    let mut seen = FxHashSet::default();
    
    for arg in args {
        match arg {
            Value::List(items) => {
                for item in items {
                    let key = format!("{:?}", item);
                    if seen.insert(key) {
                        result_items.push(item.clone());
                    }
                }
            }
            _ => return Err(anyhow!("set-union: expected sets (lists)")),
        }
    }
    
    Ok(Value::List(result_items))
}

fn set_intersection(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Ok(Value::List(vec![]));
    }
    
    let first_set = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("set-intersection: expected sets (lists)")),
    };
    
    let mut result = Vec::new();
    
    'outer: for item in first_set {
        let item_key = format!("{:?}", item);
        
        // Check if item exists in all other sets
        for arg in &args[1..] {
            match arg {
                Value::List(items) => {
                    if !items.iter().any(|i| format!("{:?}", i) == item_key) {
                        continue 'outer;
                    }
                }
                _ => return Err(anyhow!("set-intersection: expected sets (lists)")),
            }
        }
        
        result.push(item.clone());
    }
    
    Ok(Value::List(result))
}

fn set_difference(args: &[Value]) -> Result<Value> {
    let first_set = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("set-difference: expected set (list)")),
    };
    
    let second_set = match &args[1] {
        Value::List(items) => items,
        _ => return Err(anyhow!("set-difference: expected set (list)")),
    };
    
    let mut second_keys = FxHashSet::default();
    for item in second_set {
        second_keys.insert(format!("{:?}", item));
    }
    
    let result: Vec<Value> = first_set.iter()
        .filter(|item| !second_keys.contains(&format!("{:?}", item)))
        .cloned()
        .collect();
    
    Ok(Value::List(result))
}

fn set_to_list(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => Ok(Value::List(items.clone())),
        _ => Err(anyhow!("set->list: expected set (list)")),
    }
}

fn list_to_set(args: &[Value]) -> Result<Value> {
    list_unique(args)
}

// Map/Dictionary operations

fn dict_new(args: &[Value]) -> Result<Value> {
    let mut map = FxHashMap::default();
    
    // Process key-value pairs if provided
    let mut i = 0;
    while i + 1 < args.len() {
        match &args[i] {
            Value::String(key) => {
                map.insert(key.clone(), args[i + 1].clone());
            }
            _ => return Err(anyhow!("dict-new: keys must be strings")),
        }
        i += 2;
    }
    
    Ok(Value::Map(map))
}

fn dict_get(args: &[Value]) -> Result<Value> {
    let dict = match &args[0] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("dict-get: expected dictionary")),
    };
    
    let key = match &args[1] {
        Value::String(k) => k,
        _ => return Err(anyhow!("dict-get: expected string key")),
    };
    
    match dict.get(key) {
        Some(value) => Ok(value.clone()),
        None => {
            if args.len() > 2 {
                Ok(args[2].clone()) // Default value
            } else {
                Ok(Value::Nil)
            }
        }
    }
}

fn dict_set(args: &[Value]) -> Result<Value> {
    let mut dict = match &args[0] {
        Value::Map(m) => m.clone(),
        _ => return Err(anyhow!("dict-set: expected dictionary")),
    };
    
    let key = match &args[1] {
        Value::String(k) => k,
        _ => return Err(anyhow!("dict-set: expected string key")),
    };
    
    dict.insert(key.clone(), args[2].clone());
    Ok(Value::Map(dict))
}

fn dict_remove(args: &[Value]) -> Result<Value> {
    let mut dict = match &args[0] {
        Value::Map(m) => m.clone(),
        _ => return Err(anyhow!("dict-remove: expected dictionary")),
    };
    
    let key = match &args[1] {
        Value::String(k) => k,
        _ => return Err(anyhow!("dict-remove: expected string key")),
    };
    
    dict.remove(key);
    Ok(Value::Map(dict))
}

fn dict_contains(args: &[Value]) -> Result<Value> {
    let dict = match &args[0] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("dict-contains?: expected dictionary")),
    };
    
    let key = match &args[1] {
        Value::String(k) => k,
        _ => return Err(anyhow!("dict-contains?: expected string key")),
    };
    
    Ok(Value::Boolean(dict.contains_key(key)))
}

fn dict_keys(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(m) => {
            let keys: Vec<Value> = m.keys()
                .map(|k| Value::String(k.clone()))
                .collect();
            Ok(Value::List(keys))
        }
        _ => Err(anyhow!("dict-keys: expected dictionary")),
    }
}

fn dict_values(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(m) => {
            let values: Vec<Value> = m.values().cloned().collect();
            Ok(Value::List(values))
        }
        _ => Err(anyhow!("dict-values: expected dictionary")),
    }
}

fn dict_merge(args: &[Value]) -> Result<Value> {
    let mut result = FxHashMap::default();
    
    for arg in args {
        match arg {
            Value::Map(m) => {
                for (k, v) in m {
                    result.insert(k.clone(), v.clone());
                }
            }
            _ => return Err(anyhow!("dict-merge: expected dictionaries")),
        }
    }
    
    Ok(Value::Map(result))
}

fn dict_to_list(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(m) => {
            let pairs: Vec<Value> = m.iter()
                .map(|(k, v)| Value::List(vec![
                    Value::String(k.clone()),
                    v.clone()
                ]))
                .collect();
            Ok(Value::List(pairs))
        }
        _ => Err(anyhow!("dict->list: expected dictionary")),
    }
}

fn list_to_dict(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut map = FxHashMap::default();
            
            for item in items {
                match item {
                    Value::List(pair) if pair.len() == 2 => {
                        match &pair[0] {
                            Value::String(key) => {
                                map.insert(key.clone(), pair[1].clone());
                            }
                            _ => return Err(anyhow!("list->dict: keys must be strings")),
                        }
                    }
                    _ => return Err(anyhow!("list->dict: expected list of key-value pairs")),
                }
            }
            
            Ok(Value::Map(map))
        }
        _ => Err(anyhow!("list->dict: expected list")),
    }
}