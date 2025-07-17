//! Core standard library functions
//!
//! Implements basic list operations, numeric functions, type predicates,
//! and other fundamental operations.

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use crate::vm_bridge::StdlibContext;
use anyhow::{anyhow, Result};

/// Register all core functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // List operations
        StdlibFunction::pure("length", length, 1, Some(1), "Return the length of a list"),
        StdlibFunction::pure("append", append, 2, Some(2), "Append an element to a list"),
        StdlibFunction::pure("reverse", reverse, 1, Some(1), "Reverse a list"),
        StdlibFunction::pure("nth", nth, 2, Some(2), "Get the nth element of a list"),
        StdlibFunction::pure(
            "take",
            take,
            2,
            Some(2),
            "Take the first n elements of a list",
        ),
        StdlibFunction::pure(
            "drop",
            drop,
            2,
            Some(2),
            "Drop the first n elements of a list",
        ),
        StdlibFunction::effectful_with_context(
            "map",
            map_ctx,
            2,
            Some(2),
            vec![],
            "Apply a function to each element of a list",
        ),
        StdlibFunction::effectful_with_context(
            "filter",
            filter_ctx,
            2,
            Some(2),
            vec![],
            "Filter a list by a predicate",
        ),
        StdlibFunction::effectful_with_context(
            "fold",
            fold_ctx,
            3,
            Some(3),
            vec![],
            "Fold a list from the left",
        ),
        StdlibFunction::effectful_with_context(
            "for-each",
            for_each_ctx,
            2,
            Some(2),
            vec![],
            "Apply a function to each element of a collection for side effects",
        ),
        StdlibFunction::pure("range", range, 1, Some(3), "Generate a range of numbers"),
        StdlibFunction::pure(
            "cons",
            cons,
            2,
            Some(2),
            "Construct a list by prepending an element",
        ),
        StdlibFunction::pure("car", car, 1, Some(1), "Get the first element of a list"),
        StdlibFunction::pure(
            "cdr",
            cdr,
            1,
            Some(1),
            "Get all but the first element of a list",
        ),
        StdlibFunction::pure("null?", is_null, 1, Some(1), "Check if a list is empty"),
        StdlibFunction::pure(
            "list-ref",
            list_ref,
            2,
            Some(2),
            "Get element at index from list",
        ),
        StdlibFunction::pure("zip", zip, 2, Some(2), "Zip two lists together"),
        StdlibFunction::pure("flatten", flatten, 1, Some(1), "Flatten a nested list"),
        StdlibFunction::pure("make-map", make_map, 0, Some(0), "Create an empty map"),
        StdlibFunction::pure(
            "map-set",
            map_set,
            3,
            Some(3),
            "Set a key-value pair in a map",
        ),
        StdlibFunction::pure("map-get", map_get, 2, Some(2), "Get a value from a map"),
        StdlibFunction::pure("map-has?", map_has, 2, Some(2), "Check if map contains key"),
        StdlibFunction::pure(
            "map-remove",
            map_remove,
            2,
            Some(2),
            "Remove a key from a map",
        ),
        StdlibFunction::pure("map-keys", map_keys, 1, Some(1), "Get all keys from a map"),
        StdlibFunction::pure(
            "map-values",
            map_values,
            1,
            Some(1),
            "Get all values from a map",
        ),
        StdlibFunction::pure("map-merge", map_merge, 2, Some(2), "Merge two maps"),
        StdlibFunction::pure("make-tagged", make_tagged, 1, None, "Create a tagged value"),
        StdlibFunction::pure("tagged?", is_tagged, 1, Some(1), "Check if value is tagged"),
        // Concurrency operations
        StdlibFunction::effectful_with_context("send-to", send_to, 2, Some(2), vec![], "Send a message to a channel or actor"),
        StdlibFunction::pure(
            "tagged-tag",
            tagged_tag,
            1,
            Some(1),
            "Get tag from tagged value",
        ),
        StdlibFunction::pure(
            "tagged-values",
            tagged_values,
            1,
            Some(1),
            "Get values from tagged value",
        ),
        // Numeric operations
        StdlibFunction::pure("abs", abs, 1, Some(1), "Absolute value"),
        StdlibFunction::pure("max", max, 2, None, "Maximum of values"),
        StdlibFunction::pure("min", min, 2, None, "Minimum of values"),
        StdlibFunction::pure("mod", modulo, 2, Some(2), "Modulo operation"),
        // Boolean operations
        StdlibFunction::pure("xor", xor, 2, Some(2), "Logical XOR"),
        // Type predicates
        StdlibFunction::pure("int?", is_int, 1, Some(1), "Check if value is an integer"),
        StdlibFunction::pure("float?", is_float, 1, Some(1), "Check if value is a float"),
        StdlibFunction::pure(
            "string?",
            is_string,
            1,
            Some(1),
            "Check if value is a string",
        ),
        StdlibFunction::pure("list?", is_list, 1, Some(1), "Check if value is a list"),
        StdlibFunction::pure("bool?", is_bool, 1, Some(1), "Check if value is a boolean"),
        StdlibFunction::pure("nil?", is_nil, 1, Some(1), "Check if value is nil"),
        StdlibFunction::pure(
            "function?",
            is_function,
            1,
            Some(1),
            "Check if value is a function",
        ),
        StdlibFunction::pure(
            "number?",
            is_number,
            1,
            Some(1),
            "Check if value is a number",
        ),
        // Comparison operations (beyond what VM provides)
        StdlibFunction::pure("<", less_than, 2, Some(2), "Less than comparison"),
        StdlibFunction::pure(">", greater_than, 2, Some(2), "Greater than comparison"),
        StdlibFunction::pure(
            "<=",
            less_equal,
            2,
            Some(2),
            "Less than or equal comparison",
        ),
        StdlibFunction::pure(
            ">=",
            greater_equal,
            2,
            Some(2),
            "Greater than or equal comparison",
        ),
        StdlibFunction::pure("=", equal, 2, Some(2), "Equal comparison"),
        StdlibFunction::pure("!=", not_equal, 2, Some(2), "Not equal comparison"),
    ]);
}

// List operations

pub fn length(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(anyhow!("length: expected list")),
    }
}

pub fn append(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut new_items = items.clone();
            new_items.push(args[1].clone());
            Ok(Value::List(new_items))
        }
        _ => Err(anyhow!("append: expected list")),
    }
}

fn reverse(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut new_items = items.clone();
            new_items.reverse();
            Ok(Value::List(new_items))
        }
        _ => Err(anyhow!("reverse: expected list")),
    }
}

fn nth(args: &[Value]) -> Result<Value> {
    let list = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("nth: expected list")),
    };

    let index = match &args[1] {
        Value::Integer(i) => *i as usize,
        _ => return Err(anyhow!("nth: expected integer index")),
    };

    list.get(index)
        .cloned()
        .ok_or_else(|| anyhow!("nth: index {} out of bounds", index))
}

fn take(args: &[Value]) -> Result<Value> {
    let n = match &args[0] {
        Value::Integer(i) => *i as usize,
        _ => return Err(anyhow!("take: expected integer count")),
    };

    match &args[1] {
        Value::List(items) => {
            let taken: Vec<_> = items.iter().take(n).cloned().collect();
            Ok(Value::List(taken))
        }
        _ => Err(anyhow!("take: expected list")),
    }
}

fn drop(args: &[Value]) -> Result<Value> {
    let n = match &args[0] {
        Value::Integer(i) => *i as usize,
        _ => return Err(anyhow!("drop: expected integer count")),
    };

    match &args[1] {
        Value::List(items) => {
            let dropped: Vec<_> = items.iter().skip(n).cloned().collect();
            Ok(Value::List(dropped))
        }
        _ => Err(anyhow!("drop: expected list")),
    }
}

pub fn map_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let func = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("map: expected function")),
    };

    match &args[1] {
        Value::List(items) => {
            let mut result = Vec::with_capacity(items.len());

            for item in items {
                // Call the function with the item
                match context.call_function_with_effects(func, &[item.clone()]) {
                    Ok(mapped_value) => result.push(mapped_value),
                    Err(e) => return Err(anyhow!("map: error applying function: {}", e)),
                }
            }

            Ok(Value::List(result))
        }
        _ => Err(anyhow!("map: expected list")),
    }
}

pub fn filter_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let pred = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("filter: expected predicate function")),
    };

    match &args[1] {
        Value::List(items) => {
            let mut result = Vec::new();

            for item in items {
                // Call the predicate with the item
                match context.call_function_with_effects(pred, &[item.clone()]) {
                    Ok(Value::Boolean(true)) => result.push(item.clone()),
                    Ok(Value::Boolean(false)) => {}
                    Ok(_) => return Err(anyhow!("filter: predicate must return boolean")),
                    Err(e) => return Err(anyhow!("filter: error evaluating predicate: {}", e)),
                }
            }

            Ok(Value::List(result))
        }
        _ => Err(anyhow!("filter: expected list")),
    }
}

pub fn fold_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let func = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("fold: expected function")),
    };

    let mut accumulator = args[1].clone();

    match &args[2] {
        Value::List(items) => {
            for item in items {
                // Call the function with accumulator and item
                match context.call_function_with_effects(func, &[accumulator, item.clone()]) {
                    Ok(new_acc) => accumulator = new_acc,
                    Err(e) => return Err(anyhow!("fold: error applying function: {}", e)),
                }
            }

            Ok(accumulator)
        }
        _ => Err(anyhow!("fold: expected list")),
    }
}

pub fn for_each_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let func = match &args[0] {
        Value::Function { .. } => &args[0],
        _ => return Err(anyhow!("for-each: expected function")),
    };
    
    match &args[1] {
        Value::List(items) => {
            for item in items {
                // Call the function with each item, ignoring the return value
                context.call_function_with_effects(func, &[item.clone()])
                    .map_err(|e| anyhow!("for-each: error applying function: {}", e))?;
            }
            Ok(Value::Nil)
        }
        Value::Tagged { tag, values } if tag == "Range" && values.len() == 3 => {
            // Handle ranges
            let start = match &values[0] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("for-each: invalid range start")),
            };
            let end = match &values[1] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("for-each: invalid range end")),
            };
            let step = match &values[2] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("for-each: invalid range step")),
            };
            
            if step == 0 {
                return Err(anyhow!("for-each: range step cannot be zero"));
            }
            
            let mut current = start;
            if step > 0 {
                while current < end {
                    context.call_function_with_effects(func, &[Value::Integer(current)])
                        .map_err(|e| anyhow!("for-each: error applying function: {}", e))?;
                    current += step;
                }
            } else {
                while current > end {
                    context.call_function_with_effects(func, &[Value::Integer(current)])
                        .map_err(|e| anyhow!("for-each: error applying function: {}", e))?;
                    current += step;
                }
            }
            Ok(Value::Nil)
        }
        _ => Err(anyhow!("for-each: expected list or range")),
    }
}

fn range(args: &[Value]) -> Result<Value> {
    let (start, end, step) = match args.len() {
        1 => {
            let end = match &args[0] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("range: expected integer")),
            };
            (0, end, 1)
        }
        2 => {
            let start = match &args[0] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("range: expected integer start")),
            };
            let end = match &args[1] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("range: expected integer end")),
            };
            (start, end, 1)
        }
        3 => {
            let start = match &args[0] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("range: expected integer start")),
            };
            let end = match &args[1] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("range: expected integer end")),
            };
            let step = match &args[2] {
                Value::Integer(i) => *i,
                _ => return Err(anyhow!("range: expected integer step")),
            };
            if step == 0 {
                return Err(anyhow!("range: step cannot be zero"));
            }
            (start, end, step)
        }
        _ => unreachable!(), // Registry ensures correct arg count
    };

    let mut result = Vec::new();
    let mut current = start;

    if step > 0 {
        while current < end {
            result.push(Value::Integer(current));
            current += step;
        }
    } else {
        while current > end {
            result.push(Value::Integer(current));
            current += step;
        }
    }

    Ok(Value::List(result))
}

// Numeric operations

fn abs(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::Integer(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => Err(anyhow!("abs: expected number")),
    }
}

fn max(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("max: expected at least one argument"));
    }

    let mut max_val = &args[0];

    for arg in &args[1..] {
        max_val = match (max_val, arg) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b > a {
                    arg
                } else {
                    max_val
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if b > a {
                    arg
                } else {
                    max_val
                }
            }
            (Value::Integer(a), Value::Float(b)) => {
                if *b > *a as f64 {
                    arg
                } else {
                    max_val
                }
            }
            (Value::Float(a), Value::Integer(b)) => {
                if *b as f64 > *a {
                    arg
                } else {
                    max_val
                }
            }
            _ => return Err(anyhow!("max: expected numbers")),
        };
    }

    Ok(max_val.clone())
}

fn min(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("min: expected at least one argument"));
    }

    let mut min_val = &args[0];

    for arg in &args[1..] {
        min_val = match (min_val, arg) {
            (Value::Integer(a), Value::Integer(b)) => {
                if b < a {
                    arg
                } else {
                    min_val
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if b < a {
                    arg
                } else {
                    min_val
                }
            }
            (Value::Integer(a), Value::Float(b)) => {
                if *b < *a as f64 {
                    arg
                } else {
                    min_val
                }
            }
            (Value::Float(a), Value::Integer(b)) => {
                if (*b as f64) < *a {
                    arg
                } else {
                    min_val
                }
            }
            _ => return Err(anyhow!("min: expected numbers")),
        };
    }

    Ok(min_val.clone())
}

fn modulo(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 {
                Err(anyhow!("mod: division by zero"))
            } else {
                Ok(Value::Integer(a % b))
            }
        }
        _ => Err(anyhow!("mod: expected integers")),
    }
}

// Boolean operations

fn xor(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a ^ b)),
        _ => Err(anyhow!("xor: expected booleans")),
    }
}

// Type predicates

fn is_int(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
}

fn is_float(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
}

fn is_string(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::String(_))))
}

fn is_list(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
}

fn is_bool(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
}

fn is_nil(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::Nil)))
}

fn is_function(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::Function { .. })))
}

fn is_number(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(
        &args[0],
        Value::Integer(_) | Value::Float(_)
    )))
}

// Comparison operations

fn less_than(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a < b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a < b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) < *b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a < (*b as f64))),
        _ => Err(anyhow!("<: expected numbers")),
    }
}

fn greater_than(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a > b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a > b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean(*a as f64 > *b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a > *b as f64)),
        _ => Err(anyhow!(">: expected numbers")),
    }
}

fn less_equal(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a <= b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a <= b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean(*a as f64 <= *b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a <= *b as f64)),
        _ => Err(anyhow!("<=: expected numbers")),
    }
}

fn greater_equal(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a >= b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a >= b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean(*a as f64 >= *b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a >= *b as f64)),
        _ => Err(anyhow!(">=: expected numbers")),
    }
}

fn equal(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a == b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a == b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean(*a as f64 == *b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a == *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a == b)),
        (Value::Nil, Value::Nil) => Ok(Value::Boolean(true)),
        _ => Ok(Value::Boolean(false)), // Different types are not equal
    }
}

fn not_equal(args: &[Value]) -> Result<Value> {
    match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a != b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a != b)),
        (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean(*a as f64 != *b)),
        (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a != *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a != b)),
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a != b)),
        (Value::Nil, Value::Nil) => Ok(Value::Boolean(false)),
        _ => Ok(Value::Boolean(true)), // Different types are not equal
    }
}

// Additional list operations

fn cons(args: &[Value]) -> Result<Value> {
    match &args[1] {
        Value::List(items) => {
            let mut new_items = vec![args[0].clone()];
            new_items.extend_from_slice(items);
            Ok(Value::List(new_items))
        }
        _ => Err(anyhow!("cons: second argument must be a list")),
    }
}

fn car(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => items
            .first()
            .cloned()
            .ok_or_else(|| anyhow!("car: empty list")),
        _ => Err(anyhow!("car: expected list")),
    }
}

fn cdr(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                Err(anyhow!("cdr: empty list"))
            } else {
                Ok(Value::List(items[1..].to_vec()))
            }
        }
        _ => Err(anyhow!("cdr: expected list")),
    }
}

fn is_null(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => Ok(Value::Boolean(items.is_empty())),
        _ => Ok(Value::Boolean(false)),
    }
}

fn list_ref(args: &[Value]) -> Result<Value> {
    let list = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("list-ref: expected list")),
    };

    let index = match &args[1] {
        Value::Integer(i) => {
            if *i < 0 {
                return Err(anyhow!("list-ref: negative index"));
            }
            *i as usize
        }
        _ => return Err(anyhow!("list-ref: expected integer index")),
    };

    list.get(index)
        .cloned()
        .ok_or_else(|| anyhow!("list-ref: index {} out of bounds", index))
}

fn zip(args: &[Value]) -> Result<Value> {
    let list1 = match &args[0] {
        Value::List(items) => items,
        _ => return Err(anyhow!("zip: expected list as first argument")),
    };

    let list2 = match &args[1] {
        Value::List(items) => items,
        _ => return Err(anyhow!("zip: expected list as second argument")),
    };

    let len = std::cmp::min(list1.len(), list2.len());
    let mut result = Vec::with_capacity(len);

    for i in 0..len {
        result.push(Value::List(vec![list1[i].clone(), list2[i].clone()]));
    }

    Ok(Value::List(result))
}

fn flatten(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut result = Vec::new();
            for item in items {
                match item {
                    Value::List(sub_items) => result.extend_from_slice(sub_items),
                    _ => result.push(item.clone()),
                }
            }
            Ok(Value::List(result))
        }
        _ => Err(anyhow!("flatten: expected list")),
    }
}

// Map operations

fn make_map(_args: &[Value]) -> Result<Value> {
    Ok(Value::Map(rustc_hash::FxHashMap::default()))
}

fn map_set(args: &[Value]) -> Result<Value> {
    let mut map = match &args[0] {
        Value::Map(m) => m.clone(),
        _ => return Err(anyhow!("map-set: expected map")),
    };

    let key = match &args[1] {
        Value::String(s) => s.clone(),
        _ => return Err(anyhow!("map-set: key must be a string")),
    };

    map.insert(key, args[2].clone());
    Ok(Value::Map(map))
}

fn map_get(args: &[Value]) -> Result<Value> {
    let map = match &args[0] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("map-get: expected map")),
    };

    let key = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("map-get: key must be a string")),
    };

    Ok(map.get(key).cloned().unwrap_or(Value::Nil))
}

fn map_has(args: &[Value]) -> Result<Value> {
    let map = match &args[0] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("map-has?: expected map")),
    };

    let key = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("map-has?: key must be a string")),
    };

    Ok(Value::Boolean(map.contains_key(key)))
}

fn map_remove(args: &[Value]) -> Result<Value> {
    let mut map = match &args[0] {
        Value::Map(m) => m.clone(),
        _ => return Err(anyhow!("map-remove: expected map")),
    };

    let key = match &args[1] {
        Value::String(s) => s.clone(),
        _ => return Err(anyhow!("map-remove: key must be a string")),
    };

    map.remove(&key);
    Ok(Value::Map(map))
}

fn map_keys(args: &[Value]) -> Result<Value> {
    let map = match &args[0] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("map-keys: expected map")),
    };

    let keys: Vec<Value> = map.keys().map(|k| Value::String(k.clone())).collect();

    Ok(Value::List(keys))
}

fn map_values(args: &[Value]) -> Result<Value> {
    let map = match &args[0] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("map-values: expected map")),
    };

    let values: Vec<Value> = map.values().cloned().collect();
    Ok(Value::List(values))
}

fn map_merge(args: &[Value]) -> Result<Value> {
    let mut result = match &args[0] {
        Value::Map(m) => m.clone(),
        _ => return Err(anyhow!("map-merge: expected map as first argument")),
    };

    let map2 = match &args[1] {
        Value::Map(m) => m,
        _ => return Err(anyhow!("map-merge: expected map as second argument")),
    };

    for (k, v) in map2 {
        result.insert(k.clone(), v.clone());
    }

    Ok(Value::Map(result))
}

// Tagged value operations

fn make_tagged(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("make-tagged: expected at least one argument (tag)"));
    }

    let tag = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(anyhow!("make-tagged: tag must be a string")),
    };

    let values = args[1..].to_vec();

    Ok(Value::Tagged { tag, values })
}

fn is_tagged(args: &[Value]) -> Result<Value> {
    Ok(Value::Boolean(matches!(&args[0], Value::Tagged { .. })))
}

fn tagged_tag(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Tagged { tag, .. } => Ok(Value::String(tag.clone())),
        _ => Err(anyhow!("tagged-tag: expected tagged value")),
    }
}

fn tagged_values(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Tagged { values, .. } => Ok(Value::List(values.clone())),
        _ => Err(anyhow!("tagged-values: expected tagged value")),
    }
}

// Concurrency operations

fn send_to(ctx: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    let target = &args[0];
    let message = &args[1];
    
    match target {
        Value::Channel(channel_id) => {
            // Send to channel
            if let Some(vm_callback) = &mut ctx.vm_callback {
                vm_callback.send_to_channel(*channel_id, message.clone())?;
            }
            Ok(Value::Nil)
        }
        Value::Actor(actor_id) => {
            // Send to actor
            if let Some(vm_callback) = &mut ctx.vm_callback {
                vm_callback.send_to_actor(*actor_id, message.clone())?;
            }
            Ok(Value::Nil)
        }
        _ => Err(anyhow!("send-to: expected channel or actor as first argument")),
    }
}
