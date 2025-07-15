//! Extended runtime support for JIT-compiled code
//!
//! This module provides additional runtime functions for complex operations
//! including maps, vectors, closures, channels, promises, and type checking.

use crate::value::{TaggedValue, value_to_tagged};
use fluentai_core::value::Value;
use rustc_hash::FxHashMap;
use std::slice;

// ===== Map Operations =====

/// Runtime helper for creating a map
#[no_mangle]
pub extern "C" fn jit_runtime_make_map(count: i64, args_ptr: *const i64) -> i64 {
    unsafe {
        let args = if count > 0 {
            slice::from_raw_parts(args_ptr, count as usize)
        } else {
            &[]
        };
        
        // Maps are created from key-value pairs, so count should be even
        if count % 2 != 0 {
            let error = Value::Error {
                kind: "ValueError".to_string(),
                message: "Map requires even number of arguments (key-value pairs)".to_string(),
                stack_trace: None,
            };
            return value_to_tagged(&error).0 as i64;
        }
        
        let mut map = FxHashMap::default();
        for i in (0..args.len()).step_by(2) {
            let key_tagged = TaggedValue(args[i] as u64);
            let key = key_tagged.to_value();
            
            // Keys must be strings
            match key {
                Value::String(k) => {
                    let value_tagged = TaggedValue(args[i + 1] as u64);
                    map.insert(k, value_tagged.to_value());
                }
                _ => {
                    let error = Value::Error {
                        kind: "TypeError".to_string(),
                        message: "Map keys must be strings".to_string(),
                        stack_trace: None,
                    };
                    return value_to_tagged(&error).0 as i64;
                }
            }
        }
        
        let map_value = Value::Map(map);
        value_to_tagged(&map_value).0 as i64
    }
}

/// Runtime helper for map get operation
#[no_mangle]
pub extern "C" fn jit_runtime_map_get(map_tagged: i64, key_tagged: i64) -> i64 {
    let map = TaggedValue(map_tagged as u64).to_value();
    let key = TaggedValue(key_tagged as u64).to_value();
    
    match (map, key) {
        (Value::Map(m), Value::String(k)) => {
            match m.get(&k) {
                Some(value) => value_to_tagged(value).0 as i64,
                None => value_to_tagged(&Value::Nil).0 as i64,
            }
        }
        (Value::Map(_), _) => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map key must be a string".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map get requires a map".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for map set operation (returns new map)
#[no_mangle]
pub extern "C" fn jit_runtime_map_set(map_tagged: i64, key_tagged: i64, value_tagged: i64) -> i64 {
    let map = TaggedValue(map_tagged as u64).to_value();
    let key = TaggedValue(key_tagged as u64).to_value();
    let value = TaggedValue(value_tagged as u64).to_value();
    
    match (map, key) {
        (Value::Map(mut m), Value::String(k)) => {
            m.insert(k, value);
            let new_map = Value::Map(m);
            value_to_tagged(&new_map).0 as i64
        }
        (Value::Map(_), _) => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map key must be a string".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map set requires a map".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for map contains operation
#[no_mangle]
pub extern "C" fn jit_runtime_map_contains(map_tagged: i64, key_tagged: i64) -> i64 {
    let map = TaggedValue(map_tagged as u64).to_value();
    let key = TaggedValue(key_tagged as u64).to_value();
    
    match (map, key) {
        (Value::Map(m), Value::String(k)) => {
            TaggedValue::from_integer(if m.contains_key(&k) { 1 } else { 0 }).0 as i64
        }
        (Value::Map(_), _) => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map key must be a string".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map contains requires a map".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for map size
#[no_mangle]
pub extern "C" fn jit_runtime_map_size(map_tagged: i64) -> i64 {
    let map = TaggedValue(map_tagged as u64).to_value();
    
    match map {
        Value::Map(m) => TaggedValue::from_integer(m.len() as i64).0 as i64,
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Map size requires a map".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

// ===== Vector Operations =====

/// Runtime helper for creating a vector
#[no_mangle]
pub extern "C" fn jit_runtime_make_vector(count: i64, args_ptr: *const i64) -> i64 {
    unsafe {
        let args = if count > 0 {
            slice::from_raw_parts(args_ptr, count as usize)
        } else {
            &[]
        };
        
        let mut items = Vec::with_capacity(args.len());
        for &arg in args {
            let tagged = TaggedValue(arg as u64);
            items.push(tagged.to_value());
        }
        
        let vector = Value::Vector(items);
        value_to_tagged(&vector).0 as i64
    }
}

/// Runtime helper for vector push (returns new vector)
#[no_mangle]
pub extern "C" fn jit_runtime_vector_push(vec_tagged: i64, elem_tagged: i64) -> i64 {
    let vec = TaggedValue(vec_tagged as u64).to_value();
    let elem = TaggedValue(elem_tagged as u64).to_value();
    
    match vec {
        Value::Vector(mut v) => {
            v.push(elem);
            let new_vec = Value::Vector(v);
            value_to_tagged(&new_vec).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Vector push requires a vector".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for vector get
#[no_mangle]
pub extern "C" fn jit_runtime_vector_get(vec_tagged: i64, idx_tagged: i64) -> i64 {
    let vec = TaggedValue(vec_tagged as u64).to_value();
    let idx = TaggedValue(idx_tagged as u64);
    
    // Extract integer index
    let idx_value = idx.to_value();
    let index = match idx_value {
        Value::Integer(i) => i as usize,
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Vector index must be an integer".to_string(),
                stack_trace: None,
            };
            return value_to_tagged(&error).0 as i64;
        }
    };
    
    match vec {
        Value::Vector(ref v) => {
            if index < v.len() {
                value_to_tagged(&v[index]).0 as i64
            } else {
                let error = Value::Error {
                    kind: "IndexError".to_string(),
                    message: format!("Vector index {} out of bounds (length {})", index, v.len()),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            }
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Vector get requires a vector".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for vector length
#[no_mangle]
pub extern "C" fn jit_runtime_vector_len(vec_tagged: i64) -> i64 {
    let vec = TaggedValue(vec_tagged as u64).to_value();
    
    match vec {
        Value::Vector(ref v) => TaggedValue::from_integer(v.len() as i64).0 as i64,
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Vector length requires a vector".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

// ===== Type Checking Utilities =====

/// Runtime helper for type checking
#[no_mangle]
pub extern "C" fn jit_runtime_is_type(value_tagged: i64, type_id: i64) -> i64 {
    let value = TaggedValue(value_tagged as u64).to_value();
    
    // Type IDs:
    // 0 = Integer
    // 1 = Float
    // 2 = String
    // 3 = Symbol
    // 4 = Boolean
    // 5 = Nil
    // 6 = List
    // 7 = Vector
    // 8 = Map
    // 9 = Function
    // 10 = Promise
    // 11 = Channel
    // 12 = Error
    
    let is_match = match type_id {
        0 => matches!(value, Value::Integer(_)),
        1 => matches!(value, Value::Float(_)),
        2 => matches!(value, Value::String(_)),
        3 => matches!(value, Value::Symbol(_)),
        4 => matches!(value, Value::Boolean(_)),
        5 => matches!(value, Value::Nil),
        6 => matches!(value, Value::List(_)),
        7 => matches!(value, Value::Vector(_)),
        8 => matches!(value, Value::Map(_)),
        9 => matches!(value, Value::Function { .. } | Value::NativeFunction { .. }),
        10 => matches!(value, Value::Promise(_)),
        11 => matches!(value, Value::Channel(_)),
        12 => matches!(value, Value::Error { .. }),
        _ => false,
    };
    
    TaggedValue::from_integer(if is_match { 1 } else { 0 }).0 as i64
}

/// Runtime helper for getting type name
#[no_mangle]
pub extern "C" fn jit_runtime_type_name(value_tagged: i64) -> i64 {
    let value = TaggedValue(value_tagged as u64).to_value();
    
    let type_name = match value {
        Value::Integer(_) => "integer",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::Symbol(_) => "symbol",
        Value::Boolean(_) => "boolean",
        Value::Nil => "nil",
        Value::List(_) => "list",
        Value::Vector(_) => "vector",
        Value::Map(_) => "map",
        Value::Function { .. } => "function",
        Value::NativeFunction { .. } => "native_function",
        Value::Promise(_) => "promise",
        Value::Channel(_) => "channel",
        Value::Error { .. } => "error",
        Value::Cell(_) => "cell",
        Value::Module { .. } => "module",
        Value::GcHandle(_) => "gc_handle",
        Value::Tagged { .. } => "tagged",
        Value::Procedure(_) => "procedure",
        Value::Future { .. } => "future",
        Value::Actor(_) => "actor",
    };
    
    let name_value = Value::String(type_name.to_string());
    value_to_tagged(&name_value).0 as i64
}

// ===== Error Handling =====

/// Runtime helper for creating an error
#[no_mangle]
pub extern "C" fn jit_runtime_make_error(kind_tagged: i64, msg_tagged: i64) -> i64 {
    let kind = TaggedValue(kind_tagged as u64).to_value();
    let msg = TaggedValue(msg_tagged as u64).to_value();
    
    match (kind, msg) {
        (Value::String(k), Value::String(m)) => {
            let error = Value::Error {
                kind: k,
                message: m,
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Error constructor requires string kind and message".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for checking if value is an error
#[no_mangle]
pub extern "C" fn jit_runtime_is_error(value_tagged: i64) -> i64 {
    let value = TaggedValue(value_tagged as u64).to_value();
    let is_error = matches!(value, Value::Error { .. });
    TaggedValue::from_integer(if is_error { 1 } else { 0 }).0 as i64
}

// ===== Closure Support =====

/// Runtime helper for creating a closure
#[no_mangle]
pub extern "C" fn jit_runtime_make_closure(chunk_id: i64, capture_count: i64, captures_ptr: *const i64) -> i64 {
    unsafe {
        let captures = if capture_count > 0 {
            slice::from_raw_parts(captures_ptr, capture_count as usize)
        } else {
            &[]
        };
        
        let mut env = Vec::with_capacity(captures.len());
        for &capture in captures {
            let tagged = TaggedValue(capture as u64);
            env.push(tagged.to_value());
        }
        
        let closure = Value::Function {
            chunk_id: chunk_id as usize,
            env,
        };
        
        value_to_tagged(&closure).0 as i64
    }
}

/// Runtime helper for accessing closure environment
#[no_mangle]
pub extern "C" fn jit_runtime_closure_get_env(closure_tagged: i64, idx: i64) -> i64 {
    let closure = TaggedValue(closure_tagged as u64).to_value();
    
    match closure {
        Value::Function { env, .. } => {
            let index = idx as usize;
            if index < env.len() {
                value_to_tagged(&env[index]).0 as i64
            } else {
                let error = Value::Error {
                    kind: "IndexError".to_string(),
                    message: format!("Closure environment index {} out of bounds", index),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            }
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Expected closure for environment access".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

// ===== Channel Operations =====

// Note: These are placeholder implementations. Real channel operations
// would require proper synchronization and blocking support.

/// Runtime helper for creating a channel
#[no_mangle]
pub extern "C" fn jit_runtime_make_channel() -> i64 {
    // In a real implementation, this would create an actual channel
    // For now, use a unique ID
    static CHANNEL_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    let id = CHANNEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    
    let channel = Value::Channel(id);
    value_to_tagged(&channel).0 as i64
}

/// Runtime helper for channel send (placeholder)
#[no_mangle]
pub extern "C" fn jit_runtime_channel_send(channel_tagged: i64, _value_tagged: i64) -> i64 {
    let channel = TaggedValue(channel_tagged as u64).to_value();
    
    match channel {
        Value::Channel(_id) => {
            // In a real implementation, this would send the value through the channel
            // For now, just return nil to indicate success
            value_to_tagged(&Value::Nil).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Channel send requires a channel".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for channel receive (placeholder)
#[no_mangle]
pub extern "C" fn jit_runtime_channel_recv(channel_tagged: i64) -> i64 {
    let channel = TaggedValue(channel_tagged as u64).to_value();
    
    match channel {
        Value::Channel(_id) => {
            // In a real implementation, this would receive from the channel
            // For now, return a dummy value
            let error = Value::Error {
                kind: "RuntimeError".to_string(),
                message: "Channel operations not yet fully implemented in JIT".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Channel receive requires a channel".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

// ===== Promise Operations =====

/// Runtime helper for creating a promise (placeholder)
#[no_mangle]
pub extern "C" fn jit_runtime_make_promise() -> i64 {
    // In a real implementation, this would create an actual promise
    // For now, use a unique ID
    static PROMISE_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    let id = PROMISE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    
    let promise = Value::Promise(id);
    value_to_tagged(&promise).0 as i64
}

/// Runtime helper for promise await (placeholder)
#[no_mangle]
pub extern "C" fn jit_runtime_promise_await(promise_tagged: i64) -> i64 {
    let promise = TaggedValue(promise_tagged as u64).to_value();
    
    match promise {
        Value::Promise(_id) => {
            // In a real implementation, this would wait for the promise
            // For now, return an error
            let error = Value::Error {
                kind: "RuntimeError".to_string(),
                message: "Promise operations not yet fully implemented in JIT".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Promise await requires a promise".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

// ===== Float Operations =====

/// Runtime helper for float addition
#[no_mangle]
pub extern "C" fn jit_runtime_float_add(a_tagged: i64, b_tagged: i64) -> i64 {
    let a = TaggedValue(a_tagged as u64).to_value();
    let b = TaggedValue(b_tagged as u64).to_value();
    
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => {
            let result = Value::Float(x + y);
            value_to_tagged(&result).0 as i64
        }
        (Value::Integer(x), Value::Float(y)) => {
            let result = Value::Float(x as f64 + y);
            value_to_tagged(&result).0 as i64
        }
        (Value::Float(x), Value::Integer(y)) => {
            let result = Value::Float(x + y as f64);
            value_to_tagged(&result).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Float addition requires numeric arguments".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for float subtraction
#[no_mangle]
pub extern "C" fn jit_runtime_float_sub(a_tagged: i64, b_tagged: i64) -> i64 {
    let a = TaggedValue(a_tagged as u64).to_value();
    let b = TaggedValue(b_tagged as u64).to_value();
    
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => {
            let result = Value::Float(x - y);
            value_to_tagged(&result).0 as i64
        }
        (Value::Integer(x), Value::Float(y)) => {
            let result = Value::Float(x as f64 - y);
            value_to_tagged(&result).0 as i64
        }
        (Value::Float(x), Value::Integer(y)) => {
            let result = Value::Float(x - y as f64);
            value_to_tagged(&result).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Float subtraction requires numeric arguments".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for float multiplication
#[no_mangle]
pub extern "C" fn jit_runtime_float_mul(a_tagged: i64, b_tagged: i64) -> i64 {
    let a = TaggedValue(a_tagged as u64).to_value();
    let b = TaggedValue(b_tagged as u64).to_value();
    
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => {
            let result = Value::Float(x * y);
            value_to_tagged(&result).0 as i64
        }
        (Value::Integer(x), Value::Float(y)) => {
            let result = Value::Float(x as f64 * y);
            value_to_tagged(&result).0 as i64
        }
        (Value::Float(x), Value::Integer(y)) => {
            let result = Value::Float(x * y as f64);
            value_to_tagged(&result).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Float multiplication requires numeric arguments".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for float division
#[no_mangle]
pub extern "C" fn jit_runtime_float_div(a_tagged: i64, b_tagged: i64) -> i64 {
    let a = TaggedValue(a_tagged as u64).to_value();
    let b = TaggedValue(b_tagged as u64).to_value();
    
    match (a, b) {
        (Value::Float(x), Value::Float(y)) => {
            if y == 0.0 {
                let error = Value::Error {
                    kind: "ArithmeticError".to_string(),
                    message: "Division by zero".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            } else {
                let result = Value::Float(x / y);
                value_to_tagged(&result).0 as i64
            }
        }
        (Value::Integer(x), Value::Float(y)) => {
            if y == 0.0 {
                let error = Value::Error {
                    kind: "ArithmeticError".to_string(),
                    message: "Division by zero".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            } else {
                let result = Value::Float(x as f64 / y);
                value_to_tagged(&result).0 as i64
            }
        }
        (Value::Float(x), Value::Integer(y)) => {
            if y == 0 {
                let error = Value::Error {
                    kind: "ArithmeticError".to_string(),
                    message: "Division by zero".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            } else {
                let result = Value::Float(x / y as f64);
                value_to_tagged(&result).0 as i64
            }
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "Float division requires numeric arguments".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Get the address of an extended runtime function by name
pub fn get_extended_runtime_function(name: &str) -> Option<*const u8> {
    match name {
        // Map operations
        "jit_runtime_make_map" => Some(jit_runtime_make_map as *const u8),
        "jit_runtime_map_get" => Some(jit_runtime_map_get as *const u8),
        "jit_runtime_map_set" => Some(jit_runtime_map_set as *const u8),
        "jit_runtime_map_contains" => Some(jit_runtime_map_contains as *const u8),
        "jit_runtime_map_size" => Some(jit_runtime_map_size as *const u8),
        
        // Vector operations
        "jit_runtime_make_vector" => Some(jit_runtime_make_vector as *const u8),
        "jit_runtime_vector_push" => Some(jit_runtime_vector_push as *const u8),
        "jit_runtime_vector_get" => Some(jit_runtime_vector_get as *const u8),
        "jit_runtime_vector_len" => Some(jit_runtime_vector_len as *const u8),
        
        // Type checking
        "jit_runtime_is_type" => Some(jit_runtime_is_type as *const u8),
        "jit_runtime_type_name" => Some(jit_runtime_type_name as *const u8),
        
        // Error handling
        "jit_runtime_make_error" => Some(jit_runtime_make_error as *const u8),
        "jit_runtime_is_error" => Some(jit_runtime_is_error as *const u8),
        
        // Closure support
        "jit_runtime_make_closure" => Some(jit_runtime_make_closure as *const u8),
        "jit_runtime_closure_get_env" => Some(jit_runtime_closure_get_env as *const u8),
        
        // Channel operations
        "jit_runtime_make_channel" => Some(jit_runtime_make_channel as *const u8),
        "jit_runtime_channel_send" => Some(jit_runtime_channel_send as *const u8),
        "jit_runtime_channel_recv" => Some(jit_runtime_channel_recv as *const u8),
        
        // Promise operations
        "jit_runtime_make_promise" => Some(jit_runtime_make_promise as *const u8),
        "jit_runtime_promise_await" => Some(jit_runtime_promise_await as *const u8),
        
        // Float operations
        "jit_runtime_float_add" => Some(jit_runtime_float_add as *const u8),
        "jit_runtime_float_sub" => Some(jit_runtime_float_sub as *const u8),
        "jit_runtime_float_mul" => Some(jit_runtime_float_mul as *const u8),
        "jit_runtime_float_div" => Some(jit_runtime_float_div as *const u8),
        
        _ => None,
    }
}