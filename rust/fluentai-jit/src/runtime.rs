//! Runtime support for JIT-compiled code
//!
//! This module provides runtime functions that JIT code can call for complex operations
//! that are easier to implement in Rust than in generated machine code.

use crate::value::{TaggedValue, value_to_tagged};
use fluentai_core::value::Value;
use std::slice;

/// Runtime function for handling dynamic function calls
/// 
/// This function is called from JIT code when a CALL instruction is executed.
/// It handles the dispatch to the appropriate function type (VM function, native function, etc.)
#[no_mangle]
pub extern "C" fn jit_runtime_call(
    func_tagged: i64,
    arg_count: i64,
    args_ptr: *const i64,
) -> i64 {
    unsafe {
        // Convert arguments to slice
        let args = if arg_count > 0 {
            slice::from_raw_parts(args_ptr, arg_count as usize)
        } else {
            &[]
        };
        
        // Decode the function value
        let func_val = TaggedValue(func_tagged as u64);
        let func = func_val.to_value();
        
        // Decode arguments
        let mut arg_values = Vec::with_capacity(args.len());
        for &arg in args {
            let tagged = TaggedValue(arg as u64);
            arg_values.push(tagged.to_value());
        }
        
        // Dispatch based on function type
        match func {
            Value::Function { chunk_id, env } => {
                // For VM functions, we need to call back into the VM
                // This is complex and would require a VM instance reference
                // For now, return an error value
                let error = Value::Error {
                    kind: "JIT".to_string(),
                    message: "VM function calls from JIT not yet implemented".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            }
            Value::NativeFunction { function, .. } => {
                // Call the native function
                match function(&arg_values) {
                    Ok(result) => value_to_tagged(&result).0 as i64,
                    Err(e) => {
                        let error = Value::Error {
                            kind: "Native".to_string(),
                            message: e.to_string(),
                            stack_trace: None,
                        };
                        value_to_tagged(&error).0 as i64
                    }
                }
            }
            _ => {
                // Not a callable value
                let error = Value::Error {
                    kind: "TypeError".to_string(),
                    message: format!("Cannot call non-function value"),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            }
        }
    }
}

/// Runtime helper for integer addition with overflow checking
#[no_mangle]
pub extern "C" fn jit_runtime_add_checked(a: i64, b: i64) -> i64 {
    match a.checked_add(b) {
        Some(result) => TaggedValue::from_integer(result).0 as i64,
        None => {
            // Return an error for overflow
            let error = Value::Error {
                kind: "ArithmeticError".to_string(),
                message: "Integer overflow in addition".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for string concatenation
#[no_mangle]
pub extern "C" fn jit_runtime_string_concat(a_tagged: i64, b_tagged: i64) -> i64 {
    unsafe {
        let a = TaggedValue(a_tagged as u64).to_value();
        let b = TaggedValue(b_tagged as u64).to_value();
        
        match (a, b) {
            (Value::String(s1), Value::String(s2)) => {
                let result = Value::String(s1 + &s2);
                value_to_tagged(&result).0 as i64
            }
            _ => {
                let error = Value::Error {
                    kind: "TypeError".to_string(),
                    message: "String concatenation requires two strings".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            }
        }
    }
}

/// Runtime helper for getting string length
#[no_mangle]
pub extern "C" fn jit_runtime_string_len(s_tagged: i64) -> i64 {
    let s = TaggedValue(s_tagged as u64).to_value();
    
    match s {
        Value::String(ref s) => TaggedValue::from_integer(s.len() as i64).0 as i64,
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "String length requires a string".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for converting string to uppercase
#[no_mangle]
pub extern "C" fn jit_runtime_string_upper(s_tagged: i64) -> i64 {
    let s = TaggedValue(s_tagged as u64).to_value();
    
    match s {
        Value::String(s) => {
            let result = Value::String(s.to_uppercase());
            value_to_tagged(&result).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "String upper requires a string".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for converting string to lowercase
#[no_mangle]
pub extern "C" fn jit_runtime_string_lower(s_tagged: i64) -> i64 {
    let s = TaggedValue(s_tagged as u64).to_value();
    
    match s {
        Value::String(s) => {
            let result = Value::String(s.to_lowercase());
            value_to_tagged(&result).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "String lower requires a string".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for creating a list
#[no_mangle]
pub extern "C" fn jit_runtime_make_list(count: i64, args_ptr: *const i64) -> i64 {
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
        
        let list = Value::List(items);
        value_to_tagged(&list).0 as i64
    }
}

/// Runtime helper for getting list length
#[no_mangle]
pub extern "C" fn jit_runtime_list_len(list_tagged: i64) -> i64 {
    let list = TaggedValue(list_tagged as u64).to_value();
    
    match list {
        Value::List(ref l) => TaggedValue::from_integer(l.len() as i64).0 as i64,
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "List length requires a list".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for checking if list is empty
#[no_mangle]
pub extern "C" fn jit_runtime_list_empty(list_tagged: i64) -> i64 {
    let list = TaggedValue(list_tagged as u64).to_value();
    
    match list {
        Value::List(ref l) => TaggedValue::from_integer(if l.is_empty() { 1 } else { 0 }).0 as i64,
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "List empty check requires a list".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for getting list head
#[no_mangle]
pub extern "C" fn jit_runtime_list_head(list_tagged: i64) -> i64 {
    let list = TaggedValue(list_tagged as u64).to_value();
    
    match list {
        Value::List(ref l) => {
            if l.is_empty() {
                let error = Value::Error {
                    kind: "RuntimeError".to_string(),
                    message: "Cannot get head of empty list".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            } else {
                value_to_tagged(&l[0]).0 as i64
            }
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "List head requires a list".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for getting list tail
#[no_mangle]
pub extern "C" fn jit_runtime_list_tail(list_tagged: i64) -> i64 {
    let list = TaggedValue(list_tagged as u64).to_value();
    
    match list {
        Value::List(l) => {
            if l.is_empty() {
                let error = Value::Error {
                    kind: "RuntimeError".to_string(),
                    message: "Cannot get tail of empty list".to_string(),
                    stack_trace: None,
                };
                value_to_tagged(&error).0 as i64
            } else {
                let tail = Value::List(l[1..].to_vec());
                value_to_tagged(&tail).0 as i64
            }
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "List tail requires a list".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Runtime helper for list cons
#[no_mangle]
pub extern "C" fn jit_runtime_list_cons(elem_tagged: i64, list_tagged: i64) -> i64 {
    let elem = TaggedValue(elem_tagged as u64).to_value();
    let list = TaggedValue(list_tagged as u64).to_value();
    
    match list {
        Value::List(l) => {
            let mut new_list = vec![elem];
            new_list.extend(l);
            let result = Value::List(new_list);
            value_to_tagged(&result).0 as i64
        }
        _ => {
            let error = Value::Error {
                kind: "TypeError".to_string(),
                message: "List cons requires a list as second argument".to_string(),
                stack_trace: None,
            };
            value_to_tagged(&error).0 as i64
        }
    }
}

/// Get the address of a runtime function by name
pub fn get_runtime_function(name: &str) -> Option<*const u8> {
    match name {
        "jit_runtime_call" => Some(jit_runtime_call as *const u8),
        "jit_runtime_add_checked" => Some(jit_runtime_add_checked as *const u8),
        "jit_runtime_string_concat" => Some(jit_runtime_string_concat as *const u8),
        "jit_runtime_string_len" => Some(jit_runtime_string_len as *const u8),
        "jit_runtime_string_upper" => Some(jit_runtime_string_upper as *const u8),
        "jit_runtime_string_lower" => Some(jit_runtime_string_lower as *const u8),
        "jit_runtime_make_list" => Some(jit_runtime_make_list as *const u8),
        "jit_runtime_list_len" => Some(jit_runtime_list_len as *const u8),
        "jit_runtime_list_empty" => Some(jit_runtime_list_empty as *const u8),
        "jit_runtime_list_head" => Some(jit_runtime_list_head as *const u8),
        "jit_runtime_list_tail" => Some(jit_runtime_list_tail as *const u8),
        "jit_runtime_list_cons" => Some(jit_runtime_list_cons as *const u8),
        _ => None,
    }
}