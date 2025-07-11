//! Test support functions for FluentAI
//! 
//! This module provides functions commonly needed in tests but not part of the core language.

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::Result;

/// Register test support functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Basic I/O
        StdlibFunction::pure("print", print, 1, None, "Print value to stdout"),
        
        // List construction
        StdlibFunction::pure("list", list, 0, None, "Create a list from arguments"),
        
        // Symbol creation
        StdlibFunction::pure("quote", quote, 1, Some(1), "Create a symbol from a string"),
    ]);
}

/// Print a value to stdout
fn print(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        println!();
    } else {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{}", format_value(arg));
        }
        println!();
    }
    Ok(Value::Nil)
}

/// Format a value for printing
fn format_value(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::List(items) => {
            let formatted: Vec<String> = items.iter().map(format_value).collect();
            format!("[{}]", formatted.join(", "))
        }
        Value::Map(map) => {
            let pairs: Vec<String> = map.iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
        _ => format!("{:?}", value),
    }
}

/// Create a list from arguments
fn list(args: &[Value]) -> Result<Value> {
    Ok(Value::List(args.to_vec()))
}

/// Create a symbol from a string
fn quote(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(anyhow::anyhow!("quote expects exactly 1 argument"));
    }
    
    match &args[0] {
        Value::String(s) => Ok(Value::Symbol(s.clone())),
        Value::Symbol(s) => Ok(Value::Symbol(s.clone())), // Already a symbol
        _ => Err(anyhow::anyhow!("quote expects a string argument")),
    }
}