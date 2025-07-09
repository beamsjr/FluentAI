//! Standard library registration for SDK

use fluentai_core_lib::{RuntimeEngine, HostFunction};
use fluentai_core::value::Value;
use crate::error::Result;

/// Register all standard library functions
pub fn register_all(engine: &RuntimeEngine) -> Result<()> {
    register_core(engine)?;
    register_math(engine)?;
    register_string(engine)?;
    register_list(engine)?;
    register_io(engine)?;
    Ok(())
}

/// Register core functions
pub fn register_core(engine: &RuntimeEngine) -> Result<()> {
    let functions = vec![
        // Type checking
        HostFunction::new("type-of", 1, |args| {
            Ok(Value::Symbol(match &args[0] {
                Value::Nil => "nil",
                Value::Boolean(_) => "bool",
                Value::Float(_) | Value::Integer(_) => "number",
                Value::String(_) => "string",
                Value::Symbol(_) => "symbol",
                Value::List(_) => "list",
                Value::Procedure { .. } | Value::Function { .. } => "function",
                                                _ => "unknown",
            }.to_string()))
        }),
        
        // Equality
        HostFunction::new("equal?", 2, |args| {
            Ok(Value::Boolean(args[0] == args[1]))
        }),
        
        // Not
        HostFunction::new("not", 1, |args| {
            Ok(Value::Boolean(match &args[0] {
                Value::Boolean(b) => !b,
                Value::Nil => true,
                _ => false,
            }))
        }),
    ];
    
    engine.register_functions(functions)?;
    Ok(())
}

/// Register math functions
pub fn register_math(engine: &RuntimeEngine) -> Result<()> {
    let functions = vec![
        // Basic arithmetic is usually in the VM, but we can add extended functions
        HostFunction::new("abs", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.abs())),
                Value::Integer(n) => Ok(Value::Integer(n.abs())),
                _ => Err(fluentai_core_lib::RuntimeError::host("abs expects a number")),
            }
        }),
        
        HostFunction::new("sqrt", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.sqrt())),
                Value::Integer(n) => Ok(Value::Float((*n as f64).sqrt())),
                _ => Err(fluentai_core_lib::RuntimeError::host("sqrt expects a number")),
            }
        }),
        
        HostFunction::new("pow", 2, |args| {
            match (&args[0], &args[1]) {
                (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
                (Value::Integer(base), Value::Integer(exp)) => Ok(Value::Float((*base as f64).powf(*exp as f64))),
                (Value::Float(base), Value::Integer(exp)) => Ok(Value::Float(base.powf(*exp as f64))),
                (Value::Integer(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
                _ => Err(fluentai_core_lib::RuntimeError::host("pow expects two numbers")),
            }
        }),
        
        HostFunction::new("sin", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.sin())),
                Value::Integer(n) => Ok(Value::Float((*n as f64).sin())),
                _ => Err(fluentai_core_lib::RuntimeError::host("sin expects a number")),
            }
        }),
        
        HostFunction::new("cos", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.cos())),
                Value::Integer(n) => Ok(Value::Float((*n as f64).cos())),
                _ => Err(fluentai_core_lib::RuntimeError::host("cos expects a number")),
            }
        }),
        
        HostFunction::new("tan", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.tan())),
                Value::Integer(n) => Ok(Value::Float((*n as f64).tan())),
                _ => Err(fluentai_core_lib::RuntimeError::host("tan expects a number")),
            }
        }),
        
        HostFunction::new("floor", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.floor())),
                Value::Integer(n) => Ok(Value::Integer(*n)),
                _ => Err(fluentai_core_lib::RuntimeError::host("floor expects a number")),
            }
        }),
        
        HostFunction::new("ceil", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.ceil())),
                Value::Integer(n) => Ok(Value::Integer(*n)),
                _ => Err(fluentai_core_lib::RuntimeError::host("ceil expects a number")),
            }
        }),
        
        HostFunction::new("round", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::Float(n.round())),
                Value::Integer(n) => Ok(Value::Integer(*n)),
                _ => Err(fluentai_core_lib::RuntimeError::host("round expects a number")),
            }
        }),
    ];
    
    engine.register_functions(functions)?;
    Ok(())
}

/// Register string functions
pub fn register_string(engine: &RuntimeEngine) -> Result<()> {
    let functions = vec![
        HostFunction::new("string-length", 1, |args| {
            match &args[0] {
                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(fluentai_core_lib::RuntimeError::host("string-length expects a string")),
            }
        }),
        
        HostFunction::new("string-append", 0, |args| {
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::String(s) => result.push_str(s),
                    _ => return Err(fluentai_core_lib::RuntimeError::host("string-append expects strings")),
                }
            }
            Ok(Value::String(result))
        }).variadic(),
        
        HostFunction::new("string-upcase", 1, |args| {
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_uppercase())),
                _ => Err(fluentai_core_lib::RuntimeError::host("string-upcase expects a string")),
            }
        }),
        
        HostFunction::new("string-downcase", 1, |args| {
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_lowercase())),
                _ => Err(fluentai_core_lib::RuntimeError::host("string-downcase expects a string")),
            }
        }),
        
        HostFunction::new("string->number", 1, |args| {
            match &args[0] {
                Value::String(s) => {
                    match s.parse::<f64>() {
                        Ok(n) => Ok(Value::Float(n)),
                        Err(_) => Ok(Value::Nil),
                    }
                }
                _ => Err(fluentai_core_lib::RuntimeError::host("string->number expects a string")),
            }
        }),
        
        HostFunction::new("number->string", 1, |args| {
            match &args[0] {
                Value::Float(n) => Ok(Value::String(n.to_string())),
                Value::Integer(n) => Ok(Value::String(n.to_string())),
                _ => Err(fluentai_core_lib::RuntimeError::host("number->string expects a number")),
            }
        }),
    ];
    
    engine.register_functions(functions)?;
    Ok(())
}

/// Register list functions
pub fn register_list(engine: &RuntimeEngine) -> Result<()> {
    let functions = vec![
        HostFunction::new("list", 0, |args| {
            Ok(Value::List(args.to_vec()))
        }).variadic(),
        
        HostFunction::new("list-length", 1, |args| {
            match &args[0] {
                Value::List(list) => Ok(Value::Integer(list.len() as i64)),
                _ => Err(fluentai_core_lib::RuntimeError::host("list-length expects a list")),
            }
        }),
        
        HostFunction::new("list-ref", 2, |args| {
            match (&args[0], &args[1]) {
                (Value::List(list), Value::Integer(n)) => {
                    let index = *n as usize;
                    if index < list.len() {
                        Ok(list[index].clone())
                    } else {
                        Err(fluentai_core_lib::RuntimeError::host("list index out of bounds"))
                    }
                }
                (Value::List(list), Value::Float(n)) => {
                    let index = *n as usize;
                    if index < list.len() {
                        Ok(list[index].clone())
                    } else {
                        Err(fluentai_core_lib::RuntimeError::host("list index out of bounds"))
                    }
                }
                _ => Err(fluentai_core_lib::RuntimeError::host("list-ref expects a list and index")),
            }
        }),
        
        HostFunction::new("list-append", 0, |args| {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(list) => result.extend(list.iter().cloned()),
                    _ => return Err(fluentai_core_lib::RuntimeError::host("list-append expects lists")),
                }
            }
            Ok(Value::List(result))
        }).variadic(),
    ];
    
    engine.register_functions(functions)?;
    Ok(())
}

/// Register IO functions
pub fn register_io(engine: &RuntimeEngine) -> Result<()> {
    let functions = vec![
        HostFunction::new("display", 1, |args| {
            print!("{}", format_value(&args[0]));
            Ok(Value::Nil)
        }),
        
        HostFunction::new("displayln", 1, |args| {
            println!("{}", format_value(&args[0]));
            Ok(Value::Nil)
        }),
        
        HostFunction::new("print", 0, |args| {
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                print!("{}", format_value(arg));
            }
            println!();
            Ok(Value::Nil)
        }).variadic(),
    ];
    
    engine.register_functions(functions)?;
    Ok(())
}

/// Format a value for display
fn format_value(value: &Value) -> String {
    match value {
        Value::Nil => "nil".to_string(),
        Value::Boolean(b) => b.to_string(),
        Value::Float(n) => n.to_string(),
        Value::Integer(n) => n.to_string(),
        Value::String(s) => s.to_string(),
        Value::Symbol(s) => s.to_string(),
        Value::List(list) => {
            let items: Vec<String> = list.iter().map(format_value).collect();
            format!("({})", items.join(" "))
        }
        _ => format!("{:?}", value),
    }
}