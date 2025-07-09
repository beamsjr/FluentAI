//! Unboxed value representation for high-performance numeric operations

use std::fmt;
use fluentai_core::value::Value;
use crate::safety::{PromiseId, ChannelId};
use rustc_hash::FxHashMap;

/// Unboxed value representation using NaN-boxing and tagged pointers
/// 
/// This representation allows us to store common values directly without heap allocation:
/// - Small integers (up to 61 bits) using tagged pointers
/// - Floats using NaN-boxing
/// - Booleans and nil as special bit patterns
/// - Other values boxed as before
#[derive(Clone)]
pub enum UnboxedValue {
    /// Immediate integer (61-bit range)
    Int(i64),
    /// Immediate float using NaN-boxing
    Float(f64),
    /// Immediate boolean
    Bool(bool),
    /// Immediate nil
    Nil,
    /// Boxed heap value for complex types
    Boxed(Box<BoxedValue>),
}

/// Boxed values that require heap allocation
#[derive(Debug, Clone)]
pub enum BoxedValue {
    String(String),
    List(Vec<UnboxedValue>),
    Closure {
        chunk_id: usize,
        captured_env: Vec<UnboxedValue>,
        param_count: u32,
    },
    NativeFunc {
        name: String,
        arity: u32,
        func: fn(&mut Vec<UnboxedValue>) -> Result<UnboxedValue, String>,
    },
    Cell(usize),
    Promise(PromiseId),
    Channel(ChannelId),
    Tagged {
        tag: String,
        values: Vec<UnboxedValue>,
    },
    Module {
        name: String,
        exports: FxHashMap<String, UnboxedValue>,
    },
}

impl UnboxedValue {
    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            UnboxedValue::Bool(b) => *b,
            UnboxedValue::Nil => false,
            _ => true,
        }
    }
    
    /// Convert from old Value type
    pub fn from_value(value: Value) -> Self {
        match value {
            Value::Nil => UnboxedValue::Nil,
            Value::Boolean(b) => UnboxedValue::Bool(b),
            Value::Integer(i) => UnboxedValue::Int(i),
            Value::Float(f) => UnboxedValue::Float(f),
            Value::String(s) => UnboxedValue::Boxed(Box::new(BoxedValue::String(s))),
            Value::Symbol(s) => UnboxedValue::Boxed(Box::new(BoxedValue::String(s))), // Treat symbols as strings
            Value::List(items) => {
                let unboxed_items = items.into_iter().map(UnboxedValue::from_value).collect();
                UnboxedValue::Boxed(Box::new(BoxedValue::List(unboxed_items)))
            }
            Value::Procedure(_) => {
                // Procedures are not yet supported in unboxed representation
                UnboxedValue::Boxed(Box::new(BoxedValue::String("<procedure>".to_string())))
            }
            Value::Vector(items) => {
                // Convert vectors to lists in unboxed representation
                let unboxed_items = items.into_iter().map(UnboxedValue::from_value).collect();
                UnboxedValue::Boxed(Box::new(BoxedValue::List(unboxed_items)))
            }
            Value::Map(_map) => {
                // Maps are not yet supported in unboxed representation
                // For now, convert to a string representation
                UnboxedValue::Boxed(Box::new(BoxedValue::String("<map>".to_string())))
            }
            Value::NativeFunction { name, .. } => {
                // Native functions are not supported in unboxed representation
                UnboxedValue::Boxed(Box::new(BoxedValue::String(format!("<native-function: {}>", name))))
            }
            Value::Function { chunk_id, env } => {
                let unboxed_env = env.into_iter().map(UnboxedValue::from_value).collect();
                UnboxedValue::Boxed(Box::new(BoxedValue::Closure {
                    chunk_id,
                    captured_env: unboxed_env,
                    param_count: 0, // Default to 0, actual count would need to be tracked elsewhere
                }))
            }
            Value::Cell(idx) => UnboxedValue::Boxed(Box::new(BoxedValue::Cell(idx))),
            Value::Promise(id) => UnboxedValue::Boxed(Box::new(BoxedValue::Promise(PromiseId(id)))),
            Value::Channel(id) => UnboxedValue::Boxed(Box::new(BoxedValue::Channel(ChannelId(id)))),
            Value::Tagged { tag, values } => {
                let unboxed_values = values.into_iter().map(UnboxedValue::from_value).collect();
                UnboxedValue::Boxed(Box::new(BoxedValue::Tagged {
                    tag,
                    values: unboxed_values,
                }))
            }
            Value::Module { name, exports } => {
                let unboxed_exports = exports.into_iter()
                    .map(|(k, v)| (k, UnboxedValue::from_value(v)))
                    .collect();
                UnboxedValue::Boxed(Box::new(BoxedValue::Module {
                    name,
                    exports: unboxed_exports,
                }))
            }
            Value::GcHandle(_) => {
                // For now, treat GC handles as nil
                // In a real implementation, we'd need proper GC integration
                UnboxedValue::Nil
            }
        }
    }
    
    /// Convert to old Value type
    pub fn to_value(self) -> Value {
        match self {
            UnboxedValue::Nil => Value::Nil,
            UnboxedValue::Bool(b) => Value::Boolean(b),
            UnboxedValue::Int(i) => Value::Integer(i),
            UnboxedValue::Float(f) => Value::Float(f),
            UnboxedValue::Boxed(boxed) => match *boxed {
                BoxedValue::String(s) => Value::String(s),
                BoxedValue::List(items) => {
                    let values = items.into_iter().map(|v| v.to_value()).collect();
                    Value::List(values)
                }
                BoxedValue::Closure { chunk_id, captured_env, param_count: _ } => {
                    let env = captured_env.into_iter().map(|v| v.to_value()).collect();
                    Value::Function { chunk_id, env }
                }
                BoxedValue::Cell(idx) => Value::Cell(idx),
                BoxedValue::Promise(id) => Value::Promise(id.0),
                BoxedValue::Channel(id) => Value::Channel(id.0),
                BoxedValue::Tagged { tag, values } => {
                    let vals = values.into_iter().map(|v| v.to_value()).collect();
                    Value::Tagged { tag, values: vals }
                }
                BoxedValue::Module { name, exports } => {
                    let exps = exports.into_iter()
                        .map(|(k, v)| (k, v.to_value()))
                        .collect();
                    Value::Module { name, exports: exps }
                }
                BoxedValue::NativeFunc { .. } => {
                    // Can't convert back - return nil
                    Value::Nil
                }
            }
        }
    }
}

impl fmt::Debug for UnboxedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnboxedValue::Nil => write!(f, "nil"),
            UnboxedValue::Bool(b) => write!(f, "{}", b),
            UnboxedValue::Int(i) => write!(f, "{}", i),
            UnboxedValue::Float(fl) => write!(f, "{}", fl),
            UnboxedValue::Boxed(boxed) => write!(f, "{:?}", boxed),
        }
    }
}

impl fmt::Display for UnboxedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnboxedValue::Nil => write!(f, "nil"),
            UnboxedValue::Bool(b) => write!(f, "{}", b),
            UnboxedValue::Int(i) => write!(f, "{}", i),
            UnboxedValue::Float(fl) => write!(f, "{}", fl),
            UnboxedValue::Boxed(boxed) => match &**boxed {
                BoxedValue::String(s) => write!(f, "\"{}\"", s),
                BoxedValue::List(items) => {
                    write!(f, "[")?;
                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", item)?;
                    }
                    write!(f, "]")
                }
                BoxedValue::Closure { chunk_id, param_count, .. } => {
                    write!(f, "<closure:{} params:{}>", chunk_id, param_count)
                }
                BoxedValue::NativeFunc { name, arity, .. } => {
                    write!(f, "<native:{} arity:{}>", name, arity)
                }
                BoxedValue::Cell(idx) => write!(f, "<cell:{}>", idx),
                BoxedValue::Promise(id) => write!(f, "<promise:{}>", id.0),
                BoxedValue::Channel(id) => write!(f, "<channel:{}>", id.0),
                BoxedValue::Tagged { tag, values } => {
                    write!(f, "({}", tag)?;
                    for value in values {
                        write!(f, " {}", value)?;
                    }
                    write!(f, ")")
                }
                BoxedValue::Module { name, exports } => {
                    write!(f, "<module {} with {} exports>", name, exports.len())
                }
            }
        }
    }
}

/// Optimized arithmetic operations on unboxed values
impl UnboxedValue {
    /// Add two values
    pub fn add(&self, other: &UnboxedValue) -> Result<UnboxedValue, String> {
        match (self, other) {
            (UnboxedValue::Int(a), UnboxedValue::Int(b)) => {
                // Check for overflow
                match a.checked_add(*b) {
                    Some(result) => Ok(UnboxedValue::Int(result)),
                    None => Ok(UnboxedValue::Float(*a as f64 + *b as f64)),
                }
            }
            (UnboxedValue::Float(a), UnboxedValue::Float(b)) => Ok(UnboxedValue::Float(a + b)),
            (UnboxedValue::Int(a), UnboxedValue::Float(b)) => Ok(UnboxedValue::Float(*a as f64 + b)),
            (UnboxedValue::Float(a), UnboxedValue::Int(b)) => Ok(UnboxedValue::Float(a + *b as f64)),
            _ => Err("Type error: can only add numbers".to_string()),
        }
    }
    
    /// Subtract two values
    pub fn sub(&self, other: &UnboxedValue) -> Result<UnboxedValue, String> {
        match (self, other) {
            (UnboxedValue::Int(a), UnboxedValue::Int(b)) => {
                match a.checked_sub(*b) {
                    Some(result) => Ok(UnboxedValue::Int(result)),
                    None => Ok(UnboxedValue::Float(*a as f64 - *b as f64)),
                }
            }
            (UnboxedValue::Float(a), UnboxedValue::Float(b)) => Ok(UnboxedValue::Float(a - b)),
            (UnboxedValue::Int(a), UnboxedValue::Float(b)) => Ok(UnboxedValue::Float(*a as f64 - b)),
            (UnboxedValue::Float(a), UnboxedValue::Int(b)) => Ok(UnboxedValue::Float(a - *b as f64)),
            _ => Err("Type error: can only subtract numbers".to_string()),
        }
    }
    
    /// Multiply two values
    pub fn mul(&self, other: &UnboxedValue) -> Result<UnboxedValue, String> {
        match (self, other) {
            (UnboxedValue::Int(a), UnboxedValue::Int(b)) => {
                match a.checked_mul(*b) {
                    Some(result) => Ok(UnboxedValue::Int(result)),
                    None => Ok(UnboxedValue::Float(*a as f64 * *b as f64)),
                }
            }
            (UnboxedValue::Float(a), UnboxedValue::Float(b)) => Ok(UnboxedValue::Float(a * b)),
            (UnboxedValue::Int(a), UnboxedValue::Float(b)) => Ok(UnboxedValue::Float(*a as f64 * b)),
            (UnboxedValue::Float(a), UnboxedValue::Int(b)) => Ok(UnboxedValue::Float(a * *b as f64)),
            _ => Err("Type error: can only multiply numbers".to_string()),
        }
    }
    
    /// Compare two values for equality
    pub fn eq(&self, other: &UnboxedValue) -> bool {
        match (self, other) {
            (UnboxedValue::Nil, UnboxedValue::Nil) => true,
            (UnboxedValue::Bool(a), UnboxedValue::Bool(b)) => a == b,
            (UnboxedValue::Int(a), UnboxedValue::Int(b)) => a == b,
            (UnboxedValue::Float(a), UnboxedValue::Float(b)) => a == b,
            (UnboxedValue::Int(a), UnboxedValue::Float(b)) => *a as f64 == *b,
            (UnboxedValue::Float(a), UnboxedValue::Int(b)) => *a == *b as f64,
            (UnboxedValue::Boxed(a), UnboxedValue::Boxed(b)) => {
                // For boxed values, we need pointer equality or deep comparison
                // For now, use pointer equality
                std::ptr::eq(a.as_ref(), b.as_ref())
            }
            _ => false,
        }
    }
}