//! Temporary Value type definition to avoid circular dependency
//! TODO: Move this to a shared crate that both VM and stdlib can depend on

use rustc_hash::FxHashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Value>),
    Map(FxHashMap<String, Value>),
    Function {
        chunk_id: usize,
        env: Vec<Value>,
    },
    Promise(u64), // Promise ID (numeric for compatibility)
    Channel(u64), // Channel ID (numeric for compatibility)
    Cell(usize),     // Index into VM's cell storage
    Tagged {
        tag: String,
        values: Vec<Value>,
    },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in map.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", k, v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Function { .. } => write!(f, "<function>"),
            Value::Promise(id) => write!(f, "<promise:{}>", id),
            Value::Channel(id) => write!(f, "<channel:{}>", id),
            Value::Cell(idx) => write!(f, "<cell:{}>", idx),
            Value::Tagged { tag, values } => {
                write!(f, "{}", tag)?;
                if !values.is_empty() {
                    write!(f, "(")?;
                    for (i, val) in values.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", val)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}