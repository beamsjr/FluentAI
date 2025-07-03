//! Runtime value representation

pub mod error;

pub use error::{ValueError, ValueResult};

use rustc_hash::FxHashMap;
use std::collections::HashMap;
use std::sync::Arc;

/// Runtime value types
#[derive(Clone)]
pub enum Value {
    /// Integer value
    Integer(i64),
    
    /// Floating point value
    Float(f64),
    
    /// String value
    String(String),
    
    /// Symbol value
    Symbol(String),
    
    /// Boolean value
    Boolean(bool),
    
    /// Nil/null value
    Nil,
    
    /// List of values
    List(Vec<Value>),
    
    /// Procedure (closure)
    Procedure(Arc<Procedure>),
    
    /// Vector (mutable array)
    Vector(Vec<Value>),
    
    /// Hash table
    Map(FxHashMap<String, Value>),
    
    /// Native function
    NativeFunction {
        name: String,
        arity: usize,
        function: Arc<dyn Fn(&[Value]) -> Result<Value, ValueError> + Send + Sync>,
    },
    
    /// Tagged value for ADTs
    Tagged {
        tag: String,
        values: Vec<Value>,
    },
}

/// Procedure representation
#[derive(Debug, Clone)]
pub struct Procedure {
    /// Optional function name (for debugging)
    pub name: Option<String>,
    
    /// Parameter names
    pub params: Vec<String>,
    
    /// Function body (AST node ID)
    pub body: crate::ast::NodeId,
    
    /// Captured environment (optional)
    pub env: Option<FxHashMap<String, Value>>,
}

impl Value {
    /// Type checking predicates
    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Integer(_))
    }
    
    pub fn is_float(&self) -> bool {
        matches!(self, Value::Float(_))
    }
    
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_))
    }
    
    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }
    
    pub fn is_symbol(&self) -> bool {
        matches!(self, Value::Symbol(_))
    }
    
    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_))
    }
    
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }
    
    pub fn is_list(&self) -> bool {
        matches!(self, Value::List(_))
    }
    
    pub fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }
    
    pub fn is_map(&self) -> bool {
        matches!(self, Value::Map(_))
    }
    
    pub fn is_procedure(&self) -> bool {
        matches!(self, Value::Procedure(_))
    }
    
    pub fn is_callable(&self) -> bool {
        matches!(self, Value::Procedure(_) | Value::NativeFunction { .. })
    }
    
    pub fn is_tagged(&self) -> bool {
        matches!(self, Value::Tagged { .. })
    }
    
    /// Type conversion helpers
    pub fn as_integer(&self) -> ValueResult<i64> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(ValueError::TypeError {
                expected: "integer",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_float(&self) -> ValueResult<f64> {
        match self {
            Value::Float(f) => Ok(*f),
            _ => Err(ValueError::TypeError {
                expected: "float",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_number(&self) -> ValueResult<f64> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(ValueError::TypeError {
                expected: "number",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_string(&self) -> ValueResult<&str> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(ValueError::TypeError {
                expected: "string",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_symbol(&self) -> ValueResult<&str> {
        match self {
            Value::Symbol(s) => Ok(s),
            _ => Err(ValueError::TypeError {
                expected: "symbol",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_boolean(&self) -> ValueResult<bool> {
        match self {
            Value::Boolean(b) => Ok(*b),
            _ => Err(ValueError::TypeError {
                expected: "boolean",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_list(&self) -> ValueResult<&[Value]> {
        match self {
            Value::List(items) => Ok(items),
            _ => Err(ValueError::TypeError {
                expected: "list",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_vector(&self) -> ValueResult<&[Value]> {
        match self {
            Value::Vector(items) => Ok(items),
            _ => Err(ValueError::TypeError {
                expected: "vector",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_map(&self) -> ValueResult<&FxHashMap<String, Value>> {
        match self {
            Value::Map(map) => Ok(map),
            _ => Err(ValueError::TypeError {
                expected: "map",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_procedure(&self) -> ValueResult<&Procedure> {
        match self {
            Value::Procedure(proc) => Ok(proc),
            _ => Err(ValueError::TypeError {
                expected: "procedure",
                actual: self.type_name(),
            }),
        }
    }
    
    pub fn as_tagged(&self) -> ValueResult<(&str, &[Value])> {
        match self {
            Value::Tagged { tag, values } => Ok((tag, values)),
            _ => Err(ValueError::TypeError {
                expected: "tagged",
                actual: self.type_name(),
            }),
        }
    }
    
    /// Get type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Boolean(_) => "boolean",
            Value::Nil => "nil",
            Value::List(_) => "list",
            Value::Procedure(_) => "procedure",
            Value::Vector(_) => "vector",
            Value::Map(_) => "map",
            Value::NativeFunction { .. } => "native-function",
            Value::Tagged { .. } => "tagged",
        }
    }
    
    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl PartialEq for Procedure {
    fn eq(&self, other: &Self) -> bool {
        // Procedures are equal if they have the same structure
        self.name == other.name &&
        self.params == other.params &&
        self.body == other.body
        // We don't compare environments as they may contain cyclic references
    }
}

impl Value {
    /// Deep equality comparison
    pub fn deep_eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::List(a), Value::List(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.deep_eq(y))
            }
            (Value::Vector(a), Value::Vector(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.deep_eq(y))
            }
            (Value::Map(a), Value::Map(b)) => {
                a.len() == b.len() && 
                a.iter().all(|(k, v)| b.get(k).map_or(false, |v2| v.deep_eq(v2)))
            }
            (Value::Procedure(a), Value::Procedure(b)) => Arc::ptr_eq(a, b) || **a == **b,
            (Value::Tagged { tag: tag_a, values: vals_a }, 
             Value::Tagged { tag: tag_b, values: vals_b }) => {
                tag_a == tag_b && 
                vals_a.len() == vals_b.len() && 
                vals_a.iter().zip(vals_b.iter()).all(|(x, y)| x.deep_eq(y))
            }
            _ => false,
        }
    }
    
    /// Numeric comparison
    pub fn compare_numeric(&self, other: &Value) -> ValueResult<std::cmp::Ordering> {
        use std::cmp::Ordering;
        
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(a.cmp(b)),
            (Value::Float(a), Value::Float(b)) => {
                if a < b {
                    Ok(Ordering::Less)
                } else if a > b {
                    Ok(Ordering::Greater)
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Value::Integer(a), Value::Float(b)) => {
                let a_float = *a as f64;
                if a_float < *b {
                    Ok(Ordering::Less)
                } else if a_float > *b {
                    Ok(Ordering::Greater)
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Value::Float(a), Value::Integer(b)) => {
                let b_float = *b as f64;
                if *a < b_float {
                    Ok(Ordering::Less)
                } else if *a > b_float {
                    Ok(Ordering::Greater)
                } else {
                    Ok(Ordering::Equal)
                }
            }
            _ => Err(ValueError::InvalidOperation(
                format!("Cannot compare {} and {} numerically", self.type_name(), other.type_name())
            )),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Float(x) => write!(f, "Float({})", x),
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Symbol(s) => write!(f, "Symbol({:?})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Nil => write!(f, "Nil"),
            Value::List(items) => f.debug_list().entries(items).finish(),
            Value::Procedure(proc) => f.debug_struct("Procedure").field("proc", proc).finish(),
            Value::Vector(items) => f.debug_struct("Vector").field("items", items).finish(),
            Value::Map(map) => f.debug_struct("Map").field("map", map).finish(),
            Value::NativeFunction { name, arity, .. } => {
                f.debug_struct("NativeFunction")
                    .field("name", name)
                    .field("arity", arity)
                    .finish()
            }
            Value::Tagged { tag, values } => {
                f.debug_struct("Tagged")
                    .field("tag", tag)
                    .field("values", values)
                    .finish()
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Procedure(a), Value::Procedure(b)) => Arc::ptr_eq(a, b),
            (Value::NativeFunction { name: n1, arity: a1, .. }, 
             Value::NativeFunction { name: n2, arity: a2, .. }) => n1 == n2 && a1 == a2,
            (Value::Tagged { tag: t1, values: v1 }, 
             Value::Tagged { tag: t2, values: v2 }) => t1 == t2 && v1 == v2,
            _ => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Value::Nil => write!(f, "nil"),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Procedure(_) => write!(f, "#<procedure>"),
            Value::Vector(items) => {
                write!(f, "#(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Map(_) => write!(f, "#<hashmap>"),
            Value::NativeFunction { name, .. } => write!(f, "#<native:{}>", name),
            Value::Tagged { tag, values } => {
                write!(f, "{}(", tag)?;
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }
        }
    }
}