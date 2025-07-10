//! Runtime value representation

pub mod error;

pub use error::{ValueError, ValueResult};

use rustc_hash::FxHashMap;
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
    Tagged { tag: String, values: Vec<Value> },

    /// VM function (bytecode chunk with environment)
    Function { chunk_id: usize, env: Vec<Value> },

    /// Promise for async operations
    Promise(u64),

    /// Future (unevaluated async computation)
    Future {
        /// The chunk ID of the async body
        chunk_id: usize,
        /// Captured environment
        env: Vec<Value>,
    },

    /// Channel for concurrent communication
    Channel(u64),
    
    /// Actor for message-passing concurrency
    Actor(u64),
    
    /// Error value
    Error {
        kind: String,
        message: String,
        stack_trace: Option<Vec<String>>,
    },

    /// Mutable cell reference
    Cell(usize),

    /// Module with exports
    Module {
        name: String,
        exports: FxHashMap<String, Value>,
    },

    /// Garbage collected handle (VM internal use)
    GcHandle(Arc<dyn std::any::Any + Send + Sync>),
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
        matches!(
            self,
            Value::Procedure(_) | Value::NativeFunction { .. } | Value::Function { .. }
        )
    }

    pub fn is_tagged(&self) -> bool {
        matches!(self, Value::Tagged { .. })
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function { .. })
    }

    pub fn is_promise(&self) -> bool {
        matches!(self, Value::Promise(_))
    }

    pub fn is_channel(&self) -> bool {
        matches!(self, Value::Channel(_))
    }

    pub fn is_cell(&self) -> bool {
        matches!(self, Value::Cell(_))
    }

    pub fn is_module(&self) -> bool {
        matches!(self, Value::Module { .. })
    }

    pub fn is_gc_handle(&self) -> bool {
        matches!(self, Value::GcHandle(_))
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

    pub fn as_function(&self) -> ValueResult<(usize, &[Value])> {
        match self {
            Value::Function { chunk_id, env } => Ok((*chunk_id, env)),
            _ => Err(ValueError::TypeError {
                expected: "function",
                actual: self.type_name(),
            }),
        }
    }

    pub fn as_promise(&self) -> ValueResult<u64> {
        match self {
            Value::Promise(id) => Ok(*id),
            _ => Err(ValueError::TypeError {
                expected: "promise",
                actual: self.type_name(),
            }),
        }
    }

    pub fn as_channel(&self) -> ValueResult<u64> {
        match self {
            Value::Channel(id) => Ok(*id),
            _ => Err(ValueError::TypeError {
                expected: "channel",
                actual: self.type_name(),
            }),
        }
    }

    pub fn as_cell(&self) -> ValueResult<usize> {
        match self {
            Value::Cell(idx) => Ok(*idx),
            _ => Err(ValueError::TypeError {
                expected: "cell",
                actual: self.type_name(),
            }),
        }
    }

    pub fn as_module(&self) -> ValueResult<(&str, &FxHashMap<String, Value>)> {
        match self {
            Value::Module { name, exports } => Ok((name, exports)),
            _ => Err(ValueError::TypeError {
                expected: "module",
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
            Value::Function { .. } => "function",
            Value::Promise(_) => "promise",
            Value::Future { .. } => "future",
            Value::Channel(_) => "channel",
            Value::Cell(_) => "cell",
            Value::Module { .. } => "module",
            Value::GcHandle(_) => "gc-handle",
            Value::Actor(_) => "actor",
            Value::Error { .. } => "error",
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
        self.name == other.name && self.params == other.params && self.body == other.body
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
                a.len() == b.len()
                    && a.iter()
                        .all(|(k, v)| b.get(k).map_or(false, |v2| v.deep_eq(v2)))
            }
            (Value::Procedure(a), Value::Procedure(b)) => Arc::ptr_eq(a, b) || **a == **b,
            (
                Value::Tagged {
                    tag: tag_a,
                    values: vals_a,
                },
                Value::Tagged {
                    tag: tag_b,
                    values: vals_b,
                },
            ) => {
                tag_a == tag_b
                    && vals_a.len() == vals_b.len()
                    && vals_a.iter().zip(vals_b.iter()).all(|(x, y)| x.deep_eq(y))
            }
            (
                Value::Function {
                    chunk_id: id_a,
                    env: env_a,
                },
                Value::Function {
                    chunk_id: id_b,
                    env: env_b,
                },
            ) => {
                id_a == id_b
                    && env_a.len() == env_b.len()
                    && env_a.iter().zip(env_b.iter()).all(|(x, y)| x.deep_eq(y))
            }
            (Value::Promise(a), Value::Promise(b)) => a == b,
            (Value::Channel(a), Value::Channel(b)) => a == b,
            (Value::Cell(a), Value::Cell(b)) => a == b,
            (
                Value::Module {
                    name: name_a,
                    exports: exp_a,
                },
                Value::Module {
                    name: name_b,
                    exports: exp_b,
                },
            ) => {
                name_a == name_b
                    && exp_a.len() == exp_b.len()
                    && exp_a
                        .iter()
                        .all(|(k, v)| exp_b.get(k).map_or(false, |v2| v.deep_eq(v2)))
            }
            (Value::GcHandle(a), Value::GcHandle(b)) => {
                // GcHandles are compared by Arc pointer equality
                Arc::ptr_eq(a, b)
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
            _ => Err(ValueError::InvalidOperation(format!(
                "Cannot compare {} and {} numerically",
                self.type_name(),
                other.type_name()
            ))),
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
            Value::NativeFunction { name, arity, .. } => f
                .debug_struct("NativeFunction")
                .field("name", name)
                .field("arity", arity)
                .finish(),
            Value::Tagged { tag, values } => f
                .debug_struct("Tagged")
                .field("tag", tag)
                .field("values", values)
                .finish(),
            Value::Function { chunk_id, env } => f
                .debug_struct("Function")
                .field("chunk_id", chunk_id)
                .field("env", env)
                .finish(),
            Value::Promise(id) => write!(f, "Promise({})", id),
            Value::Channel(id) => write!(f, "Channel({})", id),
            Value::Cell(idx) => write!(f, "Cell({})", idx),
            Value::Module { name, exports } => f
                .debug_struct("Module")
                .field("name", name)
                .field("exports", exports)
                .finish(),
            Value::GcHandle(_) => write!(f, "GcHandle(..)"),
            Value::Actor(id) => write!(f, "Actor({})", id),
            Value::Error { kind, message, .. } => write!(f, "Error({}: {})", kind, message),
            Value::Future { chunk_id, env } => f
                .debug_struct("Future")
                .field("chunk_id", chunk_id)
                .field("env", env)
                .finish(),
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
            (
                Value::NativeFunction {
                    name: n1,
                    arity: a1,
                    ..
                },
                Value::NativeFunction {
                    name: n2,
                    arity: a2,
                    ..
                },
            ) => n1 == n2 && a1 == a2,
            (
                Value::Tagged {
                    tag: t1,
                    values: v1,
                },
                Value::Tagged {
                    tag: t2,
                    values: v2,
                },
            ) => t1 == t2 && v1 == v2,
            (
                Value::Function {
                    chunk_id: id1,
                    env: env1,
                },
                Value::Function {
                    chunk_id: id2,
                    env: env2,
                },
            ) => id1 == id2 && env1 == env2,
            (Value::Promise(a), Value::Promise(b)) => a == b,
            (
                Value::Future {
                    chunk_id: id1,
                    env: env1,
                },
                Value::Future {
                    chunk_id: id2,
                    env: env2,
                },
            ) => id1 == id2 && env1 == env2,
            (Value::Channel(a), Value::Channel(b)) => a == b,
            (Value::Cell(a), Value::Cell(b)) => a == b,
            (
                Value::Module {
                    name: n1,
                    exports: e1,
                },
                Value::Module {
                    name: n2,
                    exports: e2,
                },
            ) => n1 == n2 && e1 == e2,
            (Value::GcHandle(a), Value::GcHandle(b)) => Arc::ptr_eq(a, b),
            (Value::Actor(a), Value::Actor(b)) => a == b,
            (
                Value::Error {
                    kind: k1,
                    message: m1,
                    ..
                },
                Value::Error {
                    kind: k2,
                    message: m2,
                    ..
                },
            ) => k1 == k2 && m1 == m2,
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
            Value::Function { .. } => write!(f, "#<function>"),
            Value::Promise(id) => write!(f, "#<promise:{}>", id),
            Value::Future { .. } => write!(f, "#<future>"),
            Value::Channel(id) => write!(f, "#<channel:{}>", id),
            Value::Cell(idx) => write!(f, "#<cell:{}>", idx),
            Value::Module { name, exports } => {
                write!(f, "#<module {} with {} exports>", name, exports.len())
            }
            Value::GcHandle(_) => write!(f, "#<gc-handle>"),
            Value::Actor(id) => write!(f, "#<actor:{}>", id),
            Value::Error { kind, message, .. } => write!(f, "#<error {} \"{}\">", kind, message),
        }
    }
}

#[cfg(test)]
#[path = "../value_tests.rs"]
mod tests;
