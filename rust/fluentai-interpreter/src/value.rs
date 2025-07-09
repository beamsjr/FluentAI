//! Runtime value representation for the interpreter

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use fluentai_core::ast::{Literal, NodeId};
use fluentai_types::types::Type;

use crate::environment::Environment;

/// Runtime value representation
#[derive(Debug, Clone)]
pub struct Value {
    /// The actual data
    pub data: ValueData,
    /// Type information if available
    pub type_info: Option<Type>,
    /// Provenance tracking - which nodes produced this value
    pub provenance: Vec<NodeId>,
    /// Timestamp when value was created (for debugging)
    pub timestamp: std::time::Instant,
    /// Effects triggered during evaluation
    pub effects_triggered: Vec<(String, Vec<Value>)>,
}

/// The actual value data
#[derive(Debug, Clone)]
pub enum ValueData {
    /// Nil value
    Nil,
    /// Boolean value
    Boolean(bool),
    /// Integer value
    Integer(i64),
    /// Float value
    Float(f64),
    /// String value
    String(String),
    /// List of values
    List(Vec<Value>),
    /// Map/dictionary
    Map(FxHashMap<String, Value>),
    /// Function closure
    Closure(Rc<Closure>),
    /// Built-in function
    BuiltinFunction {
        name: String,
        arity: usize,
        variadic: bool,
    },
    /// Module value
    Module {
        name: String,
        exports: FxHashMap<String, Value>,
    },
    // /// Promise/Future for async
    // #[cfg(feature = "async")]
    // Promise(Rc<RefCell<PromiseState>>),
    /// Channel for concurrency
    Channel(Rc<RefCell<ChannelState>>),
}

/// Function closure
#[derive(Debug)]
pub struct Closure {
    /// Parameter names
    pub params: Vec<String>,
    /// Body expression node
    pub body: NodeId,
    /// Captured environment
    pub env: Environment,
    /// Whether this is an async function
    pub is_async: bool,
}

// /// Promise state for async operations
// #[cfg(feature = "async")]
// #[derive(Debug, Clone)]
// pub enum PromiseState {
//     Pending,
//     Resolved(Value),
//     Rejected(String),
// }

/// Channel state for concurrent operations
#[derive(Debug)]
pub struct ChannelState {
    /// Channel buffer
    pub buffer: Vec<Value>,
    /// Maximum capacity
    pub capacity: usize,
    /// Whether channel is closed
    pub closed: bool,
}

impl Value {
    /// Create a new value with empty provenance
    pub fn new(data: ValueData) -> Self {
        Self {
            data,
            type_info: None,
            provenance: Vec::new(),
            timestamp: std::time::Instant::now(),
            effects_triggered: Vec::new(),
        }
    }

    /// Create a value from a literal
    pub fn from_literal(lit: &Literal) -> Self {
        let data = match lit {
            Literal::Nil => ValueData::Nil,
            Literal::Boolean(b) => ValueData::Boolean(*b),
            Literal::Integer(i) => ValueData::Integer(*i),
            Literal::Float(f) => ValueData::Float(*f),
            Literal::String(s) => ValueData::String(s.clone()),
        };
        Self::new(data)
    }

    /// Create an integer value
    pub fn from_integer(i: i64) -> Self {
        Self::new(ValueData::Integer(i))
    }

    /// Add provenance information
    pub fn with_provenance(mut self, node_id: NodeId) -> Self {
        self.provenance.push(node_id);
        self
    }

    /// Add type information
    pub fn with_type(mut self, type_info: Type) -> Self {
        self.type_info = Some(type_info);
        self
    }

    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        match &self.data {
            ValueData::Nil => false,
            ValueData::Boolean(b) => *b,
            ValueData::Integer(i) => *i != 0,
            ValueData::Float(f) => *f != 0.0,
            ValueData::String(s) => !s.is_empty(),
            ValueData::List(l) => !l.is_empty(),
            ValueData::Map(m) => !m.is_empty(),
            _ => true,
        }
    }

    /// Convert to boolean
    pub fn to_bool(&self) -> bool {
        self.is_truthy()
    }

    /// Try to convert to integer
    pub fn to_integer(&self) -> Option<i64> {
        match &self.data {
            ValueData::Integer(i) => Some(*i),
            ValueData::Float(f) => Some(*f as i64),
            ValueData::String(s) => s.parse().ok(),
            ValueData::Boolean(true) => Some(1),
            ValueData::Boolean(false) => Some(0),
            _ => None,
        }
    }

    /// Try to convert to float
    pub fn to_float(&self) -> Option<f64> {
        match &self.data {
            ValueData::Float(f) => Some(*f),
            ValueData::Integer(i) => Some(*i as f64),
            ValueData::String(s) => s.parse().ok(),
            ValueData::Boolean(true) => Some(1.0),
            ValueData::Boolean(false) => Some(0.0),
            _ => None,
        }
    }

    /// Convert to string representation
    pub fn to_string(&self) -> String {
        match &self.data {
            ValueData::Nil => "nil".to_string(),
            ValueData::Boolean(b) => b.to_string(),
            ValueData::Integer(i) => i.to_string(),
            ValueData::Float(f) => f.to_string(),
            ValueData::String(s) => s.clone(),
            ValueData::List(items) => {
                let strs: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                format!("[{}]", strs.join(", "))
            }
            ValueData::Map(m) => {
                let pairs: Vec<String> = m
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.to_string()))
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            }
            ValueData::Closure(_) => "<function>".to_string(),
            ValueData::BuiltinFunction { name, .. } => format!("<builtin:{}>", name),
            ValueData::Module { name, .. } => format!("<module:{}>", name),
            // #[cfg(feature = "async")]
            // ValueData::Promise(_) => "<promise>".to_string(),
            ValueData::Channel(_) => "<channel>".to_string(),
        }
    }

    /// Convert to boolean if possible
    pub fn to_boolean(&self) -> Option<bool> {
        match &self.data {
            ValueData::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    /// Convert to list if possible
    pub fn to_list(&self) -> Option<&Vec<Value>> {
        match &self.data {
            ValueData::List(items) => Some(items),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self.data, &other.data) {
            (ValueData::Nil, ValueData::Nil) => true,
            (ValueData::Boolean(a), ValueData::Boolean(b)) => a == b,
            (ValueData::Integer(a), ValueData::Integer(b)) => a == b,
            (ValueData::Float(a), ValueData::Float(b)) => (a - b).abs() < f64::EPSILON,
            (ValueData::String(a), ValueData::String(b)) => a == b,
            (ValueData::List(a), ValueData::List(b)) => a == b,
            (ValueData::Map(a), ValueData::Map(b)) => a == b,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_creation() {
        let val = Value::new(ValueData::Integer(42));
        assert!(matches!(val.data, ValueData::Integer(42)));
    }

    #[test]
    fn test_literal_conversion() {
        let val = Value::from_literal(&Literal::String("hello".to_string()));
        assert_eq!(val.to_string(), "hello");
    }

    #[test]
    fn test_truthiness() {
        assert!(!Value::new(ValueData::Nil).is_truthy());
        assert!(!Value::new(ValueData::Boolean(false)).is_truthy());
        assert!(Value::new(ValueData::Boolean(true)).is_truthy());
        assert!(Value::new(ValueData::Integer(1)).is_truthy());
        assert!(!Value::new(ValueData::Integer(0)).is_truthy());
    }
}
