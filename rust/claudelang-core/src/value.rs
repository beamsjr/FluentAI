//! Runtime value representation

use crate::ast::NodeId;
use std::fmt;
use std::rc::Rc;

/// Runtime values in ClaudeLang
#[derive(Debug, Clone)]
pub enum Value {
    // Primitive values
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
    
    // Compound values
    List(Vec<Value>),
    Function(Function),
    
    // Special values
    Error(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: NodeId,
    pub env: Environment,
    pub is_builtin: bool,
}

#[derive(Debug, Clone)]
pub struct Environment {
    bindings: Rc<FxHashMap<String, Value>>,
    parent: Option<Rc<Environment>>,
}

use rustc_hash::FxHashMap;

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: Rc::new(FxHashMap::default()),
            parent: None,
        }
    }

    pub fn with_parent(parent: Environment) -> Self {
        Self {
            bindings: Rc::new(FxHashMap::default()),
            parent: Some(Rc::new(parent)),
        }
    }

    pub fn bind(&self, name: String, value: Value) -> Self {
        let mut new_bindings = (*self.bindings).clone();
        new_bindings.insert(name, value);
        Self {
            bindings: Rc::new(new_bindings),
            parent: self.parent.clone(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| {
                p.bindings.get(name)
            })
        })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::List(values) => {
                write!(f, "[")?;
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Function(_) => write!(f, "<function>"),
            Value::Error(msg) => write!(f, "Error: {}", msg),
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "Integer",
            Value::Float(_) => "Float",
            Value::String(_) => "String",
            Value::Boolean(_) => "Boolean",
            Value::Nil => "Nil",
            Value::List(_) => "List",
            Value::Function(_) => "Function",
            Value::Error(_) => "Error",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }
}