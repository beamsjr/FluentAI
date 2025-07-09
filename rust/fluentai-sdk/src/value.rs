//! Value conversion traits

use fluentai_core::value::Value;
use std::sync::Arc;
use crate::error::{Error, Result};

/// Trait for converting Rust values to FluentAI values
pub trait IntoValue {
    fn into_value(self) -> Result<Value>;
}

/// Trait for converting FluentAI values to Rust values
pub trait FromValue: Sized {
    fn from_value(value: &Value) -> Result<Self>;
}

// IntoValue implementations

impl IntoValue for Value {
    fn into_value(self) -> Result<Value> {
        Ok(self)
    }
}

impl IntoValue for bool {
    fn into_value(self) -> Result<Value> {
        Ok(Value::Boolean(self))
    }
}

impl IntoValue for i32 {
    fn into_value(self) -> Result<Value> {
        Ok(Value::Integer(self as i64))
    }
}

impl IntoValue for i64 {
    fn into_value(self) -> Result<Value> {
        Ok(Value::Integer(self))
    }
}

impl IntoValue for f32 {
    fn into_value(self) -> Result<Value> {
        Ok(Value::Float(self as f64))
    }
}

impl IntoValue for f64 {
    fn into_value(self) -> Result<Value> {
        Ok(Value::Float(self))
    }
}

impl IntoValue for String {
    fn into_value(self) -> Result<Value> {
        Ok(Value::String(self))
    }
}

impl IntoValue for &str {
    fn into_value(self) -> Result<Value> {
        Ok(Value::String(self.to_string()))
    }
}

impl<T: IntoValue> IntoValue for Vec<T> {
    fn into_value(self) -> Result<Value> {
        let values: Result<Vec<Value>> = self.into_iter()
            .map(|v| v.into_value())
            .collect();
        Ok(Value::List(values?))
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(self) -> Result<Value> {
        match self {
            Some(v) => v.into_value(),
            None => Ok(Value::Nil),
        }
    }
}

// FromValue implementations

impl FromValue for bool {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::Boolean(b) => Ok(*b),
            Value::Nil => Ok(false),
            _ => Err(Error::type_error(format!(
                "Expected boolean, got {:?}", value
            ))),
        }
    }
}

impl FromValue for i32 {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::Integer(n) => Ok(*n as i32),
            Value::Float(n) => Ok(*n as i32),
            _ => Err(Error::type_error(format!(
                "Expected number, got {:?}", value
            ))),
        }
    }
}

impl FromValue for i64 {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::Integer(n) => Ok(*n),
            Value::Float(n) => Ok(*n as i64),
            _ => Err(Error::type_error(format!(
                "Expected number, got {:?}", value
            ))),
        }
    }
}

impl FromValue for f32 {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::Float(n) => Ok(*n as f32),
            Value::Integer(n) => Ok(*n as f32),
            _ => Err(Error::type_error(format!(
                "Expected number, got {:?}", value
            ))),
        }
    }
}

impl FromValue for f64 {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::Float(n) => Ok(*n),
            Value::Integer(n) => Ok(*n as f64),
            _ => Err(Error::type_error(format!(
                "Expected number, got {:?}", value
            ))),
        }
    }
}

impl FromValue for String {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::String(s) => Ok(s.clone()),
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(Error::type_error(format!(
                "Expected string, got {:?}", value
            ))),
        }
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::List(list) => {
                list.iter()
                    .map(|v| T::from_value(v))
                    .collect()
            }
            _ => Err(Error::type_error(format!(
                "Expected list, got {:?}", value
            ))),
        }
    }
}

impl<T: FromValue> FromValue for Option<T> {
    fn from_value(value: &Value) -> Result<Self> {
        match value {
            Value::Nil => Ok(None),
            _ => Ok(Some(T::from_value(value)?)),
        }
    }
}

/// Helper function to convert multiple arguments
pub fn from_values<T: FromValue>(values: &[Value]) -> Result<Vec<T>> {
    values.iter().map(|v| T::from_value(v)).collect()
}

/// Helper function to convert a single argument
pub fn from_value<T: FromValue>(value: &Value) -> Result<T> {
    T::from_value(value)
}

/// Helper function to convert to value
pub fn to_value<T: IntoValue>(value: T) -> Result<Value> {
    value.into_value()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_into_value() {
        assert_eq!(42i32.into_value().unwrap(), Value::Integer(42));
        assert_eq!(true.into_value().unwrap(), Value::Boolean(true));
        assert_eq!("hello".into_value().unwrap(), Value::String("hello".to_string()));
        
        let list = vec![1, 2, 3].into_value().unwrap();
        match list {
            Value::List(l) => {
                assert_eq!(l.len(), 3);
                assert_eq!(l[0], Value::Integer(1));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_from_value() {
        let n = Value::Float(42.0);
        assert_eq!(i32::from_value(&n).unwrap(), 42);
        assert_eq!(f64::from_value(&n).unwrap(), 42.0);
        
        let s = Value::String("hello".to_string());
        assert_eq!(String::from_value(&s).unwrap(), "hello");
        
        let list = Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let vec: Vec<i32> = Vec::from_value(&list).unwrap();
        assert_eq!(vec, vec![1, 2, 3]);
    }
}