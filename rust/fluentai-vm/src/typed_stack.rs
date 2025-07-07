//! Type-specialized stack frames for efficient unboxed value storage

use crate::bytecode::Value;
use anyhow::{anyhow, Result};

/// Type tags for stack values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeTag {
    Int,
    Float,
    Bool,
    Nil,
    Boxed,
}

/// A type-specialized stack that tracks types and stores unboxed values efficiently
pub struct TypedStack {
    /// Stack of values - can be either unboxed or boxed
    values: Vec<StackValue>,
    /// Type tags for each value on the stack
    type_tags: Vec<TypeTag>,
    /// Maximum stack size
    max_size: usize,
}

/// Stack value that can be stored efficiently
#[derive(Clone)]
pub enum StackValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
    Boxed(Box<Value>),
}

impl TypedStack {
    /// Create a new typed stack with given capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            type_tags: Vec::with_capacity(capacity),
            max_size: capacity,
        }
    }
    
    /// Push a value onto the stack
    pub fn push(&mut self, value: Value) -> Result<()> {
        if self.values.len() >= self.max_size {
            return Err(anyhow!("Stack overflow"));
        }
        
        match value {
            Value::Int(i) => {
                self.values.push(StackValue::Int(i));
                self.type_tags.push(TypeTag::Int);
            }
            Value::Float(f) => {
                self.values.push(StackValue::Float(f));
                self.type_tags.push(TypeTag::Float);
            }
            Value::Bool(b) => {
                self.values.push(StackValue::Bool(b));
                self.type_tags.push(TypeTag::Bool);
            }
            Value::Nil => {
                self.values.push(StackValue::Nil);
                self.type_tags.push(TypeTag::Nil);
            }
            other => {
                self.values.push(StackValue::Boxed(Box::new(other)));
                self.type_tags.push(TypeTag::Boxed);
            }
        }
        Ok(())
    }
    
    /// Pop a value from the stack
    pub fn pop(&mut self) -> Result<Value> {
        match (self.values.pop(), self.type_tags.pop()) {
            (Some(value), Some(_tag)) => Ok(self.stack_value_to_value(value)),
            _ => Err(anyhow!("Stack underflow")),
        }
    }
    
    /// Push an integer directly (no boxing)
    pub fn push_int(&mut self, value: i64) -> Result<()> {
        if self.values.len() >= self.max_size {
            return Err(anyhow!("Stack overflow"));
        }
        self.values.push(StackValue::Int(value));
        self.type_tags.push(TypeTag::Int);
        Ok(())
    }
    
    /// Push a float directly (no boxing)
    pub fn push_float(&mut self, value: f64) -> Result<()> {
        if self.values.len() >= self.max_size {
            return Err(anyhow!("Stack overflow"));
        }
        self.values.push(StackValue::Float(value));
        self.type_tags.push(TypeTag::Float);
        Ok(())
    }
    
    /// Pop two integers for binary operation
    pub fn pop_int_pair(&mut self) -> Result<(i64, i64)> {
        if self.values.len() < 2 {
            return Err(anyhow!("Stack underflow"));
        }
        
        let b_tag = self.type_tags.pop().unwrap();
        let b_val = self.values.pop().unwrap();
        let a_tag = self.type_tags.pop().unwrap();
        let a_val = self.values.pop().unwrap();
        
        match (a_tag, a_val, b_tag, b_val) {
            (TypeTag::Int, StackValue::Int(a), TypeTag::Int, StackValue::Int(b)) => Ok((a, b)),
            _ => Err(anyhow!("Expected two integers on stack")),
        }
    }
    
    /// Pop two floats for binary operation
    pub fn pop_float_pair(&mut self) -> Result<(f64, f64)> {
        if self.values.len() < 2 {
            return Err(anyhow!("Stack underflow"));
        }
        
        let b_tag = self.type_tags.pop().unwrap();
        let b_val = self.values.pop().unwrap();
        let a_tag = self.type_tags.pop().unwrap();
        let a_val = self.values.pop().unwrap();
        
        match (a_tag, a_val, b_tag, b_val) {
            (TypeTag::Float, StackValue::Float(a), TypeTag::Float, StackValue::Float(b)) => Ok((a, b)),
            _ => Err(anyhow!("Expected two floats on stack")),
        }
    }
    
    /// Peek at the top value without popping
    pub fn peek(&self) -> Option<&StackValue> {
        self.values.last()
    }
    
    /// Get the type tag of the top value
    pub fn peek_type(&self) -> Option<TypeTag> {
        self.type_tags.last().copied()
    }
    
    /// Get current stack depth
    pub fn len(&self) -> usize {
        self.values.len()
    }
    
    /// Check if stack is empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
    
    /// Clear the stack
    pub fn clear(&mut self) {
        self.values.clear();
        self.type_tags.clear();
    }
    
    /// Truncate stack to given size
    pub fn truncate(&mut self, len: usize) {
        self.values.truncate(len);
        self.type_tags.truncate(len);
    }
    
    /// Get value at index (0 is bottom of stack)
    pub fn get(&self, index: usize) -> Option<Value> {
        self.values.get(index).map(|v| self.stack_value_to_value(v.clone()))
    }
    
    /// Set value at index
    pub fn set(&mut self, index: usize, value: Value) -> Result<()> {
        if index >= self.values.len() {
            return Err(anyhow!("Stack index out of bounds"));
        }
        
        match value {
            Value::Int(i) => {
                self.values[index] = StackValue::Int(i);
                self.type_tags[index] = TypeTag::Int;
            }
            Value::Float(f) => {
                self.values[index] = StackValue::Float(f);
                self.type_tags[index] = TypeTag::Float;
            }
            Value::Bool(b) => {
                self.values[index] = StackValue::Bool(b);
                self.type_tags[index] = TypeTag::Bool;
            }
            Value::Nil => {
                self.values[index] = StackValue::Nil;
                self.type_tags[index] = TypeTag::Nil;
            }
            other => {
                self.values[index] = StackValue::Boxed(Box::new(other));
                self.type_tags[index] = TypeTag::Boxed;
            }
        }
        Ok(())
    }
    
    /// Convert stack value to regular value
    fn stack_value_to_value(&self, stack_val: StackValue) -> Value {
        match stack_val {
            StackValue::Int(i) => Value::Int(i),
            StackValue::Float(f) => Value::Float(f),
            StackValue::Bool(b) => Value::Bool(b),
            StackValue::Nil => Value::Nil,
            StackValue::Boxed(boxed) => *boxed,
        }
    }
}

/// Type-specialized operations for better performance
impl TypedStack {
    /// Optimized integer addition
    pub fn add_int(&mut self) -> Result<()> {
        let (a, b) = self.pop_int_pair()?;
        match a.checked_add(b) {
            Some(result) => self.push_int(result),
            None => self.push_float(a as f64 + b as f64),
        }
    }
    
    /// Optimized integer subtraction
    pub fn sub_int(&mut self) -> Result<()> {
        let (a, b) = self.pop_int_pair()?;
        match a.checked_sub(b) {
            Some(result) => self.push_int(result),
            None => self.push_float(a as f64 - b as f64),
        }
    }
    
    /// Optimized integer multiplication
    pub fn mul_int(&mut self) -> Result<()> {
        let (a, b) = self.pop_int_pair()?;
        match a.checked_mul(b) {
            Some(result) => self.push_int(result),
            None => self.push_float(a as f64 * b as f64),
        }
    }
    
    /// Optimized float addition
    pub fn add_float(&mut self) -> Result<()> {
        let (a, b) = self.pop_float_pair()?;
        self.push_float(a + b)
    }
    
    /// Optimized float subtraction
    pub fn sub_float(&mut self) -> Result<()> {
        let (a, b) = self.pop_float_pair()?;
        self.push_float(a - b)
    }
    
    /// Optimized float multiplication  
    pub fn mul_float(&mut self) -> Result<()> {
        let (a, b) = self.pop_float_pair()?;
        self.push_float(a * b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_typed_stack_basic() {
        let mut stack = TypedStack::new(100);
        
        // Test integer operations
        stack.push_int(10).unwrap();
        stack.push_int(20).unwrap();
        stack.add_int().unwrap();
        
        let result = stack.pop().unwrap();
        assert_eq!(result, Value::Int(30));
        
        // Test float operations
        stack.push_float(1.5).unwrap();
        stack.push_float(2.5).unwrap();
        stack.add_float().unwrap();
        
        let result = stack.pop().unwrap();
        assert_eq!(result, Value::Float(4.0));
    }
    
    #[test]
    fn test_overflow_to_float() {
        let mut stack = TypedStack::new(100);
        
        // Test integer overflow converts to float
        stack.push_int(i64::MAX).unwrap();
        stack.push_int(1).unwrap();
        stack.add_int().unwrap();
        
        let result = stack.pop().unwrap();
        match result {
            Value::Float(f) => {
                // i64::MAX + 1 should overflow to float
                let expected = (i64::MAX as f64) + 1.0;
                assert_eq!(f, expected);
            }
            _ => panic!("Expected float after overflow"),
        }
    }
}