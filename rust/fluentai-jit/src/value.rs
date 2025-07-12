//! Tagged value representation for JIT compilation
//!
//! This module provides a tagged pointer representation for FluentAI values
//! that can be efficiently manipulated in JIT-compiled code.

use fluentai_core::value::Value;
use std::ptr;

/// Tag bits for different value types (using lowest 3 bits)
#[repr(u8)]
pub enum ValueTag {
    Integer = 0b000,  // Integer (shifted left 3)
    Float = 0b001,    // Pointer to heap-allocated f64
    String = 0b010,   // Pointer to heap-allocated String
    List = 0b011,     // Pointer to heap-allocated List
    Closure = 0b100,  // Pointer to heap-allocated Closure
    Symbol = 0b101,   // Symbol ID (shifted left 3)
    Tagged = 0b110,   // Pointer to Tagged value
    Other = 0b111,    // Other heap objects (Boolean, Nil, etc.)
}

const TAG_MASK: u64 = 0b111;
const VALUE_MASK: u64 = !TAG_MASK;

/// A tagged value that can be passed through JIT code as a single u64
#[derive(Clone, Copy, Debug)]
pub struct TaggedValue(pub u64);

impl TaggedValue {
    /// Create a tagged integer value
    pub fn from_integer(n: i64) -> Self {
        // Shift left by 3 bits and set tag to Integer
        TaggedValue(((n as u64) << 3) | ValueTag::Integer as u64)
    }

    /// Create a tagged symbol value
    pub fn from_symbol_id(id: u32) -> Self {
        // Shift left by 3 bits and set tag to Symbol
        TaggedValue(((id as u64) << 3) | ValueTag::Symbol as u64)
    }

    /// Create a tagged pointer to a heap value
    pub fn from_ptr<T>(ptr: *const T, tag: ValueTag) -> Self {
        debug_assert_eq!(ptr as usize & TAG_MASK as usize, 0, "Pointer must be 8-byte aligned");
        TaggedValue((ptr as u64) | tag as u64)
    }

    /// Get the tag of this value
    pub fn tag(&self) -> ValueTag {
        unsafe { std::mem::transmute((self.0 & TAG_MASK) as u8) }
    }

    /// Get the value with tag bits cleared
    pub fn untagged(&self) -> u64 {
        self.0 & VALUE_MASK
    }

    /// Extract an integer value (panics if not an integer)
    pub fn to_integer(&self) -> i64 {
        debug_assert_eq!(self.tag() as u8, ValueTag::Integer as u8);
        (self.untagged() >> 3) as i64
    }

    /// Extract a symbol ID (panics if not a symbol)
    pub fn to_symbol_id(&self) -> u32 {
        debug_assert_eq!(self.tag() as u8, ValueTag::Symbol as u8);
        (self.untagged() >> 3) as u32
    }

    /// Extract a pointer (panics if not a pointer type)
    pub fn to_ptr<T>(&self) -> *const T {
        let tag = self.tag() as u8;
        debug_assert!(tag != ValueTag::Integer as u8 && tag != ValueTag::Symbol as u8);
        self.untagged() as *const T
    }

    /// Convert to a VM Value
    pub fn to_value(&self) -> Value {
        match self.tag() {
            ValueTag::Integer => Value::Integer(self.to_integer()),
            ValueTag::Symbol => {
                // For now, just create a placeholder symbol
                // In real implementation, we'd look up the symbol from an intern table
                Value::Symbol(format!("symbol_{}", self.to_symbol_id()))
            }
            ValueTag::Float => {
                let ptr = self.to_ptr::<f64>();
                unsafe { Value::Float(*ptr) }
            }
            ValueTag::String => {
                let ptr = self.to_ptr::<String>();
                unsafe { Value::String((*ptr).clone()) }
            }
            ValueTag::List => {
                let ptr = self.to_ptr::<Vec<Value>>();
                unsafe { Value::List((*ptr).clone()) }
            }
            ValueTag::Other => {
                // Check the heap object type
                let ptr = self.to_ptr::<HeapValue>();
                unsafe {
                    match &*ptr {
                        HeapValue::Boolean(b) => Value::Boolean(*b),
                        HeapValue::Nil => Value::Nil,
                        HeapValue::Error { kind, message, stack_trace } => Value::Error {
                            kind: kind.clone(),
                            message: message.clone(),
                            stack_trace: stack_trace.clone(),
                        },
                    }
                }
            }
            _ => {
                // For now, return Nil for unimplemented types
                Value::Nil
            }
        }
    }
}

/// Heap-allocated values that don't fit in a tagged pointer
#[derive(Debug)]
pub enum HeapValue {
    Boolean(bool),
    Nil,
    Error { kind: String, message: String, stack_trace: Option<Vec<String>> },
}

/// Convert a VM Value to a tagged value
pub fn value_to_tagged(value: &Value) -> TaggedValue {
    match value {
        Value::Integer(n) => TaggedValue::from_integer(*n),
        Value::Symbol(s) => {
            // For now, use a simple hash as the symbol ID
            // In real implementation, we'd use a proper intern table
            let id = s.bytes().fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32));
            TaggedValue::from_symbol_id(id)
        }
        Value::Float(f) => {
            let boxed = Box::new(*f);
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Float)
        }
        Value::String(s) => {
            let boxed = Box::new(s.clone());
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::String)
        }
        Value::List(l) => {
            let boxed = Box::new(l.clone());
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::List)
        }
        Value::Boolean(b) => {
            let heap_val = Box::new(HeapValue::Boolean(*b));
            TaggedValue::from_ptr(Box::into_raw(heap_val), ValueTag::Other)
        }
        Value::Nil => {
            let heap_val = Box::new(HeapValue::Nil);
            TaggedValue::from_ptr(Box::into_raw(heap_val), ValueTag::Other)
        }
        Value::Error { kind, message, stack_trace } => {
            let heap_val = Box::new(HeapValue::Error {
                kind: kind.clone(),
                message: message.clone(),
                stack_trace: stack_trace.clone(),
            });
            TaggedValue::from_ptr(Box::into_raw(heap_val), ValueTag::Other)
        }
        _ => {
            // For unimplemented types, return a nil value
            let heap_val = Box::new(HeapValue::Nil);
            TaggedValue::from_ptr(Box::into_raw(heap_val), ValueTag::Other)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_tagging() {
        let tagged = TaggedValue::from_integer(42);
        assert_eq!(tagged.tag() as u8, ValueTag::Integer as u8);
        assert_eq!(tagged.to_integer(), 42);
        
        let value = tagged.to_value();
        match value {
            Value::Integer(n) => assert_eq!(n, 42),
            _ => panic!("Expected Integer"),
        }
    }

    #[test]
    fn test_symbol_tagging() {
        let tagged = TaggedValue::from_symbol_id(123);
        assert_eq!(tagged.tag() as u8, ValueTag::Symbol as u8);
        assert_eq!(tagged.to_symbol_id(), 123);
    }

    #[test]
    fn test_float_tagging() {
        let value = Value::Float(3.14);
        let tagged = value_to_tagged(&value);
        assert_eq!(tagged.tag() as u8, ValueTag::Float as u8);
        
        let converted = tagged.to_value();
        match converted {
            Value::Float(f) => assert_eq!(f, 3.14),
            _ => panic!("Expected Float"),
        }
    }
}