//! Tagged value representation for JIT compilation
//!
//! This module provides a tagged pointer representation for FluentAI values
//! that can be efficiently manipulated in JIT-compiled code.

use fluentai_core::value::Value;

/// Tag bits for different value types (using lowest 3 bits)
#[repr(u8)]
#[derive(Debug)]
pub enum ValueTag {
    /// Integer value (stored inline, shifted left 3 bits)
    Integer = 0b000,
    /// Pointer to heap-allocated f64
    Float = 0b001,
    /// Pointer to heap-allocated String
    String = 0b010,
    /// Pointer to heap-allocated List
    List = 0b011,
    /// Pointer to heap-allocated Closure
    Closure = 0b100,
    /// Symbol ID (stored inline, shifted left 3 bits)
    Symbol = 0b101,
    /// Pointer to Tagged value
    Tagged = 0b110,
    /// Other heap objects (Boolean, Nil, Error, etc.)
    Other = 0b111,
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
        // Use arithmetic shift to preserve sign
        ((self.0 as i64) >> 3)
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
                // Look up the symbol from the intern table
                let symbol_id = self.to_symbol_id();
                match crate::symbol_table::lookup(symbol_id) {
                    Some(symbol) => Value::Symbol(symbol),
                    None => Value::Symbol(format!("unknown_symbol_{}", symbol_id))
                }
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
                // For Other tag, we store the Value directly
                let ptr = self.to_ptr::<Value>();
                unsafe {
                    (*ptr).clone()
                }
            }
            ValueTag::Closure => {
                // Closure tag is reserved but currently we use Other tag for closures
                // If we encounter this, treat it as Other
                let ptr = self.to_ptr::<Value>();
                unsafe {
                    (*ptr).clone()
                }
            }
            ValueTag::Tagged => {
                // Tagged tag is reserved but currently we use Other tag for tagged values
                // If we encounter this, treat it as Other
                let ptr = self.to_ptr::<Value>();
                unsafe {
                    (*ptr).clone()
                }
            }
        }
    }
}

/// Heap-allocated values that don't fit in a tagged pointer
#[derive(Debug)]
pub enum HeapValue {
    /// Boolean value
    Boolean(bool),
    /// Nil value
    Nil,
    /// Error value with details
    Error { 
        /// Error type/kind
        kind: String, 
        /// Error message
        message: String, 
        /// Optional stack trace
        stack_trace: Option<Vec<String>> 
    },
}

/// Convert a VM Value to a tagged value
pub fn value_to_tagged(value: &Value) -> TaggedValue {
    match value {
        Value::Integer(n) => TaggedValue::from_integer(*n),
        Value::Symbol(s) => {
            // Use the proper intern table to get/create symbol ID
            let id = crate::symbol_table::intern(s);
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
            let boxed = Box::new(Value::Boolean(*b));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Nil => {
            let boxed = Box::new(Value::Nil);
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Error { kind, message, stack_trace } => {
            let boxed = Box::new(Value::Error {
                kind: kind.clone(),
                message: message.clone(),
                stack_trace: stack_trace.clone(),
            });
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Map(m) => {
            // Box the map and tag it as Other (since we don't have a specific Map tag)
            let boxed = Box::new(Value::Map(m.clone()));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Vector(v) => {
            // Box the vector and tag it as Other
            let boxed = Box::new(Value::Vector(v.clone()));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Function { chunk_id, env } => {
            // Box the function value
            let boxed = Box::new(Value::Function { chunk_id: *chunk_id, env: env.clone() });
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Channel(id) => {
            // Box the channel value
            let boxed = Box::new(Value::Channel(*id));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Promise(id) => {
            // Box the promise value
            let boxed = Box::new(Value::Promise(*id));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::NativeFunction { name, arity, function } => {
            // Box the native function value
            let boxed = Box::new(Value::NativeFunction { 
                name: name.clone(), 
                arity: *arity, 
                function: function.clone() 
            });
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Procedure(proc) => {
            // Box the procedure value
            let boxed = Box::new(Value::Procedure(proc.clone()));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Tagged { tag, values } => {
            // Box the tagged value
            let boxed = Box::new(Value::Tagged { 
                tag: tag.clone(), 
                values: values.clone() 
            });
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Future { chunk_id, env } => {
            // Box the future value
            let boxed = Box::new(Value::Future { 
                chunk_id: *chunk_id, 
                env: env.clone() 
            });
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Actor(id) => {
            // Box the actor value
            let boxed = Box::new(Value::Actor(*id));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Cell(id) => {
            // Box the cell value
            let boxed = Box::new(Value::Cell(*id));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Module { name, exports } => {
            // Box the module value
            let boxed = Box::new(Value::Module { 
                name: name.clone(), 
                exports: exports.clone() 
            });
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::GcHandle(handle) => {
            // Box the GC handle value
            let boxed = Box::new(Value::GcHandle(handle.clone()));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
        }
        Value::Set(items) => {
            // Box the set value
            let boxed = Box::new(Value::Set(items.clone()));
            TaggedValue::from_ptr(Box::into_raw(boxed), ValueTag::Other)
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

    #[test]
    fn test_all_value_types_conversion() {
        use std::sync::Arc;
        use fluentai_core::value::Procedure;
        use rustc_hash::FxHashMap;
        
        // Test all value types can be converted
        let test_values = vec![
            Value::Integer(42),
            Value::Float(3.14),
            Value::String("hello".to_string()),
            Value::Symbol("test_symbol".to_string()),
            Value::Boolean(true),
            Value::Nil,
            Value::List(vec![Value::Integer(1), Value::Integer(2)]),
            Value::Vector(vec![Value::Integer(3), Value::Integer(4)]),
            Value::Map({
                let mut map = FxHashMap::default();
                map.insert("key".to_string(), Value::Integer(5));
                map
            }),
            Value::Tagged { tag: "Point".to_string(), values: vec![Value::Integer(10), Value::Integer(20)] },
            Value::Function { chunk_id: 1, env: vec![] },
            Value::Future { chunk_id: 2, env: vec![Value::Integer(99)] },
            Value::Promise(123),
            Value::Channel(456),
            Value::Actor(789),
            Value::Cell(101),
            Value::Module { 
                name: "TestModule".to_string(), 
                exports: FxHashMap::default() 
            },
            Value::Error { 
                kind: "TestError".to_string(), 
                message: "Test message".to_string(), 
                stack_trace: None 
            },
            Value::NativeFunction {
                name: "test_func".to_string(),
                arity: 2,
                function: Arc::new(|_| Ok(Value::Nil)),
            },
            Value::GcHandle(Arc::new(42u64)),
            Value::Set(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]),
        ];
        
        for value in test_values {
            let tagged = value_to_tagged(&value);
            let converted = tagged.to_value();
            
            // For most types, we can use PartialEq
            match (&value, &converted) {
                (Value::Integer(a), Value::Integer(b)) => assert_eq!(a, b),
                (Value::Float(a), Value::Float(b)) => assert_eq!(a, b),
                (Value::String(a), Value::String(b)) => assert_eq!(a, b),
                (Value::Symbol(a), Value::Symbol(b)) => assert_eq!(a, b),
                (Value::Boolean(a), Value::Boolean(b)) => assert_eq!(a, b),
                (Value::Nil, Value::Nil) => {},
                (Value::List(a), Value::List(b)) => assert_eq!(a, b),
                (Value::Vector(a), Value::Vector(b)) => assert_eq!(a, b),
                (Value::Map(a), Value::Map(b)) => assert_eq!(a, b),
                (Value::Tagged { tag: t1, values: v1 }, Value::Tagged { tag: t2, values: v2 }) => {
                    assert_eq!(t1, t2);
                    assert_eq!(v1, v2);
                },
                (Value::Function { chunk_id: c1, env: e1 }, Value::Function { chunk_id: c2, env: e2 }) => {
                    assert_eq!(c1, c2);
                    assert_eq!(e1, e2);
                },
                (Value::Future { chunk_id: c1, env: e1 }, Value::Future { chunk_id: c2, env: e2 }) => {
                    assert_eq!(c1, c2);
                    assert_eq!(e1, e2);
                },
                (Value::Promise(a), Value::Promise(b)) => assert_eq!(a, b),
                (Value::Channel(a), Value::Channel(b)) => assert_eq!(a, b),
                (Value::Actor(a), Value::Actor(b)) => assert_eq!(a, b),
                (Value::Cell(a), Value::Cell(b)) => assert_eq!(a, b),
                (Value::Module { name: n1, exports: e1 }, Value::Module { name: n2, exports: e2 }) => {
                    assert_eq!(n1, n2);
                    assert_eq!(e1, e2);
                },
                (Value::Error { kind: k1, message: m1, stack_trace: s1 }, 
                 Value::Error { kind: k2, message: m2, stack_trace: s2 }) => {
                    assert_eq!(k1, k2);
                    assert_eq!(m1, m2);
                    assert_eq!(s1, s2);
                },
                (Value::NativeFunction { name: n1, arity: a1, .. }, 
                 Value::NativeFunction { name: n2, arity: a2, .. }) => {
                    assert_eq!(n1, n2);
                    assert_eq!(a1, a2);
                    // Can't compare function pointers
                },
                (Value::GcHandle(_), Value::GcHandle(_)) => {
                    // Can't compare Any trait objects
                },
                (Value::Set(a), Value::Set(b)) => {
                    assert_eq!(a.len(), b.len());
                    for item in a {
                        assert!(b.contains(item), "Set missing item: {:?}", item);
                    }
                },
                _ => panic!("Value type mismatch: {:?} != {:?}", value, converted),
            }
        }
    }
}