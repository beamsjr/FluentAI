//! Enhanced error handling for the VM
//!
//! Provides detailed error types with context and stack traces

use std::fmt;
use crate::bytecode::Value;

/// VM error types with rich context
#[derive(Debug, Clone)]
pub enum VMError {
    /// Stack overflow
    StackOverflow {
        current_depth: usize,
        max_depth: usize,
    },
    
    /// Stack underflow
    StackUnderflow {
        operation: String,
        stack_size: usize,
    },
    
    /// Call stack overflow
    CallStackOverflow {
        current_depth: usize,
        max_depth: usize,
    },
    
    /// Type error
    TypeError {
        operation: String,
        expected: String,
        got: String,
        location: Option<SourceLocation>,
    },
    
    /// Arithmetic error
    ArithmeticError {
        operation: String,
        message: String,
    },
    
    /// Division by zero
    DivisionByZero {
        location: Option<SourceLocation>,
    },
    
    /// Integer overflow
    IntegerOverflow {
        operation: String,
        operands: (i64, i64),
    },
    
    /// Invalid opcode
    InvalidOpcode {
        opcode: u8,
        location: Option<SourceLocation>,
    },
    
    /// Invalid constant index
    InvalidConstantIndex {
        index: u32,
        max_index: usize,
    },
    
    /// Invalid local variable index
    InvalidLocalIndex {
        index: usize,
        frame_size: usize,
    },
    
    /// Invalid jump target
    InvalidJumpTarget {
        target: usize,
        chunk_size: usize,
    },
    
    /// Resource limit exceeded
    ResourceLimitExceeded {
        resource: String,
        limit: usize,
        requested: usize,
    },
    
    /// Module error
    ModuleError {
        module_name: String,
        message: String,
    },
    
    /// Async/channel error
    AsyncError {
        message: String,
    },
    
    /// Cell error
    CellError {
        index: usize,
        message: String,
    },
    
    /// Unknown identifier
    UnknownIdentifier {
        name: String,
        location: Option<SourceLocation>,
    },
    
    /// Runtime error
    RuntimeError {
        message: String,
        stack_trace: Option<StackTrace>,
    },
}

/// Source location information
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: Option<String>,
    pub line: usize,
    pub column: usize,
    pub function: Option<String>,
}

/// Stack frame information
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub chunk_id: usize,
    pub ip: usize,
    pub location: Option<SourceLocation>,
}

/// Stack trace
#[derive(Debug, Clone)]
pub struct StackTrace {
    pub frames: Vec<StackFrame>,
}

impl StackTrace {
    pub fn new() -> Self {
        Self { frames: Vec::new() }
    }
    
    pub fn push_frame(&mut self, frame: StackFrame) {
        self.frames.push(frame);
    }
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VMError::StackOverflow { current_depth, max_depth } => {
                write!(f, "Stack overflow: depth {} exceeds maximum {}", current_depth, max_depth)
            }
            VMError::StackUnderflow { operation, stack_size } => {
                write!(f, "Stack underflow in {}: stack size is {}", operation, stack_size)
            }
            VMError::CallStackOverflow { current_depth, max_depth } => {
                write!(f, "Call stack overflow: depth {} exceeds maximum {}", current_depth, max_depth)
            }
            VMError::TypeError { operation, expected, got, location } => {
                write!(f, "Type error in {}: expected {}, got {}", operation, expected, got)?;
                if let Some(loc) = location {
                    write!(f, " at {}:{}:{}", 
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line, loc.column)?;
                }
                Ok(())
            }
            VMError::ArithmeticError { operation, message } => {
                write!(f, "Arithmetic error in {}: {}", operation, message)
            }
            VMError::DivisionByZero { location } => {
                write!(f, "Division by zero")?;
                if let Some(loc) = location {
                    write!(f, " at {}:{}:{}", 
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line, loc.column)?;
                }
                Ok(())
            }
            VMError::IntegerOverflow { operation, operands } => {
                write!(f, "Integer overflow in {}: {} and {}", operation, operands.0, operands.1)
            }
            VMError::InvalidOpcode { opcode, location } => {
                write!(f, "Invalid opcode: {:#x}", opcode)?;
                if let Some(loc) = location {
                    write!(f, " at {}:{}:{}", 
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line, loc.column)?;
                }
                Ok(())
            }
            VMError::InvalidConstantIndex { index, max_index } => {
                write!(f, "Invalid constant index: {} (max: {})", index, max_index)
            }
            VMError::InvalidLocalIndex { index, frame_size } => {
                write!(f, "Invalid local variable index: {} (frame size: {})", index, frame_size)
            }
            VMError::InvalidJumpTarget { target, chunk_size } => {
                write!(f, "Invalid jump target: {} (chunk size: {})", target, chunk_size)
            }
            VMError::ResourceLimitExceeded { resource, limit, requested } => {
                write!(f, "Resource limit exceeded for {}: requested {}, limit {}", 
                    resource, requested, limit)
            }
            VMError::ModuleError { module_name, message } => {
                write!(f, "Module error in '{}': {}", module_name, message)
            }
            VMError::AsyncError { message } => {
                write!(f, "Async error: {}", message)
            }
            VMError::CellError { index, message } => {
                write!(f, "Cell error at index {}: {}", index, message)
            }
            VMError::UnknownIdentifier { name, location } => {
                write!(f, "Unknown identifier: '{}'", name)?;
                if let Some(loc) = location {
                    write!(f, " at {}:{}:{}", 
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line, loc.column)?;
                }
                Ok(())
            }
            VMError::RuntimeError { message, stack_trace } => {
                write!(f, "Runtime error: {}", message)?;
                if let Some(trace) = stack_trace {
                    write!(f, "\nStack trace:")?;
                    for (i, frame) in trace.frames.iter().enumerate() {
                        write!(f, "\n  {}: {} at chunk {} ip {}", 
                            i, frame.function_name, frame.chunk_id, frame.ip)?;
                        if let Some(loc) = &frame.location {
                            write!(f, " ({}:{}:{})", 
                                loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                                loc.line, loc.column)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for VMError {}

/// Helper to get type name from Value
pub fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Bool(_) => "bool",
        Value::Int(_) => "int",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::List(_) => "list",
        Value::Map(_) => "map",
        Value::Function { .. } => "function",
        Value::Promise(_) => "promise",
        Value::Channel(_) => "channel",
        Value::Cell(_) => "cell",
        Value::Tagged { .. } => "tagged",
        Value::Module { .. } => "module",
    }
}

/// Convert anyhow::Error to VMError
impl From<anyhow::Error> for VMError {
    fn from(err: anyhow::Error) -> Self {
        VMError::RuntimeError {
            message: err.to_string(),
            stack_trace: None,
        }
    }
}

/// Result type for VM operations
pub type VMResult<T> = Result<T, VMError>;