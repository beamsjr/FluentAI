//! Enhanced error handling for the VM
//!
//! Provides detailed error types with context and stack traces

use fluentai_core::value::Value;
use std::fmt;

/// VM error types with rich context
#[derive(Debug, Clone)]
pub enum VMError {
    /// Stack overflow
    StackOverflow {
        current_depth: usize,
        max_depth: usize,
        stack_trace: Option<StackTrace>,
    },

    /// Stack underflow
    StackUnderflow {
        operation: String,
        stack_size: usize,
        stack_trace: Option<StackTrace>,
    },

    /// Call stack overflow
    CallStackOverflow {
        current_depth: usize,
        max_depth: usize,
        stack_trace: Option<StackTrace>,
    },

    /// Type error
    TypeError {
        operation: String,
        expected: String,
        got: String,
        location: Option<SourceLocation>,
        stack_trace: Option<StackTrace>,
    },

    /// Arithmetic error
    ArithmeticError { operation: String, message: String },

    /// Division by zero
    DivisionByZero {
        location: Option<SourceLocation>,
        stack_trace: Option<StackTrace>,
    },

    /// Integer overflow
    IntegerOverflow {
        operation: String,
        operands: (i64, i64),
        stack_trace: Option<StackTrace>,
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
        stack_trace: Option<StackTrace>,
    },

    /// Invalid local variable index
    InvalidLocalIndex {
        index: usize,
        frame_size: usize,
        stack_trace: Option<StackTrace>,
    },

    /// Invalid jump target
    InvalidJumpTarget {
        target: usize,
        chunk_size: usize,
        stack_trace: Option<StackTrace>,
    },

    /// Resource limit exceeded
    ResourceLimitExceeded {
        resource: String,
        limit: usize,
        requested: usize,
        stack_trace: Option<StackTrace>,
    },

    /// Module error
    ModuleError {
        module_name: String,
        message: String,
        stack_trace: Option<StackTrace>,
    },

    /// Async/channel error
    AsyncError {
        message: String,
        stack_trace: Option<StackTrace>,
    },

    /// Cell error
    CellError {
        index: usize,
        message: String,
        stack_trace: Option<StackTrace>,
    },

    /// Unknown identifier
    UnknownIdentifier {
        name: String,
        location: Option<SourceLocation>,
        stack_trace: Option<StackTrace>,
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

/// Helper function to format stack traces
fn format_stack_trace(f: &mut fmt::Formatter<'_>, trace: &Option<StackTrace>) -> fmt::Result {
    if let Some(trace) = trace {
        write!(f, "\nStack trace:")?;
        for (i, frame) in trace.frames.iter().enumerate() {
            write!(
                f,
                "\n  {}: {} at chunk {} ip {}",
                i, frame.function_name, frame.chunk_id, frame.ip
            )?;
            if let Some(loc) = &frame.location {
                write!(
                    f,
                    " ({}:{}:{})",
                    loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                    loc.line,
                    loc.column
                )?;
            }
        }
    }
    Ok(())
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VMError::StackOverflow {
                current_depth,
                max_depth,
                stack_trace,
            } => {
                write!(
                    f,
                    "Stack overflow: depth {} exceeds maximum {}",
                    current_depth, max_depth
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::StackUnderflow {
                operation,
                stack_size,
                stack_trace,
            } => {
                write!(
                    f,
                    "Stack underflow in {}: stack size is {}",
                    operation, stack_size
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::CallStackOverflow {
                current_depth,
                max_depth,
                stack_trace,
            } => {
                write!(
                    f,
                    "Call stack overflow: depth {} exceeds maximum {}",
                    current_depth, max_depth
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::TypeError {
                operation,
                expected,
                got,
                location,
                stack_trace,
            } => {
                write!(
                    f,
                    "Type error in {}: expected {}, got {}",
                    operation, expected, got
                )?;
                if let Some(loc) = location {
                    write!(
                        f,
                        " at {}:{}:{}",
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line,
                        loc.column
                    )?;
                }
                format_stack_trace(f, stack_trace)
            }
            VMError::ArithmeticError { operation, message } => {
                write!(f, "Arithmetic error in {}: {}", operation, message)
            }
            VMError::DivisionByZero {
                location,
                stack_trace,
            } => {
                write!(f, "Division by zero")?;
                if let Some(loc) = location {
                    write!(
                        f,
                        " at {}:{}:{}",
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line,
                        loc.column
                    )?;
                }
                format_stack_trace(f, stack_trace)
            }
            VMError::IntegerOverflow {
                operation,
                operands,
                stack_trace,
            } => {
                write!(
                    f,
                    "Integer overflow in {}: {} and {}",
                    operation, operands.0, operands.1
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::InvalidOpcode { opcode, location } => {
                write!(f, "Invalid opcode: {:#x}", opcode)?;
                if let Some(loc) = location {
                    write!(
                        f,
                        " at {}:{}:{}",
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line,
                        loc.column
                    )?;
                }
                Ok(())
            }
            VMError::InvalidConstantIndex {
                index,
                max_index,
                stack_trace,
            } => {
                write!(f, "Invalid constant index: {} (max: {})", index, max_index)?;
                format_stack_trace(f, stack_trace)
            }
            VMError::InvalidLocalIndex {
                index,
                frame_size,
                stack_trace,
            } => {
                write!(
                    f,
                    "Invalid local variable index: {} (frame size: {})",
                    index, frame_size
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::InvalidJumpTarget {
                target,
                chunk_size,
                stack_trace,
            } => {
                write!(
                    f,
                    "Invalid jump target: {} (chunk size: {})",
                    target, chunk_size
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::ResourceLimitExceeded {
                resource,
                limit,
                requested,
                stack_trace,
            } => {
                write!(
                    f,
                    "Resource limit exceeded for {}: requested {}, limit {}",
                    resource, requested, limit
                )?;
                format_stack_trace(f, stack_trace)
            }
            VMError::ModuleError {
                module_name,
                message,
                stack_trace,
            } => {
                write!(f, "Module error in '{}': {}", module_name, message)?;
                format_stack_trace(f, stack_trace)
            }
            VMError::AsyncError {
                message,
                stack_trace,
            } => {
                write!(f, "Async error: {}", message)?;
                format_stack_trace(f, stack_trace)
            }
            VMError::CellError {
                index,
                message,
                stack_trace,
            } => {
                write!(f, "Cell error at index {}: {}", index, message)?;
                format_stack_trace(f, stack_trace)
            }
            VMError::UnknownIdentifier {
                name,
                location,
                stack_trace,
            } => {
                write!(f, "Unknown identifier: '{}'", name)?;
                if let Some(loc) = location {
                    write!(
                        f,
                        " at {}:{}:{}",
                        loc.file.as_ref().unwrap_or(&"<unknown>".to_string()),
                        loc.line,
                        loc.column
                    )?;
                }
                format_stack_trace(f, stack_trace)
            }
            VMError::RuntimeError {
                message,
                stack_trace,
            } => {
                write!(f, "Runtime error: {}", message)?;
                format_stack_trace(f, stack_trace)
            }
        }
    }
}

impl std::error::Error for VMError {}

/// Helper to get type name from Value
pub fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Boolean(_) => "bool",
        Value::Integer(_) => "int",
        Value::Float(_) => "float",
        Value::String(_) => "string",
        Value::Symbol(_) => "symbol",
        Value::List(_) => "list",
        Value::Procedure(_) => "procedure",
        Value::Vector(_) => "vector",
        Value::Map(_) => "map",
        Value::NativeFunction { .. } => "native-function",
        Value::Function { .. } => "function",
        Value::Promise(_) => "promise",
        Value::Channel(_) => "channel",
        Value::Cell(_) => "cell",
        Value::Tagged { .. } => "tagged",
        Value::Module { .. } => "module",
        Value::GcHandle(_) => "gc-handle",
        Value::Actor(_) => "actor",
        Value::Error { .. } => "error",
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
