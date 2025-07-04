//! Debug event system for VM visualization

use serde::{Deserialize, Serialize};
use tokio::sync::mpsc;
use fluentai_vm::bytecode::{Instruction, Value};

/// Debug events emitted by the VM during execution
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DebugEvent {
    /// VM started execution
    Started {
        timestamp: u64,
    },
    
    /// VM stopped execution
    Stopped {
        timestamp: u64,
        reason: StopReason,
    },
    
    /// About to execute an instruction
    InstructionPre {
        timestamp: u64,
        pc: usize,
        instruction: InstructionInfo,
        stack_size: usize,
    },
    
    /// Finished executing an instruction
    InstructionPost {
        timestamp: u64,
        pc: usize,
        stack_size: usize,
        stack_top: Option<String>, // Serialized top value
    },
    
    /// Stack operation
    StackPush {
        timestamp: u64,
        value: String, // Serialized value
        stack_size: usize,
    },
    
    StackPop {
        timestamp: u64,
        value: String, // Serialized value
        stack_size: usize,
    },
    
    /// Function call
    FunctionCall {
        timestamp: u64,
        name: Option<String>,
        arg_count: usize,
        call_depth: usize,
    },
    
    FunctionReturn {
        timestamp: u64,
        value: String, // Serialized return value
        call_depth: usize,
    },
    
    /// Variable binding
    VariableBind {
        timestamp: u64,
        name: String,
        value: String, // Serialized value
        scope: usize,
    },
    
    /// Error occurred
    Error {
        timestamp: u64,
        message: String,
        pc: Option<usize>,
    },
    
    /// Breakpoint hit
    BreakpointHit {
        timestamp: u64,
        pc: usize,
        breakpoint_id: usize,
    },
}

/// Reason for VM stopping
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StopReason {
    Completed,
    Error(String),
    Breakpoint,
    UserInterrupt,
}

/// Serializable instruction information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstructionInfo {
    pub opcode: String,
    pub arg: Option<u32>,
}

impl From<&Instruction> for InstructionInfo {
    fn from(inst: &Instruction) -> Self {
        Self {
            opcode: format!("{:?}", inst.opcode),
            arg: if inst.arg != 0 { Some(inst.arg) } else { None },
        }
    }
}

/// Channel type for sending debug events
pub type DebugEventSender = mpsc::UnboundedSender<DebugEvent>;
pub type DebugEventReceiver = mpsc::UnboundedReceiver<DebugEvent>;

/// Create a debug event channel
pub fn debug_channel() -> (DebugEventSender, DebugEventReceiver) {
    mpsc::unbounded_channel()
}

/// Helper to serialize values for debug events
pub fn serialize_value(value: &Value) -> String {
    match value {
        Value::Nil => "nil".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::List(items) => {
            let items_str: Vec<String> = items.iter()
                .take(5) // Limit to first 5 items
                .map(serialize_value)
                .collect();
            let suffix = if items.len() > 5 { ", ..." } else { "" };
            format!("[{}{}]", items_str.join(", "), suffix)
        }
        Value::Function { chunk_id, .. } => {
            format!("<function:chunk_{}>", chunk_id)
        }
        Value::Tagged { tag, values } => {
            let values_str: Vec<String> = values.iter()
                .map(serialize_value)
                .collect();
            format!("{}({})", tag, values_str.join(", "))
        }
        Value::Cell(_) => "<cell>".to_string(),
        Value::Module { name, .. } => format!("<module:{}>", name),
        Value::Map(map) => {
            let items: Vec<String> = map.iter()
                .take(3)
                .map(|(k, v)| format!("{}: {}", k, serialize_value(v)))
                .collect();
            let suffix = if map.len() > 3 { ", ..." } else { "" };
            format!("{{{}{}}}", items.join(", "), suffix)
        }
        Value::Promise(id) => format!("<promise:{}>", id),
        Value::Channel(id) => format!("<channel:{}>", id),
    }
}

/// Get current timestamp in microseconds
pub fn timestamp_micros() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_micros() as u64
}