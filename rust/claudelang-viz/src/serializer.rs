//! Serialization of VM state for visualization

use serde::{Deserialize, Serialize};
use claudelang_vm::VM;
use std::collections::HashMap;

/// Snapshot of VM state at a point in time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMSnapshot {
    /// Current program counter
    pub pc: usize,
    
    /// Current chunk being executed
    pub chunk_id: usize,
    
    /// Stack contents
    pub stack: Vec<String>,
    
    /// Call stack
    pub call_stack: Vec<CallFrame>,
    
    /// Local variables in current scope
    pub locals: HashMap<String, String>,
    
    /// Global variables
    pub globals: HashMap<String, String>,
    
    /// Current instruction
    pub current_instruction: Option<String>,
    
    /// Statistics
    pub stats: VMStats,
}

/// Call frame information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallFrame {
    pub function_name: Option<String>,
    pub pc: usize,
    pub chunk_id: usize,
    pub local_count: usize,
}

/// VM execution statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMStats {
    pub instructions_executed: u64,
    pub max_stack_depth: usize,
    pub function_calls: u64,
    pub allocations: u64,
}

/// Create a snapshot of the current VM state
pub fn snapshot_vm(_vm: &VM) -> VMSnapshot {
    // This is a placeholder - actual implementation would need
    // access to VM internals which may require modifications to the VM
    VMSnapshot {
        pc: 0,
        chunk_id: 0,
        stack: vec![],
        call_stack: vec![],
        locals: HashMap::new(),
        globals: HashMap::new(),
        current_instruction: None,
        stats: VMStats {
            instructions_executed: 0,
            max_stack_depth: 0,
            function_calls: 0,
            allocations: 0,
        },
    }
}

/// Message types for WebSocket communication
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum VisualizationMessage {
    /// Initial connection established
    Connected {
        session_id: String,
    },
    
    /// AST graph data
    ASTGraph {
        graph: crate::layout::GraphLayout,
    },
    
    /// VM state update
    VMState {
        snapshot: VMSnapshot,
    },
    
    /// Debug event
    DebugEvent {
        event: crate::debug::DebugEvent,
    },
    
    /// Control command from client
    Control {
        command: ControlCommand,
    },
    
    /// Error message
    Error {
        message: String,
    },
}

/// Control commands from the client
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "command")]
pub enum ControlCommand {
    /// Start/resume execution
    Start,
    
    /// Pause execution
    Pause,
    
    /// Step to next instruction
    Step,
    
    /// Step over function call
    StepOver,
    
    /// Step out of current function
    StepOut,
    
    /// Reset VM
    Reset,
    
    /// Set breakpoint
    SetBreakpoint {
        pc: usize,
    },
    
    /// Remove breakpoint
    RemoveBreakpoint {
        pc: usize,
    },
    
    /// Load program
    LoadProgram {
        source: String,
    },
}