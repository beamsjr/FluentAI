//! Debug support for VM execution

use crate::bytecode::{Instruction, Value};
use tokio::sync::mpsc;

/// Debug event types
#[derive(Debug, Clone)]
pub enum VMDebugEvent {
    /// About to execute an instruction
    PreInstruction {
        pc: usize,
        instruction: Instruction,
        stack_size: usize,
    },
    
    /// Finished executing an instruction
    PostInstruction {
        pc: usize,
        stack_size: usize,
        stack_top: Option<Value>,
    },
    
    /// Function call
    FunctionCall {
        name: Option<String>,
        arg_count: usize,
        call_depth: usize,
    },
    
    /// Function return
    FunctionReturn {
        value: Value,
        call_depth: usize,
    },
    
    /// Variable binding
    VariableBind {
        name: String,
        value: Value,
        is_global: bool,
    },
    
    /// Stack push
    StackPush {
        value: Value,
    },
    
    /// Stack pop
    StackPop {
        value: Value,
    },
    
    /// Error occurred
    Error {
        message: String,
        pc: Option<usize>,
    },
    
    /// Breakpoint hit
    Breakpoint {
        pc: usize,
    },
}

/// Debug configuration
#[derive(Debug, Clone)]
pub struct DebugConfig {
    /// Enable debug events
    pub enabled: bool,
    
    /// Breakpoints (program counter locations)
    pub breakpoints: Vec<usize>,
    
    /// Step mode
    pub step_mode: StepMode,
    
    /// Event channel
    pub event_sender: Option<mpsc::UnboundedSender<VMDebugEvent>>,
}

/// Stepping modes for debugger
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StepMode {
    /// Run normally
    Run,
    
    /// Step one instruction
    Step,
    
    /// Step over function calls
    StepOver,
    
    /// Step out of current function
    StepOut,
}

impl Default for DebugConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            breakpoints: Vec::new(),
            step_mode: StepMode::Run,
            event_sender: None,
        }
    }
}

impl DebugConfig {
    /// Create a new debug configuration with events enabled
    pub fn with_events(sender: mpsc::UnboundedSender<VMDebugEvent>) -> Self {
        Self {
            enabled: true,
            breakpoints: Vec::new(),
            step_mode: StepMode::Run,
            event_sender: Some(sender),
        }
    }
    
    /// Send a debug event
    pub fn send_event(&self, event: VMDebugEvent) {
        if let Some(sender) = &self.event_sender {
            let _ = sender.send(event);
        }
    }
    
    /// Check if we should break at this PC
    pub fn should_break(&self, pc: usize) -> bool {
        self.breakpoints.contains(&pc)
    }
    
    /// Add a breakpoint
    pub fn add_breakpoint(&mut self, pc: usize) {
        if !self.breakpoints.contains(&pc) {
            self.breakpoints.push(pc);
        }
    }
    
    /// Remove a breakpoint
    pub fn remove_breakpoint(&mut self, pc: usize) {
        self.breakpoints.retain(|&bp| bp != pc);
    }
}