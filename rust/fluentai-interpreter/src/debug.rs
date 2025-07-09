//! Debug support for the interpreter

use crate::environment::Environment;
use crate::provenance::{SourceLocation, StackFrame};
use crate::value::Value;
use fluentai_core::ast::NodeId;
use rustc_hash::{FxHashMap, FxHashSet};

/// Debug mode configuration
#[derive(Debug, Clone)]
pub struct DebugMode {
    /// Whether debugging is enabled
    pub enabled: bool,
    /// Current step mode
    pub step_mode: StepMode,
    /// Active breakpoints
    pub breakpoints: FxHashMap<String, Breakpoint>,
    /// Watch expressions
    pub watches: Vec<WatchExpression>,
    /// Maximum call depth to track
    pub max_call_depth: usize,
    /// Whether to track all variable changes
    pub track_variables: bool,
}

impl Default for DebugMode {
    fn default() -> Self {
        Self {
            enabled: false,
            step_mode: StepMode::Continue,
            breakpoints: FxHashMap::default(),
            watches: Vec::new(),
            max_call_depth: 100,
            track_variables: false,
        }
    }
}

/// Step execution mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepMode {
    /// Continue normal execution
    Continue,
    /// Step into function calls
    StepInto,
    /// Step over function calls
    StepOver,
    /// Step out of current function
    StepOut,
    /// Run to next breakpoint
    Next,
}

/// Breakpoint definition
#[derive(Debug, Clone)]
pub struct Breakpoint {
    /// Unique identifier
    pub id: String,
    /// Breakpoint location
    pub location: BreakpointLocation,
    /// Condition to evaluate
    pub condition: Option<String>,
    /// Hit count
    pub hit_count: usize,
    /// Whether breakpoint is enabled
    pub enabled: bool,
}

/// Where a breakpoint can be set
#[derive(Debug, Clone)]
pub enum BreakpointLocation {
    /// Break at function entry
    Function(String),
    /// Break at specific node
    Node(NodeId),
    /// Break at source location
    SourceLocation(SourceLocation),
    /// Break on error
    OnError,
}

/// Watch expression
#[derive(Debug, Clone)]
pub struct WatchExpression {
    /// Expression to evaluate
    pub expression: String,
    /// Last evaluated value
    pub last_value: Option<Value>,
}

/// Debug event that can occur during execution
#[derive(Debug, Clone)]
pub enum DebugEvent {
    /// Entering a node
    NodeEnter {
        node_id: NodeId,
        node_type: String,
        location: Option<SourceLocation>,
    },
    /// Exiting a node
    NodeExit { node_id: NodeId, value: Value },
    /// Entering a function
    FunctionEnter {
        name: String,
        arguments: Vec<Value>,
        call_depth: usize,
    },
    /// Exiting a function
    FunctionExit {
        name: String,
        return_value: Value,
        call_depth: usize,
    },
    /// Variable bound
    VariableBound {
        name: String,
        value: Value,
        scope_depth: usize,
    },
    /// Variable updated
    VariableUpdated {
        name: String,
        old_value: Value,
        new_value: Value,
    },
    /// Breakpoint hit
    BreakpointHit {
        breakpoint: Breakpoint,
        context: DebugContext,
    },
    /// Watch value changed
    WatchChanged {
        expression: String,
        old_value: Option<Value>,
        new_value: Value,
    },
    /// Error occurred
    Error {
        error: String,
        stack_trace: Vec<StackFrame>,
    },
}

/// Debug context at a point in execution
#[derive(Debug, Clone)]
pub struct DebugContext {
    /// Current node
    pub current_node: NodeId,
    /// Current environment
    pub environment: Environment,
    /// Call stack
    pub call_stack: Vec<StackFrame>,
    /// Local variables
    pub locals: FxHashMap<String, Value>,
}

/// Debug handler trait
pub trait DebugHandler: Send + Sync {
    /// Handle a debug event
    fn handle_event(&mut self, event: DebugEvent) -> DebugAction;

    /// Check if we should break at this point
    fn should_break(&self, context: &DebugContext) -> bool;
}

/// Action to take after a debug event
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugAction {
    /// Continue execution
    Continue,
    /// Step to next instruction
    Step,
    /// Pause execution
    Pause,
    /// Abort execution
    Abort,
}

/// Default debug handler that can be used interactively
pub struct InteractiveDebugHandler {
    /// Current action
    current_action: DebugAction,
    /// Whether we're paused
    paused: bool,
    /// Breakpoints that have been hit
    hit_breakpoints: FxHashSet<String>,
}

impl InteractiveDebugHandler {
    pub fn new() -> Self {
        Self {
            current_action: DebugAction::Continue,
            paused: false,
            hit_breakpoints: FxHashSet::default(),
        }
    }

    pub fn pause(&mut self) {
        self.paused = true;
        self.current_action = DebugAction::Pause;
    }

    pub fn resume(&mut self) {
        self.paused = false;
        self.current_action = DebugAction::Continue;
    }

    pub fn step(&mut self) {
        self.current_action = DebugAction::Step;
    }
}

impl DebugHandler for InteractiveDebugHandler {
    fn handle_event(&mut self, event: DebugEvent) -> DebugAction {
        match event {
            DebugEvent::BreakpointHit { ref breakpoint, .. } => {
                self.hit_breakpoints.insert(breakpoint.id.clone());
                self.paused = true;
                DebugAction::Pause
            }
            DebugEvent::Error { .. } => {
                self.paused = true;
                DebugAction::Pause
            }
            _ => {
                if self.paused {
                    self.current_action
                } else {
                    DebugAction::Continue
                }
            }
        }
    }

    fn should_break(&self, _context: &DebugContext) -> bool {
        self.paused
    }
}

/// Debugger interface for the interpreter
pub struct Debugger {
    /// Debug mode configuration
    pub mode: DebugMode,
    /// Debug event handler
    pub handler: Box<dyn DebugHandler>,
    /// Execution trace
    pub trace: Vec<DebugEvent>,
    /// Current call depth
    call_depth: usize,
}

impl Debugger {
    /// Create a new debugger
    pub fn new(mode: DebugMode, handler: Box<dyn DebugHandler>) -> Self {
        Self {
            mode,
            handler,
            trace: Vec::new(),
            call_depth: 0,
        }
    }

    /// Record a debug event
    pub fn record_event(&mut self, event: DebugEvent) -> DebugAction {
        if self.mode.enabled {
            self.trace.push(event.clone());
            self.handler.handle_event(event)
        } else {
            DebugAction::Continue
        }
    }

    /// Check if we should break
    pub fn should_break(&self, context: &DebugContext) -> bool {
        if !self.mode.enabled {
            return false;
        }

        // Check breakpoints
        for (_, bp) in &self.mode.breakpoints {
            if bp.enabled && self.matches_breakpoint(bp, context) {
                return true;
            }
        }

        // Check step mode
        match self.mode.step_mode {
            StepMode::StepInto => true,
            StepMode::StepOver => self.call_depth == 0,
            StepMode::StepOut => false,
            _ => false,
        }
    }

    fn matches_breakpoint(&self, bp: &Breakpoint, context: &DebugContext) -> bool {
        match &bp.location {
            BreakpointLocation::Node(node_id) => context.current_node == *node_id,
            BreakpointLocation::Function(name) => context
                .call_stack
                .last()
                .map(|frame| frame.function_name == *name)
                .unwrap_or(false),
            _ => false,
        }
    }

    /// Increment call depth
    pub fn enter_function(&mut self) {
        self.call_depth += 1;
    }

    /// Decrement call depth
    pub fn exit_function(&mut self) {
        self.call_depth = self.call_depth.saturating_sub(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug_mode_default() {
        let mode = DebugMode::default();
        assert!(!mode.enabled);
        assert_eq!(mode.step_mode, StepMode::Continue);
    }

    #[test]
    fn test_breakpoint() {
        let bp = Breakpoint {
            id: "bp1".to_string(),
            location: BreakpointLocation::Function("test".to_string()),
            condition: None,
            hit_count: 0,
            enabled: true,
        };

        assert!(bp.enabled);
        assert_eq!(bp.id, "bp1");
    }
}
