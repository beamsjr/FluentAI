/// UI debugger for FluentAI renderer
use std::collections::{HashMap, VecDeque};
use crate::components::{ComponentId, ComponentEvent};
use crate::primitives::Renderable;

/// UI debugger for tracking events and state
pub struct UIDebugger {
    enabled: bool,
    event_log: VecDeque<DebugEvent>,
    state_snapshots: VecDeque<StateSnapshot>,
    breakpoints: HashMap<String, DebugBreakpoint>,
    console_output: VecDeque<ConsoleMessage>,
    max_events: usize,
    max_snapshots: usize,
    paused: bool,
    step_mode: StepMode,
}

#[derive(Debug, Clone)]
pub struct DebugEvent {
    pub timestamp: std::time::Instant,
    pub event_type: String,
    pub component_id: Option<ComponentId>,
    pub data: serde_json::Value,
    pub stack_trace: Vec<StackFrame>,
}

#[derive(Debug, Clone)]
pub struct StateSnapshot {
    pub timestamp: std::time::Instant,
    pub component_states: HashMap<ComponentId, serde_json::Value>,
    pub global_state: serde_json::Value,
}

#[derive(Debug, Clone)]
pub struct DebugBreakpoint {
    pub id: String,
    pub condition: BreakpointCondition,
    pub enabled: bool,
    pub hit_count: u32,
}

#[derive(Debug, Clone)]
pub enum BreakpointCondition {
    EventType(String),
    ComponentId(ComponentId),
    StateChange(String),
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct ConsoleMessage {
    pub timestamp: std::time::Instant,
    pub level: LogLevel,
    pub message: String,
    pub source: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StepMode {
    Continue,
    StepInto,
    StepOver,
    StepOut,
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub file: String,
    pub line: u32,
    pub column: u32,
}

impl UIDebugger {
    pub fn new() -> Self {
        Self {
            enabled: false,
            event_log: VecDeque::with_capacity(1000),
            state_snapshots: VecDeque::with_capacity(100),
            breakpoints: HashMap::new(),
            console_output: VecDeque::with_capacity(500),
            max_events: 1000,
            max_snapshots: 100,
            paused: false,
            step_mode: StepMode::Continue,
        }
    }
    
    /// Enable debugger
    pub fn enable(&mut self) {
        self.enabled = true;
        self.log(LogLevel::Info, "Debugger enabled", "UIDebugger");
    }
    
    /// Disable debugger
    pub fn disable(&mut self) {
        self.enabled = false;
    }
    
    /// Check if debugger is paused
    pub fn is_paused(&self) -> bool {
        self.paused
    }
    
    /// Pause execution
    pub fn pause(&mut self) {
        self.paused = true;
        self.log(LogLevel::Info, "Execution paused", "UIDebugger");
    }
    
    /// Resume execution
    pub fn resume(&mut self) {
        self.paused = false;
        self.step_mode = StepMode::Continue;
        self.log(LogLevel::Info, "Execution resumed", "UIDebugger");
    }
    
    /// Step into next operation
    pub fn step_into(&mut self) {
        self.step_mode = StepMode::StepInto;
        self.paused = false;
    }
    
    /// Step over current operation
    pub fn step_over(&mut self) {
        self.step_mode = StepMode::StepOver;
        self.paused = false;
    }
    
    /// Step out of current function
    pub fn step_out(&mut self) {
        self.step_mode = StepMode::StepOut;
        self.paused = false;
    }
    
    /// Log an event
    pub fn log_event(&mut self, event: DebugEvent) {
        if !self.enabled {
            return;
        }
        
        // Check breakpoints
        if self.should_break_on_event(&event) {
            self.pause();
        }
        
        self.event_log.push_back(event);
        
        // Trim old events
        while self.event_log.len() > self.max_events {
            self.event_log.pop_front();
        }
    }
    
    /// Log a UI event
    pub fn log_ui_event(&mut self, component_id: ComponentId, event: &ComponentEvent) {
        let debug_event = DebugEvent {
            timestamp: std::time::Instant::now(),
            event_type: format!("{:?}", event),
            component_id: Some(component_id),
            data: serde_json::json!({
                "event": format!("{:?}", event),
            }),
            stack_trace: self.capture_stack_trace(),
        };
        
        self.log_event(debug_event);
    }
    
    /// Take a state snapshot
    pub fn snapshot_state(&mut self, component_states: HashMap<ComponentId, serde_json::Value>) {
        if !self.enabled {
            return;
        }
        
        let snapshot = StateSnapshot {
            timestamp: std::time::Instant::now(),
            component_states,
            global_state: serde_json::json!({}), // TODO: Capture global state
        };
        
        self.state_snapshots.push_back(snapshot);
        
        // Trim old snapshots
        while self.state_snapshots.len() > self.max_snapshots {
            self.state_snapshots.pop_front();
        }
    }
    
    /// Add a breakpoint
    pub fn add_breakpoint(&mut self, id: String, condition: BreakpointCondition) {
        self.breakpoints.insert(id.clone(), DebugBreakpoint {
            id,
            condition,
            enabled: true,
            hit_count: 0,
        });
    }
    
    /// Remove a breakpoint
    pub fn remove_breakpoint(&mut self, id: &str) {
        self.breakpoints.remove(id);
    }
    
    /// Toggle breakpoint
    pub fn toggle_breakpoint(&mut self, id: &str) {
        if let Some(bp) = self.breakpoints.get_mut(id) {
            bp.enabled = !bp.enabled;
        }
    }
    
    /// Check if should break on event
    fn should_break_on_event(&mut self, event: &DebugEvent) -> bool {
        for breakpoint in self.breakpoints.values_mut() {
            if !breakpoint.enabled {
                continue;
            }
            
            let should_break = match &breakpoint.condition {
                BreakpointCondition::EventType(event_type) => {
                    event.event_type == *event_type
                }
                BreakpointCondition::ComponentId(id) => {
                    event.component_id == Some(*id)
                }
                BreakpointCondition::StateChange(_key) => {
                    // TODO: Check if state key changed
                    false
                }
                BreakpointCondition::Custom(_expr) => {
                    // TODO: Evaluate custom expression
                    false
                }
            };
            
            if should_break {
                breakpoint.hit_count += 1;
                return true;
            }
        }
        
        false
    }
    
    /// Log a console message
    pub fn log(&mut self, level: LogLevel, message: &str, source: &str) {
        if !self.enabled && level != LogLevel::Error {
            return;
        }
        
        let console_msg = ConsoleMessage {
            timestamp: std::time::Instant::now(),
            level,
            message: message.to_string(),
            source: source.to_string(),
        };
        
        self.console_output.push_back(console_msg);
        
        // Trim old messages
        while self.console_output.len() > 500 {
            self.console_output.pop_front();
        }
        
        // Also print to stdout in debug mode
        #[cfg(debug_assertions)]
        {
            let prefix = match level {
                LogLevel::Debug => "🐛",
                LogLevel::Info => "ℹ️",
                LogLevel::Warning => "⚠️",
                LogLevel::Error => "❌",
            };
            println!("{} [{}] {}: {}", prefix, source, level.as_str(), message);
        }
    }
    
    /// Capture current stack trace
    fn capture_stack_trace(&self) -> Vec<StackFrame> {
        // TODO: Implement actual stack trace capture
        vec![
            StackFrame {
                function_name: "render".to_string(),
                file: "renderer.rs".to_string(),
                line: 123,
                column: 5,
            },
        ]
    }
    
    /// Get event log
    pub fn get_event_log(&self) -> &VecDeque<DebugEvent> {
        &self.event_log
    }
    
    /// Get console output
    pub fn get_console_output(&self) -> &VecDeque<ConsoleMessage> {
        &self.console_output
    }
    
    /// Get state snapshots
    pub fn get_snapshots(&self) -> &VecDeque<StateSnapshot> {
        &self.state_snapshots
    }
    
    /// Clear all logs
    pub fn clear_logs(&mut self) {
        self.event_log.clear();
        self.console_output.clear();
        self.state_snapshots.clear();
    }
    
    /// Export debug session
    pub fn export_session(&self) -> serde_json::Value {
        serde_json::json!({
            "events": self.event_log.iter().map(|e| {
                serde_json::json!({
                    "timestamp": e.timestamp.elapsed().as_millis(),
                    "type": e.event_type,
                    "componentId": e.component_id.map(|id| id.to_string()),
                    "data": e.data,
                    "stackTrace": e.stack_trace.iter().map(|f| {
                        serde_json::json!({
                            "function": f.function_name,
                            "file": f.file,
                            "line": f.line,
                            "column": f.column,
                        })
                    }).collect::<Vec<_>>(),
                })
            }).collect::<Vec<_>>(),
            
            "console": self.console_output.iter().map(|msg| {
                serde_json::json!({
                    "timestamp": msg.timestamp.elapsed().as_millis(),
                    "level": msg.level.as_str(),
                    "message": msg.message,
                    "source": msg.source,
                })
            }).collect::<Vec<_>>(),
            
            "snapshots": self.state_snapshots.iter().map(|s| {
                serde_json::json!({
                    "timestamp": s.timestamp.elapsed().as_millis(),
                    "componentStates": s.component_states,
                    "globalState": s.global_state,
                })
            }).collect::<Vec<_>>(),
            
            "breakpoints": self.breakpoints.values().map(|bp| {
                serde_json::json!({
                    "id": bp.id,
                    "condition": format!("{:?}", bp.condition),
                    "enabled": bp.enabled,
                    "hitCount": bp.hit_count,
                })
            }).collect::<Vec<_>>(),
        })
    }
}

impl LogLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            LogLevel::Debug => "DEBUG",
            LogLevel::Info => "INFO",
            LogLevel::Warning => "WARNING",
            LogLevel::Error => "ERROR",
        }
    }
}

/// Time travel debugging support
pub struct TimeTravelDebugger {
    snapshots: Vec<(std::time::Instant, StateSnapshot)>,
    current_index: usize,
}

impl TimeTravelDebugger {
    pub fn new() -> Self {
        Self {
            snapshots: Vec::new(),
            current_index: 0,
        }
    }
    
    /// Record a snapshot
    pub fn record(&mut self, snapshot: StateSnapshot) {
        let timestamp = std::time::Instant::now();
        
        // If we're not at the end, truncate future snapshots
        if self.current_index < self.snapshots.len() {
            self.snapshots.truncate(self.current_index);
        }
        
        self.snapshots.push((timestamp, snapshot));
        self.current_index = self.snapshots.len();
    }
    
    /// Step backward in time
    pub fn step_back(&mut self) -> Option<&StateSnapshot> {
        if self.current_index > 0 {
            self.current_index -= 1;
            Some(&self.snapshots[self.current_index].1)
        } else {
            None
        }
    }
    
    /// Step forward in time
    pub fn step_forward(&mut self) -> Option<&StateSnapshot> {
        if self.current_index < self.snapshots.len() - 1 {
            self.current_index += 1;
            Some(&self.snapshots[self.current_index].1)
        } else {
            None
        }
    }
    
    /// Jump to specific time
    pub fn jump_to_time(&mut self, target: std::time::Instant) -> Option<&StateSnapshot> {
        // Find closest snapshot
        let mut closest_index = 0;
        let mut closest_diff = std::time::Duration::MAX;
        
        for (i, (timestamp, _)) in self.snapshots.iter().enumerate() {
            let diff = if *timestamp > target {
                *timestamp - target
            } else {
                target - *timestamp
            };
            
            if diff < closest_diff {
                closest_diff = diff;
                closest_index = i;
            }
        }
        
        self.current_index = closest_index;
        self.snapshots.get(closest_index).map(|(_, snapshot)| snapshot)
    }
}