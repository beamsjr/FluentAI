//! REPL environment and execution mode management

use std::collections::HashMap;
use std::path::PathBuf;
use fluentai_interpreter::{Interpreter, InterpreterOptions, ExecutionMode as InterpreterExecutionMode};
use fluentai_core::ast::Graph;
use crate::error::{ReplError, ReplResult};

/// Execution mode for the REPL
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    /// Direct tree-walking interpretation
    TreeWalking,
    /// Compile to bytecode and run on VM
    Bytecode,
    /// JIT compile to native code
    #[cfg(feature = "jit")]
    JIT,
}

impl From<ExecutionMode> for InterpreterExecutionMode {
    fn from(mode: ExecutionMode) -> Self {
        match mode {
            ExecutionMode::TreeWalking => InterpreterExecutionMode::TreeWalking,
            ExecutionMode::Bytecode => InterpreterExecutionMode::Bytecode,
            #[cfg(feature = "jit")]
            ExecutionMode::JIT => InterpreterExecutionMode::JIT,
        }
    }
}

/// REPL environment state
pub struct ReplEnvironment {
    /// Current execution mode
    mode: ExecutionMode,
    /// Tree-walking interpreter
    interpreter: Interpreter,
    /// Current working directory
    cwd: PathBuf,
    /// User-defined variables for inspection
    user_vars: HashMap<String, String>,
    /// Debug mode enabled
    debug_enabled: bool,
    /// Command history enabled
    history_enabled: bool,
}

impl ReplEnvironment {
    /// Create a new REPL environment
    pub fn new() -> ReplResult<Self> {
        let interpreter_options = InterpreterOptions::default();
        let interpreter = Interpreter::new(interpreter_options);
        let cwd = std::env::current_dir()?;

        Ok(Self {
            mode: ExecutionMode::TreeWalking,
            interpreter,
            cwd,
            user_vars: HashMap::new(),
            debug_enabled: false,
            history_enabled: true,
        })
    }

    /// Get current execution mode
    pub fn mode(&self) -> ExecutionMode {
        self.mode
    }

    /// Set execution mode
    pub fn set_mode(&mut self, mode: ExecutionMode) -> ReplResult<()> {
        self.mode = mode;
        
        // Update interpreter options
        let mut options = InterpreterOptions::default();
        options.mode = mode.into();
        self.interpreter = Interpreter::new(options);
        
        Ok(())
    }

    /// Execute code in the current mode
    pub fn execute(&mut self, graph: &Graph) -> ReplResult<String> {
        match self.mode {
            ExecutionMode::TreeWalking => {
                let value = self.interpreter.interpret(graph)?;
                Ok(value.to_string())
            }
            ExecutionMode::Bytecode => {
                // TODO: Compile to bytecode and run on VM
                // For now, fallback to tree-walking
                let value = self.interpreter.interpret(graph)?;
                Ok(value.to_string())
            }
            #[cfg(feature = "jit")]
            ExecutionMode::JIT => {
                // TODO: JIT compile and execute
                // For now, fallback to tree-walking
                let value = self.interpreter.interpret(graph)?;
                Ok(value.to_string())
            }
        }
    }

    /// Enable or disable debug mode
    pub fn set_debug(&mut self, enabled: bool) {
        self.debug_enabled = enabled;
        
        // Update interpreter debug mode
        let mut options = InterpreterOptions::default();
        options.debug_mode.enabled = enabled;
        options.mode = self.mode.into();
        self.interpreter = Interpreter::new(options);
    }

    /// Check if debug mode is enabled
    pub fn is_debug_enabled(&self) -> bool {
        self.debug_enabled
    }

    /// Enable or disable history
    pub fn set_history_enabled(&mut self, enabled: bool) {
        self.history_enabled = enabled;
    }

    /// Check if history is enabled
    pub fn is_history_enabled(&self) -> bool {
        self.history_enabled
    }

    /// Get current working directory
    pub fn cwd(&self) -> &PathBuf {
        &self.cwd
    }

    /// Set current working directory
    pub fn set_cwd(&mut self, path: PathBuf) -> ReplResult<()> {
        if !path.is_dir() {
            return Err(ReplError::Config(format!("Not a directory: {:?}", path)));
        }
        self.cwd = path;
        Ok(())
    }

    /// Set a user variable
    pub fn set_var(&mut self, name: String, value: String) {
        self.user_vars.insert(name, value);
    }

    /// Get a user variable
    pub fn get_var(&self, name: &str) -> Option<&String> {
        self.user_vars.get(name)
    }

    /// Get all user variables
    pub fn vars(&self) -> &HashMap<String, String> {
        &self.user_vars
    }

    /// Clear all user variables
    pub fn clear_vars(&mut self) {
        self.user_vars.clear();
    }

    /// Get interpreter environment for variable inspection
    pub fn interpreter_env(&self) -> &Interpreter {
        &self.interpreter
    }

    /// Reset the environment
    pub fn reset(&mut self) -> ReplResult<()> {
        self.user_vars.clear();
        let options = InterpreterOptions {
            mode: self.mode.into(),
            debug_mode: fluentai_interpreter::DebugMode {
                enabled: self.debug_enabled,
                ..Default::default()
            },
            ..Default::default()
        };
        self.interpreter = Interpreter::new(options);
        // VM will be re-initialized when bytecode mode is implemented
        Ok(())
    }
}

impl Default for ReplEnvironment {
    fn default() -> Self {
        Self::new().expect("Failed to create default REPL environment")
    }
}