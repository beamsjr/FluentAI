//! REPL command handling

use std::collections::HashMap;
use crate::error::{ReplError, ReplResult};
use crate::environment::{ReplEnvironment, ExecutionMode};

/// REPL command
#[derive(Debug, Clone)]
pub struct Command {
    /// Command name
    pub name: String,
    /// Command aliases
    pub aliases: Vec<String>,
    /// Command description
    pub description: String,
    /// Command usage
    pub usage: String,
    /// Command handler
    pub handler: CommandHandler,
}

/// Command handler function type
pub type CommandHandler = fn(&mut ReplEnvironment, &[String]) -> ReplResult<CommandResult>;

/// Result of executing a command
#[derive(Debug)]
pub enum CommandResult {
    /// Command executed successfully
    Success(String),
    /// Exit the REPL
    Exit,
    /// Clear the screen
    Clear,
    /// Show help
    Help,
    /// Continue normal execution
    Continue,
}

/// Command registry
pub struct CommandRegistry {
    commands: HashMap<String, Command>,
}

impl CommandRegistry {
    /// Create a new command registry with default commands
    pub fn new() -> Self {
        let mut registry = Self {
            commands: HashMap::new(),
        };
        
        registry.register_default_commands();
        registry
    }

    /// Register default commands
    fn register_default_commands(&mut self) {
        // Help command
        self.register(Command {
            name: "help".to_string(),
            aliases: vec!["h".to_string(), "?".to_string()],
            description: "Show help information".to_string(),
            usage: ":help [command]".to_string(),
            handler: cmd_help,
        });

        // Exit command
        self.register(Command {
            name: "exit".to_string(),
            aliases: vec!["quit".to_string(), "q".to_string()],
            description: "Exit the REPL".to_string(),
            usage: ":exit".to_string(),
            handler: cmd_exit,
        });

        // Clear command
        self.register(Command {
            name: "clear".to_string(),
            aliases: vec!["cls".to_string()],
            description: "Clear the screen".to_string(),
            usage: ":clear".to_string(),
            handler: cmd_clear,
        });

        // Mode command
        self.register(Command {
            name: "mode".to_string(),
            aliases: vec!["m".to_string()],
            description: "Get or set execution mode".to_string(),
            usage: ":mode [tree|bytecode|jit]".to_string(),
            handler: cmd_mode,
        });

        // Debug command
        self.register(Command {
            name: "debug".to_string(),
            aliases: vec!["d".to_string()],
            description: "Toggle debug mode".to_string(),
            usage: ":debug [on|off]".to_string(),
            handler: cmd_debug,
        });

        // Vars command
        self.register(Command {
            name: "vars".to_string(),
            aliases: vec!["v".to_string()],
            description: "Show all variables".to_string(),
            usage: ":vars".to_string(),
            handler: cmd_vars,
        });

        // Set command
        self.register(Command {
            name: "set".to_string(),
            aliases: vec!["s".to_string()],
            description: "Set a configuration variable".to_string(),
            usage: ":set <name> <value>".to_string(),
            handler: cmd_set,
        });

        // Load command
        self.register(Command {
            name: "load".to_string(),
            aliases: vec!["l".to_string()],
            description: "Load and execute a file".to_string(),
            usage: ":load <file>".to_string(),
            handler: cmd_load,
        });

        // Save command
        self.register(Command {
            name: "save".to_string(),
            aliases: vec![],
            description: "Save session history to a file".to_string(),
            usage: ":save <file>".to_string(),
            handler: cmd_save,
        });

        // Reset command
        self.register(Command {
            name: "reset".to_string(),
            aliases: vec![],
            description: "Reset the REPL environment".to_string(),
            usage: ":reset".to_string(),
            handler: cmd_reset,
        });

        // Time command
        self.register(Command {
            name: "time".to_string(),
            aliases: vec!["t".to_string()],
            description: "Time the execution of an expression".to_string(),
            usage: ":time <expression>".to_string(),
            handler: cmd_time,
        });
    }

    /// Register a command
    pub fn register(&mut self, command: Command) {
        // Register by name
        self.commands.insert(command.name.clone(), command.clone());
        
        // Register aliases
        for alias in &command.aliases {
            self.commands.insert(alias.clone(), command.clone());
        }
    }

    /// Get a command by name
    pub fn get(&self, name: &str) -> Option<&Command> {
        self.commands.get(name)
    }

    /// Get all unique commands (no aliases)
    pub fn all_commands(&self) -> Vec<&Command> {
        let mut seen = std::collections::HashSet::new();
        let mut commands = Vec::new();
        
        for cmd in self.commands.values() {
            if seen.insert(&cmd.name) {
                commands.push(cmd);
            }
        }
        
        commands.sort_by(|a, b| a.name.cmp(&b.name));
        commands
    }

    /// Execute a command
    pub fn execute(&self, env: &mut ReplEnvironment, input: &str) -> ReplResult<CommandResult> {
        let parts: Vec<String> = input.split_whitespace().map(|s| s.to_string()).collect();
        
        if parts.is_empty() {
            return Ok(CommandResult::Continue);
        }

        let cmd_name = &parts[0];
        let args = &parts[1..];

        if let Some(command) = self.get(cmd_name) {
            (command.handler)(env, args)
        } else {
            Err(ReplError::Command(format!("Unknown command: {}", cmd_name)))
        }
    }
}

// Command implementations

fn cmd_help(_env: &mut ReplEnvironment, _args: &[String]) -> ReplResult<CommandResult> {
    Ok(CommandResult::Help)
}

fn cmd_exit(_env: &mut ReplEnvironment, _args: &[String]) -> ReplResult<CommandResult> {
    Ok(CommandResult::Exit)
}

fn cmd_clear(_env: &mut ReplEnvironment, _args: &[String]) -> ReplResult<CommandResult> {
    Ok(CommandResult::Clear)
}

fn cmd_mode(env: &mut ReplEnvironment, args: &[String]) -> ReplResult<CommandResult> {
    if args.is_empty() {
        let mode = match env.mode() {
            ExecutionMode::TreeWalking => "tree",
            ExecutionMode::Bytecode => "bytecode",
            #[cfg(feature = "jit")]
            ExecutionMode::JIT => "jit",
        };
        Ok(CommandResult::Success(format!("Current mode: {}", mode)))
    } else {
        let mode = match args[0].as_str() {
            "tree" | "treewalk" | "tree-walking" => ExecutionMode::TreeWalking,
            "bytecode" | "bc" | "vm" => ExecutionMode::Bytecode,
            #[cfg(feature = "jit")]
            "jit" => ExecutionMode::JIT,
            _ => return Err(ReplError::Command(format!("Unknown mode: {}", args[0]))),
        };
        
        env.set_mode(mode)?;
        Ok(CommandResult::Success(format!("Mode set to: {}", args[0])))
    }
}

fn cmd_debug(env: &mut ReplEnvironment, args: &[String]) -> ReplResult<CommandResult> {
    if args.is_empty() {
        let enabled = env.is_debug_enabled();
        Ok(CommandResult::Success(format!("Debug mode: {}", if enabled { "on" } else { "off" })))
    } else {
        let enabled = match args[0].as_str() {
            "on" | "true" | "1" => true,
            "off" | "false" | "0" => false,
            _ => return Err(ReplError::Command(format!("Invalid debug setting: {}", args[0]))),
        };
        
        env.set_debug(enabled);
        Ok(CommandResult::Success(format!("Debug mode: {}", if enabled { "on" } else { "off" })))
    }
}

fn cmd_vars(env: &mut ReplEnvironment, _args: &[String]) -> ReplResult<CommandResult> {
    let vars = env.vars();
    if vars.is_empty() {
        Ok(CommandResult::Success("No variables set".to_string()))
    } else {
        let mut output = String::new();
        for (name, value) in vars {
            output.push_str(&format!("{} = {}\n", name, value));
        }
        Ok(CommandResult::Success(output))
    }
}

fn cmd_set(env: &mut ReplEnvironment, args: &[String]) -> ReplResult<CommandResult> {
    if args.len() < 2 {
        return Err(ReplError::Command("Usage: :set <name> <value>".to_string()));
    }
    
    let name = args[0].clone();
    let value = args[1..].join(" ");
    
    env.set_var(name.clone(), value.clone());
    Ok(CommandResult::Success(format!("Set {} = {}", name, value)))
}

fn cmd_load(env: &mut ReplEnvironment, args: &[String]) -> ReplResult<CommandResult> {
    if args.is_empty() {
        return Err(ReplError::Command("Usage: :load <file>".to_string()));
    }
    
    let path = &args[0];
    let content = std::fs::read_to_string(path)?;
    
    // Parse and execute the file
    use fluentai_parser::parse;
    let graph = parse(&content)?;
    let result = env.execute(&graph)?;
    
    Ok(CommandResult::Success(format!("Loaded {}\n{}", path, result)))
}

fn cmd_save(_env: &mut ReplEnvironment, args: &[String]) -> ReplResult<CommandResult> {
    if args.is_empty() {
        return Err(ReplError::Command("Usage: :save <file>".to_string()));
    }
    
    // TODO: Implement session saving
    Ok(CommandResult::Success("Session saving not yet implemented".to_string()))
}

fn cmd_reset(env: &mut ReplEnvironment, _args: &[String]) -> ReplResult<CommandResult> {
    env.reset()?;
    Ok(CommandResult::Success("Environment reset".to_string()))
}

fn cmd_time(env: &mut ReplEnvironment, args: &[String]) -> ReplResult<CommandResult> {
    if args.is_empty() {
        return Err(ReplError::Command("Usage: :time <expression>".to_string()));
    }
    
    let expr = args.join(" ");
    
    use std::time::Instant;
    use fluentai_parser::parse;
    
    let start = Instant::now();
    let graph = parse(&expr)?;
    let parse_time = start.elapsed();
    
    let start = Instant::now();
    let result = env.execute(&graph)?;
    let exec_time = start.elapsed();
    
    Ok(CommandResult::Success(format!(
        "{}\nParse time: {:?}\nExecution time: {:?}",
        result, parse_time, exec_time
    )))
}

impl Default for CommandRegistry {
    fn default() -> Self {
        Self::new()
    }
}