//! Main REPL implementation

use rustyline::{Editor, Config, EditMode, CompletionType};
use rustyline::error::ReadlineError;
use colored::*;
use std::path::PathBuf;

use crate::error::ReplResult;
use crate::environment::ReplEnvironment;
use crate::commands::{CommandRegistry, CommandResult};
use crate::history::HistoryManager;
use crate::highlighter::ReplHelper;

/// REPL configuration
#[derive(Debug, Clone)]
pub struct ReplConfig {
    /// History file path
    pub history_file: Option<PathBuf>,
    /// Maximum history entries
    pub max_history: usize,
    /// Enable syntax highlighting
    pub highlighting: bool,
    /// Enable auto-completion
    pub completion: bool,
    /// Prompt string
    pub prompt: String,
    /// Continuation prompt
    pub continuation_prompt: String,
    /// Exit on Ctrl-D
    pub exit_on_eof: bool,
    /// Show banner on startup
    pub show_banner: bool,
}

impl Default for ReplConfig {
    fn default() -> Self {
        Self {
            history_file: None,
            max_history: 10000,
            highlighting: true,
            completion: true,
            prompt: "fluentai> ".to_string(),
            continuation_prompt: "        ... ".to_string(),
            exit_on_eof: true,
            show_banner: true,
        }
    }
}

/// Main REPL structure
pub struct Repl {
    /// REPL configuration
    config: ReplConfig,
    /// REPL environment
    env: ReplEnvironment,
    /// Command registry
    commands: CommandRegistry,
    /// History manager
    history: HistoryManager,
    /// Multi-line input buffer
    input_buffer: String,
    /// Whether we're in multi-line mode
    multiline_mode: bool,
    /// Paren depth for multi-line detection
    paren_depth: i32,
}

impl Repl {
    /// Create a new REPL
    pub fn new(config: ReplConfig) -> ReplResult<Self> {
        let env = ReplEnvironment::new()?;
        let commands = CommandRegistry::new();
        let mut history = HistoryManager::new()?;
        
        // Set custom history file if provided
        if let Some(ref path) = config.history_file {
            history.set_history_file(path.clone());
        }
        
        Ok(Self {
            config,
            env,
            commands,
            history,
            input_buffer: String::new(),
            multiline_mode: false,
            paren_depth: 0,
        })
    }

    /// Run the REPL
    pub fn run(&mut self) -> ReplResult<()> {
        if self.config.show_banner {
            self.print_banner();
        }

        // Setup rustyline
        let config = Config::builder()
            .history_ignore_space(true)
            .completion_type(CompletionType::List)
            .edit_mode(EditMode::Emacs)
            .build();

        let mut editor = Editor::with_config(config)?;
        editor.set_helper(Some(ReplHelper::new()));

        // Load history
        if self.history.is_enabled() {
            if let Ok(entries) = self.history.load() {
                for entry in entries {
                    let _ = editor.add_history_entry(entry);
                }
            }
        }

        // Main REPL loop
        loop {
            let prompt = if self.multiline_mode {
                &self.config.continuation_prompt
            } else {
                &self.config.prompt
            };

            match editor.readline(prompt) {
                Ok(line) => {
                    if line.trim().is_empty() && !self.multiline_mode {
                        continue;
                    }

                    // Add to history
                    if self.history.is_enabled() && !line.trim().is_empty() {
                        let _ = editor.add_history_entry(&line);
                        let _ = self.history.append(&line);
                    }

                    // Handle the input
                    match self.handle_input(&line) {
                        Ok(should_continue) => {
                            if !should_continue {
                                break;
                            }
                        }
                        Err(e) => {
                            eprintln!("{}: {}", "Error".red().bold(), e);
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    if self.multiline_mode {
                        self.input_buffer.clear();
                        self.multiline_mode = false;
                        self.paren_depth = 0;
                        println!("^C");
                    } else {
                        println!("\nUse :exit to quit, or press Ctrl-D");
                    }
                }
                Err(ReadlineError::Eof) => {
                    if self.config.exit_on_eof {
                        println!();
                        break;
                    }
                }
                Err(err) => {
                    eprintln!("{}: {}", "Error".red().bold(), err);
                    break;
                }
            }
        }

        // Save history
        if self.history.is_enabled() {
            let entries: Vec<String> = editor
                .history()
                .iter()
                .map(|s| s.to_string())
                .collect();
            let _ = self.history.save(&entries);
        }

        Ok(())
    }

    /// Handle a line of input
    fn handle_input(&mut self, line: &str) -> ReplResult<bool> {
        // Check for commands
        if line.trim().starts_with(':') && !self.multiline_mode {
            return self.handle_command(line.trim());
        }

        // Update paren depth
        self.update_paren_depth(line);

        // Add to buffer
        if self.multiline_mode {
            self.input_buffer.push('\n');
        }
        self.input_buffer.push_str(line);

        // Check if we should continue multi-line input
        if self.paren_depth > 0 {
            self.multiline_mode = true;
            return Ok(true);
        }

        // We have a complete expression
        self.multiline_mode = false;
        let input = self.input_buffer.trim().to_string();
        self.input_buffer.clear();

        if input.is_empty() {
            return Ok(true);
        }

        // Parse and execute
        use fluentai_parser::parse;
        match parse(&input) {
            Ok(graph) => {
                match self.env.execute(&graph) {
                    Ok(result) => {
                        println!("{}", result.cyan());
                    }
                    Err(e) => {
                        eprintln!("{}: {}", "Runtime error".red().bold(), e);
                    }
                }
            }
            Err(e) => {
                eprintln!("{}: {}", "Parse error".red().bold(), e);
            }
        }

        Ok(true)
    }

    /// Handle a command
    fn handle_command(&mut self, input: &str) -> ReplResult<bool> {
        let cmd_input = &input[1..]; // Remove the ':'
        
        match self.commands.execute(&mut self.env, cmd_input)? {
            CommandResult::Success(msg) => {
                println!("{}", msg);
                Ok(true)
            }
            CommandResult::Exit => Ok(false),
            CommandResult::Clear => {
                self.clear_screen();
                Ok(true)
            }
            CommandResult::Help => {
                self.print_help();
                Ok(true)
            }
            CommandResult::Continue => Ok(true),
        }
    }

    /// Update parenthesis depth
    fn update_paren_depth(&mut self, line: &str) {
        for ch in line.chars() {
            match ch {
                '(' | '[' | '{' => self.paren_depth += 1,
                ')' | ']' | '}' => self.paren_depth -= 1,
                '"' => {
                    // Skip string contents
                    // This is a simplified version that doesn't handle escapes
                    let mut in_string = true;
                    let mut chars = line.chars();
                    while let Some(ch) = chars.next() {
                        if ch == '"' && in_string {
                            in_string = false;
                            break;
                        }
                    }
                }
                _ => {}
            }
        }
        
        // Don't let it go negative
        if self.paren_depth < 0 {
            self.paren_depth = 0;
        }
    }

    /// Print the banner
    fn print_banner(&self) {
        println!("{}", "╔═══════════════════════════════════════╗".bright_blue());
        println!("{} {} {}", 
            "║".bright_blue(),
            "      FluentAi Interactive REPL      ".bright_white().bold(),
            "║".bright_blue()
        );
        println!("{}", "╚═══════════════════════════════════════╝".bright_blue());
        println!();
        println!("Type {} for help, {} to exit", 
            ":help".bright_cyan(), 
            ":exit".bright_cyan()
        );
        println!();
    }

    /// Print help information
    fn print_help(&self) {
        println!("{}", "Available commands:".bright_white().bold());
        println!();
        
        for cmd in self.commands.all_commands() {
            let aliases = if cmd.aliases.is_empty() {
                String::new()
            } else {
                format!(" ({})", cmd.aliases.join(", "))
            };
            
            println!("  {}{} - {}", 
                cmd.name.bright_cyan(),
                aliases.bright_black(),
                cmd.description
            );
            println!("    {}", cmd.usage.bright_black());
        }
        
        println!();
        println!("{}", "Keyboard shortcuts:".bright_white().bold());
        println!("  {} - Exit REPL", "Ctrl-D".bright_cyan());
        println!("  {} - Cancel current input", "Ctrl-C".bright_cyan());
        println!("  {} - Clear screen", "Ctrl-L".bright_cyan());
        println!();
    }

    /// Clear the screen
    fn clear_screen(&self) {
        print!("\x1B[2J\x1B[1;1H");
    }
}

/// Create and run a REPL with default configuration
pub fn run_repl() -> ReplResult<()> {
    let config = ReplConfig::default();
    let mut repl = Repl::new(config)?;
    repl.run()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_paren_depth() {
        let config = ReplConfig::default();
        let mut repl = Repl::new(config).unwrap();
        
        repl.update_paren_depth("(+ 1");
        assert_eq!(repl.paren_depth, 1);
        
        repl.update_paren_depth("(* 2 3))");
        assert_eq!(repl.paren_depth, 0); // 1 open - 2 close = -1, but clamped to 0
        
        repl.paren_depth = 0;
        repl.update_paren_depth("(let ((x 1) (y 2))");
        assert_eq!(repl.paren_depth, 1);
    }
}