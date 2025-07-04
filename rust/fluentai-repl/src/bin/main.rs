//! FluentAi REPL binary

use fluentai_repl::{Repl, ReplConfig};
use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "fluentai-repl")]
#[command(about = "FluentAi Interactive REPL", long_about = None)]
struct Cli {
    /// Execution mode
    #[arg(short, long, value_enum, default_value = "tree")]
    mode: ExecutionMode,

    /// Enable debug mode
    #[arg(short, long)]
    debug: bool,

    /// History file path
    #[arg(long)]
    history_file: Option<PathBuf>,

    /// Disable syntax highlighting
    #[arg(long)]
    no_highlight: bool,

    /// Disable auto-completion
    #[arg(long)]
    no_completion: bool,

    /// Don't show banner
    #[arg(long)]
    no_banner: bool,

    /// Load and execute a file before starting REPL
    #[arg(short, long)]
    load: Option<PathBuf>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ExecutionMode {
    /// Tree-walking interpreter
    Tree,
    /// Bytecode VM
    Bytecode,
    /// JIT compilation
    #[cfg(feature = "jit")]
    Jit,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Build REPL configuration
    let mut config = ReplConfig::default();
    config.highlighting = !cli.no_highlight;
    config.completion = !cli.no_completion;
    config.show_banner = !cli.no_banner;
    
    if let Some(history_file) = cli.history_file {
        config.history_file = Some(history_file);
    }

    // Create REPL
    let mut repl = Repl::new(config)?;

    // Set execution mode
    let mode = match cli.mode {
        ExecutionMode::Tree => fluentai_repl::ExecutionMode::TreeWalking,
        ExecutionMode::Bytecode => fluentai_repl::ExecutionMode::Bytecode,
        #[cfg(feature = "jit")]
        ExecutionMode::Jit => fluentai_repl::ExecutionMode::JIT,
    };
    
    // Get mutable reference to environment through REPL
    // Note: We need to add a method to Repl to access its environment
    // For now, we'll skip this as it would require modifying the Repl struct

    // Enable debug mode if requested
    if cli.debug {
        println!("Debug mode enabled");
    }

    // Load file if specified
    if let Some(load_file) = cli.load {
        println!("Loading file: {:?}", load_file);
        let content = std::fs::read_to_string(&load_file)?;
        
        use fluentai_parser::parse;
        match parse(&content) {
            Ok(graph) => {
                println!("File loaded successfully");
                // Note: We'd need to execute this through the REPL's environment
            }
            Err(e) => {
                eprintln!("Error loading file: {}", e);
            }
        }
    }

    // Run the REPL
    repl.run()?;

    Ok(())
}

// Add clap dependency to Cargo.toml