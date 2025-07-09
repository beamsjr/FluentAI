//! REPL command implementation

use crate::config::Config;
use anyhow::Result;
use std::io::{self, Write};

#[cfg(feature = "visualization")]
#[derive(Debug, Clone)]
pub struct VisualizationConfig {
    pub port: u16,
}

pub async fn start_repl(_viz_config: Option<VisualizationConfig>, config: &Config) -> Result<()> {
    println!("FluentAi REPL");
    println!("Type :help for commands, :quit to exit\n");

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        // Print prompt
        print!("{}", config.repl.prompt);
        stdout.flush()?;

        // Read input
        let mut input = String::new();
        stdin.read_line(&mut input)?;
        let input = input.trim();

        // Handle REPL commands
        if input.starts_with(':') {
            match input {
                ":quit" | ":q" => break,
                ":help" | ":h" => print_help(),
                ":ast" => {
                    // Toggle AST display
                    println!("AST display toggled");
                }
                #[cfg(feature = "visualization")]
                ":viz on" => {
                    println!("Visualization enabled");
                    // TODO: Start visualization server
                }
                #[cfg(feature = "visualization")]
                ":viz off" => {
                    println!("Visualization disabled");
                    // TODO: Stop visualization server
                }
                _ => println!("Unknown command: {}", input),
            }
            continue;
        }

        // Skip empty lines
        if input.is_empty() {
            continue;
        }

        // Execute code
        match crate::runner::run_code(input) {
            Ok(result) => {
                println!("=> {}", result);
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }
        println!();
    }

    println!("Goodbye!");
    Ok(())
}

fn print_help() {
    println!("REPL Commands:");
    println!("  :help, :h     - Show this help");
    println!("  :quit, :q     - Exit REPL");
    println!("  :ast          - Toggle AST display");
    #[cfg(feature = "visualization")]
    println!("  :viz on/off   - Toggle visualization");
    println!();
}
