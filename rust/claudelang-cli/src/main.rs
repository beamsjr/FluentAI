//! ClaudeLang CLI - Main interpreter and tools

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

mod commands;
mod config;
mod runner;

use commands::{package, repl, run};

#[derive(Parser)]
#[command(name = "claudelang")]
#[command(about = "ClaudeLang interpreter and tools", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
    
    /// Path to configuration file
    #[arg(long, global = true)]
    config: Option<PathBuf>,
    
    /// Enable debug output
    #[arg(long, global = true)]
    debug: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a ClaudeLang program
    Run {
        /// Path to the ClaudeLang file
        file: PathBuf,
        
        /// Optimization level (0=none, 1=basic, 2=standard, 3=aggressive)
        #[arg(short = 'O', long = "opt", default_value = "2")]
        optimization: u8,
        
        /// Enable visualization
        #[cfg(feature = "visualization")]
        #[arg(long, short = 'v')]
        visualize: bool,
        
        /// Visualization server port
        #[cfg(feature = "visualization")]
        #[arg(long, default_value = "8080")]
        viz_port: u16,
        
        /// Delay between execution steps (ms)
        #[cfg(feature = "visualization")]
        #[arg(long, default_value = "0")]
        viz_delay: u64,
        
        /// Automatically open browser
        #[cfg(feature = "visualization")]
        #[arg(long)]
        viz_open: bool,
        
        /// Program arguments
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
    
    /// Start interactive REPL
    Repl {
        /// Enable visualization
        #[cfg(feature = "visualization")]
        #[arg(long, short = 'v')]
        visualize: bool,
        
        /// Visualization server port
        #[cfg(feature = "visualization")]
        #[arg(long, default_value = "8080")]
        viz_port: u16,
    },
    
    /// Package management commands
    #[command(subcommand)]
    Package(PackageCommands),
    
    /// Run with visualization (shortcut for run --visualize)
    #[cfg(feature = "visualization")]
    Viz {
        /// Path to the ClaudeLang file
        file: PathBuf,
        
        /// Server port
        #[arg(long, default_value = "8080")]
        port: u16,
        
        /// Automatically open browser
        #[arg(long)]
        open: bool,
        
        /// Program arguments
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
}

#[derive(Subcommand)]
enum PackageCommands {
    /// Initialize a new package
    Init {
        /// Package name
        #[arg(long)]
        name: Option<String>,
    },
    
    /// Install dependencies
    Install {
        /// Include dev dependencies
        #[arg(long)]
        dev: bool,
    },
    
    /// Add a dependency
    Add {
        /// Package name
        package: String,
        
        /// Version requirement
        #[arg(long)]
        version: Option<String>,
        
        /// Add as dev dependency
        #[arg(long)]
        dev: bool,
    },
    
    /// Remove a dependency
    Remove {
        /// Package name
        package: String,
    },
    
    /// Update dependencies
    Update {
        /// Specific packages to update
        packages: Vec<String>,
    },
    
    /// List installed packages
    List {
        /// Show as tree
        #[arg(long)]
        tree: bool,
    },
    
    /// Search for packages
    Search {
        /// Search query
        query: String,
        
        /// Maximum results
        #[arg(long, default_value = "20")]
        limit: usize,
    },
    
    /// Publish a package
    Publish {
        /// Authentication token
        #[arg(long, env = "CLAUDELANG_TOKEN")]
        token: String,
    },
    
    /// Run a package script
    Run {
        /// Script name
        script: String,
        
        /// Script arguments
        args: Vec<String>,
    },
    
    /// Clean unused packages
    Clean,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    
    // Initialize logging
    tracing_subscriber::fmt::init();
    
    // Load configuration
    let config = config::load_config(cli.config)?;
    
    // Handle commands
    match cli.command {
        Some(Commands::Run { 
            file, 
            optimization,
            #[cfg(feature = "visualization")]
            visualize,
            #[cfg(feature = "visualization")]
            viz_port,
            #[cfg(feature = "visualization")]
            viz_delay,
            #[cfg(feature = "visualization")]
            viz_open,
            args 
        }) => {
            #[cfg(feature = "visualization")]
            let viz_config = if visualize {
                Some(run::VisualizationConfig {
                    port: viz_port,
                    delay_ms: viz_delay,
                    auto_open: viz_open,
                })
            } else {
                None
            };
            
            #[cfg(not(feature = "visualization"))]
            let viz_config = None;
            
            run::run_file(&file, args, viz_config, optimization, &config).await?;
        }
        
        #[cfg(feature = "visualization")]
        Some(Commands::Viz { file, port, open, args }) => {
            let viz_config = Some(run::VisualizationConfig {
                port,
                delay_ms: 0,
                auto_open: open,
            });
            run::run_file(&file, args, viz_config, 2, &config).await?; // Default to standard optimization
        }
        
        Some(Commands::Repl { 
            #[cfg(feature = "visualization")]
            visualize,
            #[cfg(feature = "visualization")]
            viz_port,
        }) => {
            #[cfg(feature = "visualization")]
            let viz_config = if visualize {
                Some(repl::VisualizationConfig {
                    port: viz_port,
                })
            } else {
                None
            };
            
            #[cfg(not(feature = "visualization"))]
            let viz_config = None;
            
            repl::start_repl(viz_config, &config).await?;
        }
        
        Some(Commands::Package(cmd)) => {
            package::handle_package_command(cmd, &config).await?;
        }
        
        None => {
            // No command specified, start REPL
            repl::start_repl(None, &config).await?;
        }
    }
    
    Ok(())
}