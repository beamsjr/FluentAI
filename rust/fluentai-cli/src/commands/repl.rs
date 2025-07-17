//! REPL command implementation

use crate::config::Config;
use anyhow::Result;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::fs;

#[cfg(feature = "visualization")]
#[derive(Debug, Clone)]
pub struct VisualizationConfig {
    pub port: u16,
}

#[cfg(feature = "visualization")]
struct ReplVisualization {
    server_handle: Option<tokio::task::JoinHandle<()>>,
    viz_handle: Option<fluentai_viz::server::ServerHandle>,
    enabled: bool,
}

#[cfg(feature = "visualization")]
impl ReplVisualization {
    fn new() -> Self {
        Self {
            server_handle: None,
            viz_handle: None,
            enabled: false,
        }
    }
    
    async fn start(&mut self, port: u16) -> Result<()> {
        if self.enabled {
            return Ok(());
        }
        
        // Create visualization server
        let config = fluentai_viz::server::ServerConfig {
            host: "127.0.0.1".to_string(),
            port,
            static_dir: std::path::PathBuf::from("static"),
        };
        
        let server = fluentai_viz::server::VisualizationServer::new(config);
        self.viz_handle = Some(server.handle());
        
        // Start server in background
        let handle = tokio::spawn(async move {
            if let Err(e) = server.run().await {
                eprintln!("Visualization server error: {}", e);
            }
        });
        
        self.server_handle = Some(handle);
        self.enabled = true;
        
        println!("Visualization server started on http://127.0.0.1:{}", port);
        
        // Try to open browser
        if let Err(e) = webbrowser::open(&format!("http://127.0.0.1:{}", port)) {
            eprintln!("Failed to open browser: {}", e);
        }
        
        Ok(())
    }
    
    async fn stop(&mut self) {
        if let Some(handle) = self.server_handle.take() {
            handle.abort();
        }
        self.viz_handle = None;
        self.enabled = false;
        println!("Visualization server stopped");
    }
}

/// REPL session manager for saving/loading sessions
struct SessionManager {
    history_file: PathBuf,
    transcript_file: Option<PathBuf>,
    history: Vec<String>,
}

impl SessionManager {
    fn new() -> Result<Self> {
        // Get history file path in user's config directory
        let config_dir = dirs::config_dir()
            .ok_or_else(|| anyhow::anyhow!("Could not find config directory"))?
            .join("fluentai");
        
        fs::create_dir_all(&config_dir)?;
        
        let history_file = config_dir.join("repl_history.txt");
        
        let mut manager = Self {
            history_file,
            transcript_file: None,
            history: Vec::new(),
        };
        
        // Load existing history
        manager.load_history()?;
        
        Ok(manager)
    }
    
    fn load_history(&mut self) -> Result<()> {
        if self.history_file.exists() {
            let content = fs::read_to_string(&self.history_file)?;
            self.history = content.lines().map(String::from).collect();
        }
        Ok(())
    }
    
    fn save_history(&self) -> Result<()> {
        let content = self.history.join("\n");
        fs::write(&self.history_file, content)?;
        Ok(())
    }
    
    fn add_to_history(&mut self, command: &str) {
        // Don't add empty commands or duplicates
        if !command.is_empty() && self.history.last() != Some(&command.to_string()) {
            self.history.push(command.to_string());
        }
    }
    
    fn start_transcript(&mut self, path: PathBuf) -> Result<()> {
        self.transcript_file = Some(path);
        if let Some(ref path) = self.transcript_file {
            fs::write(path, format!("FluentAI REPL Session - {}\n\n", chrono::Local::now()))?;
        }
        Ok(())
    }
    
    fn append_to_transcript(&self, content: &str) -> Result<()> {
        if let Some(ref path) = self.transcript_file {
            use std::fs::OpenOptions;
            use std::io::Write;
            
            let mut file = OpenOptions::new()
                .append(true)
                .create(true)
                .open(path)?;
            
            writeln!(file, "{}", content)?;
        }
        Ok(())
    }
    
    fn stop_transcript(&mut self) {
        if self.transcript_file.is_some() {
            println!("Session transcript saved");
            self.transcript_file = None;
        }
    }
}

pub async fn start_repl(viz_config: Option<VisualizationConfig>, config: &Config) -> Result<()> {
    println!("FluentAi REPL");
    println!("Type :help for commands, :quit to exit\n");

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    
    // Initialize session manager
    let mut session = SessionManager::new()?;
    
    #[cfg(feature = "visualization")]
    let mut visualization = ReplVisualization::new();
    
    #[cfg(feature = "visualization")]
    let viz_port = viz_config.as_ref().map(|c| c.port).unwrap_or(8080);

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
                ":history" => {
                    // Show command history
                    println!("Command history:");
                    for (i, cmd) in session.history.iter().enumerate() {
                        println!("  {}: {}", i + 1, cmd);
                    }
                }
                ":save" => {
                    // Save current history
                    if let Err(e) = session.save_history() {
                        eprintln!("Failed to save history: {}", e);
                    } else {
                        println!("History saved");
                    }
                }
                cmd if cmd.starts_with(":save ") => {
                    // Save session transcript
                    let path = cmd.trim_start_matches(":save ").trim();
                    if let Err(e) = session.start_transcript(PathBuf::from(path)) {
                        eprintln!("Failed to start transcript: {}", e);
                    } else {
                        println!("Recording session to: {}", path);
                    }
                }
                ":load" => {
                    // Reload history
                    if let Err(e) = session.load_history() {
                        eprintln!("Failed to load history: {}", e);
                    } else {
                        println!("History reloaded");
                    }
                }
                #[cfg(feature = "visualization")]
                ":viz on" => {
                    if let Err(e) = visualization.start(viz_port).await {
                        eprintln!("Failed to start visualization: {}", e);
                    }
                }
                #[cfg(feature = "visualization")]
                ":viz off" => {
                    visualization.stop().await;
                }
                _ => println!("Unknown command: {}", input),
            }
            
            // Add command to history
            session.add_to_history(input);
            
            // Add to transcript
            let _ = session.append_to_transcript(&format!("> {}", input));
            
            continue;
        }

        // Skip empty lines
        if input.is_empty() {
            continue;
        }
        
        // Add code to history and transcript
        session.add_to_history(input);
        let _ = session.append_to_transcript(&format!("> {}", input));

        // Execute code
        #[cfg(feature = "visualization")]
        {
            if visualization.enabled {
                // Run with visualization
                match run_code_with_viz(input, &visualization).await {
                    Ok(result) => {
                        let output = format!("=> {}", result);
                        println!("{}", output);
                        let _ = session.append_to_transcript(&output);
                    }
                    Err(e) => {
                        let error = format!("Error: {}", e);
                        eprintln!("{}", error);
                        let _ = session.append_to_transcript(&error);
                    }
                }
            } else {
                match crate::runner::run_code(input) {
                    Ok(result) => {
                        let output = format!("=> {}", result);
                        println!("{}", output);
                        let _ = session.append_to_transcript(&output);
                    }
                    Err(e) => {
                        let error = format!("Error: {}", e);
                        eprintln!("{}", error);
                        let _ = session.append_to_transcript(&error);
                    }
                }
            }
        }
        
        #[cfg(not(feature = "visualization"))]
        {
            match crate::runner::run_code(input) {
                Ok(result) => {
                    let output = format!("=> {}", result);
                    println!("{}", output);
                    let _ = session.append_to_transcript(&output);
                }
                Err(e) => {
                    let error = format!("Error: {}", e);
                    eprintln!("{}", error);
                    let _ = session.append_to_transcript(&error);
                }
            }
        }
        println!();
    }

    #[cfg(feature = "visualization")]
    {
        // Stop visualization server if running
        visualization.stop().await;
    }
    
    // Save history before exiting
    if let Err(e) = session.save_history() {
        eprintln!("Failed to save history: {}", e);
    }
    
    // Stop transcript if active
    session.stop_transcript();
    
    println!("Goodbye!");
    Ok(())
}

#[cfg(feature = "visualization")]
async fn run_code_with_viz(code: &str, viz: &ReplVisualization) -> Result<fluentai_core::value::Value> {
    use fluentai_parser::parse_flc;
    use fluentai_vm::{Compiler, CompilerOptions, VM};
    use fluentai_core::traits::OptimizationLevel;
    use fluentai_viz::serializer::VisualizationMessage;
    use fluentai_viz::layout::ASTLayouter;
    
    // Parse
    let ast = parse_flc(code)?;
    
    // Send AST visualization if server is running
    if let Some(ref handle) = viz.viz_handle {
        // Layout the AST
        let layouter = ASTLayouter::default();
        let graph = layouter.layout(&ast);
        
        // Send AST graph message
        handle.broadcast_message(VisualizationMessage::ASTGraph { graph }).await;
    }
    
    // Compile
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: true,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&ast)?;
    
    // Execute
    let mut vm = VM::new(bytecode);
    
    // TODO: Add VM state visualization during execution
    
    let result = vm.run()?;
    
    Ok(result)
}

fn print_help() {
    println!("REPL Commands:");
    println!("  :help, :h       - Show this help");
    println!("  :quit, :q       - Exit REPL");
    println!("  :ast            - Toggle AST display");
    println!("  :history        - Show command history");
    println!("  :save           - Save current history");
    println!("  :save <file>    - Start recording session to file");
    println!("  :load           - Reload history from disk");
    #[cfg(feature = "visualization")]
    println!("  :viz on/off     - Toggle visualization");
    println!();
    println!("History is automatically saved on exit and loaded on startup.");
    println!();
}
