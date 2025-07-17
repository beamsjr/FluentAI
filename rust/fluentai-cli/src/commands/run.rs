//! Run command implementation

use crate::config::Config;
use anyhow::Result;
use fluentai_core::traits::OptimizationLevel;
use std::path::Path;

#[derive(Debug, Clone)]
pub enum RunOptimization {
    Manual(OptimizationLevel),
    #[cfg(feature = "ai-analysis")]
    AI,
    #[cfg(feature = "ai-analysis")]
    Hybrid(OptimizationLevel),
    #[cfg(all(feature = "ai-analysis", feature = "rl"))]
    ReinforcementLearning,
}

#[derive(Debug, Clone)]
pub struct VisualizationConfig {
    pub port: u16,
    pub delay_ms: u64,
    pub auto_open: bool,
    pub static_dir: Option<std::path::PathBuf>,
}

pub async fn run_file(
    path: &Path,
    args: Vec<String>,
    viz_config: Option<VisualizationConfig>,
    optimization: String,
    watch: bool,
    learning_mode: bool,
    _config: &Config,
) -> Result<()> {
    let opt_config = match optimization.as_str() {
        "0" | "none" => RunOptimization::Manual(OptimizationLevel::None),
        "1" | "basic" => RunOptimization::Manual(OptimizationLevel::Basic),
        "2" | "standard" => RunOptimization::Manual(OptimizationLevel::Standard),
        "3" | "aggressive" => RunOptimization::Manual(OptimizationLevel::Aggressive),
        #[cfg(feature = "ai-analysis")]
        "ai" => RunOptimization::AI,
        #[cfg(feature = "ai-analysis")]
        "hybrid" => RunOptimization::Hybrid(OptimizationLevel::Standard),
        #[cfg(all(feature = "ai-analysis", feature = "rl"))]
        "rl" => RunOptimization::ReinforcementLearning,
        _ => {
            // Try to parse as number for backward compatibility
            match optimization.parse::<u8>() {
                Ok(0) => RunOptimization::Manual(OptimizationLevel::None),
                Ok(1) => RunOptimization::Manual(OptimizationLevel::Basic),
                Ok(2) => RunOptimization::Manual(OptimizationLevel::Standard),
                Ok(_) => RunOptimization::Manual(OptimizationLevel::Aggressive),
                Err(_) => {
                    eprintln!("Invalid optimization level: {}", optimization);
                    eprintln!("Valid options: 0/none, 1/basic, 2/standard, 3/aggressive");
                    #[cfg(feature = "ai-analysis")]
                    eprintln!("               ai, hybrid");
                    #[cfg(all(feature = "ai-analysis", feature = "rl"))]
                    eprintln!("               rl");
                    return Err(anyhow::anyhow!("Invalid optimization level"));
                }
            }
        }
    };

    if watch {
        run_with_watch(path, args, viz_config, opt_config, learning_mode).await
    } else {
        println!(
            "Running: {} (optimization: {:?}, learning mode: {})",
            path.display(),
            opt_config,
            if learning_mode { "enabled" } else { "disabled" }
        );

        let result = crate::runner::run_file_with_opt(path, args, viz_config, opt_config, learning_mode).await?;

        println!("\nResult: {}", result);

        Ok(())
    }
}

/// Run file with file watching enabled
async fn run_with_watch(
    path: &Path,
    args: Vec<String>,
    viz_config: Option<VisualizationConfig>,
    opt_config: RunOptimization,
    learning_mode: bool,
) -> Result<()> {
    use notify::{Watcher, RecursiveMode, Event, EventKind};
    use std::sync::mpsc::channel;
    use std::time::Duration;
    
    println!("Watching {} for changes... (Ctrl+C to stop)", path.display());
    
    // Run once initially
    println!("\n{}", "=".repeat(60));
    println!(
        "Running: {} (optimization: {:?}, learning mode: {})",
        path.display(),
        opt_config,
        if learning_mode { "enabled" } else { "disabled" }
    );
    
    match crate::runner::run_file_with_opt(path, args.clone(), viz_config.clone(), opt_config.clone(), learning_mode).await {
        Ok(result) => println!("\nResult: {}", result),
        Err(e) => eprintln!("\nError: {}", e),
    }
    
    // Set up file watcher
    let (tx, rx) = channel();
    
    let mut watcher = notify::recommended_watcher(move |res: Result<Event, notify::Error>| {
        if let Ok(event) = res {
            let _ = tx.send(event);
        }
    })?;
    
    // Watch the file and its parent directory
    let watch_path = path.to_path_buf();
    let parent_dir = path.parent().unwrap_or(Path::new("."));
    watcher.watch(parent_dir, RecursiveMode::NonRecursive)?;
    
    println!("\nWatching for changes...");
    
    // Event loop
    loop {
        match rx.recv() {
            Ok(event) => {
                // Check if the event is for our file
                let is_our_file = event.paths.iter().any(|p| p == &watch_path);
                
                // Only rerun on modify or create events for our file
                match event.kind {
                    EventKind::Modify(_) | EventKind::Create(_) if is_our_file => {
                        // Small delay to ensure file write is complete
                        tokio::time::sleep(Duration::from_millis(100)).await;
                        
                        println!("\n{}", "=".repeat(60));
                        println!("File changed, rerunning...");
                        println!(
                            "Running: {} (optimization: {:?}, learning mode: {})",
                            path.display(),
                            opt_config,
                            if learning_mode { "enabled" } else { "disabled" }
                        );
                        
                        match crate::runner::run_file_with_opt(path, args.clone(), viz_config.clone(), opt_config.clone(), learning_mode).await {
                            Ok(result) => println!("\nResult: {}", result),
                            Err(e) => eprintln!("\nError: {}", e),
                        }
                        
                        println!("\nWatching for changes...");
                    }
                    _ => {} // Ignore other events
                }
            }
            Err(e) => {
                eprintln!("Watch error: {}", e);
                break;
            }
        }
    }
    
    Ok(())
}
