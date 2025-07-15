//! Run command implementation

use crate::config::Config;
use anyhow::Result;
use fluentai_optimizer::OptimizationLevel;
use std::path::Path;

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
    optimization: u8,
    watch: bool,
    _config: &Config,
) -> Result<()> {
    let opt_level = match optimization {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Basic,
        2 => OptimizationLevel::Standard,
        _ => OptimizationLevel::Aggressive,
    };

    if watch {
        run_with_watch(path, args, viz_config, opt_level).await
    } else {
        println!(
            "Running: {} (optimization: {:?})",
            path.display(),
            opt_level
        );

        let result = crate::runner::run_file(path, args, viz_config, opt_level).await?;

        println!("\nResult: {}", result);

        Ok(())
    }
}

/// Run file with file watching enabled
async fn run_with_watch(
    path: &Path,
    args: Vec<String>,
    viz_config: Option<VisualizationConfig>,
    opt_level: OptimizationLevel,
) -> Result<()> {
    use notify::{Watcher, RecursiveMode, Event, EventKind};
    use std::sync::mpsc::channel;
    use std::time::Duration;
    
    println!("Watching {} for changes... (Ctrl+C to stop)", path.display());
    
    // Run once initially
    println!("\n{}", "=".repeat(60));
    println!(
        "Running: {} (optimization: {:?})",
        path.display(),
        opt_level
    );
    
    match crate::runner::run_file(path, args.clone(), viz_config.clone(), opt_level).await {
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
                            "Running: {} (optimization: {:?})",
                            path.display(),
                            opt_level
                        );
                        
                        match crate::runner::run_file(path, args.clone(), viz_config.clone(), opt_level).await {
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
