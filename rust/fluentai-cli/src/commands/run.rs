//! Run command implementation

use anyhow::Result;
use std::path::Path;
use crate::config::Config;
use fluentai_optimizer::OptimizationLevel;

#[derive(Debug, Clone)]
pub struct VisualizationConfig {
    pub port: u16,
    pub delay_ms: u64,
    pub auto_open: bool,
}

pub async fn run_file(
    path: &Path,
    args: Vec<String>,
    viz_config: Option<VisualizationConfig>,
    optimization: u8,
    _config: &Config,
) -> Result<()> {
    let opt_level = match optimization {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Basic,
        2 => OptimizationLevel::Standard,
        _ => OptimizationLevel::Aggressive,
    };
    
    println!("Running: {} (optimization: {:?})", path.display(), opt_level);
    
    let result = crate::runner::run_file(path, args, viz_config, opt_level).await?;
    
    println!("\nResult: {}", result);
    
    Ok(())
}