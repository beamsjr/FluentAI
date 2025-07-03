//! Run command implementation

use anyhow::Result;
use std::path::Path;
use crate::config::Config;

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
    _config: &Config,
) -> Result<()> {
    println!("Running: {}", path.display());
    
    let result = crate::runner::run_file(path, args, viz_config).await?;
    
    println!("\nResult: {}", result);
    
    Ok(())
}