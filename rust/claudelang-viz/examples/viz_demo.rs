//! Example demonstrating the ClaudeLang visualizer

use claudelang_viz::{VisualizationServer, ServerConfig};
use std::path::PathBuf;
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();
    
    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("claudelang-viz/static"),
    };
    
    // Create and start server
    let server = VisualizationServer::new(config);
    
    println!("Starting ClaudeLang Visualizer...");
    println!("Open http://127.0.0.1:8080 in your browser");
    
    server.run().await?;
    
    Ok(())
}