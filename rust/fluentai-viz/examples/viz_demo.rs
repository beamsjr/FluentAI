//! Example demonstrating the FluentAi visualizer

use anyhow::Result;
use fluentai_viz::{ServerConfig, VisualizationServer};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("fluentai-viz/static"),
    };

    // Create and start server
    let server = VisualizationServer::new(config);

    println!("Starting FluentAi Visualizer...");
    println!("Open http://127.0.0.1:8080 in your browser");

    server.run().await?;

    Ok(())
}
