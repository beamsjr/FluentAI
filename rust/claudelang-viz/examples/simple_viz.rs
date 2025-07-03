//! Simple visualization example - just shows the UI

use anyhow::Result;
use claudelang_viz::{VisualizationServer, ServerConfig};
use std::path::PathBuf;

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
    
    println!("===========================================");
    println!("ClaudeLang Visualizer");
    println!("===========================================");
    println!("Server starting...");
    println!("Open http://127.0.0.1:8080 in your browser");
    println!();
    println!("This demo just shows the UI without running");
    println!("any ClaudeLang code. You can explore the");
    println!("interface and see how it would look during");
    println!("program execution.");
    println!("===========================================");
    println!();
    println!("Press Ctrl+C to stop the server");
    
    server.run().await?;
    
    Ok(())
}