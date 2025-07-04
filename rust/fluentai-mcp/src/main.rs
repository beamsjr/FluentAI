//! MCP Server for FluentAi

use anyhow::Result;
use tracing::info;
use tracing_subscriber;

mod server;
mod handlers;

use server::McpServer;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();
    
    info!("Starting FluentAi MCP Server");
    
    // Create and run the MCP server
    let server = McpServer::new();
    server.run().await?;
    
    Ok(())
}