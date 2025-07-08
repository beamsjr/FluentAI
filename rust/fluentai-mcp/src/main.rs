//! MCP Server for FluentAi

use anyhow::Result;
use clap::Parser;
use tracing::info;
use tracing_subscriber;

mod server;
mod handlers;
mod transport;

use server::McpServer;
use transport::TransportType;

/// FluentAi MCP Server
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Transport type to use
    #[arg(short, long, value_enum, default_value = "stdio")]
    transport: TransportMode,
    
    /// Port to listen on (for HTTP transport)
    #[arg(short, long, default_value = "3000")]
    port: u16,
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum TransportMode {
    /// Traditional stdio transport
    Stdio,
    /// HTTP transport with SSE
    Http,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();
    
    // Parse command line arguments
    let args = Args::parse();
    
    info!("Starting FluentAi MCP Server");
    
    // Create the appropriate transport
    let transport_type = match args.transport {
        TransportMode::Stdio => {
            info!("Using stdio transport");
            TransportType::Stdio
        }
        TransportMode::Http => {
            info!("Using HTTP transport on port {}", args.port);
            TransportType::Http { port: args.port }
        }
    };
    
    let transport = transport_type.create().await?;
    
    // Create and run the MCP server
    let server = McpServer::new(transport).await?;
    server.run().await?;
    
    Ok(())
}