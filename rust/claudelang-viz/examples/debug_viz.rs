//! Debug visualization to test AST graph

use anyhow::Result;
use claudelang_parser::parse;
use claudelang_viz::{
    VisualizationServer, ServerConfig,
    layout::ASTLayouter,
    serializer::VisualizationMessage,
};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();
    
    println!("===========================================");
    println!("ClaudeLang Visualization Debug");
    println!("===========================================");
    
    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("claudelang-viz/static"),
    };
    
    // Create visualization server
    let server = VisualizationServer::new(config);
    
    // Get a handle to interact with the server
    let server_handle = server.handle();
    
    // Spawn the server
    let server_task = tokio::spawn(async move {
        server.run().await
    });
    
    // Wait for server to start
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    
    println!("Server started on http://127.0.0.1:8080");
    println!("Open browser and check console for debug messages");
    println!();
    
    // Parse program
    let source = "(+ 1 2)";
    println!("Simple program: {}", source);
    
    let ast = parse(source)?;
    
    // Layout and send AST graph
    let layouter = ASTLayouter::default();
    let graph_layout = layouter.layout(&ast);
    
    // Print debug info
    println!("Graph layout:");
    println!("  Nodes: {}", graph_layout.nodes.len());
    for node in &graph_layout.nodes {
        println!("    Node {}: {} at ({}, {})", 
            node.id, node.label, node.position.x, node.position.y);
    }
    println!("  Edges: {}", graph_layout.edges.len());
    for edge in &graph_layout.edges {
        println!("    Edge: {} -> {}", edge.source, edge.target);
    }
    println!("  Dimensions: {}x{}", graph_layout.width, graph_layout.height);
    
    // Wait a bit for clients to connect
    println!("\nWaiting 3 seconds for clients to connect...");
    tokio::time::sleep(std::time::Duration::from_secs(3)).await;
    
    // Send AST graph to clients
    println!("Sending AST graph...");
    server_handle.broadcast_message(VisualizationMessage::ASTGraph { 
        graph: graph_layout 
    }).await;
    
    println!("AST graph sent!");
    println!();
    println!("Check the browser console for any errors.");
    println!("The graph should show: + with children 1 and 2");
    println!();
    println!("Server running. Press Ctrl+C to stop.");
    
    // Keep running
    server_task.await??;
    
    Ok(())
}