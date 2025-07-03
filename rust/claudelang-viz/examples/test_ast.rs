//! Test AST transmission

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
    println!("Testing AST Transmission");
    println!("===========================================");
    
    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8081, // Different port to avoid conflict
        static_dir: PathBuf::from("claudelang-viz/static"),
    };
    
    // Create visualization server
    let server = VisualizationServer::new(config);
    let server_handle = server.handle();
    
    // Spawn the server
    let server_task = tokio::spawn(async move {
        server.run().await
    });
    
    // Wait for server to start
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    
    println!("Server started on http://127.0.0.1:8081");
    println!();
    
    // Try different programs
    let programs = vec![
        ("Simple", "(+ 1 2)"),
        ("Nested", "(* (+ 1 2) (- 4 3))"),
        ("Let binding", "(let ((x 5)) (* x x))"),
    ];
    
    for (name, source) in programs {
        println!("Parsing {}: {}", name, source);
        
        match parse(source) {
            Ok(ast) => {
                let layouter = ASTLayouter::default();
                let graph_layout = layouter.layout(&ast);
                
                println!("  Nodes: {}, Edges: {}", 
                    graph_layout.nodes.len(), 
                    graph_layout.edges.len());
                
                // Wait 3 seconds between each
                tokio::time::sleep(std::time::Duration::from_secs(3)).await;
                
                println!("  Sending AST...");
                server_handle.broadcast_message(VisualizationMessage::ASTGraph { 
                    graph: graph_layout 
                }).await;
                println!("  Sent!");
            }
            Err(e) => {
                println!("  Parse error: {}", e);
            }
        }
        println!();
    }
    
    println!("All ASTs sent. Check http://127.0.0.1:8081");
    println!("Press Ctrl+C to stop.");
    
    // Keep running
    server_task.await??;
    
    Ok(())
}