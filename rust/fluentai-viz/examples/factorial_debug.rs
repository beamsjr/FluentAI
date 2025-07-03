//! Debug factorial visualization

use anyhow::Result;
use fluentai_parser::parse;
use fluentai_viz::{
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
    println!("Factorial Visualization Debug");
    println!("===========================================");
    
    // Kill any existing servers
    println!("Killing any existing servers...");
    std::process::Command::new("lsof")
        .args(&["-ti:8080"])
        .output()
        .ok()
        .and_then(|output| {
            if !output.stdout.is_empty() {
                let pids = String::from_utf8_lossy(&output.stdout);
                for pid in pids.trim().lines() {
                    std::process::Command::new("kill")
                        .args(&["-9", pid])
                        .output()
                        .ok();
                }
            }
            Some(())
        });
    
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    
    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("fluentai-viz/static"),
    };
    
    // Create visualization server
    let server = VisualizationServer::new(config);
    let server_handle = server.handle();
    
    // Spawn the server
    let server_task = tokio::spawn(async move {
        server.run().await
    });
    
    // Wait for server to start
    tokio::time::sleep(std::time::Duration::from_secs(2)).await;
    
    println!("Server started on http://127.0.0.1:8080");
    println!();
    
    // Start with simple, then complex
    let programs = vec![
        ("Simple test", "(+ 1 2)"),
        ("Medium complexity", "(* (+ 1 2) (- 4 3))"),
        ("Factorial", r#"
            (letrec ((factorial (lambda (n)
                                  (if (= n 0)
                                      1
                                      (* n (factorial (- n 1)))))))
              (factorial 4))
        "#),
    ];
    
    for (name, source) in programs {
        println!("Testing: {}", name);
        println!("Source: {}", source.trim());
        
        match parse(source) {
            Ok(ast) => {
                let layouter = ASTLayouter::default();
                let graph_layout = layouter.layout(&ast);
                
                println!("\nGraph statistics:");
                println!("  Nodes: {}", graph_layout.nodes.len());
                println!("  Edges: {}", graph_layout.edges.len());
                println!("  Dimensions: {}x{}", graph_layout.width, graph_layout.height);
                
                // Print first few nodes
                println!("\n  First 5 nodes:");
                for (i, node) in graph_layout.nodes.iter().take(5).enumerate() {
                    println!("    {}: {} at ({}, {})", 
                        i, node.label, node.position.x, node.position.y);
                }
                
                println!("\n  Waiting 5 seconds for you to check browser...");
                tokio::time::sleep(std::time::Duration::from_secs(5)).await;
                
                println!("  Sending AST graph...");
                server_handle.broadcast_message(VisualizationMessage::ASTGraph { 
                    graph: graph_layout 
                }).await;
                println!("  Sent!\n");
                
                println!("  Check your browser now!");
                println!("  Press Enter to continue to next example...");
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).ok();
            }
            Err(e) => {
                println!("  Parse error: {}", e);
            }
        }
        println!("\n---\n");
    }
    
    println!("All tests complete.");
    println!("Server still running. Press Ctrl+C to stop.");
    
    // Keep running
    server_task.await??;
    
    Ok(())
}