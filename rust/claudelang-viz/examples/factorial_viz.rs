//! Factorial visualization - shows recursive function calls

use anyhow::Result;
use claudelang_parser::parse;
use claudelang_vm::{Compiler, VM, DebugConfig, VMDebugEvent};
use claudelang_viz::{
    VisualizationServer, ServerConfig,
    debug::{DebugEvent as VizDebugEvent, debug_channel, serialize_value, timestamp_micros},
    layout::ASTLayouter,
    serializer::VisualizationMessage,
};
use std::path::PathBuf;
use tokio::sync::mpsc;

/// Convert VM debug event to visualization debug event
fn convert_debug_event(vm_event: VMDebugEvent) -> VizDebugEvent {
    match vm_event {
        VMDebugEvent::PreInstruction { pc, instruction, stack_size } => {
            VizDebugEvent::InstructionPre {
                timestamp: timestamp_micros(),
                pc,
                instruction: (&instruction).into(),
                stack_size,
            }
        }
        VMDebugEvent::PostInstruction { pc, stack_size, stack_top } => {
            VizDebugEvent::InstructionPost {
                timestamp: timestamp_micros(),
                pc,
                stack_size,
                stack_top: stack_top.map(|v| serialize_value(&v)),
            }
        }
        VMDebugEvent::FunctionCall { name, arg_count, call_depth } => {
            VizDebugEvent::FunctionCall {
                timestamp: timestamp_micros(),
                name,
                arg_count,
                call_depth,
            }
        }
        VMDebugEvent::FunctionReturn { value, call_depth } => {
            VizDebugEvent::FunctionReturn {
                timestamp: timestamp_micros(),
                value: serialize_value(&value),
                call_depth,
            }
        }
        VMDebugEvent::StackPush { value } => {
            VizDebugEvent::StackPush {
                timestamp: timestamp_micros(),
                value: serialize_value(&value),
                stack_size: 0,
            }
        }
        VMDebugEvent::StackPop { value } => {
            VizDebugEvent::StackPop {
                timestamp: timestamp_micros(),
                value: serialize_value(&value),
                stack_size: 0,
            }
        }
        VMDebugEvent::Error { message, pc } => {
            VizDebugEvent::Error {
                timestamp: timestamp_micros(),
                message,
                pc,
            }
        }
        VMDebugEvent::Breakpoint { pc } => {
            VizDebugEvent::BreakpointHit {
                timestamp: timestamp_micros(),
                pc,
                breakpoint_id: 0,
            }
        }
        _ => {
            VizDebugEvent::Error {
                timestamp: timestamp_micros(),
                message: "Unknown event type".to_string(),
                pc: None,
            }
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();
    
    println!("===========================================");
    println!("ClaudeLang Factorial Visualization");
    println!("===========================================");
    
    // Create channels
    let (vm_debug_tx, mut vm_debug_rx) = mpsc::unbounded_channel::<VMDebugEvent>();
    let (viz_debug_tx, viz_debug_rx) = debug_channel();
    
    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("claudelang-viz/static"),
    };
    
    // Create visualization server with debug receiver
    let mut server = VisualizationServer::new(config);
    server.set_debug_receiver(viz_debug_rx);
    
    // Get a handle to interact with the server
    let server_handle = server.handle();
    
    // Convert VM debug events to viz debug events
    let viz_debug_tx_clone = viz_debug_tx.clone();
    tokio::spawn(async move {
        while let Some(vm_event) = vm_debug_rx.recv().await {
            let viz_event = convert_debug_event(vm_event);
            let _ = viz_debug_tx_clone.send(viz_event);
            // Add delay to see visualization
            tokio::time::sleep(std::time::Duration::from_millis(200)).await;
        }
    });
    
    // Spawn the server
    let server_task = tokio::spawn(async move {
        server.run().await
    });
    
    // Wait for server to start
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;
    
    println!("Server started on http://127.0.0.1:8080");
    println!("Open this URL in your browser to see the visualization");
    println!();
    
    // Factorial with helper functions
    let source = r#"
        (letrec ((factorial (lambda (n)
                              (if (= n 0)
                                  1
                                  (* n (factorial (- n 1))))))
                 (double (lambda (x) (* x 2)))
                 (add-one (lambda (x) (+ x 1))))
          (+ (factorial 4)
             (double (add-one 5))))
    "#;
    
    println!("Program: Factorial with helper functions");
    println!("{}", source);
    println!();
    println!("This will calculate:");
    println!("- factorial(4) = 4! = 24");
    println!("- add-one(5) = 6");
    println!("- double(6) = 12");
    println!("- Result: 24 + 12 = 36");
    println!();
    println!("The AST will show:");
    println!("- Multiple function definitions");
    println!("- Recursive calls");
    println!("- Function applications");
    
    let ast = parse(source)?;
    
    // Layout and send AST graph
    let layouter = ASTLayouter::default();
    let graph_layout = layouter.layout(&ast);
    
    println!();
    println!("Graph statistics:");
    println!("  Total nodes: {}", graph_layout.nodes.len());
    println!("  Total edges: {}", graph_layout.edges.len());
    println!("  Graph dimensions: {}x{}", graph_layout.width, graph_layout.height);
    
    // Send AST graph to clients
    server_handle.broadcast_message(VisualizationMessage::ASTGraph { 
        graph: graph_layout 
    }).await;
    
    println!();
    println!("AST graph sent to visualization");
    
    // Wait before starting execution
    println!("Starting execution in 5 seconds...");
    println!("Watch for:");
    println!("- Recursive factorial calls");
    println!("- Stack depth changes");
    println!("- Function call/return events");
    tokio::time::sleep(std::time::Duration::from_secs(5)).await;
    
    // Send start event
    let _ = viz_debug_tx.send(VizDebugEvent::Started {
        timestamp: timestamp_micros(),
    });
    
    // Compile and run
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    // Create VM with debug support
    let mut vm = VM::new(bytecode);
    let debug_config = DebugConfig::with_events(vm_debug_tx);
    vm.set_debug_config(debug_config);
    
    // Run the VM
    match vm.run() {
        Ok(result) => {
            println!();
            println!("Result: {:?}", result);
            println!("Expected: Int(36)");
            let _ = viz_debug_tx.send(VizDebugEvent::Stopped {
                timestamp: timestamp_micros(),
                reason: claudelang_viz::debug::StopReason::Completed,
            });
        }
        Err(e) => {
            eprintln!("VM Error: {}", e);
            let _ = viz_debug_tx.send(VizDebugEvent::Stopped {
                timestamp: timestamp_micros(),
                reason: claudelang_viz::debug::StopReason::Error(e.to_string()),
            });
        }
    }
    
    println!();
    println!("Execution complete. Server still running.");
    println!("The debug log shows the recursive calls!");
    println!("Press Ctrl+C to stop.");
    
    // Wait for server
    server_task.await??;
    
    Ok(())
}