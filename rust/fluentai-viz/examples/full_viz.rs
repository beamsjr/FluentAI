//! Properly integrated visualization example

use anyhow::Result;
use fluentai_parser::parse;
use fluentai_viz::{
    debug::{debug_channel, serialize_value, timestamp_micros, DebugEvent as VizDebugEvent},
    layout::ASTLayouter,
    serializer::VisualizationMessage,
    ServerConfig, VisualizationServer,
};
use fluentai_vm::{Compiler, DebugConfig, VMDebugEvent, VM};
use std::path::PathBuf;
use tokio::sync::mpsc;

/// Convert VM debug event to visualization debug event
fn convert_debug_event(vm_event: VMDebugEvent) -> VizDebugEvent {
    match vm_event {
        VMDebugEvent::PreInstruction {
            pc,
            instruction,
            stack_size,
        } => VizDebugEvent::InstructionPre {
            timestamp: timestamp_micros(),
            pc,
            instruction: (&instruction).into(),
            stack_size,
        },
        VMDebugEvent::PostInstruction {
            pc,
            stack_size,
            stack_top,
        } => VizDebugEvent::InstructionPost {
            timestamp: timestamp_micros(),
            pc,
            stack_size,
            stack_top: stack_top.map(|v| serialize_value(&v)),
        },
        VMDebugEvent::FunctionCall {
            name,
            arg_count,
            call_depth,
        } => VizDebugEvent::FunctionCall {
            timestamp: timestamp_micros(),
            name,
            arg_count,
            call_depth,
        },
        VMDebugEvent::FunctionReturn { value, call_depth } => VizDebugEvent::FunctionReturn {
            timestamp: timestamp_micros(),
            value: serialize_value(&value),
            call_depth,
        },
        VMDebugEvent::StackPush { value } => VizDebugEvent::StackPush {
            timestamp: timestamp_micros(),
            value: serialize_value(&value),
            stack_size: 0,
        },
        VMDebugEvent::StackPop { value } => VizDebugEvent::StackPop {
            timestamp: timestamp_micros(),
            value: serialize_value(&value),
            stack_size: 0,
        },
        VMDebugEvent::Error { message, pc } => VizDebugEvent::Error {
            timestamp: timestamp_micros(),
            message,
            pc,
        },
        VMDebugEvent::Breakpoint { pc } => VizDebugEvent::BreakpointHit {
            timestamp: timestamp_micros(),
            pc,
            breakpoint_id: 0,
        },
        _ => VizDebugEvent::Error {
            timestamp: timestamp_micros(),
            message: "Unknown event type".to_string(),
            pc: None,
        },
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    println!("===========================================");
    println!("FluentAi Visualization - Proper Example");
    println!("===========================================");

    // Create channels
    let (vm_debug_tx, mut vm_debug_rx) = mpsc::unbounded_channel::<VMDebugEvent>();
    let (viz_debug_tx, viz_debug_rx) = debug_channel();

    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("fluentai-viz/static"),
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
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }
    });

    // Spawn the server
    let server_task = tokio::spawn(async move { server.run().await });

    // Wait for server to start
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;

    println!("Server started on http://127.0.0.1:8080");
    println!("Open this URL in your browser to see the visualization");
    println!();

    // Parse program
    let source = "(+ (* 3 4) (* 5 6))";
    println!("Program: {}", source);

    let ast = parse(source)?;

    // Layout and send AST graph
    let layouter = ASTLayouter::default();
    let graph_layout = layouter.layout(&ast);

    // Send AST graph to clients
    server_handle
        .broadcast_message(VisualizationMessage::ASTGraph {
            graph: graph_layout,
        })
        .await;

    println!("AST graph sent to visualization");

    // Wait before starting execution
    println!("Starting execution in 3 seconds...");
    tokio::time::sleep(std::time::Duration::from_secs(3)).await;

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
            println!("Result: {:?}", result);
            let _ = viz_debug_tx.send(VizDebugEvent::Stopped {
                timestamp: timestamp_micros(),
                reason: fluentai_viz::debug::StopReason::Completed,
            });
        }
        Err(e) => {
            eprintln!("VM Error: {}", e);
            let _ = viz_debug_tx.send(VizDebugEvent::Stopped {
                timestamp: timestamp_micros(),
                reason: fluentai_viz::debug::StopReason::Error(e.to_string()),
            });
        }
    }

    println!();
    println!("Execution complete. Server still running.");
    println!("Press Ctrl+C to stop.");

    // Wait for server
    server_task.await??;

    Ok(())
}
