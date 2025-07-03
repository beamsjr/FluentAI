//! Core execution logic for running ClaudeLang programs

use anyhow::Result;
use claudelang_parser::parse;
use claudelang_vm::{Compiler, CompilerOptions, VM, bytecode::Value};
use claudelang_optimizer::OptimizationLevel;
use std::path::Path;

#[cfg(feature = "visualization")]
use claudelang_vm::{DebugConfig, VMDebugEvent};

#[cfg(feature = "visualization")]
use claudelang_viz::{
    VisualizationServer, ServerConfig,
    debug::{DebugEvent as VizDebugEvent, debug_channel, serialize_value, timestamp_micros},
    layout::ASTLayouter,
    serializer::VisualizationMessage,
};

#[cfg(feature = "visualization")]
use tokio::sync::mpsc;

/// Run ClaudeLang code and return the result
pub fn run_code(code: &str) -> Result<Value> {
    run_code_with_options(code, OptimizationLevel::Standard)
}

/// Run ClaudeLang code with specific optimization level
pub fn run_code_with_options(code: &str, opt_level: OptimizationLevel) -> Result<Value> {
    // Parse
    let ast = parse(code)?;
    
    // Compile with optimization
    let options = CompilerOptions {
        optimization_level: opt_level,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&ast)?;
    
    // Execute
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    Ok(result)
}

/// Run ClaudeLang code from a file
pub async fn run_file(
    path: &Path, 
    _args: Vec<String>,
    viz_config: Option<crate::commands::run::VisualizationConfig>,
    opt_level: OptimizationLevel,
) -> Result<Value> {
    let code = std::fs::read_to_string(path)?;
    
    #[cfg(feature = "visualization")]
    if let Some(viz) = viz_config {
        run_with_visualization(&code, viz, opt_level).await
    } else {
        run_code_with_options(&code, opt_level)
    }
    
    #[cfg(not(feature = "visualization"))]
    {
        let _ = viz_config; // Suppress warning
        run_code_with_options(&code, opt_level)
    }
}

#[cfg(feature = "visualization")]
/// Run code with visualization enabled
pub async fn run_with_visualization(
    code: &str,
    viz_config: crate::commands::run::VisualizationConfig,
    opt_level: OptimizationLevel,
) -> Result<Value> {
    use std::path::PathBuf;
    
    // Parse first to get AST
    let ast = parse(code)?;
    
    // Create channels
    let (vm_debug_tx, mut vm_debug_rx) = mpsc::unbounded_channel::<VMDebugEvent>();
    let (viz_debug_tx, viz_debug_rx) = debug_channel();
    
    // Create server configuration
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: viz_config.port,
        static_dir: PathBuf::from("claudelang-viz/static"), // TODO: Make configurable
    };
    
    // Create visualization server
    let mut server = VisualizationServer::new(config);
    server.set_debug_receiver(viz_debug_rx);
    
    // Get a handle to interact with the server
    let server_handle = server.handle();
    
    // Convert VM debug events to viz debug events
    let viz_debug_tx_clone = viz_debug_tx.clone();
    let delay_ms = viz_config.delay_ms;
    tokio::spawn(async move {
        while let Some(vm_event) = vm_debug_rx.recv().await {
            let viz_event = convert_debug_event(vm_event);
            let _ = viz_debug_tx_clone.send(viz_event);
            
            // Add delay if configured
            if delay_ms > 0 {
                tokio::time::sleep(tokio::time::Duration::from_millis(delay_ms)).await;
            }
        }
    });
    
    // Spawn the server
    let server_task = tokio::spawn(async move {
        server.run().await
    });
    
    // Wait for server to start
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    
    println!("Visualization server started on http://127.0.0.1:{}", viz_config.port);
    
    // Open browser if requested
    if viz_config.auto_open {
        let url = format!("http://127.0.0.1:{}", viz_config.port);
        if let Err(e) = webbrowser::open(&url) {
            eprintln!("Failed to open browser: {}", e);
        }
    }
    
    // Layout and send AST graph
    let layouter = ASTLayouter::default();
    let graph_layout = layouter.layout(&ast);
    
    server_handle.broadcast_message(VisualizationMessage::ASTGraph { 
        graph: graph_layout 
    }).await;
    
    // Send start event
    let _ = viz_debug_tx.send(VizDebugEvent::Started {
        timestamp: timestamp_micros(),
    });
    
    // Compile with optimization
    let options = CompilerOptions {
        optimization_level: opt_level,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&ast)?;
    
    // Create VM with debug support
    let mut vm = VM::new(bytecode);
    let debug_config = DebugConfig::with_events(vm_debug_tx);
    vm.set_debug_config(debug_config);
    
    // Run the VM
    let result = match vm.run() {
        Ok(value) => {
            let _ = viz_debug_tx.send(VizDebugEvent::Stopped {
                timestamp: timestamp_micros(),
                reason: claudelang_viz::debug::StopReason::Completed,
            });
            Ok(value)
        }
        Err(e) => {
            let _ = viz_debug_tx.send(VizDebugEvent::Stopped {
                timestamp: timestamp_micros(),
                reason: claudelang_viz::debug::StopReason::Error(e.to_string()),
            });
            Err(e)
        }
    };
    
    println!("\nVisualization server will continue running. Press Ctrl+C to stop.");
    
    // Keep server running in background
    tokio::spawn(async move {
        let _ = server_task.await;
    });
    
    // Give user time to explore visualization
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
    
    result
}

#[cfg(feature = "visualization")]
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