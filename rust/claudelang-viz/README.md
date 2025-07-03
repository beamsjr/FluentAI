# ClaudeLang Visualizer

Real-time visualization system for ClaudeLang that displays AST structure and VM execution state in an interactive web interface.

## Features

- **AST Graph Visualization**: Interactive node-based representation of program structure
- **Real-time VM State**: Live updates of stack, locals, and call stack during execution
- **Debug Controls**: Step-through debugging with breakpoints
- **WebSocket Communication**: Low-latency bidirectional updates
- **Dark Theme UI**: Optimized for extended debugging sessions

## Architecture

```
┌─────────────────┐     WebSocket      ┌─────────────────┐
│                 │ ◄─────────────────► │                 │
│   Web Browser   │                     │ Viz Server      │
│   (D3.js UI)    │                     │ (Axum/Tokio)    │
│                 │                     │                 │
└─────────────────┘                     └────────┬────────┘
                                                 │
                                                 │ Debug Events
                                                 │
                                        ┌────────▼────────┐
                                        │                 │
                                        │   ClaudeLang    │
                                        │       VM        │
                                        │                 │
                                        └─────────────────┘
```

## Usage

### Basic Example

```rust
use claudelang_viz::{VisualizationServer, ServerConfig};
use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<()> {
    let config = ServerConfig {
        host: "127.0.0.1".to_string(),
        port: 8080,
        static_dir: PathBuf::from("static"),
    };
    
    let server = VisualizationServer::new(config);
    
    println!("Open http://127.0.0.1:8080 in your browser");
    server.run().await?;
    
    Ok(())
}
```

### With VM Integration

```rust
use claudelang_vm::{VM, DebugConfig};

// Create VM with debug support
let mut vm = VM::new(bytecode);
let (debug_tx, debug_rx) = mpsc::unbounded_channel();
let debug_config = DebugConfig::with_events(debug_tx);
vm.set_debug_config(debug_config);

// Process debug events
server.process_debug_events(debug_rx).await;
```

## Debug Events

The system supports various debug event types:

- **InstructionPre/Post**: Before and after instruction execution
- **StackPush/Pop**: Stack operations
- **FunctionCall/Return**: Function boundaries
- **VariableBind**: Variable assignments
- **Breakpoint**: Breakpoint hits
- **Error**: Runtime errors

## Web Interface

### Controls
- **Start**: Begin or resume execution
- **Pause**: Pause execution
- **Step**: Execute one instruction
- **Reset**: Reset VM state

### Views
- **Code Editor**: Load and edit ClaudeLang programs
- **AST Graph**: Interactive graph visualization using D3.js
- **Stack View**: Current stack contents
- **Locals View**: Local variable bindings
- **Call Stack**: Function call hierarchy
- **Debug Log**: Chronological event stream

## Building

```bash
cargo build -p claudelang-viz
```

## Running Examples

```bash
# Basic visualization server
cargo run --example viz_demo

# Integrated example with VM
cargo run --example integrated_viz
```

## Development

The visualizer consists of:

1. **Rust Backend** (`src/`)
   - `server.rs`: HTTP/WebSocket server
   - `debug.rs`: Debug event definitions
   - `layout.rs`: Graph layout algorithms
   - `serializer.rs`: State serialization

2. **Web Frontend** (`static/`)
   - `index.html`: UI structure
   - `style.css`: Dark theme styling
   - `visualizer.js`: Client-side logic

## Future Enhancements

- [ ] Heatmap visualization for hot code paths
- [ ] Memory usage tracking
- [ ] Effect flow visualization
- [ ] Time-travel debugging
- [ ] Export/import debug sessions
- [ ] Collaborative debugging support

## License

Part of the ClaudeLang project.