# FluentAI Developer Tools Guide

A comprehensive guide to using the FluentAI renderer's built-in developer tools for debugging, profiling, and optimizing your applications.

## Quick Start

The DevTools are built into the FluentAI renderer and can be activated with simple keyboard shortcuts:

- **F12** - Toggle DevTools master control
- **Ctrl+Shift+I** - Open Component Inspector
- **Ctrl+Shift+P** - Show Performance Profiler
- **Ctrl+R** - Force hot reload
- **D** - Toggle debug overlay (AR/3D scenes)

## Features Overview

### 1. DevTools Master Control (F12)

The main DevTools toggle that enables/disables all developer features. When enabled:
- Shows the DevTools overlay panel
- Activates keyboard shortcuts
- Enables performance monitoring
- Starts hot reload server

```rust
use fluentai_renderer::devtools::DevTools;

let mut devtools = DevTools::new();
devtools.enable();
```

### 2. Component Inspector (Ctrl+Shift+I)

Inspect and debug your UI component hierarchy in real-time.

#### Features:
- **Component Tree View** - Visualize the entire component hierarchy
- **Property Inspector** - View and modify component properties
- **Selection Highlighting** - Click to select and highlight components
- **Breakpoints** - Set breakpoints on component events

#### Usage Example:
```rust
// In your render loop
devtools.inspector().update_tree(&root_component, &context);

// Select a component
devtools.inspector().select_component(component_id);

// Get selected component properties
if let Some(props) = devtools.inspector().get_selected_properties() {
    println!("Position: {:?}", props.position);
    println!("Size: {:?}", props.size);
}
```

### 3. Performance Profiler (Ctrl+Shift+P)

Monitor your application's performance metrics in real-time.

#### Metrics Tracked:
- **FPS** - Frames per second (color-coded: green >55, yellow >30, red <30)
- **Frame Time** - Time to render each frame in milliseconds
- **Draw Calls** - Number of draw operations per frame
- **Memory Usage** - Current memory consumption
- **Component Count** - Total active components
- **Render Time Breakdown** - Layout vs render time

#### Usage Example:
```rust
// Start frame measurement
devtools.profiler().frame_start();

// Your render code here...

// End frame measurement
devtools.profiler().frame_end();

// Get stats
let stats = devtools.profiler().get_stats();
println!("FPS: {:.1}", stats.fps);
println!("Frame time: {:.2}ms", stats.frame_time_ms);
```

### 4. Hot Reload Server (Ctrl+R)

Automatically reload components when source files change.

#### Features:
- **File Watching** - Monitors `src/`, `assets/`, and `styles/` directories
- **WebSocket Server** - Pushes reload events to connected clients
- **Selective Reloading** - Only reloads affected components
- **State Preservation** - Maintains component state across reloads

#### Setup:
```rust
// Configure hot reload
devtools.hot_reload.add_watch_path(PathBuf::from("my_components"));

// Add reload callback
devtools.hot_reload.on_reload(|event| {
    match event.event_type {
        ReloadEventType::ComponentChanged => {
            // Reload component
        }
        ReloadEventType::StyleChanged => {
            // Reload styles
        }
        _ => {}
    }
});
```

### 5. UI Debugger

Debug UI layouts and interactions.

#### Features:
- **Layout Boundaries** - Show component bounding boxes
- **Padding/Margin Visualization** - See spacing visually
- **Event Flow** - Trace event propagation
- **Console Integration** - Log UI events to console

```rust
// Enable specific debug features
devtools.debugger().show_layout_bounds(true);
devtools.debugger().show_event_flow(true);

// Log UI events
devtools.debugger().log_events(true);
```

### 6. AR/3D Debug Overlay (D key)

Special debugging features for AR and 3D applications.

#### Visualizations:
- **Physics Bodies** - Green circles show physics centers
- **Collision Boundaries** - Yellow boxes show collision areas
- **Force Vectors** - Magenta arrows show forces/velocities
- **Spatial Anchors** - Blue markers show AR anchor points
- **Gesture Areas** - Colored zones show interactive areas

```rust
// Toggle debug overlay
debug_overlay.toggle();

// Configure what to show
debug_overlay.show_physics(true);
debug_overlay.show_anchors(true);
debug_overlay.show_gestures(true);
```

### 7. Render Batching Monitor

Optimize draw call batching.

#### Information Displayed:
- Batch count and efficiency
- Texture atlas usage
- State change frequency
- GPU memory usage

```rust
let batch_stats = renderer.get_batch_stats();
println!("Batches: {}", batch_stats.batch_count);
println!("Efficiency: {:.1}%", batch_stats.efficiency * 100.0);
```

## Best Practices

### Performance Optimization

1. **Monitor Frame Budget** - Keep frame time under 16.6ms for 60 FPS
2. **Reduce Draw Calls** - Use the batch monitor to optimize
3. **Profile First** - Use the profiler before optimizing
4. **Hot Reload During Development** - Iterate quickly without restarts

### Debugging Workflow

1. **Enable DevTools** with F12
2. **Inspect Components** to understand structure
3. **Profile Performance** to find bottlenecks
4. **Use Debug Overlays** for visual debugging
5. **Set Breakpoints** for complex interactions

### Component Development

```rust
// Example: Debuggable component
impl Component for MyComponent {
    fn render(&self, ctx: &ComponentContext) -> Vec<Renderable> {
        // Add debug info in dev mode
        #[cfg(debug_assertions)]
        if ctx.devtools_enabled {
            console::log_1(&format!("Rendering: {:?}", self.id()).into());
        }
        
        // Regular render code
        vec![/* renderables */]
    }
}
```

## Interactive Demo

To see all features in action, run the DevTools showcase:

```bash
# Build the demo
chmod +x build_devtools_demo.sh
./build_devtools_demo.sh

# Run the demo
cd examples
./serve_devtools.py

# Open http://localhost:8080/devtools_showcase.html
```

## Keyboard Shortcuts Reference

| Shortcut | Action | Description |
|----------|--------|-------------|
| F12 | Toggle DevTools | Enable/disable all developer tools |
| Ctrl+Shift+I | Component Inspector | Inspect UI hierarchy |
| Ctrl+Shift+P | Performance Profiler | Show performance metrics |
| Ctrl+R | Force Reload | Trigger hot reload |
| D | Debug Overlay | Toggle AR/3D debug visualization |
| Ctrl+Shift+D | UI Debugger | Show layout boundaries |
| Ctrl+L | Clear Console | Clear debug console |
| Escape | Close Inspector | Close current tool panel |

## Troubleshooting

### DevTools Not Showing
- Ensure DevTools are enabled: `devtools.enable()`
- Check that you're in debug mode: `#[cfg(debug_assertions)]`
- Verify keyboard shortcuts aren't captured by browser

### Performance Impact
- DevTools add overhead; disable for production builds
- Use conditional compilation: `#[cfg(feature = "devtools")]`
- Profile the profiler itself if needed

### Hot Reload Issues
- Check file watcher has permissions
- Ensure WebSocket port (3030) is available
- Verify watch paths exist and are accessible

## API Reference

### DevTools
```rust
pub struct DevTools {
    pub fn new() -> Self
    pub fn enable(&mut self)
    pub fn disable(&mut self)
    pub fn is_enabled(&self) -> bool
    pub fn inspector(&mut self) -> &mut ComponentInspector
    pub fn profiler(&mut self) -> &mut RenderProfiler
    pub fn debugger(&mut self) -> &mut UIDebugger
    pub fn check_hot_reload(&mut self) -> Option<ReloadEvent>
    pub fn render_overlay(&self) -> Vec<Renderable>
    pub fn handle_key(&mut self, key: KeyCode, modifiers: ModifiersState) -> bool
}
```

### ComponentInspector
```rust
pub struct ComponentInspector {
    pub fn update_tree(&mut self, root: &dyn Component, ctx: &ComponentContext)
    pub fn select_component(&mut self, id: ComponentId)
    pub fn get_component_tree(&self) -> Option<&ComponentNode>
    pub fn get_selected_properties(&self) -> Option<&ComponentProperties>
    pub fn add_breakpoint(&mut self, component_id: ComponentId, breakpoint: Breakpoint)
}
```

### RenderProfiler
```rust
pub struct RenderProfiler {
    pub fn frame_start(&mut self)
    pub fn frame_end(&mut self)
    pub fn get_stats(&self) -> &ProfilerStats
    pub fn get_frame_history(&self) -> &[FrameStats]
    pub fn mark_phase(&mut self, phase: ProfilePhase)
}
```

## Integration Examples

### React-style DevTools
```rust
// Custom React-like component inspector
let react_inspector = ReactStyleInspector::new(devtools.inspector());
react_inspector.show_props_panel();
react_inspector.show_state_panel();
```

### Performance Monitoring Dashboard
```rust
// Export metrics to external monitoring
let stats = devtools.profiler().get_stats();
monitoring::send_metrics(json!({
    "fps": stats.fps,
    "frame_time": stats.frame_time_ms,
    "memory": stats.memory_mb,
}));
```

### Automated Testing
```rust
// Use DevTools in tests
#[test]
fn test_component_performance() {
    let mut devtools = DevTools::new();
    devtools.enable();
    
    // Render components
    render_test_scene(&mut devtools);
    
    let stats = devtools.profiler().get_stats();
    assert!(stats.fps >= 55.0, "Performance below threshold");
    assert!(stats.draw_calls < 100, "Too many draw calls");
}
```

## Future Enhancements

Planned features for future releases:

1. **Network Inspector** - Monitor API calls and WebSocket traffic
2. **State Time Travel** - Rewind and replay application state
3. **Performance Recordings** - Save and analyze performance traces
4. **Remote Debugging** - Connect to devices over network
5. **Custom Tool Plugins** - Extend DevTools with custom panels
6. **AI-Powered Optimization** - Automatic performance suggestions

## Contributing

To add new developer tools:

1. Create a new module in `src/devtools/`
2. Implement the tool interface
3. Add keyboard shortcuts in `DevTools::handle_key`
4. Update the overlay renderer
5. Add documentation and examples

See [CONTRIBUTING.md](../CONTRIBUTING.md) for more details.