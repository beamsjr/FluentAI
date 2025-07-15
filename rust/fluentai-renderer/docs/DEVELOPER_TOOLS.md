# FluentAI Developer Tools

This document describes the comprehensive developer tools available in FluentAI renderer for debugging, profiling, and live development.

## Overview

FluentAI Developer Tools provide:
- **Component Inspector** - Real-time UI hierarchy inspection
- **Performance Profiler** - Frame timing and optimization
- **Hot Reload** - Live code updates without restart
- **UI Debugger** - Event tracking and state debugging
- **DevTools Overlay** - Visual debugging interface

## Getting Started

### Enabling DevTools

```rust
use fluentai_renderer::devtools::DevTools;

let mut devtools = DevTools::new();
devtools.enable();
```

In FluentAI:
```fluentai
let devtools = DevTools.create();
devtools.enable();

// Or toggle with debug mode
if debug_mode {
    devtools.enable();
}
```

### Keyboard Shortcuts

- **F12** - Toggle DevTools overlay
- **Ctrl+Shift+I** - Toggle component inspector
- **Ctrl+Shift+P** - Toggle performance profiler
- **Ctrl+R** - Force reload
- **Ctrl+Shift+D** - Toggle debugger

## Component Inspector

The component inspector provides real-time visualization of your UI hierarchy.

### Features

1. **Component Tree View**
   - Hierarchical component structure
   - Real-time updates
   - Expand/collapse nodes
   - Component selection

2. **Property Inspection**
   ```fluentai
   // Add debug names to components
   UI.Button("Click me") {
       debug_name: "submit_button",
       debug_props: {
           "enabled": true,
           "click_count": click_count,
       }
   }
   ```

3. **Layout Debugging**
   ```fluentai
   devtools.inspector.show_layout_bounds(true);
   devtools.inspector.show_padding(true);
   devtools.inspector.show_margins(true);
   ```

4. **Component Highlighting**
   ```fluentai
   // Highlight specific components
   devtools.inspector.highlight(component_id, Color.red());
   
   // Highlight on hover
   devtools.inspector.highlight_on_hover(true);
   ```

### Usage Example

```fluentai
// Inspecting component properties
devtools.inspector.on_select((component) => {
    $(f"Selected: {component.name}").print();
    $(f"Properties: {component.props}").print();
});

// Setting breakpoints
devtools.inspector.add_breakpoint(
    component_id,
    event: "click",
    condition: "props.enabled == true"
);
```

## Performance Profiler

Track and optimize rendering performance in real-time.

### Metrics Tracked

1. **Frame Timing**
   - FPS (Frames Per Second)
   - Frame time (ms)
   - Target vs actual frame rate

2. **Phase Timing**
   - Layout time
   - Render time
   - GPU time

3. **Resource Usage**
   - Draw calls
   - Vertex count
   - Memory allocation
   - Texture memory

### Profiling Code Sections

```fluentai
// Manual timing
let timer = devtools.profiler.start_timer("expensive_operation");
expensive_operation();
timer.stop(); // Automatically logs duration

// Scoped timing
devtools.profiler.scoped("data_processing", () => {
    process_large_dataset();
});
```

### Performance Overlay

```fluentai
// Show real-time performance stats
UI.PerformanceOverlay {
    position: .top_right,
    show_fps: true,
    show_frame_time: true,
    show_memory: true,
    graph_history: 120, // frames
}
```

### Optimization Helpers

```fluentai
// Get optimization suggestions
let suggestions = devtools.profiler.get_suggestions();
for suggestion in suggestions {
    $(f"⚡ {suggestion}").print();
}

// Common suggestions:
// - "Reduce draw calls by batching similar elements"
// - "Consider GPU instancing for repeated elements"
// - "Layout thrashing detected - batch DOM updates"
```

## Hot Reload

Live code updates without losing application state.

### Setup

1. **File Watching**
   ```fluentai
   devtools.hot_reload.watch([
       "src/**/*.flc",
       "assets/**/*",
       "styles/**/*.css"
   ]);
   ```

2. **State Preservation**
   ```fluentai
   // Mark state for preservation
   @preserve
   effect AppState {
       user_data: UserData,
       session: Session,
   }
   
   // Custom preservation
   devtools.hot_reload.preserve("custom_data", {
       save: () => my_complex_state.serialize(),
       restore: (data) => my_complex_state.deserialize(data),
   });
   ```

3. **Module Hot Swapping**
   ```fluentai
   // Components automatically hot-swap
   @hot_reload
   @UI.component
   function MyComponent() -> UI.Element {
       // Component implementation
   }
   ```

### Reload Events

```fluentai
devtools.hot_reload.on_reload((event) => {
    match event.type {
        case .BeforeReload => {
            // Save any transient state
            save_form_data();
        },
        case .AfterReload => {
            // Restore state
            restore_form_data();
            show_toast("Hot reload complete!");
        },
        case .Error(error) => {
            show_error(f"Hot reload failed: {error}");
        },
    }
});
```

### WebSocket Integration

The hot reload server provides WebSocket endpoints for browser integration:

```javascript
// In browser dev tools
const ws = new WebSocket('ws://localhost:3030/devtools');

ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    switch (data.type) {
        case 'reload':
            window.location.reload();
            break;
        case 'css':
            updateStyles(data.content);
            break;
        case 'component':
            hotSwapComponent(data.id, data.code);
            break;
    }
};
```

## UI Debugger

Comprehensive debugging for UI events and state changes.

### Event Logging

```fluentai
// Automatic event logging
devtools.debugger.log_events([
    "click",
    "focus",
    "state_change",
    "network_request"
]);

// Manual event logging
devtools.debugger.log_event({
    type: "custom_event",
    component: component_id,
    data: { custom: "data" },
    stack_trace: true,
});
```

### Breakpoints

```fluentai
// Event breakpoints
devtools.debugger.break_on_event("click", {
    component: "submit_button",
    condition: "event.shiftKey == true",
});

// State change breakpoints
devtools.debugger.break_on_state_change("user.logged_in");

// Conditional breakpoints
devtools.debugger.add_breakpoint({
    condition: "counter > 10 && items.length == 0",
    action: () => {
        devtools.pause();
        $(f"Breakpoint hit: {get_current_state()}").print();
    },
});
```

### Time Travel Debugging

```fluentai
// Enable state snapshots
devtools.debugger.enable_time_travel({
    max_snapshots: 100,
    snapshot_interval: 1000, // ms
});

// Navigate through time
devtools.debugger.step_back(); // Go to previous state
devtools.debugger.step_forward(); // Go to next state
devtools.debugger.jump_to_time(timestamp); // Jump to specific time

// Export/import debug sessions
let session = devtools.debugger.export_session();
save_to_file("debug_session.json", session);

// Later...
let session = load_from_file("debug_session.json");
devtools.debugger.import_session(session);
```

### Console Integration

```fluentai
// Enhanced logging
devtools.console.log("Info message");
devtools.console.warn("Warning message");
devtools.console.error("Error message");
devtools.console.debug("Debug details");

// Structured logging
devtools.console.table(data);
devtools.console.group("Network Requests");
devtools.console.log("Request 1");
devtools.console.log("Request 2");
devtools.console.groupEnd();

// Timing
devtools.console.time("operation");
perform_operation();
devtools.console.timeEnd("operation");
```

## DevTools Overlay

Visual debugging interface overlaid on your application.

### Overlay Panels

1. **Component Tree Panel**
   - Interactive hierarchy
   - Search and filter
   - Drag to rearrange

2. **Properties Panel**
   - Live property editing
   - Color picker for colors
   - Number sliders

3. **Performance Panel**
   - Real-time graphs
   - Frame timeline
   - Memory usage

4. **Console Panel**
   - Log output
   - Command input
   - Filter by level

### Customization

```fluentai
devtools.overlay.configure({
    position: .right, // .left, .bottom, .floating
    width: 400,
    opacity: 0.95,
    theme: .dark, // .light, .auto
    panels: {
        inspector: true,
        profiler: true,
        console: true,
        network: false,
    },
});
```

## Integration with FluentAI

### Effect System Integration

```fluentai
// Debug effects
effect DebugState {
    breakpoints: List<Breakpoint>,
    logs: List<LogEntry>,
    snapshots: List<StateSnapshot>,
}

// Automatic effect tracking
@debug
effect AppState {
    // All changes to this effect are logged
    counter: int = 0,
}
```

### Component Debugging

```fluentai
// Debug component decorator
@debug({
    track_props: true,
    track_renders: true,
    track_events: ["click", "hover"],
})
@UI.component
function DebuggedComponent(props: Props) -> UI.Element {
    // Component implementation
}
```

### Performance Monitoring

```fluentai
// Automatic performance tracking
@profile
function expensive_computation(data: LargeData) -> Result {
    // Function is automatically profiled
}

// Manual performance marks
devtools.performance.mark("data_fetch_start");
let data = fetch_data().await();
devtools.performance.mark("data_fetch_end");
devtools.performance.measure("data_fetch", "data_fetch_start", "data_fetch_end");
```

## Best Practices

### Development Workflow

1. **Start with DevTools Enabled**
   ```fluentai
   let is_dev = Platform.is_development();
   if is_dev {
       DevTools.enable();
   }
   ```

2. **Use Debug Names**
   ```fluentai
   UI.Button("Submit") {
       debug_name: "form_submit_button",
       debug_group: "form_controls",
   }
   ```

3. **Add Meaningful Logs**
   ```fluentai
   devtools.log("User action", {
       action: "form_submit",
       form_id: form.id,
       validation: validation_result,
   });
   ```

### Performance Tips

1. **Profile First**
   - Always measure before optimizing
   - Focus on actual bottlenecks
   - Use flame graphs for deep analysis

2. **Batch Operations**
   ```fluentai
   devtools.profiler.batch("updates", () => {
       // Multiple state updates
       update_item1();
       update_item2();
       update_item3();
   });
   ```

3. **Conditional Debugging**
   ```fluentai
   // Only in development
   #[cfg(debug)]
   {
       devtools.expensive_check();
   }
   ```

### Production Considerations

1. **Strip DevTools in Production**
   ```rust
   #[cfg(not(debug_assertions))]
   pub fn create_devtools() -> DummyDevTools {
       DummyDevTools // No-op implementation
   }
   ```

2. **Minimal Runtime Overhead**
   - DevTools use lazy initialization
   - Zero cost when disabled
   - Conditional compilation for sensitive code

3. **Security**
   - DevTools disabled by default in production
   - Require explicit flag to enable
   - Sanitize logged data

## Troubleshooting

### Common Issues

1. **Hot Reload Not Working**
   - Check file watcher permissions
   - Ensure WebSocket port (3030) is available
   - Verify file paths are correct

2. **Performance Overhead**
   - Disable unnecessary tracking
   - Reduce snapshot frequency
   - Use sampling profiler for production

3. **Memory Leaks**
   - Clear old snapshots periodically
   - Limit event log size
   - Disable time travel in production

## Future Enhancements

- **Network Inspector** - Track API calls and responses
- **Redux DevTools Integration** - For state management debugging
- **Remote Debugging** - Debug apps on other devices
- **Replay Debugging** - Record and replay user sessions
- **AI-Powered Insights** - Automatic performance suggestions