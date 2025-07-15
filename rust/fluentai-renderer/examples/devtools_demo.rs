// Developer tools demonstration

use fluentai_renderer::devtools::*;
use fluentai_renderer::components::{Component, ComponentId};
use fluentai_renderer::ui_builder::*;
use std::time::Duration;
use std::thread;

fn main() {
    println!("FluentAI Developer Tools Demo");
    println!("=============================\n");
    
    // Create developer tools instance
    let mut devtools = DevTools::new();
    devtools.enable();
    
    // Demo 1: Component Inspector
    demo_inspector(&mut devtools);
    
    // Demo 2: Performance Profiler
    demo_profiler(&mut devtools);
    
    // Demo 3: Hot Reload
    demo_hot_reload(&mut devtools);
    
    // Demo 4: UI Debugger
    demo_debugger(&mut devtools);
    
    // Demo 5: DevTools Overlay
    demo_overlay(&mut devtools);
}

fn demo_inspector(devtools: &mut DevTools) {
    println!("Demo 1: Component Inspector");
    println!("---------------------------");
    
    let inspector = devtools.inspector();
    
    // Create a sample UI tree
    let ui = Stack::new()
        .direction(StackDirection::Vertical)
        .child(
            Text::new("Hello World").into()
        )
        .child(
            Button::new("Click Me").into()
        );
    
    // Update inspector with component tree
    // inspector.update_tree(&ui, &ctx);
    
    println!("Component tree structure:");
    if let Some(tree) = inspector.get_component_tree() {
        print_component_tree(tree, 0);
    }
    
    // Export tree as JSON
    let tree_json = inspector.export_tree_json();
    println!("\nExported tree: {}", serde_json::to_string_pretty(&tree_json).unwrap());
    
    println!("\nInspector features:");
    println!("  - Real-time component hierarchy");
    println!("  - Property inspection");
    println!("  - Component selection");
    println!("  - Breakpoint setting");
    
    println!();
}

fn print_component_tree(node: &inspector::ComponentNode, depth: usize) {
    let indent = "  ".repeat(depth);
    println!("{}{} ({})", indent, node.name, node.id);
    
    for child in &node.children {
        print_component_tree(child, depth + 1);
    }
}

fn demo_profiler(devtools: &mut DevTools) {
    println!("Demo 2: Performance Profiler");
    println!("----------------------------");
    
    let profiler = devtools.profiler();
    
    // Simulate frame rendering
    for frame in 0..60 {
        profiler.begin_frame();
        
        // Layout phase
        profiler.begin_layout();
        thread::sleep(Duration::from_micros(500)); // Simulate layout work
        profiler.end_layout();
        
        // Render phase
        profiler.begin_render();
        thread::sleep(Duration::from_micros(1000)); // Simulate render work
        profiler.end_render();
        
        // Record some draw calls
        for _ in 0..10 {
            profiler.record_draw_call(100);
        }
        
        profiler.end_frame();
        
        if frame % 20 == 0 {
            thread::sleep(Duration::from_millis(16)); // 60 FPS target
        }
    }
    
    // Get statistics
    let stats = profiler.get_stats();
    
    println!("Performance Statistics:");
    println!("  FPS: {:.1}", stats.fps);
    println!("  Frame time: {:.2}ms", stats.frame_time_ms);
    println!("  Layout time: {:.2}ms", stats.layout_time_ms);
    println!("  Render time: {:.2}ms", stats.render_time_ms);
    println!("  Draw calls: {}", stats.draw_calls);
    println!("  Vertices: {}", stats.vertices);
    println!("  Memory: {:.1} MB", stats.memory_mb);
    
    // Export profiling data
    let profile_data = profiler.export_data();
    println!("\nProfile data exported (truncated)");
    
    println!("\nProfiler features:");
    println!("  - Frame time tracking");
    println!("  - Phase timing (layout/render)");
    println!("  - Draw call counting");
    println!("  - Memory tracking");
    println!("  - Historical data");
    
    println!();
}

fn demo_hot_reload(devtools: &mut DevTools) {
    println!("Demo 3: Hot Reload");
    println!("------------------");
    
    // Hot reload is already started when devtools is enabled
    println!("Hot reload server running on http://localhost:3030");
    
    // Check for changes (in real app, this would be in the main loop)
    for _ in 0..5 {
        if let Some(event) = devtools.check_hot_reload() {
            println!("Hot reload event: {:?}", event);
            
            match event.event_type {
                hot_reload::ReloadEventType::ComponentChanged => {
                    println!("  → Reloading component: {}", event.path.display());
                }
                hot_reload::ReloadEventType::StyleChanged => {
                    println!("  → Reloading styles: {}", event.path.display());
                }
                hot_reload::ReloadEventType::AssetChanged => {
                    println!("  → Reloading asset: {}", event.path.display());
                }
                _ => {}
            }
        }
        
        thread::sleep(Duration::from_millis(100));
    }
    
    println!("\nHot reload features:");
    println!("  - File system watching");
    println!("  - Automatic reload on changes");
    println!("  - State preservation");
    println!("  - WebSocket communication");
    println!("  - Module hot swapping");
    
    println!();
}

fn demo_debugger(devtools: &mut DevTools) {
    println!("Demo 4: UI Debugger");
    println!("-------------------");
    
    let debugger = devtools.debugger();
    
    // Log some events
    debugger.log(debugger::LogLevel::Info, "Application started", "main");
    debugger.log(debugger::LogLevel::Debug, "Initializing components", "ui");
    
    // Add breakpoints
    debugger.add_breakpoint(
        "click_events".to_string(),
        debugger::BreakpointCondition::EventType("Click".to_string()),
    );
    
    // Simulate UI events
    let component_id = ComponentId::new();
    debugger.log_ui_event(
        component_id,
        &crate::components::ComponentEvent::Click { x: 100.0, y: 200.0 },
    );
    
    // Log with different levels
    debugger.log(debugger::LogLevel::Warning, "Performance degradation detected", "profiler");
    debugger.log(debugger::LogLevel::Error, "Failed to load resource", "loader");
    
    // Export debug session
    let session_data = debugger.export_session();
    println!("Debug session exported");
    
    println!("\nDebugger features:");
    println!("  - Event logging");
    println!("  - State snapshots");
    println!("  - Breakpoints");
    println!("  - Console output");
    println!("  - Time travel debugging");
    println!("  - Stack traces");
    
    println!();
}

fn demo_overlay(devtools: &mut DevTools) {
    println!("Demo 5: DevTools Overlay");
    println!("------------------------");
    
    println!("DevTools overlay provides visual debugging:");
    println!("  - Component hierarchy visualization");
    println!("  - Real-time performance graphs");
    println!("  - Console output display");
    println!("  - Interactive component selection");
    
    println!("\nKeyboard shortcuts:");
    println!("  F12 - Toggle overlay");
    println!("  Ctrl+Shift+I - Toggle inspector");
    println!("  Ctrl+Shift+P - Toggle profiler");
    println!("  Ctrl+R - Force reload");
    
    // Simulate keyboard input
    println!("\nSimulating F12 press...");
    devtools.handle_key(KeyCode::F12, ModifiersState::NONE);
    
    // Get overlay renderables
    let overlay_renderables = devtools.render_overlay();
    println!("Overlay would render {} elements", overlay_renderables.len());
    
    println!();
}

// Import required types
use fluentai_renderer::components::{ComponentEvent, ComponentContext};