# FluentAI Renderer Integration Guide

This guide demonstrates how to integrate all renderer features into a complete FluentAI application.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Architecture Overview](#architecture-overview)
3. [Feature Integration](#feature-integration)
4. [Performance Best Practices](#performance-best-practices)
5. [Common Patterns](#common-patterns)
6. [Troubleshooting](#troubleshooting)

## Quick Start

### Basic Setup

```fluentai
// main.flc
use UI;
use Platform;
use DevTools;

// Initialize renderer with all features
public function main() {
    // Platform setup
    Platform.init();
    
    // Enable dev tools in development
    if Platform.is_development() {
        DevTools.enable();
    }
    
    // Create window
    let window = UI.Window {
        title: "My FluentAI App",
        size: Platform.is_mobile() ? Platform.screen_size() : (1280, 800),
        renderer: {
            backend: .WebGPU,
            vsync: true,
            antialiasing: 4,
        },
    };
    
    // Mount app
    window.mount(App);
    
    // Start event loop
    UI.run(window);
}
```

### Minimal App Structure

```fluentai
// App.flc
use UI;

// Reactive state
reactive! {
    theme: Theme = Theme.auto(),
    user: Option<User> = None,
}

// Main app component
@UI.component
public function App() -> UI.Element {
    ui! {
        Stack(direction: .vertical) {
            AppHeader()
            
            Stack(direction: .horizontal, flex: 1) {
                if !Platform.is_mobile() {
                    Sidebar()
                }
                
                MainContent()
            }
            
            if Platform.is_development() {
                DevTools.overlay()
            }
        }
    }
}
```

## Architecture Overview

### Layer Structure

```
┌─────────────────────────────────────┐
│         Application Layer           │
│  (Your FluentAI Components & Logic) │
├─────────────────────────────────────┤
│         UI Framework Layer          │
│  (Components, Layouts, Animations)  │
├─────────────────────────────────────┤
│        Rendering Layer              │
│  (WebGPU, Batching, Instancing)    │
├─────────────────────────────────────┤
│        Platform Layer               │
│  (OS Integration, Input, Files)    │
└─────────────────────────────────────┘
```

### Feature Map

| Feature | Module | Key Components |
|---------|---------|----------------|
| Rendering | `primitives`, `renderer` | Shapes, Text, Gradients |
| Components | `components` | Base trait, Built-in components |
| Layouts | `components.flexbox/grid` | Flexbox, CSS Grid |
| Animation | `animation`, `transitions` | Easing, Transitions |
| Optimization | `batching`, `instancing` | Draw call reduction |
| Reactivity | `reactive_v2`, `ui_builder` | State management |
| Platform | `platform` | OS integration |
| DevTools | `devtools` | Debugging, Profiling |

## Feature Integration

### 1. Rendering Features

```fluentai
// Custom rendering with all features
@UI.component
function CustomGraphics() -> UI.Element {
    UI.Canvas {
        size: (400, 300),
        
        on_draw: (ctx) => {
            // Gradient background
            ctx.fill_gradient(
                gradient: Gradient.linear(
                    start: (0, 0),
                    end: (400, 300),
                    stops: [
                        (0.0, Color.blue()),
                        (1.0, Color.purple()),
                    ],
                ),
                rect: Rect(0, 0, 400, 300),
            );
            
            // Path with bezier curves
            ctx.begin_path();
            ctx.move_to(50, 150);
            ctx.bezier_curve_to(
                cp1: (100, 50),
                cp2: (200, 250),
                end: (250, 150),
            );
            ctx.stroke(Color.white(), width: 3);
            
            // Text with custom font
            ctx.draw_text(
                text: "FluentAI",
                position: (200, 150),
                font: Font {
                    family: "Inter",
                    size: 48,
                    weight: .bold,
                },
                color: Color.white(),
                align: .center,
            );
            
            // Shadow effect
            ctx.with_shadow(
                color: Color.black().with_alpha(0.5),
                blur: 10,
                offset: (5, 5),
                () => {
                    ctx.fill_circle(
                        center: (300, 100),
                        radius: 40,
                        color: Color.yellow(),
                    );
                }
            );
        },
    }
}
```

### 2. Component System

```fluentai
// Custom component with full features
@UI.component
public function DataCard(data: CardData) -> UI.Element {
    // Local state
    reactive! {
        expanded: bool = false,
        hover: bool = false,
    }
    
    // Computed properties
    computed! {
        background_color = if hover {
            theme.surface_hover
        } else {
            theme.surface
        },
        
        content_height = if expanded { 200 } else { 100 },
    }
    
    // Effects
    effect! {
        [expanded] => {
            DevTools.log(f"Card expanded: {expanded}");
        }
    }
    
    // Animated container
    UI.AnimatedContainer {
        height: content_height,
        background: background_color,
        padding: 16,
        border_radius: 8,
        shadow: if hover { Shadow.large() } else { Shadow.small() },
        transition: Transition.all(300.ms, .ease_out),
        
        on_mouse_enter: () => set_hover(true),
        on_mouse_leave: () => set_hover(false),
        
        children: [
            // Header
            UI.Row {
                justify: .space_between,
                
                children: [
                    UI.Text(data.title)
                        .font_size(18)
                        .bold(),
                    
                    UI.IconButton(if expanded { "expand_less" } else { "expand_more" }) {
                        on_click: () => set_expanded(!expanded),
                    },
                ],
            },
            
            // Content
            if expanded {
                UI.Stack {
                    margin_top: 12,
                    
                    children: [
                        UI.Text(data.description)
                            .color(theme.text_secondary),
                        
                        UI.Row {
                            margin_top: 16,
                            spacing: 8,
                            
                            children: data.tags.map(tag => 
                                UI.Chip(tag)
                                    .variant(.outlined)
                            ),
                        },
                    ],
                }
            },
        ],
    }
}
```

### 3. Layout System

```fluentai
// Responsive layout with flexbox and grid
@UI.component
function ResponsiveGallery(items: List<Item>) -> UI.Element {
    let columns = if Platform.screen_width() > 1200 { 4 }
                  else if Platform.screen_width() > 768 { 3 }
                  else if Platform.screen_width() > 480 { 2 }
                  else { 1 };
    
    UI.Grid {
        columns: columns,
        gap: 16,
        padding: 16,
        
        children: items.map(item => 
            UI.GridItem {
                // Span multiple columns for featured items
                column_span: if item.featured { 2 } else { 1 },
                row_span: if item.featured { 2 } else { 1 },
                
                child: GalleryCard(item),
            }
        ),
    }
}

// Flexbox layout with advanced features
@UI.component
function FlexibleToolbar() -> UI.Element {
    UI.Flex {
        direction: .row,
        justify: .space_between,
        align: .center,
        wrap: .wrap,
        gap: 12,
        
        children: [
            UI.FlexItem {
                flex: 0, // Don't grow
                child: Logo(),
            },
            
            UI.FlexItem {
                flex: 1, // Fill available space
                child: SearchBar(),
            },
            
            UI.FlexItem {
                flex: 0,
                order: if Platform.is_mobile() { -1 } else { 0 }, // Reorder on mobile
                child: UserMenu(),
            },
        ],
    }
}
```

### 4. Animation System

```fluentai
// Complex animations with chaining
@UI.component
function AnimatedLogo() -> UI.Element {
    reactive! {
        scale: float = 0.0,
        rotation: float = 0.0,
        opacity: float = 0.0,
    }
    
    // Chain animations on mount
    UI.on_mount(() => {
        Animation.sequence([
            // Fade in
            Animation.to(opacity, 1.0, 300.ms, .ease_out),
            
            // Scale and rotate together
            Animation.parallel([
                Animation.to(scale, 1.0, 500.ms, .ease_out_back),
                Animation.to(rotation, 360.0, 500.ms, .ease_in_out),
            ]),
            
            // Subtle pulse loop
            Animation.loop(
                Animation.sequence([
                    Animation.to(scale, 1.1, 1000.ms, .ease_in_out),
                    Animation.to(scale, 1.0, 1000.ms, .ease_in_out),
                ]),
            ),
        ]).start();
    });
    
    UI.Transform {
        scale: scale,
        rotation: rotation.degrees(),
        
        child: UI.Opacity {
            opacity: opacity,
            child: Logo(),
        },
    }
}

// Page transitions
@UI.component
function PageContainer(page: Page) -> UI.Element {
    UI.TransitionGroup {
        transition: Transition.custom({
            enter: {
                from: { x: 100, opacity: 0 },
                to: { x: 0, opacity: 1 },
                duration: 300.ms,
                easing: .ease_out,
            },
            exit: {
                from: { x: 0, opacity: 1 },
                to: { x: -100, opacity: 0 },
                duration: 200.ms,
                easing: .ease_in,
            },
        }),
        
        child: match page {
            case .home => HomePage(),
            case .about => AboutPage(),
            case .contact => ContactPage(),
        },
    }
}
```

### 5. Performance Optimization

```fluentai
// Batched rendering for large lists
@UI.component
function OptimizedList(items: List<Item>) -> UI.Element {
    UI.VirtualList {
        items: items,
        item_height: 80,
        buffer_size: 5,
        
        // Enable batching
        render_config: {
            batch_similar: true,
            use_instancing: true,
            max_batch_size: 100,
        },
        
        item_builder: (item, index) => {
            // Use simple components for better batching
            UI.BatchableItem {
                key: item.id,
                
                child: ListItem(item),
            }
        },
        
        // Placeholder while scrolling fast
        placeholder: UI.Skeleton {
            height: 80,
            animation: .pulse,
        },
    }
}

// GPU instancing for particles
@UI.component
function ParticleEffect() -> UI.Element {
    let particles = ParticleSystem {
        max_particles: 10000,
        emitter: {
            position: (400, 300),
            rate: 100, // particles per second
            spread: 45.degrees(),
        },
        particle: {
            lifetime: 3.seconds(),
            size: 4,
            color: Color.gold(),
            velocity: (0, -100),
            acceleration: (0, 50),
        },
    };
    
    UI.Canvas {
        size: (800, 600),
        
        // Use GPU instancing
        render_mode: .instanced,
        
        on_update: (dt) => {
            particles.update(dt);
        },
        
        on_draw: (ctx) => {
            ctx.draw_particles(particles);
        },
    }
}
```

### 6. Platform Integration

```fluentai
// Cross-platform file handling
@UI.component
function FileUploader() -> UI.Element {
    reactive! {
        files: List<File> = [],
        uploading: bool = false,
    }
    
    let handle_files = (new_files: List<File>) => {
        set_files(files + new_files);
        
        // Platform-specific handling
        match Platform.type() {
            case .web => upload_via_fetch(new_files),
            case .desktop => upload_via_native(new_files),
            case .mobile => upload_via_mobile_api(new_files),
        }
    };
    
    UI.DropZone {
        on_drop: handle_files,
        accept: ["image/*", ".pdf"],
        
        child: UI.Stack {
            align: .center,
            padding: 40,
            background: theme.surface_variant,
            border: Border.dashed(2, theme.outline),
            
            children: [
                UI.Icon("cloud_upload", size: 48),
                
                UI.Text("Drop files here or click to browse")
                    .margin_top(16),
                
                UI.Button("Browse Files") {
                    on_click: async () => {
                        let files = perform Platform.show_file_picker({
                            multiple: true,
                            accept: ["image/*", ".pdf"],
                        }).await();
                        
                        if let Some(files) = files {
                            handle_files(files);
                        }
                    },
                    margin_top: 16,
                },
            ],
        },
    }
}
```

### 7. DevTools Integration

```fluentai
// Development-friendly component
@UI.component
@debug
function DebuggableComponent(props: Props) -> UI.Element {
    // Automatic prop tracking
    DevTools.track_props("DebuggableComponent", props);
    
    // Performance monitoring
    let render_timer = DevTools.start_timer("DebuggableComponent.render");
    defer render_timer.stop();
    
    // State debugging
    reactive! {
        count: int = 0,
        items: List<Item> = [],
    }
    
    // Snapshot state on changes
    effect! {
        [count, items] => {
            DevTools.snapshot_state("DebuggableComponent", {
                count: count,
                item_count: items.length(),
            });
        }
    }
    
    ui! {
        Stack {
            // Debug overlay in development
            #[cfg(debug)]
            UI.DebugOverlay {
                component_id: "debuggable_component",
                show_props: true,
                show_state: true,
                show_performance: true,
            }
            
            // Normal content
            Content()
        }
    }
}
```

## Performance Best Practices

### 1. Component Design

```fluentai
// ❌ Bad: Heavy component with many features
@UI.component
function HeavyComponent(data: Data) -> UI.Element {
    // Too much state
    reactive! {
        // ... 20+ state variables
    }
    
    // Too many effects
    effect! {
        // ... complex logic
    }
    
    // Deep nesting
    UI.Stack {
        UI.Stack {
            UI.Stack {
                // ... deeply nested
            }
        }
    }
}

// ✅ Good: Focused, composable components
@UI.component
function LightComponent(data: Data) -> UI.Element {
    DataDisplay(data)
}

@UI.component
function DataDisplay(data: Data) -> UI.Element {
    UI.Row {
        DataIcon(data.type),
        DataContent(data),
        DataActions(data.id),
    }
}
```

### 2. Rendering Optimization

```fluentai
// Use render batching
UI.configure_renderer({
    batching: {
        enabled: true,
        max_batch_size: 1000,
        similar_threshold: 0.95,
    },
    instancing: {
        enabled: true,
        min_instances: 10,
    },
});

// Optimize large lists
@UI.component
function OptimizedDataTable(rows: List<Row>) -> UI.Element {
    UI.Table {
        // Virtual scrolling for large datasets
        virtual: rows.length() > 100,
        
        // Memoize row rendering
        row_builder: memoize((row) => TableRow(row)),
        
        // Batch updates
        update_mode: .batched,
        
        rows: rows,
    }
}
```

### 3. State Management

```fluentai
// ❌ Bad: Unnecessary re-renders
reactive! {
    large_object: LargeObject = create_large_object(),
}

// Every small change triggers full re-render
effect! {
    [large_object] => {
        update_ui(large_object);
    }
}

// ✅ Good: Granular state
reactive! {
    // Split into focused pieces
    user_name: string = "",
    user_avatar: string = "",
    user_preferences: Preferences = Preferences.default(),
}

// Targeted updates
effect! {
    [user_name] => update_name_display(user_name),
    [user_avatar] => update_avatar(user_avatar),
    [user_preferences] => update_preferences(user_preferences),
}
```

## Common Patterns

### 1. Loading States

```fluentai
@UI.component
function AsyncContent<T>(loader: () -> Future<T>, builder: (T) -> UI.Element) -> UI.Element {
    reactive! {
        state: LoadState<T> = LoadState.Loading,
    }
    
    UI.on_mount(() => {
        spawn async {
            set_state(LoadState.Loading);
            
            match loader().await {
                Ok(data) => set_state(LoadState.Success(data)),
                Err(error) => set_state(LoadState.Error(error)),
            }
        };
    });
    
    match state {
        case LoadState.Loading => UI.Spinner(),
        case LoadState.Success(data) => builder(data),
        case LoadState.Error(error) => ErrorDisplay(error),
    }
}
```

### 2. Form Handling

```fluentai
@UI.component
function Form() -> UI.Element {
    reactive! {
        values: FormValues = FormValues.default(),
        errors: Map<string, string> = Map.new(),
        submitting: bool = false,
    }
    
    let validate = () => {
        let new_errors = Map.new();
        
        if values.email.is_empty() {
            new_errors.insert("email", "Email is required");
        }
        
        set_errors(new_errors);
        new_errors.is_empty()
    };
    
    let submit = async () => {
        if !validate() {
            return;
        }
        
        set_submitting(true);
        
        match API.submit_form(values).await {
            Ok(result) => navigate_to("/success"),
            Err(error) => set_errors(Map.from([("form", error.message)])),
        }
        
        set_submitting(false);
    };
    
    UI.Form {
        on_submit: submit,
        
        children: [
            FormField(
                name: "email",
                label: "Email",
                value: values.email,
                error: errors.get("email"),
                on_change: (v) => set_values({...values, email: v}),
            ),
            
            UI.Button("Submit") {
                type: .submit,
                loading: submitting,
                disabled: submitting,
            },
        ],
    }
}
```

### 3. Responsive Design

```fluentai
// Responsive breakpoints
let breakpoints = {
    mobile: 480,
    tablet: 768,
    desktop: 1024,
    wide: 1440,
};

@UI.component
function ResponsiveLayout() -> UI.Element {
    let screen_width = use_screen_width();
    
    let layout = if screen_width < breakpoints.mobile {
        LayoutType.Mobile
    } else if screen_width < breakpoints.tablet {
        LayoutType.Tablet
    } else {
        LayoutType.Desktop
    };
    
    UI.ResponsiveContainer {
        layout: layout,
        
        // Different layouts for different screens
        mobile: MobileLayout(),
        tablet: TabletLayout(),
        desktop: DesktopLayout(),
    }
}
```

## Troubleshooting

### Common Issues

1. **Poor Performance**
   - Enable DevTools profiler
   - Check for unnecessary re-renders
   - Use virtual scrolling for large lists
   - Enable render batching

2. **Memory Leaks**
   - Use `defer` for cleanup
   - Unsubscribe from events
   - Clear large data structures
   - Profile with DevTools

3. **Layout Issues**
   - Use DevTools inspector
   - Check flex/grid properties
   - Verify constraints
   - Test responsive breakpoints

4. **Animation Jank**
   - Use GPU-accelerated properties
   - Batch DOM updates
   - Profile with DevTools
   - Reduce animation complexity

### Debug Commands

```fluentai
// Performance debugging
DevTools.profile("operation_name", () => {
    // Code to profile
});

// Memory debugging
DevTools.heap_snapshot("before_operation");
perform_operation();
DevTools.heap_snapshot("after_operation");
DevTools.compare_snapshots("before_operation", "after_operation");

// Render debugging
DevTools.show_render_boundaries(true);
DevTools.show_repaint_regions(true);
DevTools.log_render_tree();
```

## Next Steps

1. **Explore Examples**
   - Check `examples/` directory
   - Run demo applications
   - Modify and experiment

2. **Read API Docs**
   - Component reference
   - Animation guide
   - Platform integration

3. **Join Community**
   - Discord server
   - GitHub discussions
   - Stack Overflow tag

4. **Contribute**
   - Report issues
   - Submit PRs
   - Write documentation
   - Share examples