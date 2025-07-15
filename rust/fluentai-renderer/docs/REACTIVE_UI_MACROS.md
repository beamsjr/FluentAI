# FluentAI Reactive UI Macros

This document describes the reactive UI macro system for FluentAI, which provides a declarative syntax for building reactive user interfaces that seamlessly integrate with FluentAI's language features.

## Overview

The reactive UI macro system consists of four main macros:

1. `ui!` - Declarative UI construction
2. `reactive!` - Reactive state management
3. `computed!` - Computed properties
4. `effect!` - Side effects

These macros work together with FluentAI's effect system to provide a powerful, reactive UI development experience.

## The `ui!` Macro

The `ui!` macro provides a declarative syntax for building UI components:

```rust
ui! {
    Stack(direction: .vertical, spacing: 10) {
        Text("Hello, World!")
            .font_size(24)
            .color(Color::blue())
        
        Button("Click me") {
            on_click: || println!("Clicked!")
        }
    }
}
```

### Features

- **Component Composition**: Nest components naturally
- **Property Chaining**: Use method chaining for properties
- **Event Handlers**: Inline event handler definitions
- **Conditional Rendering**: Use `if` expressions
- **List Rendering**: Use `for` loops

### Syntax

```rust
ui! {
    ComponentName(prop1: value1, prop2: value2) {
        // Children components
        ChildComponent("text")
            .method1(value)
            .method2(value)
        
        // Event handlers
        Button("label") {
            on_click: || { /* handler */ }
            on_hover: |event| { /* handler */ }
        }
        
        // Conditional rendering
        if condition {
            ConditionalComponent()
        }
        
        // List rendering
        for item in items {
            ItemComponent(item)
        }
    }
}
```

## The `reactive!` Macro

The `reactive!` macro creates reactive state variables:

```rust
reactive! {
    count: i32 = 0,
    name: String = "User".to_string(),
    items: Vec<Item> = vec![]
}
```

### Features

- **Automatic Tracking**: Dependencies are automatically tracked
- **Change Propagation**: Updates trigger re-renders
- **Type Safety**: Full Rust type checking

### Generated Code

For each reactive variable, the macro generates:
- A getter function: `get_count()`
- A setter function: `set_count(value)`
- Automatic change notification

## The `computed!` Macro

The `computed!` macro creates derived state that automatically updates:

```rust
computed! {
    total = items.iter().map(|i| i.price).sum(),
    display_name = format!("{} ({})", name, count),
    filtered_items = items.iter().filter(|i| i.active).collect()
}
```

### Features

- **Dependency Tracking**: Automatically tracks which reactive variables are used
- **Lazy Evaluation**: Only computes when accessed
- **Memoization**: Caches results until dependencies change

## The `effect!` Macro

The `effect!` macro creates side effects that run when dependencies change:

```rust
effect! {
    [count] => {
        println!("Count changed to: {}", count);
        save_to_storage("count", count);
    }
}

effect! {
    [items, filter] => {
        let filtered = apply_filter(items, filter);
        update_display(filtered);
    }
}
```

### Features

- **Explicit Dependencies**: Clearly declare what triggers the effect
- **Automatic Cleanup**: Previous effects are cleaned up
- **Batch Updates**: Multiple changes are batched

## Integration with FluentAI

The reactive UI system integrates seamlessly with FluentAI's language features:

### With Effects

```fluentai
effect UIState {
    counter: int = 0,
    message: string = ""
}

@UI.component
function CounterApp() -> UI.Element {
    ui! {
        Stack {
            Text(f"Count: {perform UIState.get().counter}")
            
            Button("Increment") {
                on_click: || perform UIState.update(s => {
                    s.counter += 1;
                    s
                })
            }
        }
    }
}
```

### With Pattern Matching

```fluentai
ui! {
    match state {
        case Loading => Spinner(),
        case Error(msg) => ErrorMessage(msg),
        case Success(data) => DataDisplay(data)
    }
}
```

### With Actors

```fluentai
actor Counter {
    count: int = 0;
    
    handle Increment() {
        self.count += 1;
    }
    
    handle GetCount() -> int {
        self.count
    }
}

ui! {
    Stack {
        Text(f"Count: {counter.GetCount()}")
        Button("Inc") {
            on_click: || counter.Increment()
        }
    }
}
```

## Complete Example: Task Manager

```fluentai
use UI;
use Color;
use Time;

struct Task {
    id: int,
    title: string,
    completed: bool,
    due_date: Option<Date>,
}

effect TaskState {
    tasks: List<Task> = [],
    filter: string = "all",
    sort_by: string = "created",
}

@UI.component
function TaskManager() -> UI.Element {
    reactive! {
        new_task_title: String = "",
        show_completed: bool = true,
    }
    
    computed! {
        visible_tasks = perform TaskState.get().tasks
            |> filter(t => show_completed || !t.completed)
            |> sort_by(|t| match sort_by {
                "title" => t.title,
                "due" => t.due_date.unwrap_or(Date.max()),
                _ => t.id
            }),
        
        stats = {
            total: tasks.len(),
            completed: tasks.filter(|t| t.completed).len(),
            pending: tasks.filter(|t| !t.completed).len()
        }
    }
    
    effect! {
        [tasks] => {
            perform Storage.save("tasks", tasks);
        }
    }
    
    ui! {
        Stack(padding: 20) {
            // Header
            Row(justify: .space_between) {
                Text("Task Manager")
                    .font_size(32)
                    .bold()
                
                Text(f"{stats.pending} pending, {stats.completed} completed")
                    .color(Color::gray())
            }
            
            // Add task form
            Row(spacing: 10) {
                TextInput(placeholder: "Add a new task...")
                    .bind(new_task_title)
                    .flex(1)
                
                Button("Add") {
                    on_click: add_task
                    disabled: new_task_title.is_empty()
                }
            }
            
            // Filters
            Row(spacing: 20) {
                Checkbox("Show completed")
                    .bind(show_completed)
                
                Select(options: ["created", "title", "due"])
                    .label("Sort by")
                    .bind(sort_by)
            }
            
            // Task list
            ScrollView {
                Stack(spacing: 5) {
                    for task in visible_tasks {
                        TaskItem(task)
                    }
                }
            }
        }
    }
}
```

## Best Practices

### 1. State Management

- Keep reactive state minimal and normalized
- Use computed properties for derived state
- Avoid circular dependencies

### 2. Performance

- Use keys in list rendering for efficient updates
- Batch state updates when possible
- Profile and optimize re-renders

### 3. Component Design

- Keep components small and focused
- Use composition over inheritance
- Extract reusable components

### 4. Integration

- Leverage FluentAI's effect system for side effects
- Use actors for complex state management
- Integrate with FluentAI's type system

## Implementation Details

The macros are implemented as procedural macros that:

1. Parse the declarative syntax
2. Generate efficient Rust code
3. Integrate with the reactive runtime
4. Provide compile-time validation

The reactive runtime uses:
- Dependency tracking via thread-local storage
- Efficient change propagation algorithms
- Batched updates for performance
- Weak references to prevent memory leaks

## Future Enhancements

- Hot module reloading
- Time-travel debugging
- Component DevTools
- Performance profiling
- Visual UI builder integration