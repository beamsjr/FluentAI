// Reactive UI demonstration with FluentAI macros

// Note: This example shows how the macros would be used once integrated with FluentAI
// The actual macro expansion happens at compile time

use fluentai_renderer::ui_builder::*;
use fluentai_renderer::ui_builder::helpers::*;
use fluentai_renderer::components::Component;
use fluentai_renderer::primitives::Color;

fn main() {
    println!("FluentAI Reactive UI Demo");
    println!("=========================\n");
    
    // Example 1: Basic declarative UI
    demo_basic_ui();
    
    // Example 2: Reactive state management
    demo_reactive_state();
    
    // Example 3: Conditional rendering
    demo_conditional_rendering();
    
    // Example 4: List rendering
    demo_list_rendering();
    
    // Example 5: Complex reactive app
    demo_todo_app();
}

fn demo_basic_ui() {
    println!("Demo 1: Basic Declarative UI");
    println!("----------------------------");
    
    // Without macros (current API)
    let ui = Stack::new()
        .direction(StackDirection::Vertical)
        .spacing(16.0)
        .padding(20.0)
        .child(
            Text::new("Welcome to FluentAI!")
                .font_size(24.0)
                .color(Color::new(0.2, 0.4, 0.8, 1.0))
                .bold()
                .into()
        )
        .child(
            Text::new("Build reactive UIs with ease")
                .font_size(16.0)
                .color(Color::new(0.4, 0.4, 0.4, 1.0))
                .into()
        )
        .child(
            Button::new("Get Started")
                .variant(ButtonVariant::Primary)
                .on_click(|| println!("Button clicked!"))
                .into()
        );
    
    println!("Created UI with {} components", 3);
    
    // With macros (future API)
    println!("\nWith macros, this would be:");
    println!(r#"
ui! {
    Stack(direction: .vertical, spacing: 16, padding: 20) {
        Text("Welcome to FluentAI!")
            .font_size(24)
            .color(Color::blue())
            .bold()
        
        Text("Build reactive UIs with ease")
            .font_size(16)
            .color(Color::gray())
        
        Button("Get Started")
            .variant(.primary)
            .on_click(|| println!("Button clicked!"))
    }
}
"#);
    
    println!();
}

fn demo_reactive_state() {
    println!("Demo 2: Reactive State Management");
    println!("---------------------------------");
    
    // Current API
    let count = ReactiveState::new(0);
    let count_clone = count.clone();
    
    count.subscribe(move |value| {
        println!("Count changed to: {}", value);
    });
    
    // Simulate user interactions
    println!("Initial count: {}", count.get());
    count.set(1);
    count.set(2);
    count.set(3);
    
    println!("\nWith macros, this would be:");
    println!(r#"
reactive! {
    count: i32 = 0,
}

effect! {
    [count] => {
        println!("Count changed to: {}", count);
    }
}

// In UI:
ui! {
    Stack {
        Text(f"Count: {count}")
        Button("+") {
            on_click: || count += 1
        }
        Button("-") {
            on_click: || count -= 1
        }
    }
}
"#);
    
    println!();
}

fn demo_conditional_rendering() {
    println!("Demo 3: Conditional Rendering");
    println!("-----------------------------");
    
    let show_details = ReactiveState::new(false);
    
    // Current API
    let ui = Stack::new()
        .direction(StackDirection::Vertical)
        .child(
            Button::new("Toggle Details")
                .on_click({
                    let show_details = show_details.clone();
                    move || {
                        let current = show_details.get();
                        show_details.set(!current);
                    }
                })
                .into()
        );
    
    // Add conditional component
    if show_details.get() {
        println!("Details are shown");
    } else {
        println!("Details are hidden");
    }
    
    println!("\nWith macros:");
    println!(r#"
reactive! {
    show_details: bool = false,
}

ui! {
    Stack {
        Button("Toggle Details") {
            on_click: || show_details = !show_details
        }
        
        if show_details {
            Stack(padding: 10, background: Color::light_gray()) {
                Text("Additional Details")
                Text("• Feature 1")
                Text("• Feature 2")
                Text("• Feature 3")
            }
        }
    }
}
"#);
    
    println!();
}

fn demo_list_rendering() {
    println!("Demo 4: List Rendering");
    println!("----------------------");
    
    #[derive(Clone)]
    struct TodoItem {
        id: u32,
        text: String,
        completed: bool,
    }
    
    let items = vec![
        TodoItem { id: 1, text: "Learn FluentAI".to_string(), completed: true },
        TodoItem { id: 2, text: "Build reactive UI".to_string(), completed: false },
        TodoItem { id: 3, text: "Deploy application".to_string(), completed: false },
    ];
    
    // Current API
    let ui = Stack::new()
        .direction(StackDirection::Vertical)
        .spacing(8.0)
        .children(
            For::new(items.clone())
                .render(|item, _| {
                    row(vec![
                        Text::new(if item.completed { "✓" } else { "○" })
                            .color(if item.completed {
                                Color::new(0.2, 0.8, 0.2, 1.0)
                            } else {
                                Color::new(0.6, 0.6, 0.6, 1.0)
                            })
                            .into(),
                        spacer(8.0),
                        Text::new(&item.text)
                            .color(if item.completed {
                                Color::new(0.6, 0.6, 0.6, 1.0)
                            } else {
                                Color::new(0.0, 0.0, 0.0, 1.0)
                            })
                            .into(),
                    ])
                    .into()
                })
                .build()
        );
    
    println!("Rendered {} todo items", items.len());
    
    println!("\nWith macros:");
    println!(r#"
reactive! {
    todos: Vec<TodoItem> = vec![
        TodoItem { id: 1, text: "Learn FluentAI", completed: true },
        TodoItem { id: 2, text: "Build reactive UI", completed: false },
        TodoItem { id: 3, text: "Deploy application", completed: false },
    ],
}

ui! {
    Stack(spacing: 8) {
        for todo in todos {
            Row(spacing: 8) {
                Text(if todo.completed { "✓" } else { "○" })
                    .color(if todo.completed { Color::green() } else { Color::gray() })
                
                Text(todo.text)
                    .strikethrough(todo.completed)
                
                Button("Toggle") {
                    on_click: || todo.completed = !todo.completed
                }
            }
        }
    }
}
"#);
    
    println!();
}

fn demo_todo_app() {
    println!("Demo 5: Complete Todo App");
    println!("-------------------------");
    
    println!("This demonstrates how a complete reactive app would look:\n");
    
    println!(r#"
// Define reactive state
reactive! {
    todos: Vec<Todo> = vec![],
    new_todo_text: String = "",
    filter: Filter = Filter::All,
}

// Computed properties
computed! {
    visible_todos = match filter {
        Filter::All => todos.clone(),
        Filter::Active => todos.filter(|t| !t.completed),
        Filter::Completed => todos.filter(|t| t.completed),
    },
    
    active_count = todos.filter(|t| !t.completed).count(),
    completed_count = todos.len() - active_count,
}

// Main UI
ui! {
    Stack(padding: 20, max_width: 600) {
        // Header
        Text("todos")
            .font_size(48)
            .color(Color::rgb(0.7, 0.3, 0.3))
            .center()
        
        // Input
        Row {
            TextInput(placeholder: "What needs to be done?")
                .bind(new_todo_text)
                .on_enter(add_todo)
            
            Button("Add") {
                on_click: add_todo
                disabled: new_todo_text.is_empty()
            }
        }
        
        // Todo list
        if todos.len() > 0 {
            Stack {
                for todo in visible_todos {
                    TodoItem(todo)
                }
            }
            
            // Footer
            Row(justify: .space_between, padding: 10) {
                Text(f"{active_count} items left")
                
                Row(spacing: 10) {
                    FilterButton("All", Filter::All)
                    FilterButton("Active", Filter::Active)
                    FilterButton("Completed", Filter::Completed)
                }
                
                if completed_count > 0 {
                    Button("Clear completed") {
                        on_click: || todos.retain(|t| !t.completed)
                    }
                }
            }
        }
    }
}

// Helper functions integrated with FluentAI
function add_todo() {
    if !new_todo_text.is_empty() {
        todos.push(Todo {
            id: generate_id(),
            text: new_todo_text.clone(),
            completed: false,
        });
        new_todo_text = "";
    }
}

// Custom components
component TodoItem(todo: Todo) {
    Row(padding: 10, hover_background: Color::light_gray()) {
        Checkbox(todo.completed) {
            on_change: |checked| todo.completed = checked
        }
        
        Text(todo.text)
            .strikethrough(todo.completed)
            .color(if todo.completed { Color::gray() } else { Color::black() })
        
        Button("×")
            .color(Color::red())
            .on_click(|| todos.retain(|t| t.id != todo.id))
    }
}

component FilterButton(label: String, filter_type: Filter) {
    Button(label)
        .variant(if filter == filter_type { .primary } else { .secondary })
        .on_click(|| filter = filter_type)
}
"#);
    
    println!("\nThis example shows:");
    println!("- Reactive state management");
    println!("- Computed properties");
    println!("- Two-way data binding");
    println!("- Custom components");
    println!("- Event handling");
    println!("- Conditional rendering");
    println!("- List rendering with keys");
    println!("- Integration with FluentAI functions");
}

// Clone trait for ReactiveState
impl<T: Clone> Clone for ReactiveState<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            listeners: self.listeners.clone(),
        }
    }
}