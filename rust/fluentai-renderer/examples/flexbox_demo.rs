// Flexbox layout demonstration

use fluentai_renderer::components::{
    Component, ComponentContext, Container, Row, Column, Button, Style,
    LayoutConstraints, LayoutSize
};
use fluentai_renderer::components::style::{StyleProperty, JustifyContent, AlignItems};
use fluentai_renderer::primitives::Color;
use cgmath::Vector2;

fn main() {
    println!("FluentAI Flexbox Layout Demo");
    println!("=============================\n");
    
    // Demo 1: Row with space-between justification
    demo_space_between();
    
    // Demo 2: Column with center alignment
    demo_center_aligned_column();
    
    // Demo 3: Nested flexbox layout
    demo_nested_layout();
}

fn demo_space_between() {
    println!("Demo 1: Row with space-between justification");
    println!("--------------------------------------------");
    
    let mut row = Row::new()
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(600.0));
            style.set("justify-content", StyleProperty::JustifyContent(JustifyContent::SpaceBetween));
            style.set("padding", StyleProperty::Padding(20.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.9, 0.9, 0.9, 1.0)));
            style
        })
        .add_child(
            Button::new("Left")
                .style({
                    let mut style = Style::new();
                    style.set("padding", StyleProperty::Padding(10.0));
                    style
                })
        )
        .add_child(
            Button::new("Center")
                .style({
                    let mut style = Style::new();
                    style.set("padding", StyleProperty::Padding(10.0));
                    style
                })
        )
        .add_child(
            Button::new("Right")
                .style({
                    let mut style = Style::new();
                    style.set("padding", StyleProperty::Padding(10.0));
                    style
                })
        );
    
    // Perform layout
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = row.layout(&constraints);
    println!("Container size: {}x{}", size.width, size.height);
    
    // Create context and render
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(800.0, 600.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(100.0, 50.0),
    };
    
    let renderables = row.render(&ctx);
    println!("Generated {} render primitives\n", renderables.len());
}

fn demo_center_aligned_column() {
    println!("Demo 2: Column with center alignment");
    println!("------------------------------------");
    
    let mut column = Column::new()
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(400.0));
            style.set("height", StyleProperty::Height(300.0));
            style.set("align-items", StyleProperty::AlignItems(AlignItems::Center));
            style.set("justify-content", StyleProperty::JustifyContent(JustifyContent::Center));
            style.set("gap", StyleProperty::Gap(20.0));
            style.set("padding", StyleProperty::Padding(30.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.85, 0.85, 0.95, 1.0)));
            style
        })
        .add_child(
            Button::new("Centered Item 1")
                .style({
                    let mut style = Style::new();
                    style.set("padding", StyleProperty::Padding(15.0));
                    style
                })
        )
        .add_child(
            Button::new("Centered Item 2")
                .style({
                    let mut style = Style::new();
                    style.set("padding", StyleProperty::Padding(15.0));
                    style.set("width", StyleProperty::Width(200.0));
                    style
                })
        );
    
    // Perform layout
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = column.layout(&constraints);
    println!("Container size: {}x{}", size.width, size.height);
    
    // Create context and render
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(800.0, 600.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(200.0, 150.0),
    };
    
    let renderables = column.render(&ctx);
    println!("Generated {} render primitives\n", renderables.len());
}

fn demo_nested_layout() {
    println!("Demo 3: Nested flexbox layout");
    println!("-----------------------------");
    
    let mut main_container = Column::new()
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(700.0));
            style.set("gap", StyleProperty::Gap(20.0));
            style.set("padding", StyleProperty::Padding(25.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.95, 0.95, 0.95, 1.0)));
            style
        })
        .add_child(
            // Header row
            Row::new()
                .style({
                    let mut style = Style::new();
                    style.set("justify-content", StyleProperty::JustifyContent(JustifyContent::SpaceBetween));
                    style.set("padding", StyleProperty::Padding(15.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.2, 0.4, 0.8, 1.0)));
                    style
                })
                .add_child(Button::new("Logo"))
                .add_child(
                    Row::new()
                        .style({
                            let mut style = Style::new();
                            style.set("gap", StyleProperty::Gap(10.0));
                            style
                        })
                        .add_child(Button::new("Home"))
                        .add_child(Button::new("About"))
                        .add_child(Button::new("Contact"))
                )
        )
        .add_child(
            // Content area
            Row::new()
                .style({
                    let mut style = Style::new();
                    style.set("gap", StyleProperty::Gap(20.0));
                    style.set("height", StyleProperty::Height(400.0));
                    style
                })
                .add_child(
                    // Sidebar
                    Column::new()
                        .style({
                            let mut style = Style::new();
                            style.set("width", StyleProperty::Width(200.0));
                            style.set("padding", StyleProperty::Padding(15.0));
                            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.9, 0.9, 0.9, 1.0)));
                            style
                        })
                        .add_child(Button::new("Menu Item 1"))
                        .add_child(Button::new("Menu Item 2"))
                        .add_child(Button::new("Menu Item 3"))
                )
                .add_child(
                    // Main content
                    Container::new()
                        .style({
                            let mut style = Style::new();
                            style.set("flex-grow", StyleProperty::FlexGrow(1.0));
                            style.set("padding", StyleProperty::Padding(20.0));
                            style.set("background-color", StyleProperty::BackgroundColor(Color::new(1.0, 1.0, 1.0, 1.0)));
                            style
                        })
                        .add_child(Button::new("Main Content Area"))
                )
        );
    
    // Perform layout
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = main_container.layout(&constraints);
    println!("Container size: {}x{}", size.width, size.height);
    
    // Create context and render
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(800.0, 600.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(50.0, 50.0),
    };
    
    let renderables = main_container.render(&ctx);
    println!("Generated {} render primitives for nested layout", renderables.len());
    
    // Print layout tree
    println!("\nLayout structure:");
    println!("└── Column (main container)");
    println!("    ├── Row (header)");
    println!("    │   ├── Button (Logo)");
    println!("    │   └── Row (nav)");
    println!("    │       ├── Button (Home)");
    println!("    │       ├── Button (About)");
    println!("    │       └── Button (Contact)");
    println!("    └── Row (content)");
    println!("        ├── Column (sidebar)");
    println!("        │   ├── Button (Menu Item 1)");
    println!("        │   ├── Button (Menu Item 2)");
    println!("        │   └── Button (Menu Item 3)");
    println!("        └── Container (main content)");
    println!("            └── Button (Main Content Area)");
}