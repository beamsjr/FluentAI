// CSS Grid layout demonstration

use fluentai_renderer::components::{
    Component, ComponentContext, Grid, Button, Container, Style,
    LayoutConstraints, GridAlign
};
use fluentai_renderer::components::style::StyleProperty;
use fluentai_renderer::primitives::Color;
use cgmath::Vector2;

fn main() {
    println!("FluentAI CSS Grid Layout Demo");
    println!("=============================\n");
    
    // Demo 1: Basic grid layout
    demo_basic_grid();
    
    // Demo 2: Grid with spans
    demo_grid_spans();
    
    // Demo 3: Responsive grid layout
    demo_responsive_grid();
    
    // Demo 4: Grid areas (dashboard layout)
    demo_dashboard_grid();
}

fn demo_basic_grid() {
    println!("Demo 1: Basic Grid Layout (3x3)");
    println!("--------------------------------");
    
    let mut grid = Grid::new()
        .columns("100px 100px 100px")
        .rows("100px 100px 100px")
        .gap(10.0)
        .style({
            let mut style = Style::new();
            style.set("padding", StyleProperty::Padding(20.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.95, 0.95, 0.95, 1.0)));
            style
        });
    
    // Add 9 items
    for i in 1..=9 {
        grid = grid.add_child(
            Button::new(&format!("Item {}", i))
                .style({
                    let mut style = Style::new();
                    style.set("padding", StyleProperty::Padding(10.0));
                    style.set("background-color", StyleProperty::BackgroundColor(
                        Color::new(0.8, 0.8 + (i as f32 * 0.02), 0.9, 1.0)
                    ));
                    style
                })
        );
    }
    
    // Perform layout
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = grid.layout(&constraints);
    println!("Grid size: {}x{}", size.width, size.height);
    println!("Expected: 360x360 (3x100px + 2x10px gap + 2x20px padding)\n");
}

fn demo_grid_spans() {
    println!("Demo 2: Grid with Spanning Items");
    println!("---------------------------------");
    
    let mut grid = Grid::new()
        .columns("1fr 1fr 1fr 1fr")
        .rows("100px 100px 100px")
        .gap(15.0)
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(600.0));
            style.set("padding", StyleProperty::Padding(30.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.9, 0.9, 0.95, 1.0)));
            style
        })
        // Header spanning all columns
        .add_child_span(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.2, 0.4, 0.8, 1.0)));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style
                })
                .add_child(Button::new("Header - Spans 4 Columns")),
            1, 1, 1, 4  // row 1, spans 1 row, col 1, spans 4 cols
        )
        // Sidebar spanning 2 rows
        .add_child_span(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.8, 0.9, 0.8, 1.0)));
                    style.set("padding", StyleProperty::Padding(15.0));
                    style
                })
                .add_child(Button::new("Sidebar")),
            2, 2, 1, 1  // row 2, spans 2 rows, col 1, spans 1 col
        )
        // Main content
        .add_child_span(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(1.0, 1.0, 1.0, 1.0)));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style
                })
                .add_child(Button::new("Main Content - 2x3")),
            2, 2, 2, 3  // row 2, spans 2 rows, col 2, spans 3 cols
        );
    
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = grid.layout(&constraints);
    println!("Grid size: {}x{}", size.width, size.height);
    println!("Layout structure:");
    println!("┌─────────────────────────────┐");
    println!("│         Header (4)          │");
    println!("├─────┬───────────────────────┤");
    println!("│ (1) │                       │");
    println!("│ Side│    Main Content       │");
    println!("│ bar │        (2x3)          │");
    println!("│     │                       │");
    println!("└─────┴───────────────────────┘\n");
}

fn demo_responsive_grid() {
    println!("Demo 3: Responsive Grid Layout");
    println!("-------------------------------");
    
    let mut grid = Grid::new()
        .columns("repeat(auto-fit, minmax(150px, 1fr))")  // Note: This syntax would need parser support
        .rows("auto")
        .gap(20.0)
        .align_items(GridAlign::Center)
        .justify_items(GridAlign::Center)
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(700.0));
            style.set("padding", StyleProperty::Padding(25.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.98, 0.98, 0.98, 1.0)));
            style
        });
    
    // Add responsive cards
    let card_colors = [
        (0.9, 0.7, 0.7),
        (0.7, 0.9, 0.7),
        (0.7, 0.7, 0.9),
        (0.9, 0.9, 0.7),
        (0.9, 0.7, 0.9),
        (0.7, 0.9, 0.9),
    ];
    
    for (i, &(r, g, b)) in card_colors.iter().enumerate() {
        grid = grid.add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(150.0));
                    style.set("height", StyleProperty::Height(120.0));
                    style.set("padding", StyleProperty::Padding(15.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(r, g, b, 1.0)));
                    style.set("border-radius", StyleProperty::BorderRadius(8.0));
                    style
                })
                .add_child(Button::new(&format!("Card {}", i + 1)))
        );
    }
    
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = grid.layout(&constraints);
    println!("Grid size: {}x{}", size.width, size.height);
    println!("Cards will wrap based on available width\n");
}

fn demo_dashboard_grid() {
    println!("Demo 4: Dashboard Grid Layout");
    println!("-----------------------------");
    
    let dashboard = Grid::new()
        .columns("200px 1fr 300px")
        .rows("60px 1fr 40px")
        .row_column_gap(0.0, 1.0)  // No row gap, 1px column gap
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(800.0));
            style.set("height", StyleProperty::Height(500.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.1, 0.1, 0.1, 1.0)));
            style
        })
        // Top nav bar (spans all columns)
        .add_child_span(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.2, 0.2, 0.3, 1.0)));
                    style.set("padding", StyleProperty::Padding(15.0));
                    style
                })
                .add_child(Button::new("Navigation Bar")),
            1, 1, 1, 3
        )
        // Left sidebar
        .add_child_at(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.15, 0.15, 0.2, 1.0)));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style
                })
                .add_child(Button::new("Sidebar Menu")),
            2, 1
        )
        // Main content area
        .add_child_at(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.95, 0.95, 0.95, 1.0)));
                    style.set("padding", StyleProperty::Padding(30.0));
                    style
                })
                .add_child(Button::new("Main Content Area")),
            2, 2
        )
        // Right panel
        .add_child_at(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.9, 0.9, 0.92, 1.0)));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style
                })
                .add_child(Button::new("Right Panel")),
            2, 3
        )
        // Footer (spans all columns)
        .add_child_span(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.1, 0.1, 0.15, 1.0)));
                    style.set("padding", StyleProperty::Padding(10.0));
                    style
                })
                .add_child(Button::new("Footer")),
            3, 1, 1, 3
        );
    
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(1024.0, 768.0)
    );
    
    let size = dashboard.layout(&constraints);
    println!("Dashboard size: {}x{}", size.width, size.height);
    println!("\nLayout structure:");
    println!("┌─────────────────────────────────────────┐");
    println!("│            Navigation Bar               │ 60px");
    println!("├──────────┬─────────────────┬───────────┤");
    println!("│          │                 │           │");
    println!("│ Sidebar  │   Main Content  │   Right   │ 1fr");
    println!("│  (200px) │      (1fr)      │  (300px)  │");
    println!("│          │                 │           │");
    println!("├──────────┴─────────────────┴───────────┤");
    println!("│                Footer                   │ 40px");
    println!("└─────────────────────────────────────────┘");
    
    // Create context and render
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(1024.0, 768.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(112.0, 134.0),
    };
    
    let renderables = dashboard.render(&ctx);
    println!("\nGenerated {} render primitives for dashboard", renderables.len());
}