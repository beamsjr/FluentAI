// Example demonstrating the FluentAI component system

use fluentai_renderer::components::{
    Component, ComponentContext, Container, Row, Column, Button, TextInput, Style
};
use fluentai_renderer::components::style::{StyleProperty, StyleBuilder};
use fluentai_renderer::primitives::{Color, Renderable};
use cgmath::Vector2;

fn main() {
    println!("FluentAI Component System Demo");
    
    // Create a simple UI layout
    let mut ui = Column::new()
        .style({
            let mut style = Style::new();
            style.set("padding", StyleProperty::Padding(20.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.95, 0.95, 0.95, 1.0)));
            style
        })
        .add_child(
            Row::new()
                .style({
                    let mut style = Style::new();
                    style.set("gap", StyleProperty::Gap(10.0));
                    style.set("margin-bottom", StyleProperty::MarginBottom(20.0));
                    style
                })
                .add_child(
                    Button::new("Submit")
                        .style({
                            let mut style = Style::new();
                            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.2, 0.5, 0.8, 1.0)));
                            style.set("padding", StyleProperty::Padding(10.0));
                            style
                        })
                        .on_click(|| {
                            println!("Submit button clicked!");
                        })
                )
                .add_child(
                    Button::new("Cancel")
                        .style({
                            let mut style = Style::new();
                            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.8, 0.2, 0.2, 1.0)));
                            style.set("padding", StyleProperty::Padding(10.0));
                            style
                        })
                        .on_click(|| {
                            println!("Cancel button clicked!");
                        })
                )
        )
        .add_child(
            TextInput::new()
                .placeholder("Enter your name...")
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(300.0));
                    style.set("height", StyleProperty::Height(40.0));
                    style
                })
                .on_change(|value| {
                    println!("Input changed: {}", value);
                })
        );
    
    // Simulate a layout pass
    let constraints = fluentai_renderer::components::LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = ui.layout(&constraints);
    println!("Layout complete. UI size: {}x{}", size.width, size.height);
    
    // Create a context for rendering
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(800.0, 600.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(100.0, 100.0),
    };
    
    // Render the UI
    let renderables = ui.render(&ctx);
    println!("Generated {} render primitives", renderables.len());
    
    // Print out what would be rendered
    for (i, renderable) in renderables.iter().enumerate() {
        match renderable {
            Renderable::Rect { color, size, .. } => {
                println!("  [{}] Rectangle: {}x{} color: ({:.2}, {:.2}, {:.2}, {:.2})", 
                    i, size.width, size.height, color.r, color.g, color.b, color.a);
            }
            Renderable::Text { content, .. } => {
                println!("  [{}] Text: \"{}\"", i, content);
            }
            _ => {
                println!("  [{}] Other primitive", i);
            }
        }
    }
}