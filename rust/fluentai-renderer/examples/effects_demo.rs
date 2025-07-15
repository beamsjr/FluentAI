// Visual effects demonstration - shadows, blur, and filters

use fluentai_renderer::components::{
    Component, ComponentContext, Container, Column, Button, Style,
    LayoutConstraints
};
use fluentai_renderer::components::style::{StyleProperty, BoxShadow, Filter};
use fluentai_renderer::primitives::{Color, Renderable};
use fluentai_renderer::effects::{BoxShadow as EffectBoxShadow, BlurFilter};
use cgmath::Vector2;

fn main() {
    println!("FluentAI Visual Effects Demo");
    println!("============================\n");
    
    // Demo 1: Box shadows
    demo_box_shadows();
    
    // Demo 2: Blur effects
    demo_blur_effects();
    
    // Demo 3: Combined effects
    demo_combined_effects();
}

fn demo_box_shadows() {
    println!("Demo 1: Box Shadow Effects");
    println!("--------------------------");
    
    let mut container = Column::new()
        .style({
            let mut style = Style::new();
            style.set("padding", StyleProperty::Padding(40.0));
            style.set("gap", StyleProperty::Gap(30.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.95, 0.95, 0.95, 1.0)));
            style
        })
        .add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(200.0));
                    style.set("height", StyleProperty::Height(100.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(1.0, 1.0, 1.0, 1.0)));
                    style.set("border-radius", StyleProperty::BorderRadius(8.0));
                    
                    // Basic shadow
                    let shadow = BoxShadow {
                        offset: Vector2::new(0.0, 4.0),
                        blur_radius: 8.0,
                        spread_radius: 0.0,
                        color: Color::new(0.0, 0.0, 0.0, 0.3),
                        inset: false,
                    };
                    style.set("box-shadow", StyleProperty::BoxShadow(shadow));
                    style
                })
                .add_child(Button::new("Basic Shadow"))
        )
        .add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(200.0));
                    style.set("height", StyleProperty::Height(100.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(1.0, 1.0, 1.0, 1.0)));
                    style.set("border-radius", StyleProperty::BorderRadius(8.0));
                    
                    // Elevated shadow
                    let shadow = BoxShadow {
                        offset: Vector2::new(0.0, 8.0),
                        blur_radius: 16.0,
                        spread_radius: 4.0,
                        color: Color::new(0.0, 0.0, 0.0, 0.25),
                        inset: false,
                    };
                    style.set("box-shadow", StyleProperty::BoxShadow(shadow));
                    style
                })
                .add_child(Button::new("Elevated Shadow"))
        )
        .add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(200.0));
                    style.set("height", StyleProperty::Height(100.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.8, 0.9, 1.0, 1.0)));
                    style.set("border-radius", StyleProperty::BorderRadius(8.0));
                    
                    // Inset shadow
                    let shadow = BoxShadow {
                        offset: Vector2::new(0.0, -2.0),
                        blur_radius: 6.0,
                        spread_radius: -2.0,
                        color: Color::new(0.0, 0.0, 0.0, 0.3),
                        inset: true,
                    };
                    style.set("box-shadow", StyleProperty::BoxShadow(shadow));
                    style
                })
                .add_child(Button::new("Inset Shadow"))
        );
    
    // Perform layout
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = container.layout(&constraints);
    println!("Container size: {}x{}", size.width, size.height);
    
    // Render with shadows
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(800.0, 600.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(100.0, 50.0),
    };
    
    let renderables = container.render(&ctx);
    
    // Count shadow layers
    let shadow_count = renderables.iter()
        .filter(|r| matches!(r, Renderable::Rect { .. }))
        .count();
    
    println!("Generated {} render primitives ({} include shadow layers)\n", 
        renderables.len(), shadow_count);
}

fn demo_blur_effects() {
    println!("Demo 2: Blur Filter Effects");
    println!("---------------------------");
    
    let mut container = Column::new()
        .style({
            let mut style = Style::new();
            style.set("padding", StyleProperty::Padding(40.0));
            style.set("gap", StyleProperty::Gap(30.0));
            style
        })
        .add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(300.0));
                    style.set("height", StyleProperty::Height(80.0));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(1.0, 1.0, 1.0, 0.8)));
                    
                    // Blur filter
                    style.set("filter", StyleProperty::Filter(Filter::Blur(4.0)));
                    style
                })
                .add_child(Button::new("Blurred Container"))
        )
        .add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(300.0));
                    style.set("height", StyleProperty::Height(80.0));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.9, 0.9, 0.9, 1.0)));
                    
                    // Brightness filter
                    style.set("filter", StyleProperty::Filter(Filter::Brightness(1.2)));
                    style
                })
                .add_child(Button::new("Brightened Container"))
        )
        .add_child(
            Container::new()
                .style({
                    let mut style = Style::new();
                    style.set("width", StyleProperty::Width(300.0));
                    style.set("height", StyleProperty::Height(80.0));
                    style.set("padding", StyleProperty::Padding(20.0));
                    style.set("background-color", StyleProperty::BackgroundColor(Color::new(0.8, 0.7, 0.6, 1.0)));
                    
                    // Grayscale filter
                    style.set("filter", StyleProperty::Filter(Filter::Grayscale(1.0)));
                    style
                })
                .add_child(Button::new("Grayscale Container"))
        );
    
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = container.layout(&constraints);
    println!("Container size: {}x{}", size.width, size.height);
    println!("Filter effects will be applied during GPU rendering\n");
}

fn demo_combined_effects() {
    println!("Demo 3: Combined Visual Effects");
    println!("--------------------------------");
    
    let mut card = Container::new()
        .style({
            let mut style = Style::new();
            style.set("width", StyleProperty::Width(350.0));
            style.set("height", StyleProperty::Height(200.0));
            style.set("padding", StyleProperty::Padding(30.0));
            style.set("background-color", StyleProperty::BackgroundColor(Color::new(1.0, 1.0, 1.0, 0.95)));
            style.set("border-radius", StyleProperty::BorderRadius(16.0));
            
            // Combine shadow and filter
            let shadow = BoxShadow {
                offset: Vector2::new(0.0, 10.0),
                blur_radius: 30.0,
                spread_radius: -5.0,
                color: Color::new(0.0, 0.0, 0.0, 0.25),
                inset: false,
            };
            style.set("box-shadow", StyleProperty::BoxShadow(shadow));
            
            // Drop shadow filter for colored shadow
            style.set("filter", StyleProperty::Filter(Filter::DropShadow {
                offset_x: 0.0,
                offset_y: 20.0,
                blur: 40.0,
                color: Color::new(0.2, 0.3, 0.8, 0.3),
            }));
            
            style
        })
        .add_child(
            Column::new()
                .style({
                    let mut style = Style::new();
                    style.set("gap", StyleProperty::Gap(15.0));
                    style
                })
                .add_child(Button::new("Glassmorphism Card"))
                .add_child(Button::new("With Multiple Effects"))
        );
    
    let constraints = LayoutConstraints::bounded(
        Vector2::new(0.0, 0.0),
        Vector2::new(800.0, 600.0)
    );
    
    let size = card.layout(&constraints);
    
    // Create context
    let ctx = ComponentContext {
        frame_time: 0.0,
        delta_time: 0.016,
        screen_size: Vector2::new(800.0, 600.0),
        size: Vector2::new(size.width, size.height),
        position: Vector2::new(225.0, 200.0),
    };
    
    let renderables = card.render(&ctx);
    
    println!("Container size: {}x{}", size.width, size.height);
    println!("Generated {} render primitives", renderables.len());
    println!("\nEffects applied:");
    println!("- Box shadow with 30px blur radius");
    println!("- Drop shadow filter with colored shadow");
    println!("- Semi-transparent background for glass effect");
    println!("- 16px border radius for smooth corners");
    
    // Demonstrate effect generation
    let shadow_effect = EffectBoxShadow::new(0.0, 10.0, 30.0, Color::new(0.0, 0.0, 0.0, 0.25));
    if let Some(Renderable::Rect { .. }) = renderables.first() {
        if let Some(shadow_layers) = shadow_effect.generate_shadow_quad(renderables.first().unwrap()) {
            println!("\nShadow decomposed into {} blur layers for smooth rendering", shadow_layers.len());
        }
    }
    
    // Show blur kernel info
    let blur = BlurFilter::gaussian(4.0);
    let kernel = blur.generate_kernel();
    println!("Gaussian blur kernel size: {} taps", kernel.len());
    println!("Kernel weights sum: {:.4}", kernel.iter().sum::<f32>());
}