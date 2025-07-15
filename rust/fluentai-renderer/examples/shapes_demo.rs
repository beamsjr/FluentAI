use fluentai_renderer::{
    Renderer, Scene,
    primitives::{Renderable, Transform, Position2D, Position3D, Size2D, Color},
    simple_text::{SimpleTextLayout, HorizontalAlign, VerticalAlign},
};
use pollster::FutureExt;
use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

fn main() {
    env_logger::init();
    
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_title("FluentAI Shapes Demo")
        .build(&event_loop)
        .unwrap();
    
    let mut renderer = Renderer::new(&window).block_on().unwrap();
    let mut scene = Scene::new();
    
    // Background
    scene.add(Renderable::Rect {
        transform: Transform::new(Position2D::new(400.0, 300.0)),
        size: Size2D::new(800.0, 600.0),
        color: Color::new(0.1, 0.1, 0.15, 1.0),
        radius: 0.0,
    });
    
    // Title text
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 400.0, y: 50.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "FluentAI Shapes Demo".to_string(),
        size: 32.0,
        color: Color::new(1.0, 1.0, 1.0, 1.0),
        font: None,
    });
    
    // Circles row
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 120.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Circles:".to_string(),
        size: 20.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Different sized circles
    for i in 0..5 {
        let x = 200.0 + i as f32 * 100.0;
        let radius = 20.0 + i as f32 * 5.0;
        let color = Color::new(
            0.2 + i as f32 * 0.15,
            0.3 + i as f32 * 0.1,
            0.8 - i as f32 * 0.1,
            1.0
        );
        
        scene.add(Renderable::Circle {
            transform: Transform {
                position: Position3D { x, y: 150.0, z: 0.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            radius,
            color,
        });
    }
    
    // Ellipses row
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 220.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Ellipses:".to_string(),
        size: 20.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Different ellipses
    for i in 0..5 {
        let x = 200.0 + i as f32 * 100.0;
        let width = 60.0 + i as f32 * 10.0;
        let height = 40.0 - i as f32 * 5.0;
        let color = Color::new(
            0.8 - i as f32 * 0.1,
            0.2 + i as f32 * 0.15,
            0.3 + i as f32 * 0.1,
            1.0
        );
        
        scene.add(Renderable::Ellipse {
            transform: Transform {
                position: Position3D { x, y: 250.0, z: 0.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            width,
            height,
            color,
        });
    }
    
    // Lines row
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 320.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Lines:".to_string(),
        size: 20.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Different lines
    for i in 0..5 {
        let x = 200.0 + i as f32 * 100.0;
        let width = 1.0 + i as f32 * 2.0;
        let color = Color::new(
            0.3 + i as f32 * 0.1,
            0.8 - i as f32 * 0.15,
            0.2 + i as f32 * 0.1,
            1.0
        );
        
        scene.add(Renderable::Line {
            start: Position3D { x, y: 350.0, z: 0.0 },
            end: Position3D { x: x + 50.0, y: 380.0 + i as f32 * 10.0, z: 0.0 },
            width,
            color,
        });
    }
    
    // Text alignment demo
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 440.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Text Alignment:".to_string(),
        size: 20.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Multi-line text with wrapping
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 200.0, y: 470.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "This is a longer text that demonstrates word wrapping and text alignment features in the FluentAI renderer.".to_string(),
        size: 16.0,
        color: Color::new(0.9, 0.9, 0.9, 1.0),
        font: None,
    });
    
    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Poll;
        
        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => *control_flow = ControlFlow::Exit,
            
            Event::WindowEvent {
                event: WindowEvent::Resized(physical_size),
                ..
            } => {
                renderer.resize(physical_size);
            }
            
            Event::MainEventsCleared => {
                window.request_redraw();
            }
            
            Event::RedrawRequested(_) => {
                match renderer.render(&scene) {
                    Ok(_) => {}
                    Err(e) => eprintln!("Render error: {}", e),
                }
            }
            
            _ => {}
        }
    });
}