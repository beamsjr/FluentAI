use fluentai_renderer::{
    Renderer, Scene,
    primitives::{Renderable, Transform, Position2D, Position3D, Size2D, Color},
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
        .with_title("FluentAI Text Rendering Demo")
        .build(&event_loop)
        .unwrap();
    
    let mut renderer = Renderer::new(&window).block_on().unwrap();
    let mut scene = Scene::new();
    
    // Add some text elements
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 100.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Hello, FluentAI!".to_string(),
        size: 24.0,
        color: Color::new(1.0, 1.0, 1.0, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 150.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Simple bitmap font rendering".to_string(),
        size: 16.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 200.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string(),
        size: 20.0,
        color: Color::new(0.2, 0.8, 0.2, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 230.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "abcdefghijklmnopqrstuvwxyz".to_string(),
        size: 20.0,
        color: Color::new(0.8, 0.2, 0.2, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 260.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "0123456789 !@#$%^&*()".to_string(),
        size: 20.0,
        color: Color::new(0.2, 0.2, 0.8, 1.0),
        font: None,
    });
    
    // Add a background rectangle
    scene.add(Renderable::Rect {
        transform: Transform::new(Position2D::new(400.0, 300.0)),
        size: Size2D::new(800.0, 600.0),
        color: Color::new(0.1, 0.1, 0.15, 1.0),
        radius: 0.0,
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