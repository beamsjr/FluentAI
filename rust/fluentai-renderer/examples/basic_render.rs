//! Basic rendering example

use fluentai_renderer::{Renderer, Scene, primitives::{Renderable, Color, Transform, Position3D, Size2D}};
use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

fn main() {
    env_logger::init();
    
    // Create event loop and window
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_title("FluentAI Renderer Demo")
        .build(&event_loop)
        .unwrap();
    
    // Create renderer
    let mut renderer = pollster::block_on(Renderer::new(&window)).unwrap();
    
    // Create scene
    let mut scene = Scene::new();
    
    // Add a red rectangle
    scene.add(Renderable::Rect {
        transform: Transform {
            position: Position3D { x: -0.5, y: 0.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        size: Size2D { width: 0.5, height: 0.5 },
        color: Color::from_hex("#FF0000").unwrap(),
        radius: 0.0,
    });
    
    // Add a blue rectangle
    scene.add(Renderable::Rect {
        transform: Transform {
            position: Position3D { x: 0.5, y: 0.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        size: Size2D { width: 0.5, height: 0.5 },
        color: Color::from_hex("#0000FF").unwrap(),
        radius: 0.0,
    });
    
    // Run event loop
    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Poll;
        
        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                WindowEvent::Resized(size) => {
                    renderer.resize(size);
                }
                _ => {}
            },
            Event::MainEventsCleared => {
                window.request_redraw();
            },
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