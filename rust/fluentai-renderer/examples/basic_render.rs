//! Basic rendering example

use fluentai_renderer::{Renderer, Scene, primitives::{Renderable, Color, Transform, Position3D, Size2D}};
use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};
use std::sync::Arc;

fn main() {
    env_logger::init();
    
    // Create event loop and window
    let event_loop = EventLoop::new().unwrap();
    let window = Arc::new(WindowBuilder::new()
        .with_title("FluentAI Renderer Demo")
        .build(&event_loop)
        .unwrap());
    
    // Create renderer
    let mut renderer = pollster::block_on(Renderer::new(window.clone())).unwrap();
    
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
    let _ = event_loop.run(move |event, elwt| {
        elwt.set_control_flow(ControlFlow::Poll);
        
        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => elwt.exit(),
                WindowEvent::Resized(size) => {
                    renderer.resize(size);
                }
                WindowEvent::RedrawRequested => {
                    match renderer.render(&scene) {
                        Ok(_) => {}
                        Err(e) => eprintln!("Render error: {}", e),
                    }
                }
                _ => {}
            },
            Event::AboutToWait => {
                window.request_redraw();
            },
            _ => {}
        }
    });
}