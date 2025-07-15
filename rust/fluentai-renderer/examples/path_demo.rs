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
        .with_title("FluentAI Path & Bezier Curves Demo")
        .build(&event_loop)
        .unwrap();
    
    let mut renderer = Renderer::new(&window).block_on().unwrap();
    let mut scene = Scene::new();
    
    // Background
    scene.add(Renderable::Rect {
        transform: Transform::new(Position2D::new(400.0, 300.0)),
        size: Size2D::new(800.0, 600.0),
        color: Color::new(0.05, 0.05, 0.1, 1.0),
        radius: 0.0,
    });
    
    // Title
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 400.0, y: 30.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Path & Bezier Curves Demo".to_string(),
        size: 28.0,
        color: Color::new(1.0, 1.0, 1.0, 1.0),
        font: None,
    });
    
    // Simple line path
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 100.0, y: 100.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 0 0 L 50 50 L 100 0 L 150 50".to_string(),
        stroke: Some((Color::new(0.2, 0.8, 0.2, 1.0), 3.0)),
        fill: None,
    });
    
    // Quadratic bezier curve
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 300.0, y: 100.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 0 50 Q 50 0 100 50".to_string(),
        stroke: Some((Color::new(0.8, 0.2, 0.2, 1.0), 3.0)),
        fill: None,
    });
    
    // Cubic bezier curve
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 500.0, y: 100.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 0 50 C 20 0 80 0 100 50".to_string(),
        stroke: Some((Color::new(0.2, 0.2, 0.8, 1.0), 3.0)),
        fill: None,
    });
    
    // Heart shape using bezier curves
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 150.0, y: 250.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 50 25 C 50 10 30 0 20 0 C 0 0 0 20 0 20 C 0 20 0 40 20 60 L 50 90 L 80 60 C 100 40 100 20 100 20 C 100 20 100 0 80 0 C 70 0 50 10 50 25 Z".to_string(),
        stroke: Some((Color::new(0.9, 0.1, 0.3, 1.0), 2.0)),
        fill: None,
    });
    
    // Star shape
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 350.0, y: 250.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 50 0 L 60 35 L 95 35 L 68 55 L 78 90 L 50 70 L 22 90 L 32 55 L 5 35 L 40 35 Z".to_string(),
        stroke: Some((Color::new(0.9, 0.7, 0.1, 1.0), 2.0)),
        fill: None,
    });
    
    // Complex path with multiple subpaths
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 550.0, y: 250.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 10 10 L 90 10 L 90 90 L 10 90 Z M 30 30 L 70 30 L 70 70 L 30 70 Z".to_string(),
        stroke: Some((Color::new(0.5, 0.8, 0.5, 1.0), 2.0)),
        fill: None,
    });
    
    // Smooth curve path
    scene.add(Renderable::Path {
        transform: Transform {
            position: Position3D { x: 100.0, y: 400.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        data: "M 0 50 C 50 0 100 100 150 50 C 200 0 250 100 300 50".to_string(),
        stroke: Some((Color::new(0.7, 0.3, 0.9, 1.0), 4.0)),
        fill: None,
    });
    
    // Labels
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 100.0, y: 180.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Lines".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 300.0, y: 180.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Quadratic".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 500.0, y: 180.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Cubic".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 150.0, y: 370.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Heart".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 350.0, y: 370.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Star".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 550.0, y: 370.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Subpaths".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 250.0, y: 480.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Smooth Wave".to_string(),
        size: 14.0,
        color: Color::new(0.7, 0.7, 0.7, 1.0),
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