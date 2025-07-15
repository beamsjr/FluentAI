use fluentai_renderer::{
    Renderer, Scene,
    primitives::{Renderable, Transform, Position2D, Position3D, Size2D, Color},
    gradient::{Gradient, GradientStop, SpreadMethod},
};
use cgmath::Vector2;
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
        .with_title("FluentAI Gradient Demo")
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
        content: "Gradient Demo".to_string(),
        size: 28.0,
        color: Color::new(1.0, 1.0, 1.0, 1.0),
        font: None,
    });
    
    // Create gradients
    let linear_gradient = Gradient::linear(
        Vector2::new(0.0, 0.0),
        Vector2::new(150.0, 0.0)
    )
    .add_stop(0.0, Color::new(1.0, 0.0, 0.0, 1.0))
    .add_stop(0.5, Color::new(1.0, 1.0, 0.0, 1.0))
    .add_stop(1.0, Color::new(0.0, 1.0, 0.0, 1.0));
    
    let radial_gradient = Gradient::radial(
        Vector2::new(75.0, 75.0),
        75.0
    )
    .add_stop(0.0, Color::new(1.0, 1.0, 1.0, 1.0))
    .add_stop(0.5, Color::new(0.5, 0.5, 1.0, 1.0))
    .add_stop(1.0, Color::new(0.0, 0.0, 0.5, 1.0));
    
    let conic_gradient = Gradient::conic(
        Vector2::new(75.0, 75.0),
        0.0
    )
    .add_stop(0.0, Color::new(1.0, 0.0, 0.0, 1.0))
    .add_stop(0.25, Color::new(1.0, 1.0, 0.0, 1.0))
    .add_stop(0.5, Color::new(0.0, 1.0, 0.0, 1.0))
    .add_stop(0.75, Color::new(0.0, 0.0, 1.0, 1.0))
    .add_stop(1.0, Color::new(1.0, 0.0, 0.0, 1.0));
    
    // Add gradients to scene
    scene.add_gradient("linear1".to_string(), linear_gradient);
    scene.add_gradient("radial1".to_string(), radial_gradient);
    scene.add_gradient("conic1".to_string(), conic_gradient);
    
    // Linear gradient rectangle
    scene.add(Renderable::GradientRect {
        transform: Transform {
            position: Position3D { x: 100.0, y: 100.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        size: Size2D::new(150.0, 100.0),
        gradient_id: "linear1".to_string(),
        radius: 0.0,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 175.0, y: 220.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Linear Gradient".to_string(),
        size: 16.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Radial gradient circle
    scene.add(Renderable::GradientCircle {
        transform: Transform {
            position: Position3D { x: 350.0, y: 150.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        radius: 75.0,
        gradient_id: "radial1".to_string(),
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 350.0, y: 250.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Radial Gradient".to_string(),
        size: 16.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Conic gradient rectangle (simulating circle)
    scene.add(Renderable::GradientRect {
        transform: Transform {
            position: Position3D { x: 500.0, y: 100.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        size: Size2D::new(150.0, 150.0),
        gradient_id: "conic1".to_string(),
        radius: 75.0, // Make it circular
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 575.0, y: 270.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Conic Gradient".to_string(),
        size: 16.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Multiple gradient stops
    let multi_stop = Gradient::linear(
        Vector2::new(0.0, 0.0),
        Vector2::new(300.0, 0.0)
    )
    .add_stop(0.0, Color::new(1.0, 0.0, 0.0, 1.0))
    .add_stop(0.2, Color::new(1.0, 0.5, 0.0, 1.0))
    .add_stop(0.4, Color::new(1.0, 1.0, 0.0, 1.0))
    .add_stop(0.6, Color::new(0.0, 1.0, 0.0, 1.0))
    .add_stop(0.8, Color::new(0.0, 0.0, 1.0, 1.0))
    .add_stop(1.0, Color::new(0.5, 0.0, 1.0, 1.0));
    
    scene.add_gradient("rainbow".to_string(), multi_stop);
    
    scene.add(Renderable::GradientRect {
        transform: Transform {
            position: Position3D { x: 100.0, y: 350.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        size: Size2D::new(300.0, 50.0),
        gradient_id: "rainbow".to_string(),
        radius: 25.0,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 250.0, y: 420.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Multi-Stop Rainbow Gradient".to_string(),
        size: 16.0,
        color: Color::new(0.8, 0.8, 0.8, 1.0),
        font: None,
    });
    
    // Note about implementation
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 400.0, y: 500.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Note: Gradients currently render as gray placeholders".to_string(),
        size: 14.0,
        color: Color::new(0.6, 0.6, 0.6, 1.0),
        font: None,
    });
    
    scene.add(Renderable::Text {
        transform: Transform {
            position: Position3D { x: 400.0, y: 520.0, z: 0.0 },
            rotation: (0.0, 0.0, 0.0),
            scale: (1.0, 1.0, 1.0),
        },
        content: "Full GPU gradient rendering will be implemented with shader support".to_string(),
        size: 14.0,
        color: Color::new(0.6, 0.6, 0.6, 1.0),
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