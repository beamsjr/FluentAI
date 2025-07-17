//! WASM bindings for 3D Continuum UI

use wasm_bindgen::prelude::*;
use web_sys::{HtmlCanvasElement, WebGl2RenderingContext};
use crate::three_d::{Scene3D, Camera3D, Light, Mesh3D, Node3D};
use crate::webgl_renderer::WebGLRenderer;
use crate::primitives::Color;
use glam::Vec3;
use std::cell::RefCell;
use std::rc::Rc;

/// 3D Continuum UI runtime for WebAssembly
#[wasm_bindgen]
pub struct Continuum3D {
    canvas: HtmlCanvasElement,
    gl_context: WebGl2RenderingContext,
    renderer: Rc<RefCell<WebGLRenderer>>,
    scene: Rc<RefCell<Scene3D>>,
    rotation: f32,
}

#[wasm_bindgen]
impl Continuum3D {
    /// Create a new 3D Continuum instance
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<Continuum3D, JsValue> {
        // Initialize logging
        console_error_panic_hook::set_once();
        console_log::init_with_level(log::Level::Info).ok();
        
        // Get canvas
        let window = web_sys::window().unwrap();
        let document = window.document().unwrap();
        let canvas = document
            .get_element_by_id(canvas_id)
            .ok_or_else(|| JsValue::from_str(&format!("Canvas '{}' not found", canvas_id)))?;
        let canvas: HtmlCanvasElement = canvas.dyn_into()?;
        
        // Get WebGL2 context
        let gl_context = canvas
            .get_context("webgl2")?
            .ok_or_else(|| JsValue::from_str("Failed to get WebGL2 context"))?
            .dyn_into::<WebGl2RenderingContext>()?;
        
        // Create renderer
        let renderer = WebGLRenderer::new(gl_context.clone())?;
        
        // Create default scene
        let scene = Scene3D::create_test_scene();
        
        log::info!("Continuum3D initialized with WebGL2!");
        
        Ok(Continuum3D {
            canvas,
            gl_context,
            renderer: Rc::new(RefCell::new(renderer)),
            scene: Rc::new(RefCell::new(scene)),
            rotation: 0.0,
        })
    }
    
    /// Initialize the holographic globe demo
    #[wasm_bindgen]
    pub fn init_globe_demo(&mut self) -> Result<(), JsValue> {
        let mut scene = Scene3D::new("Holographic Globe".to_string());
        
        // Add materials
        let earth_material = crate::three_d::Material3D::new_pbr(
            "Earth".to_string(),
            Color::new(0.2, 0.5, 0.8, 0.9), // Semi-transparent blue
            0.3,
            0.6,
        );
        let continent_material = crate::three_d::Material3D::new_pbr(
            "Continents".to_string(),
            Color::new(0.3, 0.7, 0.3, 0.8), // Semi-transparent green
            0.2,
            0.7,
        );
        let todo_material = crate::three_d::Material3D::new_pbr(
            "TodoPin".to_string(),
            Color::new(1.0, 0.5, 0.0, 1.0), // Orange
            0.0,
            0.3,
        );
        
        let earth_idx = scene.add_material(earth_material);
        let continent_idx = scene.add_material(continent_material);
        let todo_idx = scene.add_material(todo_material);
        
        // Create globe (sphere)
        let mut globe = Mesh3D::create_sphere(2.0, 64, 32);
        globe.material_index = Some(earth_idx);
        let globe_idx = scene.add_mesh(globe);
        
        // Create proper todo pin mesh (sphere on top of a cone)
        let mut todo_pin = create_pin_mesh(0.3, 0.5);
        todo_pin.material_index = Some(todo_idx);
        let pin_idx = scene.add_mesh(todo_pin);
        
        // Add globe node
        let mut globe_node = crate::three_d::Node3D::new("Globe".to_string());
        globe_node.mesh_index = Some(globe_idx);
        let globe_node_idx = scene.add_node(globe_node);
        
        // Add todo pins at various locations on the globe
        let pin_locations = vec![
            ("New York", 40.7, -74.0),
            ("London", 51.5, -0.1),
            ("Tokyo", 35.7, 139.7),
            ("Sydney", -33.9, 151.2),
            ("São Paulo", -23.5, -46.6),
        ];
        
        for (i, (city, lat, lon)) in pin_locations.iter().enumerate() {
            let lat_rad = (*lat as f32).to_radians();
            let lon_rad = (*lon as f32).to_radians();
            
            // Convert lat/lon to 3D position on sphere
            let x = 2.1 * lat_rad.cos() * lon_rad.sin();
            let y = 2.1 * lat_rad.sin();
            let z = 2.1 * lat_rad.cos() * lon_rad.cos();
            
            let mut pin_node = crate::three_d::Node3D::new(format!("Pin_{}", city));
            pin_node.mesh_index = Some(pin_idx);
            pin_node.set_position(Vec3::new(x as f32, y as f32, z as f32));
            
            scene.add_child(globe_node_idx, pin_node)
                .ok_or_else(|| JsValue::from_str("Failed to add pin node"))?;
        }
        
        // Add lighting
        scene.lights.clear();
        scene.add_light(Light::new_ambient(
            "ambient".to_string(),
            Color::new(0.3, 0.3, 0.4, 1.0),
        ));
        scene.add_light(Light::new_directional(
            "sun".to_string(),
            Vec3::new(-1.0, -0.5, -1.0),
            Color::new(1.0, 0.95, 0.8, 1.0),
            1.2,
        ));
        
        // Position camera
        scene.camera = Camera3D::new_perspective(
            Vec3::new(5.0, 3.0, 5.0),
            Vec3::ZERO,
            std::f32::consts::FRAC_PI_4,
            self.canvas.width() as f32 / self.canvas.height() as f32,
        );
        
        *self.scene.borrow_mut() = scene;
        
        log::info!("Holographic globe demo initialized!");
        Ok(())
    }
    
    /// Update the 3D scene (rotate globe)
    #[wasm_bindgen]
    pub fn update(&mut self, delta_time: f32) {
        self.rotation += delta_time * 0.5; // Rotate at 0.5 rad/s
        
        let mut scene = self.scene.borrow_mut();
        if let Some(globe) = scene.get_node_mut("Globe") {
            globe.set_rotation(Vec3::new(0.0, self.rotation, 0.1));
        }
    }
    
    /// Render the 3D scene
    #[wasm_bindgen]
    pub fn render(&mut self) -> Result<(), JsValue> {
        // Update canvas size if needed
        let width = self.canvas.client_width() as u32;
        let height = self.canvas.client_height() as u32;
        if self.canvas.width() != width || self.canvas.height() != height {
            self.canvas.set_width(width);
            self.canvas.set_height(height);
            self.gl_context.viewport(0, 0, width as i32, height as i32);
            
            // Update camera aspect ratio
            let mut scene = self.scene.borrow_mut();
            scene.camera.set_aspect_ratio(width as f32 / height as f32);
        }
        
        // Render
        let scene = self.scene.borrow();
        let mut renderer = self.renderer.borrow_mut();
        renderer.render_scene(&scene)?;
        
        Ok(())
    }
    
    /// Handle mouse movement for camera orbit
    #[wasm_bindgen]
    pub fn on_mouse_move(&mut self, delta_x: f32, delta_y: f32, is_dragging: bool) {
        if !is_dragging {
            return;
        }
        
        let mut scene = self.scene.borrow_mut();
        scene.camera.orbit(delta_x * 0.01, delta_y * 0.01);
    }
    
    /// Handle scroll for zoom
    #[wasm_bindgen]
    pub fn on_scroll(&mut self, delta: f32) {
        let mut scene = self.scene.borrow_mut();
        scene.camera.zoom(delta * 0.1);
    }
    
    /// Load a glTF model
    #[wasm_bindgen]
    pub fn load_gltf_url(&mut self, url: &str) -> Result<(), JsValue> {
        // In a real implementation, we'd fetch the glTF file
        // For now, just log
        log::info!("Would load glTF from: {}", url);
        Ok(())
    }
}

/// Create a pin-shaped mesh for TODO markers
fn create_pin_mesh(head_radius: f32, total_height: f32) -> Mesh3D {
    use crate::three_d::{Mesh3D, Vertex3D, Vec3, Vec2};
    use std::f32::consts::PI;
    
    let mut mesh = Mesh3D::new("todo_pin".to_string());
    
    // Pin consists of:
    // 1. Sphere head at the top
    // 2. Cone/needle pointing down
    
    let cone_height = total_height - head_radius;
    let cone_base_radius = head_radius * 0.3;
    let segments = 16;
    
    // Generate cone vertices (pointing down from sphere)
    let mut vertex_offset = 0;
    
    // Cone tip (bottom point)
    mesh.vertices.push(Vertex3D::new(
        Vec3::new(0.0, -cone_height, 0.0),
        Vec3::new(0.0, -1.0, 0.0),
        Vec2::new(0.5, 1.0)
    ));
    vertex_offset += 1;
    
    // Cone base vertices (where it meets the sphere)
    for i in 0..segments {
        let angle = (i as f32 / segments as f32) * 2.0 * PI;
        let x = angle.cos() * cone_base_radius;
        let z = angle.sin() * cone_base_radius;
        
        // Normal points outward and down
        let normal = Vec3::new(x, -cone_base_radius, z).normalize();
        
        mesh.vertices.push(Vertex3D::new(
            Vec3::new(x, 0.0, z),
            normal,
            Vec2::new(i as f32 / segments as f32, 0.0)
        ));
    }
    
    // Create cone triangles
    for i in 0..segments {
        let next = (i + 1) % segments;
        // Triangle from tip to base edge
        mesh.indices.push(0);
        mesh.indices.push(vertex_offset + i as u32);
        mesh.indices.push(vertex_offset + next as u32);
    }
    
    vertex_offset += segments;
    
    // Generate sphere vertices for the head
    let rings = 8;
    let sphere_start_idx = mesh.vertices.len() as u32;
    
    for ring in 0..=rings {
        let phi = PI * (ring as f32) / (rings as f32);
        let y = head_radius * phi.cos();
        let ring_radius = head_radius * phi.sin();
        
        for segment in 0..=segments {
            let theta = 2.0 * PI * (segment as f32) / (segments as f32);
            let x = ring_radius * theta.cos();
            let z = ring_radius * theta.sin();
            
            let pos = Vec3::new(x, y + head_radius, z);
            let normal = pos.normalize();
            let uv = Vec2::new(
                segment as f32 / segments as f32,
                ring as f32 / rings as f32
            );
            
            mesh.vertices.push(Vertex3D::new(pos, normal, uv));
        }
    }
    
    // Create sphere triangles
    for ring in 0..rings {
        for segment in 0..segments {
            let curr = sphere_start_idx + ring * (segments + 1) + segment;
            let next = curr + 1;
            let above = curr + segments + 1;
            let above_next = above + 1;
            
            // Two triangles per quad
            mesh.indices.push(curr);
            mesh.indices.push(next);
            mesh.indices.push(above);
            
            mesh.indices.push(next);
            mesh.indices.push(above_next);
            mesh.indices.push(above);
        }
    }
    
    mesh
}

/// Create and initialize a 3D Continuum runtime
#[wasm_bindgen]
pub fn create_continuum_3d(canvas_id: &str) -> Result<Continuum3D, JsValue> {
    Continuum3D::new(canvas_id)
}