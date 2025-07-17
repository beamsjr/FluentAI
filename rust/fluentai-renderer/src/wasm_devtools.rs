// WASM wrapper for DevTools demo using our actual 3D renderer
// This uses the real FluentAI 3D rendering system with WebGL

use wasm_bindgen::prelude::*;
use web_sys::{console, HtmlCanvasElement};
use crate::webgl_renderer::WebGLRenderer;
use crate::three_d::{Scene3D, Camera3D, Mesh3D, Material3D, MaterialType, Light, LightType};
use crate::primitives::{Color, Transform, Position3D};
use crate::ar::debug_overlay::DebugOverlay;
use glam::{Vec3, Mat4, Quat};
use std::f32::consts::PI;

// Since devtools module is not available in WASM, we'll implement
// the essential parts we need for the demo
#[wasm_bindgen]
pub struct WasmDevToolsDemo {
    renderer: WebGLRenderer,
    scene: Scene3D,
    camera: Camera3D,
    particles: Vec<Particle3D>,
    animation_enabled: bool,
    debug_overlay: DebugOverlay,
    frame_count: u32,
    last_fps_time: f64,
    fps: f32,
    time: f32,
}

struct Particle3D {
    position: Vec3,
    velocity: Vec3,
    rotation: Vec3,
    rotation_speed: Vec3,
    scale: f32,
    color: Color,
    phase: f32,
    mesh_index: usize,
}

#[wasm_bindgen]
impl WasmDevToolsDemo {
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<WasmDevToolsDemo, JsValue> {
        console::log_1(&"Initializing FluentAI 3D WebGL DevTools Demo...".into());
        
        // Get canvas element
        let document = web_sys::window().unwrap().document().unwrap();
        let canvas = document.get_element_by_id(canvas_id)
            .ok_or_else(|| JsValue::from_str("Canvas not found"))?;
        let canvas: HtmlCanvasElement = canvas.dyn_into()?;
        
        // Create WebGL renderer
        let renderer = WebGLRenderer::new(canvas)
            .map_err(|e| JsValue::from_str(&format!("Failed to create WebGL renderer: {:?}", e)))?;
        
        // Create 3D scene
        let mut scene = Scene3D::new();
        
        // Set up camera with a nice angle
        let window = web_sys::window().unwrap();
        let width = window.inner_width().unwrap().as_f64().unwrap() as f32;
        let height = window.inner_height().unwrap().as_f64().unwrap() as f32;
        
        let camera = Camera3D {
            position: Vec3::new(0.0, 5.0, 10.0),
            target: Vec3::ZERO,
            up: Vec3::Y,
            fov: 60.0,
            aspect: width / height,
            near: 0.1,
            far: 1000.0,
        };
        
        // Add lights to the scene
        scene.lights.push(Light {
            enabled: true,
            light_type: LightType::Ambient { 
                color: Color::new(0.2, 0.2, 0.3, 1.0) 
            },
        });
        
        scene.lights.push(Light {
            enabled: true,
            light_type: LightType::Directional {
                direction: Vec3::new(-0.3, -1.0, -0.5).normalize(),
                color: Color::new(1.0, 0.95, 0.9, 1.0),
                intensity: 1.0,
            },
        });
        
        // Create some basic materials for particles
        scene.materials.push(Material3D {
            material_type: MaterialType::Standard {
                base_color: Color::new(0.6, 0.7, 1.0, 0.8),
                metallic: 0.3,
                roughness: 0.7,
            },
        });
        
        Ok(WasmDevToolsDemo {
            renderer,
            scene,
            camera,
            particles: Vec::new(),
            animation_enabled: true,
            debug_overlay: DebugOverlay::new(),
            frame_count: 0,
            last_fps_time: 0.0,
            fps: 60.0,
            time: 0.0,
        })
    }
    
    pub fn init_particles(&mut self, count: u32) {
        self.particles.clear();
        
        // Create a sphere mesh for particles
        let sphere_mesh = self.create_sphere_mesh(1.0, 16, 8);
        let sphere_index = self.scene.meshes.len();
        self.scene.meshes.push(sphere_mesh);
        
        // Create cube mesh for variety
        let cube_mesh = self.create_cube_mesh(1.0);
        let cube_index = self.scene.meshes.len();
        self.scene.meshes.push(cube_mesh);
        
        // Create materials with different colors
        for i in 0..5 {
            let hue = (i as f32 / 5.0) * 360.0;
            let color = self.hsl_to_rgb(hue, 0.7, 0.6);
            self.scene.materials.push(Material3D {
                material_type: MaterialType::Standard {
                    base_color: Color::new(color.0, color.1, color.2, 0.8),
                    metallic: 0.3 + (i as f32 * 0.1),
                    roughness: 0.2 + (i as f32 * 0.15),
                },
            });
        }
        
        for i in 0..count {
            let use_sphere = i % 3 != 0; // Mix of spheres and cubes
            self.particles.push(Particle3D::new(
                if use_sphere { sphere_index } else { cube_index }
            ));
        }
        
        console::log_1(&format!("Initialized {} 3D particles", count).into());
    }
    
    pub fn add_particles(&mut self, count: u32) {
        let sphere_index = 0; // Assume first mesh is sphere
        for _ in 0..count {
            self.particles.push(Particle3D::new(sphere_index));
        }
    }
    
    pub fn clear_particles(&mut self) {
        self.particles.clear();
    }
    
    pub fn set_particle_size(&mut self, size: f32) {
        for particle in &mut self.particles {
            particle.scale = size / 4.0; // Normalize from UI scale
        }
    }
    
    pub fn set_particle_speed(&mut self, speed: f32) {
        for particle in &mut self.particles {
            let factor = speed / 50.0;
            particle.velocity *= factor;
            particle.rotation_speed *= factor;
        }
    }
    
    pub fn toggle_animation(&mut self) {
        self.animation_enabled = !self.animation_enabled;
    }
    
    pub fn toggle_debug_overlay(&mut self) {
        self.debug_overlay.toggle();
    }
    
    pub fn render(&mut self, timestamp: f64) -> Result<(), JsValue> {
        // Calculate FPS
        self.frame_count += 1;
        if timestamp - self.last_fps_time >= 1000.0 {
            self.fps = (self.frame_count as f64 * 1000.0 / (timestamp - self.last_fps_time)) as f32;
            self.frame_count = 0;
            self.last_fps_time = timestamp;
        }
        
        self.time = (timestamp * 0.001) as f32;
        
        // Update camera for a nice rotating view
        let cam_angle = self.time * 0.2;
        self.camera.position = Vec3::new(
            cam_angle.cos() * 15.0,
            8.0 + cam_angle.sin() * 2.0,
            cam_angle.sin() * 15.0
        );
        self.camera.target = Vec3::new(0.0, 0.0, 0.0);
        
        // Update particles
        if self.animation_enabled {
            let dt = 0.016;
            for particle in &mut self.particles {
                particle.update(dt, self.time);
            }
        }
        
        // Clear the scene nodes
        self.scene.nodes.clear();
        self.scene.root_nodes.clear();
        
        // Add a ground plane
        let ground_node = self.scene.add_node(
            Mat4::from_scale_rotation_translation(
                Vec3::new(20.0, 0.1, 20.0),
                Quat::IDENTITY,
                Vec3::new(0.0, -5.0, 0.0)
            ),
            Some(0), // First mesh is ground
            None
        );
        self.scene.root_nodes.push(ground_node);
        
        // Add particle nodes to the scene
        for (i, particle) in self.particles.iter().enumerate() {
            let transform = Mat4::from_scale_rotation_translation(
                Vec3::splat(particle.scale),
                Quat::from_euler(glam::EulerRot::XYZ, particle.rotation.x, particle.rotation.y, particle.rotation.z),
                particle.position
            );
            
            let material_index = (i % 5) + 1; // Skip ground material
            let node = self.scene.add_node(
                transform,
                Some(particle.mesh_index),
                Some(material_index)
            );
            self.scene.root_nodes.push(node);
        }
        
        // Render the 3D scene using our WebGL renderer
        self.renderer.render_scene(&self.scene, &self.camera)?;
        
        Ok(())
    }
    
    pub fn get_stats(&self) -> JsValue {
        let stats = serde_json::json!({
            "fps": self.fps,
            "particleCount": self.particles.len(),
            "drawCalls": self.particles.len(),
            "memory": 20.0 + (self.particles.len() as f32 * 0.1),
            "debugOverlay": self.debug_overlay.is_enabled(),
        });
        
        serde_wasm_bindgen::to_value(&stats).unwrap_or(JsValue::NULL)
    }
    
    // Handle keyboard input
    pub fn handle_key(&mut self, key: &str, ctrl: bool, shift: bool, _alt: bool) {
        match key {
            "d" | "D" => {
                self.toggle_debug_overlay();
                console::log_1(&"Toggled debug overlay".into());
            }
            "p" | "P" if ctrl && shift => {
                console::log_1(&format!("Performance: {} FPS", self.fps).into());
            }
            _ => {}
        }
    }
}

    // Helper method to create sphere mesh
    fn create_sphere_mesh(&self, radius: f32, segments: u32, rings: u32) -> Mesh3D {
        let mut positions = Vec::new();
        let mut normals = Vec::new();
        let mut texcoords = Vec::new();
        let mut indices = Vec::new();
        
        for ring in 0..=rings {
            let theta = (ring as f32 / rings as f32) * PI;
            let sin_theta = theta.sin();
            let cos_theta = theta.cos();
            
            for segment in 0..=segments {
                let phi = (segment as f32 / segments as f32) * 2.0 * PI;
                let sin_phi = phi.sin();
                let cos_phi = phi.cos();
                
                let x = sin_theta * cos_phi;
                let y = cos_theta;
                let z = sin_theta * sin_phi;
                
                positions.extend_from_slice(&[x * radius, y * radius, z * radius]);
                normals.extend_from_slice(&[x, y, z]);
                texcoords.extend_from_slice(&[
                    segment as f32 / segments as f32,
                    ring as f32 / rings as f32,
                ]);
            }
        }
        
        // Generate indices
        for ring in 0..rings {
            for segment in 0..segments {
                let current = ring * (segments + 1) + segment;
                let next = current + segments + 1;
                
                indices.push(current as u16);
                indices.push(next as u16);
                indices.push(current as u16 + 1);
                
                indices.push(current as u16 + 1);
                indices.push(next as u16);
                indices.push(next as u16 + 1);
            }
        }
        
        Mesh3D {
            positions,
            normals,
            texcoords,
            indices,
            material_index: Some(0),
        }
    }
    
    // Helper method to create cube mesh
    fn create_cube_mesh(&self, size: f32) -> Mesh3D {
        let half = size / 2.0;
        
        let positions = vec![
            // Front face
            -half, -half,  half,
             half, -half,  half,
             half,  half,  half,
            -half,  half,  half,
            // Back face
            -half, -half, -half,
            -half,  half, -half,
             half,  half, -half,
             half, -half, -half,
            // Top face
            -half,  half, -half,
            -half,  half,  half,
             half,  half,  half,
             half,  half, -half,
            // Bottom face
            -half, -half, -half,
             half, -half, -half,
             half, -half,  half,
            -half, -half,  half,
            // Right face
             half, -half, -half,
             half,  half, -half,
             half,  half,  half,
             half, -half,  half,
            // Left face
            -half, -half, -half,
            -half, -half,  half,
            -half,  half,  half,
            -half,  half, -half,
        ];
        
        let normals = vec![
            // Front face
            0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,
            0.0, 0.0, 1.0,
            // Back face
            0.0, 0.0, -1.0,
            0.0, 0.0, -1.0,
            0.0, 0.0, -1.0,
            0.0, 0.0, -1.0,
            // Top face
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            // Bottom face
            0.0, -1.0, 0.0,
            0.0, -1.0, 0.0,
            0.0, -1.0, 0.0,
            0.0, -1.0, 0.0,
            // Right face
            1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,
            1.0, 0.0, 0.0,
            // Left face
            -1.0, 0.0, 0.0,
            -1.0, 0.0, 0.0,
            -1.0, 0.0, 0.0,
            -1.0, 0.0, 0.0,
        ];
        
        let texcoords = vec![
            // Repeat for each face
            0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0,
            0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0,
            0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0,
            0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0,
            0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0,
            0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0,
        ];
        
        let indices = vec![
            0,  1,  2,    0,  2,  3,    // front
            4,  5,  6,    4,  6,  7,    // back
            8,  9,  10,   8,  10, 11,   // top
            12, 13, 14,   12, 14, 15,   // bottom
            16, 17, 18,   16, 18, 19,   // right
            20, 21, 22,   20, 22, 23,   // left
        ];
        
        Mesh3D {
            positions,
            normals,
            texcoords,
            indices,
            material_index: Some(0),
        }
    }
    
    // Helper to convert HSL to RGB
    fn hsl_to_rgb(&self, h: f32, s: f32, l: f32) -> (f32, f32, f32) {
        let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
        let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
        let m = l - c / 2.0;
        
        let (r, g, b) = if h < 60.0 {
            (c, x, 0.0)
        } else if h < 120.0 {
            (x, c, 0.0)
        } else if h < 180.0 {
            (0.0, c, x)
        } else if h < 240.0 {
            (0.0, x, c)
        } else if h < 300.0 {
            (x, 0.0, c)
        } else {
            (c, 0.0, x)
        };
        
        (r + m, g + m, b + m)
    }
}

impl Particle3D {
    fn new(mesh_index: usize) -> Self {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        
        Self {
            position: Vec3::new(
                rng.gen_range(-10.0..10.0),
                rng.gen_range(-3.0..3.0),
                rng.gen_range(-10.0..10.0),
            ),
            velocity: Vec3::new(
                rng.gen_range(-2.0..2.0),
                rng.gen_range(-1.0..3.0),
                rng.gen_range(-2.0..2.0),
            ),
            rotation: Vec3::new(
                rng.gen_range(0.0..PI * 2.0),
                rng.gen_range(0.0..PI * 2.0),
                rng.gen_range(0.0..PI * 2.0),
            ),
            rotation_speed: Vec3::new(
                rng.gen_range(-2.0..2.0),
                rng.gen_range(-2.0..2.0),
                rng.gen_range(-2.0..2.0),
            ),
            scale: rng.gen_range(0.2..0.8),
            color: Color::new(
                rng.gen_range(0.4..0.9),
                rng.gen_range(0.5..0.9),
                rng.gen_range(0.6..1.0),
                0.9,
            ),
            phase: rng.gen_range(0.0..PI * 2.0),
            mesh_index,
        }
    }
    
    fn update(&mut self, dt: f32, time: f32) {
        // Update position
        self.position += self.velocity * dt;
        
        // Add some floating motion
        self.position.y += (time + self.phase).sin() * 0.01;
        
        // Update rotation
        self.rotation += self.rotation_speed * dt;
        
        // Bounce off ground
        if self.position.y < -4.0 {
            self.position.y = -4.0;
            self.velocity.y = self.velocity.y.abs() * 0.8;
        }
        
        // Apply gravity
        self.velocity.y -= 9.8 * dt;
        
        // Wrap around in X and Z
        let bounds = 15.0;
        if self.position.x > bounds {
            self.position.x = -bounds;
        } else if self.position.x < -bounds {
            self.position.x = bounds;
        }
        
        if self.position.z > bounds {
            self.position.z = -bounds;
        } else if self.position.z < -bounds {
            self.position.z = bounds;
        }
    }
}

// Export the initialization function
#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
    console::log_1(&"FluentAI WASM DevTools module loaded".into());
}