// A working 3D demo for WASM using our actual FluentAI renderer
use wasm_bindgen::prelude::*;
use web_sys::{console, HtmlCanvasElement, WebGl2RenderingContext};
use crate::webgl_renderer::WebGLRenderer;
use crate::three_d::{Scene3D, Camera3D, Node3D, Mesh3D, Material3D, MaterialType, Light, LightType, CameraType, Vertex3D};
use crate::primitives::Color;
use glam::{Vec2, Vec3, Mat4, Quat};
use std::f32::consts::PI;
use std::collections::VecDeque;

#[wasm_bindgen]
pub struct FluentAI3DDemo {
    renderer: WebGLRenderer,
    scene: Scene3D,
    camera: Camera3D,
    time: f32,
    mouse_x: f32,
    mouse_y: f32,
    canvas_width: f32,
    canvas_height: f32,
    // Particle system parameters
    particle_count: usize,
    orbit_speed: f32,
    particle_size: f32,
    orbit_radius: f32,
    glow_intensity: f32,
    mouse_gravity: f32,
    // Particle history for trails
    particle_history: Vec<VecDeque<Vec3>>,
    particle_random_offsets: Vec<Vec3>,
    particle_random_phases: Vec<f32>,
    tail_length: usize,
}

#[wasm_bindgen] 
impl FluentAI3DDemo {
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<FluentAI3DDemo, JsValue> {
        console::log_1(&"Initializing FluentAI 3D Demo...".into());
        
        // Get canvas
        let document = web_sys::window().unwrap().document().unwrap();
        let canvas = document.get_element_by_id(canvas_id)
            .ok_or_else(|| JsValue::from_str("Canvas not found"))?;
        let canvas: HtmlCanvasElement = canvas.dyn_into()?;
        
        // Get WebGL2 context
        console::log_1(&"Getting WebGL2 context...".into());
        let gl = canvas
            .get_context("webgl2")?
            .ok_or_else(|| JsValue::from_str("WebGL2 not supported"))?
            .dyn_into::<WebGl2RenderingContext>()?;
        console::log_1(&"WebGL2 context obtained successfully".into());
        
        // Create WebGL renderer
        let mut renderer = WebGLRenderer::new(gl)?;
        
        // Create default particle texture
        renderer.create_default_particle_texture()?;
        
        // Create scene
        let mut scene = Scene3D::new("Demo Scene".to_string());
        
        // Set up camera
        let window = web_sys::window().unwrap();
        let width = window.inner_width().unwrap().as_f64().unwrap() as f32;
        let height = window.inner_height().unwrap().as_f64().unwrap() as f32;
        
        let camera = Camera3D {
            position: Vec3::new(5.0, 5.0, 5.0),
            target: Vec3::ZERO,
            up: Vec3::Y,
            projection: CameraType::Perspective {
                fov: 60.0_f32.to_radians(),
                near: 0.1,
                far: 100.0,
            },
            aspect_ratio: width / height,
        };
        
        // Add lights
        scene.lights.push(Light {
            id: "ambient".to_string(),
            enabled: true,
            light_type: LightType::Ambient { 
                color: Color::new(0.05, 0.05, 0.1, 1.0) // Very dark with slight blue tint
            },
            cast_shadows: false,
        });
        
        scene.lights.push(Light {
            id: "directional".to_string(),
            enabled: true,
            light_type: LightType::Directional {
                direction: Vec3::new(-1.0, -1.0, -0.5).normalize(),
                color: Color::new(1.0, 0.95, 0.8, 1.0),
                intensity: 1.0,
            },
            cast_shadows: true,
        });
        
        // Create materials
        // Particle material - cooler cyan/blue tones like the reference
        scene.materials.push(Material3D {
            name: "particle_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.5, 0.8, 1.0, 1.0), // More cyan
                metallic: 0.0,
                roughness: 0.1,
                emissive: Color::new(0.2, 0.5, 0.8, 1.0), // Cyan/blue glow
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.8,
            double_sided: true,
        });
        
        // Glowing center material - more like a star
        scene.materials.push(Material3D {
            name: "glow_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(1.0, 1.0, 1.0, 0.2), // Much more transparent
                metallic: 0.0,
                roughness: 0.0,
                emissive: Color::new(30.0, 30.0, 30.0, 1.0), // Even more extreme brightness
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.2, // Very transparent to show rays through it
            double_sided: true,
        });
        
        // Secondary glow material - bluish outer glow
        scene.materials.push(Material3D {
            name: "outer_glow_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.7, 0.8, 1.0, 0.1), // Even more transparent
                metallic: 0.0,
                roughness: 0.0,
                emissive: Color::new(8.0, 10.0, 15.0, 1.0), // Blue-heavy emission
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.1, // Very transparent
            double_sided: true,
        });
        
        // Star ray material - for the light rays
        scene.materials.push(Material3D {
            name: "ray_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(1.0, 1.0, 1.0, 0.8), // Brighter base
                metallic: 0.0,
                roughness: 0.0,
                emissive: Color::new(25.0, 25.0, 30.0, 1.0), // Much brighter rays
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.7, // More opaque rays
            double_sided: true,
        });
        
        // Create sphere meshes for particles
        let mut small_sphere = create_sphere_mesh(1.0, 12, 6); // Lower poly for particles
        small_sphere.material_index = Some(0); // Particle material
        scene.meshes.push(small_sphere);
        
        // Create a higher quality sphere for the center
        let mut center_sphere = create_sphere_mesh(1.0, 24, 12);
        center_sphere.material_index = Some(1); // Glow material
        scene.meshes.push(center_sphere);
        
        // Create outer glow sphere
        let mut outer_glow_sphere = create_sphere_mesh(1.0, 16, 8);
        outer_glow_sphere.material_index = Some(2); // Outer glow material
        scene.meshes.push(outer_glow_sphere);
        
        // Create a flattened sphere for star rays
        let mut ray_sphere = create_sphere_mesh(1.0, 8, 4);
        ray_sphere.material_index = Some(3); // Ray material
        scene.meshes.push(ray_sphere);
        
        // Initialize particle history and random offsets
        let max_particles = 500;
        let mut particle_history = Vec::with_capacity(max_particles);
        let mut particle_random_offsets = Vec::with_capacity(max_particles);
        let mut particle_random_phases = Vec::with_capacity(max_particles);
        
        for i in 0..max_particles {
            particle_history.push(VecDeque::with_capacity(20));
            // Random offsets for each particle
            let seed = i as f32 * 0.7853981634; // Random-ish seed
            particle_random_offsets.push(Vec3::new(
                (seed * 1.3).sin() * 0.5,
                (seed * 2.1).cos() * 0.3,
                (seed * 0.9).sin() * 0.5
            ));
            particle_random_phases.push((seed * 3.7).sin() * PI * 2.0);
        }
        
        Ok(FluentAI3DDemo {
            renderer,
            scene,
            camera,
            time: 0.0,
            mouse_x: 0.5,
            mouse_y: 0.5,
            canvas_width: width,
            canvas_height: height,
            particle_count: 100,
            orbit_speed: 1.0,
            particle_size: 0.5,
            orbit_radius: 3.0,
            glow_intensity: 1.0,
            mouse_gravity: 1.0,
            particle_history,
            particle_random_offsets,
            particle_random_phases,
            tail_length: 10,
        })
    }
    
    pub fn update_mouse_position(&mut self, x: f32, y: f32) {
        // Normalize mouse position to 0-1 range
        self.mouse_x = x / self.canvas_width;
        self.mouse_y = y / self.canvas_height;
    }
    
    pub fn update_canvas_size(&mut self, width: f32, height: f32) {
        self.canvas_width = width;
        self.canvas_height = height;
        self.camera.aspect_ratio = width / height;
        self.renderer.set_viewport(width as i32, height as i32);
    }
    
    pub fn set_particle_count(&mut self, count: usize) {
        self.particle_count = count.max(1).min(500);
    }
    
    pub fn set_orbit_speed(&mut self, speed: f32) {
        self.orbit_speed = speed.max(0.0).min(3.0);
    }
    
    pub fn set_particle_size(&mut self, size: f32) {
        self.particle_size = size.max(0.3).min(2.3);
    }
    
    pub fn set_orbit_radius(&mut self, radius: f32) {
        self.orbit_radius = radius.max(1.0).min(6.0);
    }
    
    pub fn set_glow_intensity(&mut self, intensity: f32) {
        self.glow_intensity = intensity.max(0.1).min(2.0);
    }
    
    pub fn set_mouse_gravity(&mut self, gravity: f32) {
        self.mouse_gravity = gravity.max(0.0).min(10.0);
    }
    
    pub fn set_tail_length(&mut self, length: usize) {
        self.tail_length = length.max(0).min(20);
    }
    
    pub fn render(&mut self, timestamp: f64) -> Result<(), JsValue> {
        self.time = (timestamp * 0.001) as f32;
        
        // Log first frame only
        if self.time < 0.1 {
            console::log_1(&format!("Rendering frame at time: {}", self.time).into());
        }
        
        // Animate camera in a smooth orbit (no mouse influence on camera)
        let mouse_influence_x = (self.mouse_x - 0.5) * 2.0; // -1 to 1
        let mouse_influence_y = (self.mouse_y - 0.5) * 2.0; // -1 to 1
        let base_angle = self.time * 0.2;
        let camera_radius = 10.0;
        
        self.camera.position = Vec3::new(
            base_angle.cos() * camera_radius,
            6.0,
            base_angle.sin() * camera_radius
        );
        
        // Clear scene nodes
        self.scene.nodes.clear();
        self.scene.roots.clear();
        
        // Prepare particle data
        let mut particle_positions = Vec::with_capacity(self.particle_count);
        let mut particle_sizes = Vec::with_capacity(self.particle_count);
        
        // Update light intensity for the glow effect
        if let Some(light) = self.scene.lights.iter_mut().find(|l| l.id == "ambient") {
            if let LightType::Ambient { ref mut color } = light.light_type {
                let glow = self.glow_intensity * 0.1; // Subtle effect
                *color = Color::new(0.05 + 0.05 * glow, 0.05 + 0.05 * glow, 0.1 + 0.1 * glow, 1.0);
            }
        }
        
        // Update center sphere's emissive material based on glow intensity
        if let Some(material) = self.scene.materials.get_mut(1) {
            if let MaterialType::PBR { ref mut emissive, ref mut base_color, .. } = material.material_type {
                // Pulsing animation for star-like effect
                let pulse = 1.0 + (self.time * 1.5).sin() * 0.15;
                *emissive = Color::new(
                    20.0 * self.glow_intensity * pulse,
                    20.0 * self.glow_intensity * pulse,
                    20.0 * self.glow_intensity * pulse,
                    1.0
                );
            }
        }
        
        // Update outer glow material
        if let Some(material) = self.scene.materials.get_mut(2) {
            if let MaterialType::PBR { ref mut emissive, .. } = material.material_type {
                let pulse = 1.0 + (self.time * 1.2).sin() * 0.2;
                *emissive = Color::new(
                    8.0 * self.glow_intensity * pulse,
                    10.0 * self.glow_intensity * pulse,
                    15.0 * self.glow_intensity * pulse,
                    1.0
                );
            }
        }
        
        // Update ray material
        if let Some(material) = self.scene.materials.get_mut(3) {
            if let MaterialType::PBR { ref mut emissive, ref mut base_color, .. } = material.material_type {
                let pulse = 1.0 + (self.time * 2.0).sin() * 0.3;
                *emissive = Color::new(
                    10.0 * self.glow_intensity * pulse,
                    12.0 * self.glow_intensity * pulse,
                    15.0 * self.glow_intensity * pulse,
                    1.0
                );
                base_color.a = 0.3 + (self.time * 3.5).sin() * 0.2;
            }
        }
        
        // Add central glowing sphere (light source) - tiny bright core
        let mut center_node = Node3D::new("center_glow".to_string());
        let core_scale = 0.05 + (self.time * 2.0).sin() * 0.01 * self.glow_intensity; // Much smaller
        center_node.transform = Mat4::from_scale_rotation_translation(
            Vec3::splat(core_scale),
            Quat::from_rotation_y(self.time * 0.5),
            Vec3::ZERO
        );
        center_node.mesh_index = Some(1); // High quality sphere mesh
        self.scene.add_node(center_node);
        
        // Add outer glow layer - also smaller
        let mut outer_glow_node = Node3D::new("outer_glow".to_string());
        let outer_scale = 0.2 + (self.time * 1.5).sin() * 0.05 * self.glow_intensity; // Much smaller
        outer_glow_node.transform = Mat4::from_scale_rotation_translation(
            Vec3::splat(outer_scale),
            Quat::from_rotation_y(-self.time * 0.3),
            Vec3::ZERO
        );
        outer_glow_node.mesh_index = Some(2); // Outer glow sphere
        self.scene.add_node(outer_glow_node);
        
        // Add multiple star ray layers
        for i in 0..3 {
            let angle = i as f32 * 60.0_f32.to_radians();
            let mut ray_node = Node3D::new(format!("star_ray_{}", i));
            
            // Create elongated star rays - much longer and thinner
            let ray_scale_x = 5.0 + (self.time * 1.8 + angle).sin() * 1.0; // Longer rays
            let ray_scale_y = 0.05; // Thinner
            let ray_scale_z = 0.05; // Thinner
            
            ray_node.transform = Mat4::from_scale_rotation_translation(
                Vec3::new(ray_scale_x, ray_scale_y, ray_scale_z),
                Quat::from_rotation_z(angle) * Quat::from_rotation_y(self.time * 0.2),
                Vec3::ZERO
            );
            ray_node.mesh_index = Some(3); // Ray material sphere
            self.scene.add_node(ray_node);
            
            // Add perpendicular ray
            let mut perp_ray_node = Node3D::new(format!("star_ray_perp_{}", i));
            perp_ray_node.transform = Mat4::from_scale_rotation_translation(
                Vec3::new(ray_scale_y, ray_scale_x, ray_scale_z),
                Quat::from_rotation_z(angle) * Quat::from_rotation_x(self.time * 0.2),
                Vec3::ZERO
            );
            perp_ray_node.mesh_index = Some(3);
            self.scene.add_node(perp_ray_node);
        }
        
        
        // Mouse ray casting - convert screen coordinates to world position
        // Convert mouse coordinates to normalized device coordinates (-1 to 1)
        let ndc_x = (self.mouse_x - 0.5) * 2.0;
        let ndc_y = -(self.mouse_y - 0.5) * 2.0; // Flip Y
        
        // Get camera parameters
        let aspect = self.camera.aspect_ratio;
        let fov = match self.camera.projection {
            CameraType::Perspective { fov, .. } => fov,
            _ => 60.0_f32.to_radians(),
        };
        
        // Calculate ray direction in view space
        let tan_half_fov = (fov / 2.0).tan();
        let ray_x = ndc_x * tan_half_fov * aspect;
        let ray_y = ndc_y * tan_half_fov;
        
        // Camera basis vectors
        let cam_forward = (self.camera.target - self.camera.position).normalize();
        let cam_right = cam_forward.cross(Vec3::Y).normalize();
        let cam_up = cam_right.cross(cam_forward).normalize();
        
        // Ray from camera through mouse position
        let ray_origin = self.camera.position;
        let ray_dir = (cam_forward + cam_right * ray_x + cam_up * ray_y).normalize();
        
        // Intersect with Y=0 plane
        let mouse_world_pos = if ray_dir.y.abs() > 0.001 {
            let t = -ray_origin.y / ray_dir.y;
            if t > 0.0 {
                ray_origin + ray_dir * t
            } else {
                Vec3::ZERO
            }
        } else {
            Vec3::ZERO
        };
        
        // Debug log mouse position occasionally
        if self.time % 2.0 < 0.02 && self.mouse_gravity > 0.0 {
            console::log_1(&format!("Mouse: screen({:.2}, {:.2}) -> NDC({:.2}, {:.2}) -> world({:.2}, {:.2}, {:.2})", 
                self.mouse_x, self.mouse_y, ndc_x, ndc_y,
                mouse_world_pos.x, mouse_world_pos.y, mouse_world_pos.z).into());
        }
        
        // Visual indicator commented out - no longer needed
        // if self.mouse_gravity > 0.1 {
        //     let mut mouse_indicator = Node3D::new("mouse_indicator".to_string());
        //     mouse_indicator.transform = Mat4::from_scale_rotation_translation(
        //         Vec3::splat(0.3 + (self.time * 5.0).sin() * 0.1), // Pulsing effect
        //         Quat::from_rotation_y(self.time * 2.0),
        //         mouse_world_pos
        //     );
        //     mouse_indicator.mesh_index = Some(0); // Particle material (blue)
        //     self.scene.add_node(mouse_indicator);
        // }
        
        // Add corona particles very close to the center for light ray effect
        let corona_count = 20;
        for i in 0..corona_count {
            let angle = (i as f32 / corona_count as f32) * PI * 2.0;
            let ray_time = self.time * 0.3 + angle;
            
            // Create elongated rays
            for j in 0..3 {
                let distance = 0.2 + (j as f32) * 0.15 + (ray_time * 2.0).sin() * 0.1;
                let ray_pos = Vec3::new(
                    angle.cos() * distance,
                    (ray_time + j as f32).sin() * 0.1,
                    angle.sin() * distance
                );
                
                particle_positions.push(ray_pos);
                particle_sizes.push(0.4 * (1.0 - (j as f32) / 3.0)); // Fade out
            }
        }
        
        // Create particles orbiting around the center
        for i in 0..self.particle_count {
            let particle_time = self.time * self.orbit_speed;
            let particle_phase = (i as f32 / self.particle_count as f32) * PI * 2.0;
            
            // Create multiple orbital patterns
            let orbit_variation = (i % 3) as f32;
            let vertical_offset = ((i % 5) as f32 - 2.0) * 0.3;
            
            // Base orbit position
            let orbit_angle = particle_phase + particle_time * (1.0 + orbit_variation * 0.2);
            let secondary_angle = particle_time * 2.0 + particle_phase * 2.0;
            
            // Get random offset and phase for this particle
            let random_offset = self.particle_random_offsets[i];
            let random_phase = self.particle_random_phases[i];
            
            // Add more complex random movement
            let random_time = particle_time + random_phase;
            let random_movement = Vec3::new(
                (random_time * 1.7).sin() * random_offset.x + (random_time * 3.3).cos() * random_offset.x * 0.5,
                (random_time * 2.3).cos() * random_offset.y + (random_time * 1.9).sin() * random_offset.y * 0.7,
                (random_time * 1.1).sin() * random_offset.z + (random_time * 2.7).cos() * random_offset.z * 0.6
            );
            
            // Calculate base particle position with randomness
            let particle_pos = Vec3::new(
                orbit_angle.cos() * self.orbit_radius + secondary_angle.cos() * 0.3,
                vertical_offset + (secondary_angle * 1.5).sin() * 0.5,
                orbit_angle.sin() * self.orbit_radius + secondary_angle.sin() * 0.3
            ) + random_movement;
            
            // Calculate localized gravity/attraction using the pre-calculated mouse position
            let to_mouse = mouse_world_pos - particle_pos;
            let distance = to_mouse.length();
            
            // Apply gravity within a certain radius
            let gravity_radius = 3.0;
            let gravity_strength = self.mouse_gravity * 1.2;
            let min_distance = 0.5; // Prevent particles from getting too close
            
            let pos = if distance < gravity_radius && distance > min_distance {
                // Gravity gets stronger as particles get closer (inverse square law-ish)
                let gravity_factor = 1.0 - (distance / gravity_radius);
                let gravity_force = gravity_factor * gravity_factor * gravity_strength;
                
                // Pull particles toward the mouse
                let pull_dir = to_mouse.normalize();
                let new_pos = particle_pos + pull_dir * gravity_force * 0.1; // Small steps for smooth motion
                
                // But also add some orbital motion when close
                if distance < gravity_radius * 0.5 {
                    let tangent = Vec3::new(-pull_dir.z, 0.0, pull_dir.x); // Perpendicular in XZ plane
                    new_pos + tangent * (gravity_factor * 0.3)
                } else {
                    new_pos
                }
            } else if distance <= min_distance {
                // Push away if too close to prevent clustering
                let push_dir = to_mouse.normalize();
                particle_pos - push_dir * 0.2
            } else {
                particle_pos
            };
            
            // Particle pulsing
            let pulse = 1.0 + (particle_time * 3.0 + particle_phase).sin() * 0.2;
            
            // Update particle history for trails
            if let Some(history) = self.particle_history.get_mut(i) {
                history.push_front(pos);
                if history.len() > self.tail_length {
                    history.pop_back();
                }
            }
            
            // Add to particle arrays
            particle_positions.push(pos);
            particle_sizes.push(self.particle_size * pulse);
            
            // Add trail particles
            if let Some(history) = self.particle_history.get(i) {
                for (j, &trail_pos) in history.iter().enumerate().take(self.tail_length) {
                    let trail_factor = 1.0 - (j as f32 / self.tail_length as f32);
                    let trail_size = self.particle_size * pulse * trail_factor * 0.7; // Smaller trail particles
                    
                    // Only add if size is significant
                    if trail_size > 0.1 {
                        particle_positions.push(trail_pos);
                        particle_sizes.push(trail_size);
                    }
                }
            }
        }
        
        // Update particle buffer in renderer
        self.renderer.update_particles(&particle_positions, &particle_sizes)?;
        
        // Render the scene to framebuffer for bloom effect
        self.renderer.render_scene_to_framebuffer(&self.scene)?;
        
        // Render particles as point sprites with additive blending
        let particle_material = &self.scene.materials[0]; // Particle material
        if let MaterialType::PBR { base_color, emissive, .. } = &particle_material.material_type {
            // Enable additive blending for particles
            self.renderer.gl.enable(WebGl2RenderingContext::BLEND);
            self.renderer.gl.blend_func(WebGl2RenderingContext::SRC_ALPHA, WebGl2RenderingContext::ONE);
            self.renderer.gl.depth_mask(false);
            
            self.renderer.render_particles(&self.camera, base_color, emissive, self.canvas_width, self.canvas_height)?;
            
            // Reset state
            self.renderer.gl.disable(WebGl2RenderingContext::BLEND);
            self.renderer.gl.depth_mask(true);
        }
        
        // Apply bloom post-processing
        self.renderer.apply_bloom(self.glow_intensity)?;
        
        Ok(())
    }
}

// Helper functions to create meshes
fn create_cube_mesh(size: f32) -> Mesh3D {
    let half = size / 2.0;
    let mut mesh = Mesh3D::new("cube".to_string());
    
    // Define cube vertices with positions, normals, and tex coords
    let face_data = vec![
        // Front face
        (Vec3::new(0.0, 0.0, 1.0), vec![
            Vec3::new(-half, -half,  half), Vec3::new( half, -half,  half),
            Vec3::new( half,  half,  half), Vec3::new(-half,  half,  half),
        ]),
        // Back face
        (Vec3::new(0.0, 0.0, -1.0), vec![
            Vec3::new( half, -half, -half), Vec3::new(-half, -half, -half),
            Vec3::new(-half,  half, -half), Vec3::new( half,  half, -half),
        ]),
        // Top face
        (Vec3::new(0.0, 1.0, 0.0), vec![
            Vec3::new(-half,  half,  half), Vec3::new( half,  half,  half),
            Vec3::new( half,  half, -half), Vec3::new(-half,  half, -half),
        ]),
        // Bottom face
        (Vec3::new(0.0, -1.0, 0.0), vec![
            Vec3::new(-half, -half, -half), Vec3::new( half, -half, -half),
            Vec3::new( half, -half,  half), Vec3::new(-half, -half,  half),
        ]),
        // Right face
        (Vec3::new(1.0, 0.0, 0.0), vec![
            Vec3::new( half, -half,  half), Vec3::new( half, -half, -half),
            Vec3::new( half,  half, -half), Vec3::new( half,  half,  half),
        ]),
        // Left face
        (Vec3::new(-1.0, 0.0, 0.0), vec![
            Vec3::new(-half, -half, -half), Vec3::new(-half, -half,  half),
            Vec3::new(-half,  half,  half), Vec3::new(-half,  half, -half),
        ]),
    ];
    
    let tex_coords = vec![
        Vec2::new(0.0, 0.0), Vec2::new(1.0, 0.0),
        Vec2::new(1.0, 1.0), Vec2::new(0.0, 1.0),
    ];
    
    // Build vertices
    for (normal, positions) in face_data {
        for (i, pos) in positions.iter().enumerate() {
            mesh.vertices.push(Vertex3D::new(*pos, normal, tex_coords[i]));
        }
    }
    
    // Build indices
    for face in 0..6 {
        let base = face * 4;
        mesh.indices.extend_from_slice(&[
            base, base + 1, base + 2,
            base, base + 2, base + 3,
        ]);
    }
    
    mesh
}

fn create_sphere_mesh(radius: f32, segments: u32, rings: u32) -> Mesh3D {
    let mut mesh = Mesh3D::new("sphere".to_string());
    
    // Generate vertices
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
            
            let position = Vec3::new(x * radius, y * radius, z * radius);
            let normal = Vec3::new(x, y, z).normalize();
            let tex_coords = Vec2::new(
                segment as f32 / segments as f32,
                ring as f32 / rings as f32,
            );
            
            mesh.vertices.push(Vertex3D::new(position, normal, tex_coords));
        }
    }
    
    // Generate indices
    for ring in 0..rings {
        for segment in 0..segments {
            let current = ring * (segments + 1) + segment;
            let next = current + segments + 1;
            
            mesh.indices.push(current);
            mesh.indices.push(next);
            mesh.indices.push(current + 1);
            
            mesh.indices.push(current + 1);
            mesh.indices.push(next);
            mesh.indices.push(next + 1);
        }
    }
    
    mesh
}

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
    console::log_1(&"FluentAI 3D Demo WASM module loaded".into());
}