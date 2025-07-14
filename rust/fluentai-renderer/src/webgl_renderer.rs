//! WebGL-based 3D renderer for browser

use wasm_bindgen::prelude::*;
use web_sys::{WebGl2RenderingContext, WebGlProgram, WebGlShader, WebGlBuffer, WebGlUniformLocation};
use crate::three_d::{Scene3D, Camera3D, Light, LightType, Mesh3D, Material3D, MaterialType};
use crate::primitives::Color;
use glam::{Mat4, Vec3};
use std::collections::HashMap;

/// Vertex shader for 3D rendering
const VERTEX_SHADER_SOURCE: &str = r#"#version 300 es
precision highp float;

in vec3 a_position;
in vec3 a_normal;
in vec2 a_texcoord;

uniform mat4 u_model;
uniform mat4 u_view;
uniform mat4 u_projection;
uniform mat4 u_normalMatrix;

out vec3 v_position;
out vec3 v_normal;
out vec2 v_texcoord;

void main() {
    vec4 worldPos = u_model * vec4(a_position, 1.0);
    v_position = worldPos.xyz;
    v_normal = normalize((u_normalMatrix * vec4(a_normal, 0.0)).xyz);
    v_texcoord = a_texcoord;
    
    gl_Position = u_projection * u_view * worldPos;
}
"#;

/// Fragment shader with basic lighting
const FRAGMENT_SHADER_SOURCE: &str = r#"#version 300 es
precision highp float;

in vec3 v_position;
in vec3 v_normal;
in vec2 v_texcoord;

uniform vec3 u_cameraPosition;
uniform vec4 u_baseColor;
uniform float u_metallic;
uniform float u_roughness;

// Lights
uniform vec3 u_ambientLight;
uniform vec3 u_lightDirection;
uniform vec3 u_lightColor;
uniform float u_lightIntensity;

out vec4 fragColor;

void main() {
    vec3 normal = normalize(v_normal);
    vec3 viewDir = normalize(u_cameraPosition - v_position);
    
    // Simple lambertian diffuse
    float NdotL = max(dot(normal, -u_lightDirection), 0.0);
    vec3 diffuse = u_baseColor.rgb * u_lightColor * u_lightIntensity * NdotL;
    
    // Simple specular
    vec3 halfVector = normalize(-u_lightDirection + viewDir);
    float NdotH = max(dot(normal, halfVector), 0.0);
    float specular = pow(NdotH, 32.0) * (1.0 - u_roughness);
    
    vec3 color = u_ambientLight * u_baseColor.rgb + diffuse + vec3(specular);
    fragColor = vec4(color, u_baseColor.a);
}
"#;

/// WebGL-based 3D renderer
pub struct WebGLRenderer {
    gl: WebGl2RenderingContext,
    program: WebGlProgram,
    // Attribute locations
    position_attrib: u32,
    normal_attrib: u32,
    texcoord_attrib: u32,
    // Uniform locations
    uniforms: HashMap<String, WebGlUniformLocation>,
    // Mesh buffers
    mesh_buffers: HashMap<usize, MeshBuffers>,
}

struct MeshBuffers {
    vertex_buffer: WebGlBuffer,
    index_buffer: WebGlBuffer,
    index_count: i32,
}

impl WebGLRenderer {
    /// Create a new WebGL renderer
    pub fn new(gl: WebGl2RenderingContext) -> Result<Self, JsValue> {
        // Compile shaders
        let vertex_shader = compile_shader(&gl, WebGl2RenderingContext::VERTEX_SHADER, VERTEX_SHADER_SOURCE)?;
        let fragment_shader = compile_shader(&gl, WebGl2RenderingContext::FRAGMENT_SHADER, FRAGMENT_SHADER_SOURCE)?;
        
        // Create program
        let program = gl.create_program()
            .ok_or_else(|| JsValue::from_str("Failed to create program"))?;
        
        gl.attach_shader(&program, &vertex_shader);
        gl.attach_shader(&program, &fragment_shader);
        gl.link_program(&program);
        
        if !gl.get_program_parameter(&program, WebGl2RenderingContext::LINK_STATUS).as_bool().unwrap_or(false) {
            let error = gl.get_program_info_log(&program).unwrap_or_else(|| "Unknown error".to_string());
            return Err(JsValue::from_str(&format!("Failed to link program: {}", error)));
        }
        
        // Get attribute locations
        let position_attrib = gl.get_attrib_location(&program, "a_position") as u32;
        let normal_attrib = gl.get_attrib_location(&program, "a_normal") as u32;
        let texcoord_attrib = gl.get_attrib_location(&program, "a_texcoord") as u32;
        
        // Get uniform locations
        let mut uniforms = HashMap::new();
        let uniform_names = vec![
            "u_model", "u_view", "u_projection", "u_normalMatrix",
            "u_cameraPosition", "u_baseColor", "u_metallic", "u_roughness",
            "u_ambientLight", "u_lightDirection", "u_lightColor", "u_lightIntensity",
        ];
        
        for name in uniform_names {
            if let Some(location) = gl.get_uniform_location(&program, name) {
                uniforms.insert(name.to_string(), location);
            }
        }
        
        // Set up WebGL state
        gl.enable(WebGl2RenderingContext::DEPTH_TEST);
        gl.enable(WebGl2RenderingContext::CULL_FACE);
        
        Ok(Self {
            gl,
            program,
            position_attrib,
            normal_attrib,
            texcoord_attrib,
            uniforms,
            mesh_buffers: HashMap::new(),
        })
    }
    
    /// Render a 3D scene
    pub fn render_scene(&mut self, scene: &Scene3D) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // Clear
        gl.clear_color(0.1, 0.1, 0.1, 1.0);
        gl.clear(WebGl2RenderingContext::COLOR_BUFFER_BIT | WebGl2RenderingContext::DEPTH_BUFFER_BIT);
        
        // Use program
        gl.use_program(Some(&self.program));
        
        // Set camera uniforms
        self.set_camera_uniforms(&scene.camera)?;
        
        // Set lighting uniforms
        self.set_lighting_uniforms(&scene.lights)?;
        
        // Render all nodes
        for &root_idx in &scene.roots {
            self.render_node(scene, root_idx, Mat4::IDENTITY)?;
        }
        
        Ok(())
    }
    
    /// Set camera-related uniforms
    fn set_camera_uniforms(&self, camera: &Camera3D) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // View matrix
        if let Some(location) = self.uniforms.get("u_view") {
            let view = camera.view_matrix();
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(&view));
        }
        
        // Projection matrix
        if let Some(location) = self.uniforms.get("u_projection") {
            let projection = camera.projection_matrix();
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(&projection));
        }
        
        // Camera position
        if let Some(location) = self.uniforms.get("u_cameraPosition") {
            gl.uniform3f(Some(location), camera.position.x, camera.position.y, camera.position.z);
        }
        
        Ok(())
    }
    
    /// Set lighting uniforms
    fn set_lighting_uniforms(&self, lights: &[Light]) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // For now, just use the first ambient and directional light
        let mut ambient = Color::new(0.1, 0.1, 0.1, 1.0);
        let mut light_dir = Vec3::new(-0.3, -1.0, -0.5);
        let mut light_color = Color::new(1.0, 1.0, 1.0, 1.0);
        let mut light_intensity = 1.0;
        
        for light in lights {
            if !light.enabled {
                continue;
            }
            
            match &light.light_type {
                LightType::Ambient { color } => {
                    ambient = *color;
                }
                LightType::Directional { direction, color, intensity } => {
                    light_dir = *direction;
                    light_color = *color;
                    light_intensity = *intensity;
                }
                _ => {} // Handle other light types later
            }
        }
        
        // Set uniforms
        if let Some(location) = self.uniforms.get("u_ambientLight") {
            gl.uniform3f(Some(location), ambient.r, ambient.g, ambient.b);
        }
        
        if let Some(location) = self.uniforms.get("u_lightDirection") {
            let dir = light_dir.normalize();
            gl.uniform3f(Some(location), dir.x, dir.y, dir.z);
        }
        
        if let Some(location) = self.uniforms.get("u_lightColor") {
            gl.uniform3f(Some(location), light_color.r, light_color.g, light_color.b);
        }
        
        if let Some(location) = self.uniforms.get("u_lightIntensity") {
            gl.uniform1f(Some(location), light_intensity);
        }
        
        Ok(())
    }
    
    /// Render a node and its children
    fn render_node(&mut self, scene: &Scene3D, node_idx: usize, parent_transform: Mat4) -> Result<(), JsValue> {
        let node = &scene.nodes[node_idx];
        let world_transform = parent_transform * node.transform;
        
        // Render mesh if present
        if let Some(mesh_idx) = node.mesh_index {
            if let Some(mesh) = scene.meshes.get(mesh_idx) {
                self.render_mesh(mesh, &world_transform, &scene.materials)?;
            }
        }
        
        // Render children
        for &child_idx in &node.children {
            self.render_node(scene, child_idx, world_transform)?;
        }
        
        Ok(())
    }
    
    /// Render a single mesh
    fn render_mesh(&mut self, mesh: &Mesh3D, transform: &Mat4, materials: &[Material3D]) -> Result<(), JsValue> {
        // Upload mesh data if not cached
        let mesh_key = mesh as *const _ as usize;
        if !self.mesh_buffers.contains_key(&mesh_key) {
            self.upload_mesh(mesh)?;
        }
        
        let gl = &self.gl;
        
        let mesh_key = mesh as *const _ as usize;
        let buffers = &self.mesh_buffers[&mesh_key];
        
        // Set transform uniforms
        if let Some(location) = self.uniforms.get("u_model") {
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(transform));
        }
        
        if let Some(location) = self.uniforms.get("u_normalMatrix") {
            let normal_matrix = transform.inverse().transpose();
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(&normal_matrix));
        }
        
        // Set material uniforms
        let material = mesh.material_index
            .and_then(|idx| materials.get(idx))
            .unwrap_or(&materials[0]);
        
        self.set_material_uniforms(material)?;
        
        // Bind vertex buffer
        gl.bind_buffer(WebGl2RenderingContext::ARRAY_BUFFER, Some(&buffers.vertex_buffer));
        
        // Set up attributes
        let stride = 8 * 4; // 8 floats per vertex
        gl.vertex_attrib_pointer_with_i32(self.position_attrib, 3, WebGl2RenderingContext::FLOAT, false, stride, 0);
        gl.enable_vertex_attrib_array(self.position_attrib);
        
        gl.vertex_attrib_pointer_with_i32(self.normal_attrib, 3, WebGl2RenderingContext::FLOAT, false, stride, 3 * 4);
        gl.enable_vertex_attrib_array(self.normal_attrib);
        
        gl.vertex_attrib_pointer_with_i32(self.texcoord_attrib, 2, WebGl2RenderingContext::FLOAT, false, stride, 6 * 4);
        gl.enable_vertex_attrib_array(self.texcoord_attrib);
        
        // Bind index buffer and draw
        gl.bind_buffer(WebGl2RenderingContext::ELEMENT_ARRAY_BUFFER, Some(&buffers.index_buffer));
        gl.draw_elements_with_i32(
            WebGl2RenderingContext::TRIANGLES,
            buffers.index_count,
            WebGl2RenderingContext::UNSIGNED_SHORT,
            0,
        );
        
        Ok(())
    }
    
    /// Upload mesh data to GPU
    fn upload_mesh(&mut self, mesh: &Mesh3D) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // Create vertex data
        let mut vertex_data = Vec::new();
        for vertex in &mesh.vertices {
            vertex_data.push(vertex.position.x);
            vertex_data.push(vertex.position.y);
            vertex_data.push(vertex.position.z);
            vertex_data.push(vertex.normal.x);
            vertex_data.push(vertex.normal.y);
            vertex_data.push(vertex.normal.z);
            vertex_data.push(vertex.tex_coords.x);
            vertex_data.push(vertex.tex_coords.y);
        }
        
        // Create vertex buffer
        let vertex_buffer = gl.create_buffer()
            .ok_or_else(|| JsValue::from_str("Failed to create vertex buffer"))?;
        
        gl.bind_buffer(WebGl2RenderingContext::ARRAY_BUFFER, Some(&vertex_buffer));
        
        unsafe {
            let array = js_sys::Float32Array::view(&vertex_data);
            gl.buffer_data_with_array_buffer_view(
                WebGl2RenderingContext::ARRAY_BUFFER,
                &array,
                WebGl2RenderingContext::STATIC_DRAW,
            );
        }
        
        // Convert indices to u16
        let index_data: Vec<u16> = mesh.indices.iter()
            .map(|&i| i as u16)
            .collect();
        
        // Create index buffer
        let index_buffer = gl.create_buffer()
            .ok_or_else(|| JsValue::from_str("Failed to create index buffer"))?;
        
        gl.bind_buffer(WebGl2RenderingContext::ELEMENT_ARRAY_BUFFER, Some(&index_buffer));
        
        unsafe {
            let array = js_sys::Uint16Array::view(&index_data);
            gl.buffer_data_with_array_buffer_view(
                WebGl2RenderingContext::ELEMENT_ARRAY_BUFFER,
                &array,
                WebGl2RenderingContext::STATIC_DRAW,
            );
        }
        
        self.mesh_buffers.insert(
            mesh as *const _ as usize,
            MeshBuffers {
                vertex_buffer,
                index_buffer,
                index_count: mesh.indices.len() as i32,
            },
        );
        
        Ok(())
    }
    
    /// Set material uniforms
    fn set_material_uniforms(&self, material: &Material3D) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        match &material.material_type {
            MaterialType::PBR { base_color, metallic, roughness, .. } => {
                if let Some(location) = self.uniforms.get("u_baseColor") {
                    gl.uniform4f(Some(location), base_color.r, base_color.g, base_color.b, material.opacity);
                }
                if let Some(location) = self.uniforms.get("u_metallic") {
                    gl.uniform1f(Some(location), *metallic);
                }
                if let Some(location) = self.uniforms.get("u_roughness") {
                    gl.uniform1f(Some(location), *roughness);
                }
            }
            _ => {
                // Default material
                if let Some(location) = self.uniforms.get("u_baseColor") {
                    gl.uniform4f(Some(location), 0.8, 0.8, 0.8, 1.0);
                }
                if let Some(location) = self.uniforms.get("u_metallic") {
                    gl.uniform1f(Some(location), 0.0);
                }
                if let Some(location) = self.uniforms.get("u_roughness") {
                    gl.uniform1f(Some(location), 0.5);
                }
            }
        }
        
        Ok(())
    }
}

/// Compile a shader
fn compile_shader(
    gl: &WebGl2RenderingContext,
    shader_type: u32,
    source: &str,
) -> Result<WebGlShader, JsValue> {
    let shader = gl.create_shader(shader_type)
        .ok_or_else(|| JsValue::from_str("Failed to create shader"))?;
    
    gl.shader_source(&shader, source);
    gl.compile_shader(&shader);
    
    if !gl.get_shader_parameter(&shader, WebGl2RenderingContext::COMPILE_STATUS).as_bool().unwrap_or(false) {
        let error = gl.get_shader_info_log(&shader).unwrap_or_else(|| "Unknown error".to_string());
        return Err(JsValue::from_str(&format!("Failed to compile shader: {}", error)));
    }
    
    Ok(shader)
}

/// Convert Mat4 to array for WebGL
fn mat4_to_array(mat: &Mat4) -> [f32; 16] {
    mat.to_cols_array()
}