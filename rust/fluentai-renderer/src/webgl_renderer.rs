//! WebGL-based 3D renderer for browser

use wasm_bindgen::prelude::*;
use web_sys::{WebGl2RenderingContext, WebGlProgram, WebGlShader, WebGlBuffer, WebGlUniformLocation, HtmlCanvasElement, WebGlTexture, WebGlFramebuffer, WebGlRenderbuffer};
use crate::three_d::{Scene3D, Camera3D, Light, LightType, Mesh3D, Material3D, MaterialType};
use crate::primitives::Color;
use glam::{Mat4, Vec3};
use std::collections::HashMap;
use wasm_bindgen::JsCast;

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
uniform vec3 u_emissive;

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
    
    vec3 color = u_ambientLight * u_baseColor.rgb + diffuse + vec3(specular) + u_emissive;
    
    // For highly emissive objects, create a star-like radial glow
    float emissiveStrength = length(u_emissive);
    if (emissiveStrength > 1.0) {
        // Fresnel-like edge glow for star appearance
        float fresnel = 1.0 - abs(dot(normal, viewDir));
        fresnel = pow(fresnel, 0.5); // Softer falloff
        color += u_emissive * fresnel * 2.0;
        
        // Make the center brighter
        float centerGlow = pow(max(0.0, dot(normal, viewDir)), 0.3);
        color += u_emissive * centerGlow;
    }
    
    // Output linear color for post-processing
    // Tone mapping will be applied after bloom
    fragColor = vec4(color, u_baseColor.a);
}
"#;

/// Vertex shader for particle rendering
const PARTICLE_VERTEX_SHADER: &str = r#"#version 300 es
precision highp float;

in vec3 a_position;
in float a_size;

uniform mat4 u_model;
uniform mat4 u_view;
uniform mat4 u_projection;
uniform vec2 u_resolution;

out vec2 v_texcoord;

void main() {
    vec4 mvPosition = u_view * u_model * vec4(a_position, 1.0);
    
    // Calculate point size based on distance
    float pointSize = a_size * 300.0 / -mvPosition.z;
    gl_PointSize = max(1.0, pointSize);
    
    gl_Position = u_projection * mvPosition;
}
"#;

/// Post-processing vertex shader (screen quad)
const POST_VERTEX_SHADER: &str = r#"#version 300 es
precision highp float;

in vec2 a_position;
out vec2 v_texcoord;

void main() {
    v_texcoord = a_position * 0.5 + 0.5;
    gl_Position = vec4(a_position, 0.0, 1.0);
}
"#;

/// Bloom extract and combine shader
const BLOOM_FRAGMENT_SHADER: &str = r#"#version 300 es
precision highp float;

in vec2 v_texcoord;
out vec4 fragColor;

uniform sampler2D u_sceneTexture;
uniform sampler2D u_bloomTexture;
uniform float u_bloomIntensity;
uniform float u_bloomThreshold;
uniform bool u_extractBright;

void main() {
    vec4 color = texture(u_sceneTexture, v_texcoord);
    
    if (u_extractBright) {
        // Extract bright pixels for bloom
        float brightness = dot(color.rgb, vec3(0.2126, 0.7152, 0.0722));
        if (brightness > u_bloomThreshold) {
            fragColor = vec4(color.rgb * (brightness - u_bloomThreshold), color.a);
        } else {
            fragColor = vec4(0.0);
        }
    } else {
        // Combine original scene with bloom
        vec4 bloom = texture(u_bloomTexture, v_texcoord);
        color.rgb += bloom.rgb * u_bloomIntensity;
        
        // Tone mapping
        color.rgb = color.rgb / (color.rgb + vec3(1.0));
        color.rgb = pow(color.rgb, vec3(1.0/2.2));
        
        fragColor = color;
    }
}
"#;

/// Gaussian blur shader
const BLUR_FRAGMENT_SHADER: &str = r#"#version 300 es
precision highp float;

in vec2 v_texcoord;
out vec4 fragColor;

uniform sampler2D u_texture;
uniform vec2 u_direction;
uniform vec2 u_resolution;

void main() {
    vec2 texelSize = 1.0 / u_resolution;
    vec4 color = vec4(0.0);
    
    // 9-tap Gaussian blur
    float weights[5];
    weights[0] = 0.227027;
    weights[1] = 0.1945946;
    weights[2] = 0.1216216;
    weights[3] = 0.054054;
    weights[4] = 0.016216;
    
    color += texture(u_texture, v_texcoord) * weights[0];
    
    for (int i = 1; i < 5; i++) {
        vec2 offset = float(i) * texelSize * u_direction;
        color += texture(u_texture, v_texcoord + offset) * weights[i];
        color += texture(u_texture, v_texcoord - offset) * weights[i];
    }
    
    fragColor = color;
}
"#;

/// Fragment shader for particles
const PARTICLE_FRAGMENT_SHADER: &str = r#"#version 300 es
precision highp float;

uniform vec4 u_baseColor;
uniform vec3 u_emissive;
uniform sampler2D u_particleTexture;
uniform bool u_useTexture;

out vec4 fragColor;

void main() {
    vec4 texColor = vec4(1.0);
    float alpha = 1.0;
    
    if (u_useTexture) {
        // Sample texture for particle sprite
        texColor = texture(u_particleTexture, gl_PointCoord);
        alpha = texColor.a;
    } else {
        // Default circular gradient for particles without texture
        vec2 coord = gl_PointCoord - vec2(0.5);
        float dist = length(coord);
        
        // Soft circular gradient
        alpha = 1.0 - smoothstep(0.0, 0.5, dist);
        
        // Discard pixels outside circle
        if (dist > 0.5) {
            discard;
        }
    }
    
    // Combine texture color with base color
    vec3 color = u_baseColor.rgb * texColor.rgb + u_emissive;
    
    // Output linear color for post-processing
    // Tone mapping will be applied after bloom
    fragColor = vec4(color, alpha * u_baseColor.a);
}
"#;

/// WebGL-based 3D renderer
pub struct WebGLRenderer {
    pub gl: WebGl2RenderingContext,
    program: WebGlProgram,
    particle_program: WebGlProgram,
    // Post-processing
    post_program: Option<WebGlProgram>,
    blur_program: Option<WebGlProgram>,
    // Attribute locations
    position_attrib: u32,
    normal_attrib: u32,
    texcoord_attrib: u32,
    // Particle attribute locations
    particle_position_attrib: u32,
    particle_size_attrib: u32,
    // Uniform locations
    uniforms: HashMap<String, WebGlUniformLocation>,
    particle_uniforms: HashMap<String, WebGlUniformLocation>,
    post_uniforms: HashMap<String, WebGlUniformLocation>,
    blur_uniforms: HashMap<String, WebGlUniformLocation>,
    // Mesh buffers
    mesh_buffers: HashMap<usize, MeshBuffers>,
    // Particle system
    particle_buffer: Option<WebGlBuffer>,
    particle_count: usize,
    // Particle texture
    particle_texture: Option<WebGlTexture>,
    // Framebuffers for post-processing
    scene_framebuffer: Option<WebGlFramebuffer>,
    scene_texture: Option<WebGlTexture>,
    scene_depth: Option<WebGlRenderbuffer>,
    bloom_framebuffers: Vec<WebGlFramebuffer>,
    bloom_textures: Vec<WebGlTexture>,
    // Screen quad for post-processing
    screen_quad_buffer: Option<WebGlBuffer>,
    // Viewport size
    viewport_width: i32,
    viewport_height: i32,
}

struct MeshBuffers {
    vertex_buffer: WebGlBuffer,
    index_buffer: WebGlBuffer,
    index_count: i32,
}

impl WebGLRenderer {
    /// Create a new WebGL renderer
    pub fn new(gl: WebGl2RenderingContext) -> Result<Self, JsValue> {
        // Compile mesh shaders
        let vertex_shader = compile_shader(&gl, WebGl2RenderingContext::VERTEX_SHADER, VERTEX_SHADER_SOURCE)?;
        let fragment_shader = compile_shader(&gl, WebGl2RenderingContext::FRAGMENT_SHADER, FRAGMENT_SHADER_SOURCE)?;
        
        // Create mesh program
        let program = gl.create_program()
            .ok_or_else(|| JsValue::from_str("Failed to create program"))?;
        
        gl.attach_shader(&program, &vertex_shader);
        gl.attach_shader(&program, &fragment_shader);
        gl.link_program(&program);
        
        if !gl.get_program_parameter(&program, WebGl2RenderingContext::LINK_STATUS).as_bool().unwrap_or(false) {
            let error = gl.get_program_info_log(&program).unwrap_or_else(|| "Unknown error".to_string());
            return Err(JsValue::from_str(&format!("Failed to link program: {}", error)));
        }
        
        // Compile particle shaders
        let particle_vertex = compile_shader(&gl, WebGl2RenderingContext::VERTEX_SHADER, PARTICLE_VERTEX_SHADER)?;
        let particle_fragment = compile_shader(&gl, WebGl2RenderingContext::FRAGMENT_SHADER, PARTICLE_FRAGMENT_SHADER)?;
        
        // Create particle program
        let particle_program = gl.create_program()
            .ok_or_else(|| JsValue::from_str("Failed to create particle program"))?;
        
        gl.attach_shader(&particle_program, &particle_vertex);
        gl.attach_shader(&particle_program, &particle_fragment);
        gl.link_program(&particle_program);
        
        if !gl.get_program_parameter(&particle_program, WebGl2RenderingContext::LINK_STATUS).as_bool().unwrap_or(false) {
            let error = gl.get_program_info_log(&particle_program).unwrap_or_else(|| "Unknown error".to_string());
            return Err(JsValue::from_str(&format!("Failed to link particle program: {}", error)));
        }
        
        // Get mesh attribute locations
        let position_attrib = gl.get_attrib_location(&program, "a_position") as u32;
        let normal_attrib = gl.get_attrib_location(&program, "a_normal") as u32;
        let texcoord_attrib = gl.get_attrib_location(&program, "a_texcoord") as u32;
        
        // Get particle attribute locations
        let particle_position_attrib = gl.get_attrib_location(&particle_program, "a_position") as u32;
        let particle_size_attrib = gl.get_attrib_location(&particle_program, "a_size") as u32;
        
        // Get mesh uniform locations
        let mut uniforms = HashMap::new();
        let uniform_names = vec![
            "u_model", "u_view", "u_projection", "u_normalMatrix",
            "u_cameraPosition", "u_baseColor", "u_metallic", "u_roughness", "u_emissive",
            "u_ambientLight", "u_lightDirection", "u_lightColor", "u_lightIntensity",
        ];
        
        for name in uniform_names {
            if let Some(location) = gl.get_uniform_location(&program, name) {
                uniforms.insert(name.to_string(), location);
            }
        }
        
        // Get particle uniform locations
        let mut particle_uniforms = HashMap::new();
        let particle_uniform_names = vec![
            "u_model", "u_view", "u_projection", "u_resolution",
            "u_baseColor", "u_emissive", "u_particleTexture", "u_useTexture",
        ];
        
        for name in particle_uniform_names {
            if let Some(location) = gl.get_uniform_location(&particle_program, name) {
                particle_uniforms.insert(name.to_string(), location);
            }
        }
        
        // Set up WebGL state
        gl.enable(WebGl2RenderingContext::DEPTH_TEST);
        gl.enable(WebGl2RenderingContext::CULL_FACE);
        
        Ok(Self {
            gl,
            program,
            particle_program,
            post_program: None,
            blur_program: None,
            position_attrib,
            normal_attrib,
            texcoord_attrib,
            particle_position_attrib,
            particle_size_attrib,
            uniforms,
            particle_uniforms,
            post_uniforms: HashMap::new(),
            blur_uniforms: HashMap::new(),
            mesh_buffers: HashMap::new(),
            particle_buffer: None,
            particle_count: 0,
            particle_texture: None,
            scene_framebuffer: None,
            scene_texture: None,
            scene_depth: None,
            bloom_framebuffers: Vec::new(),
            bloom_textures: Vec::new(),
            screen_quad_buffer: None,
            viewport_width: 800,
            viewport_height: 600,
        })
    }
    
    /// Set viewport size
    pub fn set_viewport(&mut self, width: i32, height: i32) {
        self.gl.viewport(0, 0, width, height);
        self.viewport_width = width;
        self.viewport_height = height;
        
        // Recreate framebuffers for new size
        let _ = self.create_framebuffers();
    }
    
    /// Create a particle texture from pixel data
    pub fn create_particle_texture(&mut self, width: i32, height: i32, pixels: &[u8]) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // Create texture
        let texture = gl.create_texture()
            .ok_or_else(|| JsValue::from_str("Failed to create texture"))?;
        
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(&texture));
        
        // Upload texture data
        gl.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
            WebGl2RenderingContext::TEXTURE_2D,
            0, // level
            WebGl2RenderingContext::RGBA as i32, // internal format
            width,
            height,
            0, // border
            WebGl2RenderingContext::RGBA, // format
            WebGl2RenderingContext::UNSIGNED_BYTE, // type
            Some(pixels),
        )?;
        
        // Set texture parameters for point sprites
        gl.tex_parameteri(
            WebGl2RenderingContext::TEXTURE_2D,
            WebGl2RenderingContext::TEXTURE_MIN_FILTER,
            WebGl2RenderingContext::LINEAR as i32,
        );
        gl.tex_parameteri(
            WebGl2RenderingContext::TEXTURE_2D,
            WebGl2RenderingContext::TEXTURE_MAG_FILTER,
            WebGl2RenderingContext::LINEAR as i32,
        );
        gl.tex_parameteri(
            WebGl2RenderingContext::TEXTURE_2D,
            WebGl2RenderingContext::TEXTURE_WRAP_S,
            WebGl2RenderingContext::CLAMP_TO_EDGE as i32,
        );
        gl.tex_parameteri(
            WebGl2RenderingContext::TEXTURE_2D,
            WebGl2RenderingContext::TEXTURE_WRAP_T,
            WebGl2RenderingContext::CLAMP_TO_EDGE as i32,
        );
        
        // Store texture
        self.particle_texture = Some(texture);
        
        Ok(())
    }
    
    /// Create a default glow texture for particles
    pub fn create_default_particle_texture(&mut self) -> Result<(), JsValue> {
        let size = 64;
        let mut pixels = vec![0u8; (size * size * 4) as usize];
        
        // Generate radial gradient
        let center = (size / 2) as f32;
        for y in 0..size {
            for x in 0..size {
                let dx = x as f32 - center;
                let dy = y as f32 - center;
                let dist = (dx * dx + dy * dy).sqrt() / center;
                
                // Soft falloff
                let alpha = (1.0 - dist).max(0.0).powf(2.0);
                let brightness = alpha;
                
                let idx = ((y * size + x) * 4) as usize;
                pixels[idx] = (brightness * 255.0) as u8;     // R
                pixels[idx + 1] = (brightness * 255.0) as u8; // G
                pixels[idx + 2] = (brightness * 255.0) as u8; // B
                pixels[idx + 3] = (alpha * 255.0) as u8;      // A
            }
        }
        
        self.create_particle_texture(size, size, &pixels)
    }
    
    /// Update particle system data
    pub fn update_particles(&mut self, positions: &[Vec3], sizes: &[f32]) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        if positions.len() != sizes.len() {
            return Err(JsValue::from_str("Particle positions and sizes must have same length"));
        }
        
        self.particle_count = positions.len();
        
        // Create interleaved vertex data: x, y, z, size
        let mut vertex_data = Vec::with_capacity(positions.len() * 4);
        for (pos, &size) in positions.iter().zip(sizes.iter()) {
            vertex_data.push(pos.x);
            vertex_data.push(pos.y);
            vertex_data.push(pos.z);
            vertex_data.push(size);
        }
        
        // Create or update buffer
        let buffer = if let Some(buf) = &self.particle_buffer {
            buf
        } else {
            let new_buffer = gl.create_buffer()
                .ok_or_else(|| JsValue::from_str("Failed to create particle buffer"))?;
            self.particle_buffer = Some(new_buffer);
            self.particle_buffer.as_ref().unwrap()
        };
        
        gl.bind_buffer(WebGl2RenderingContext::ARRAY_BUFFER, Some(buffer));
        
        unsafe {
            let array = js_sys::Float32Array::view(&vertex_data);
            gl.buffer_data_with_array_buffer_view(
                WebGl2RenderingContext::ARRAY_BUFFER,
                &array,
                WebGl2RenderingContext::DYNAMIC_DRAW,
            );
        }
        
        Ok(())
    }
    
    /// Initialize post-processing shaders and resources
    pub fn init_post_processing(&mut self) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // Compile post-processing shaders
        let post_vertex = compile_shader(gl, WebGl2RenderingContext::VERTEX_SHADER, POST_VERTEX_SHADER)?;
        let bloom_fragment = compile_shader(gl, WebGl2RenderingContext::FRAGMENT_SHADER, BLOOM_FRAGMENT_SHADER)?;
        let blur_fragment = compile_shader(gl, WebGl2RenderingContext::FRAGMENT_SHADER, BLUR_FRAGMENT_SHADER)?;
        
        // Create bloom program
        let post_program = gl.create_program()
            .ok_or_else(|| JsValue::from_str("Failed to create post program"))?;
        gl.attach_shader(&post_program, &post_vertex);
        gl.attach_shader(&post_program, &bloom_fragment);
        gl.link_program(&post_program);
        
        if !gl.get_program_parameter(&post_program, WebGl2RenderingContext::LINK_STATUS).as_bool().unwrap_or(false) {
            return Err(JsValue::from_str("Failed to link post program"));
        }
        
        // Create blur program
        let blur_program = gl.create_program()
            .ok_or_else(|| JsValue::from_str("Failed to create blur program"))?;
        gl.attach_shader(&blur_program, &post_vertex);
        gl.attach_shader(&blur_program, &blur_fragment);
        gl.link_program(&blur_program);
        
        if !gl.get_program_parameter(&blur_program, WebGl2RenderingContext::LINK_STATUS).as_bool().unwrap_or(false) {
            return Err(JsValue::from_str("Failed to link blur program"));
        }
        
        // Get uniform locations for post program
        let post_uniform_names = vec![
            "u_sceneTexture", "u_bloomTexture", "u_bloomIntensity", 
            "u_bloomThreshold", "u_extractBright"
        ];
        
        for name in post_uniform_names {
            if let Some(location) = gl.get_uniform_location(&post_program, name) {
                self.post_uniforms.insert(name.to_string(), location);
            }
        }
        
        // Get uniform locations for blur program
        let blur_uniform_names = vec!["u_texture", "u_direction", "u_resolution"];
        
        for name in blur_uniform_names {
            if let Some(location) = gl.get_uniform_location(&blur_program, name) {
                self.blur_uniforms.insert(name.to_string(), location);
            }
        }
        
        self.post_program = Some(post_program);
        self.blur_program = Some(blur_program);
        
        // Create screen quad
        self.create_screen_quad()?;
        
        // Create framebuffers
        self.create_framebuffers()?;
        
        Ok(())
    }
    
    /// Create screen quad for post-processing
    fn create_screen_quad(&mut self) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        let vertices: [f32; 8] = [
            -1.0, -1.0,
             1.0, -1.0,
            -1.0,  1.0,
             1.0,  1.0,
        ];
        
        let buffer = gl.create_buffer()
            .ok_or_else(|| JsValue::from_str("Failed to create screen quad buffer"))?;
        
        gl.bind_buffer(WebGl2RenderingContext::ARRAY_BUFFER, Some(&buffer));
        
        unsafe {
            let array = js_sys::Float32Array::view(&vertices);
            gl.buffer_data_with_array_buffer_view(
                WebGl2RenderingContext::ARRAY_BUFFER,
                &array,
                WebGl2RenderingContext::STATIC_DRAW,
            );
        }
        
        self.screen_quad_buffer = Some(buffer);
        Ok(())
    }
    
    /// Create framebuffers for post-processing
    fn create_framebuffers(&mut self) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        // Check for floating point texture support
        let ext_color_buffer_float = gl.get_extension("EXT_color_buffer_float").ok().flatten();
        let use_float_textures = ext_color_buffer_float.is_some();
        
        // Clean up old framebuffers
        if let Some(fb) = &self.scene_framebuffer {
            gl.delete_framebuffer(Some(fb));
        }
        if let Some(tex) = &self.scene_texture {
            gl.delete_texture(Some(tex));
        }
        if let Some(rb) = &self.scene_depth {
            gl.delete_renderbuffer(Some(rb));
        }
        
        for fb in &self.bloom_framebuffers {
            gl.delete_framebuffer(Some(fb));
        }
        for tex in &self.bloom_textures {
            gl.delete_texture(Some(tex));
        }
        
        self.bloom_framebuffers.clear();
        self.bloom_textures.clear();
        
        // Create scene framebuffer
        let scene_fb = gl.create_framebuffer()
            .ok_or_else(|| JsValue::from_str("Failed to create scene framebuffer"))?;
        let scene_tex = gl.create_texture()
            .ok_or_else(|| JsValue::from_str("Failed to create scene texture"))?;
        let scene_depth = gl.create_renderbuffer()
            .ok_or_else(|| JsValue::from_str("Failed to create depth renderbuffer"))?;
        
        // Setup scene color texture
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(&scene_tex));
        if use_float_textures {
            // Use floating point textures for better HDR
            gl.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                WebGl2RenderingContext::TEXTURE_2D,
                0,
                WebGl2RenderingContext::RGBA16F as i32,
                self.viewport_width,
                self.viewport_height,
                0,
                WebGl2RenderingContext::RGBA,
                WebGl2RenderingContext::FLOAT,
                None,
            )?;
        } else {
            // Fallback to standard RGBA
            gl.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                WebGl2RenderingContext::TEXTURE_2D,
                0,
                WebGl2RenderingContext::RGBA as i32,
                self.viewport_width,
                self.viewport_height,
                0,
                WebGl2RenderingContext::RGBA,
                WebGl2RenderingContext::UNSIGNED_BYTE,
                None,
            )?;
        }
        gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_MIN_FILTER, WebGl2RenderingContext::LINEAR as i32);
        gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_MAG_FILTER, WebGl2RenderingContext::LINEAR as i32);
        gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_WRAP_S, WebGl2RenderingContext::CLAMP_TO_EDGE as i32);
        gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_WRAP_T, WebGl2RenderingContext::CLAMP_TO_EDGE as i32);
        
        // Setup depth renderbuffer
        gl.bind_renderbuffer(WebGl2RenderingContext::RENDERBUFFER, Some(&scene_depth));
        gl.renderbuffer_storage(
            WebGl2RenderingContext::RENDERBUFFER,
            WebGl2RenderingContext::DEPTH_COMPONENT24,
            self.viewport_width,
            self.viewport_height,
        );
        
        // Attach to framebuffer
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, Some(&scene_fb));
        gl.framebuffer_texture_2d(
            WebGl2RenderingContext::FRAMEBUFFER,
            WebGl2RenderingContext::COLOR_ATTACHMENT0,
            WebGl2RenderingContext::TEXTURE_2D,
            Some(&scene_tex),
            0,
        );
        gl.framebuffer_renderbuffer(
            WebGl2RenderingContext::FRAMEBUFFER,
            WebGl2RenderingContext::DEPTH_ATTACHMENT,
            WebGl2RenderingContext::RENDERBUFFER,
            Some(&scene_depth),
        );
        
        // Create bloom framebuffers (2 for ping-pong)
        for i in 0..2 {
            let bloom_fb = gl.create_framebuffer()
                .ok_or_else(|| JsValue::from_str(&format!("Failed to create bloom framebuffer {}", i)))?;
            let bloom_tex = gl.create_texture()
                .ok_or_else(|| JsValue::from_str(&format!("Failed to create bloom texture {}", i)))?;
            
            gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(&bloom_tex));
            if use_float_textures {
                gl.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                    WebGl2RenderingContext::TEXTURE_2D,
                    0,
                    WebGl2RenderingContext::RGBA16F as i32,
                    self.viewport_width / 2, // Half resolution for performance
                    self.viewport_height / 2,
                    0,
                    WebGl2RenderingContext::RGBA,
                    WebGl2RenderingContext::FLOAT,
                    None,
                )?;
            } else {
                gl.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array(
                    WebGl2RenderingContext::TEXTURE_2D,
                    0,
                    WebGl2RenderingContext::RGBA as i32,
                    self.viewport_width / 2, // Half resolution for performance
                    self.viewport_height / 2,
                    0,
                    WebGl2RenderingContext::RGBA,
                    WebGl2RenderingContext::UNSIGNED_BYTE,
                    None,
                )?;
            }
            gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_MIN_FILTER, WebGl2RenderingContext::LINEAR as i32);
            gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_MAG_FILTER, WebGl2RenderingContext::LINEAR as i32);
            gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_WRAP_S, WebGl2RenderingContext::CLAMP_TO_EDGE as i32);
            gl.tex_parameteri(WebGl2RenderingContext::TEXTURE_2D, WebGl2RenderingContext::TEXTURE_WRAP_T, WebGl2RenderingContext::CLAMP_TO_EDGE as i32);
            
            gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, Some(&bloom_fb));
            gl.framebuffer_texture_2d(
                WebGl2RenderingContext::FRAMEBUFFER,
                WebGl2RenderingContext::COLOR_ATTACHMENT0,
                WebGl2RenderingContext::TEXTURE_2D,
                Some(&bloom_tex),
                0,
            );
            
            // Check framebuffer completeness
            let fb_status = gl.check_framebuffer_status(WebGl2RenderingContext::FRAMEBUFFER);
            if fb_status != WebGl2RenderingContext::FRAMEBUFFER_COMPLETE {
                return Err(JsValue::from_str(&format!("Bloom framebuffer {} incomplete: {}", i, fb_status)));
            }
            
            self.bloom_framebuffers.push(bloom_fb);
            self.bloom_textures.push(bloom_tex);
        }
        
        // Check main framebuffer completeness
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, Some(&scene_fb));
        let fb_status = gl.check_framebuffer_status(WebGl2RenderingContext::FRAMEBUFFER);
        if fb_status != WebGl2RenderingContext::FRAMEBUFFER_COMPLETE {
            return Err(JsValue::from_str(&format!("Scene framebuffer incomplete: {}", fb_status)));
        }
        
        // Restore default framebuffer
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, None);
        
        self.scene_framebuffer = Some(scene_fb);
        self.scene_texture = Some(scene_tex);
        self.scene_depth = Some(scene_depth);
        
        Ok(())
    }
    
    /// Render a 3D scene  
    pub fn render_scene(&mut self, scene: &Scene3D) -> Result<(), JsValue> {
        self.render_scene_with_target(scene, false)
    }
    
    /// Render a 3D scene to framebuffer for post-processing
    pub fn render_scene_to_framebuffer(&mut self, scene: &Scene3D) -> Result<(), JsValue> {
        // Initialize post-processing if not already done
        if self.post_program.is_none() {
            self.init_post_processing()?;
        }
        self.render_scene_with_target(scene, true)
    }
    
    /// Internal render method that can target framebuffer or screen
    fn render_scene_with_target(&mut self, scene: &Scene3D, use_framebuffer: bool) -> Result<(), JsValue> {
        // Bind appropriate render target
        if use_framebuffer && self.scene_framebuffer.is_some() {
            self.gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, self.scene_framebuffer.as_ref());
        } else {
            self.gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, None);
        }
        self.gl.viewport(0, 0, self.viewport_width, self.viewport_height);
        
        // Clear with modern gradient-like background
        self.gl.clear_color(0.97, 0.97, 0.99, 1.0); // Very light gray background
        self.gl.clear(WebGl2RenderingContext::COLOR_BUFFER_BIT | WebGl2RenderingContext::DEPTH_BUFFER_BIT);
        
        // Use program
        self.gl.use_program(Some(&self.program));
        
        // Set camera uniforms
        self.set_camera_uniforms(&scene.camera)?;
        
        // Set lighting uniforms
        self.set_lighting_uniforms(&scene.lights)?;
        
        // First pass: render opaque objects
        self.gl.depth_mask(true);
        self.gl.enable(WebGl2RenderingContext::BLEND);
        self.gl.blend_func(WebGl2RenderingContext::SRC_ALPHA, WebGl2RenderingContext::ONE_MINUS_SRC_ALPHA);
        
        // Render all nodes
        for &root_idx in &scene.roots {
            self.render_node(scene, root_idx, Mat4::IDENTITY)?;
        }
        
        // Reset state
        self.gl.depth_mask(true);
        self.gl.disable(WebGl2RenderingContext::BLEND);
        
        Ok(())
    }
    
    /// Render particles as point sprites
    pub fn render_particles(&self, camera: &Camera3D, base_color: &Color, emissive: &Color, canvas_width: f32, canvas_height: f32) -> Result<(), JsValue> {
        if self.particle_count == 0 || self.particle_buffer.is_none() {
            return Ok(());
        }
        
        let gl = &self.gl;
        
        // Use particle program
        gl.use_program(Some(&self.particle_program));
        
        // Set uniforms
        if let Some(location) = self.particle_uniforms.get("u_view") {
            let view = camera.view_matrix();
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(&view));
        }
        
        if let Some(location) = self.particle_uniforms.get("u_projection") {
            let projection = camera.projection_matrix();
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(&projection));
        }
        
        if let Some(location) = self.particle_uniforms.get("u_model") {
            gl.uniform_matrix4fv_with_f32_array(Some(location), false, &mat4_to_array(&Mat4::IDENTITY));
        }
        
        if let Some(location) = self.particle_uniforms.get("u_resolution") {
            gl.uniform2f(Some(location), canvas_width, canvas_height);
        }
        
        if let Some(location) = self.particle_uniforms.get("u_baseColor") {
            gl.uniform4f(Some(location), base_color.r, base_color.g, base_color.b, base_color.a);
        }
        
        if let Some(location) = self.particle_uniforms.get("u_emissive") {
            gl.uniform3f(Some(location), emissive.r, emissive.g, emissive.b);
        }
        
        // Set texture uniforms
        if let Some(texture) = &self.particle_texture {
            if let Some(location) = self.particle_uniforms.get("u_useTexture") {
                gl.uniform1i(Some(location), 1); // true
            }
            
            if let Some(location) = self.particle_uniforms.get("u_particleTexture") {
                gl.uniform1i(Some(location), 0); // texture unit 0
            }
            
            // Bind texture to unit 0
            gl.active_texture(WebGl2RenderingContext::TEXTURE0);
            gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(texture));
        } else {
            if let Some(location) = self.particle_uniforms.get("u_useTexture") {
                gl.uniform1i(Some(location), 0); // false
            }
        }
        
        // Bind particle buffer
        if let Some(buffer) = &self.particle_buffer {
            gl.bind_buffer(WebGl2RenderingContext::ARRAY_BUFFER, Some(buffer));
            
            // Set up attributes - interleaved format: x, y, z, size
            let stride = 4 * 4; // 4 floats * 4 bytes
            gl.vertex_attrib_pointer_with_i32(self.particle_position_attrib, 3, WebGl2RenderingContext::FLOAT, false, stride, 0);
            gl.enable_vertex_attrib_array(self.particle_position_attrib);
            
            gl.vertex_attrib_pointer_with_i32(self.particle_size_attrib, 1, WebGl2RenderingContext::FLOAT, false, stride, 3 * 4);
            gl.enable_vertex_attrib_array(self.particle_size_attrib);
            
            // Draw points
            gl.draw_arrays(WebGl2RenderingContext::POINTS, 0, self.particle_count as i32);
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
            MaterialType::PBR { base_color, metallic, roughness, emissive, .. } => {
                if let Some(location) = self.uniforms.get("u_baseColor") {
                    gl.uniform4f(Some(location), base_color.r, base_color.g, base_color.b, material.opacity);
                }
                if let Some(location) = self.uniforms.get("u_metallic") {
                    gl.uniform1f(Some(location), *metallic);
                }
                if let Some(location) = self.uniforms.get("u_roughness") {
                    gl.uniform1f(Some(location), *roughness);
                }
                if let Some(location) = self.uniforms.get("u_emissive") {
                    gl.uniform3f(Some(location), emissive.r, emissive.g, emissive.b);
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
                if let Some(location) = self.uniforms.get("u_emissive") {
                    gl.uniform3f(Some(location), 0.0, 0.0, 0.0);
                }
            }
        }
        
        Ok(())
    }
    
    /// Apply bloom post-processing effect
    pub fn apply_bloom(&mut self, intensity: f32) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        if self.post_program.is_none() || self.bloom_framebuffers.len() < 2 {
            return Ok(()); // Post-processing not initialized
        }
        
        let post_program = self.post_program.as_ref().unwrap();
        let blur_program = self.blur_program.as_ref().unwrap();
        
        // Step 1: Extract bright pixels
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, Some(&self.bloom_framebuffers[0]));
        gl.viewport(0, 0, self.viewport_width / 2, self.viewport_height / 2);
        gl.clear(WebGl2RenderingContext::COLOR_BUFFER_BIT);
        
        gl.use_program(Some(post_program));
        
        // Bind scene texture
        gl.active_texture(WebGl2RenderingContext::TEXTURE0);
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, self.scene_texture.as_ref());
        if let Some(loc) = self.post_uniforms.get("u_sceneTexture") {
            gl.uniform1i(Some(loc), 0);
        }
        
        // Set extraction mode
        if let Some(loc) = self.post_uniforms.get("u_extractBright") {
            gl.uniform1i(Some(loc), 1);
        }
        if let Some(loc) = self.post_uniforms.get("u_bloomThreshold") {
            gl.uniform1f(Some(loc), 0.3); // Even lower threshold for star-like glow
        }
        
        // Draw screen quad
        self.draw_screen_quad()?;
        
        // Step 2: Blur bright pixels (ping-pong between framebuffers)
        gl.use_program(Some(blur_program));
        
        // Set resolution
        if let Some(loc) = self.blur_uniforms.get("u_resolution") {
            gl.uniform2f(Some(loc), (self.viewport_width / 2) as f32, (self.viewport_height / 2) as f32);
        }
        
        // Horizontal blur pass
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, Some(&self.bloom_framebuffers[1]));
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(&self.bloom_textures[0]));
        
        if let Some(loc) = self.blur_uniforms.get("u_texture") {
            gl.uniform1i(Some(loc), 0);
        }
        if let Some(loc) = self.blur_uniforms.get("u_direction") {
            gl.uniform2f(Some(loc), 1.0, 0.0); // Horizontal
        }
        
        self.draw_screen_quad()?;
        
        // Vertical blur pass
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, Some(&self.bloom_framebuffers[0]));
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(&self.bloom_textures[1]));
        
        if let Some(loc) = self.blur_uniforms.get("u_direction") {
            gl.uniform2f(Some(loc), 0.0, 1.0); // Vertical
        }
        
        self.draw_screen_quad()?;
        
        // Step 3: Combine scene with bloom
        gl.bind_framebuffer(WebGl2RenderingContext::FRAMEBUFFER, None); // Back to screen
        gl.viewport(0, 0, self.viewport_width, self.viewport_height);
        gl.clear(WebGl2RenderingContext::COLOR_BUFFER_BIT);
        
        gl.use_program(Some(post_program));
        
        // Bind textures
        gl.active_texture(WebGl2RenderingContext::TEXTURE0);
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, self.scene_texture.as_ref());
        gl.active_texture(WebGl2RenderingContext::TEXTURE1);
        gl.bind_texture(WebGl2RenderingContext::TEXTURE_2D, Some(&self.bloom_textures[0]));
        
        if let Some(loc) = self.post_uniforms.get("u_sceneTexture") {
            gl.uniform1i(Some(loc), 0);
        }
        if let Some(loc) = self.post_uniforms.get("u_bloomTexture") {
            gl.uniform1i(Some(loc), 1);
        }
        if let Some(loc) = self.post_uniforms.get("u_extractBright") {
            gl.uniform1i(Some(loc), 0); // Combine mode
        }
        if let Some(loc) = self.post_uniforms.get("u_bloomIntensity") {
            gl.uniform1f(Some(loc), intensity);
        }
        
        self.draw_screen_quad()?;
        
        Ok(())
    }
    
    /// Draw screen quad for post-processing
    fn draw_screen_quad(&self) -> Result<(), JsValue> {
        let gl = &self.gl;
        
        if let Some(buffer) = &self.screen_quad_buffer {
            gl.bind_buffer(WebGl2RenderingContext::ARRAY_BUFFER, Some(buffer));
            
            // Get position attribute location from current program
            let pos_attrib = if self.post_program.is_some() {
                gl.get_attrib_location(gl.get_parameter(WebGl2RenderingContext::CURRENT_PROGRAM)?
                    .as_ref()
                    .dyn_ref::<WebGlProgram>()
                    .unwrap(), "a_position") as u32
            } else {
                0
            };
            
            gl.vertex_attrib_pointer_with_i32(pos_attrib, 2, WebGl2RenderingContext::FLOAT, false, 0, 0);
            gl.enable_vertex_attrib_array(pos_attrib);
            
            gl.draw_arrays(WebGl2RenderingContext::TRIANGLE_STRIP, 0, 4);
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