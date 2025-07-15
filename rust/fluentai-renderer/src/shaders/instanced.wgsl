// Instanced rendering shader for GPU instancing

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) color: vec4<f32>,
}

struct InstanceInput {
    // Model matrix columns
    @location(2) model_matrix_0: vec4<f32>,
    @location(3) model_matrix_1: vec4<f32>,
    @location(4) model_matrix_2: vec4<f32>,
    @location(5) model_matrix_3: vec4<f32>,
    // Instance color tint
    @location(6) instance_color: vec4<f32>,
    // UV offset and scale for texture atlasing
    @location(7) uv_offset_scale: vec4<f32>, // xy = offset, zw = scale
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) tex_coords: vec2<f32>,
}

struct Uniforms {
    view_proj: mat4x4<f32>,
}

@group(0) @binding(0) var<uniform> uniforms: Uniforms;

@vertex
fn vs_main(
    vertex: VertexInput,
    instance: InstanceInput,
) -> VertexOutput {
    var out: VertexOutput;
    
    // Reconstruct model matrix from instance data
    let model_matrix = mat4x4<f32>(
        instance.model_matrix_0,
        instance.model_matrix_1,
        instance.model_matrix_2,
        instance.model_matrix_3,
    );
    
    // Transform vertex position
    let world_position = model_matrix * vec4<f32>(vertex.position, 1.0);
    out.clip_position = uniforms.view_proj * world_position;
    
    // Combine vertex color with instance tint
    out.color = vertex.color * instance.instance_color;
    
    // Calculate texture coordinates with atlas offset/scale
    // Assuming vertex positions are in [-0.5, 0.5] range
    let base_uv = (vertex.position.xy + 0.5);
    out.tex_coords = instance.uv_offset_scale.xy + base_uv * instance.uv_offset_scale.zw;
    
    return out;
}

@fragment
fn fs_solid(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}

// Textured fragment shader
@group(1) @binding(0) var texture: texture_2d<f32>;
@group(1) @binding(1) var texture_sampler: sampler;

@fragment
fn fs_textured(in: VertexOutput) -> @location(0) vec4<f32> {
    let tex_color = textureSample(texture, texture_sampler, in.tex_coords);
    return tex_color * in.color;
}

// Particle-specific vertex shader with billboard support
@vertex
fn vs_particle_billboard(
    vertex: VertexInput,
    instance: InstanceInput,
) -> VertexOutput {
    var out: VertexOutput;
    
    // Extract position from model matrix
    let world_position = vec3<f32>(
        instance.model_matrix_3.x,
        instance.model_matrix_3.y,
        instance.model_matrix_3.z
    );
    
    // Extract scale from model matrix
    let scale_x = length(vec3<f32>(instance.model_matrix_0.x, instance.model_matrix_0.y, instance.model_matrix_0.z));
    let scale_y = length(vec3<f32>(instance.model_matrix_1.x, instance.model_matrix_1.y, instance.model_matrix_1.z));
    
    // Billboard calculation - face the camera
    let view_right = vec3<f32>(uniforms.view_proj[0][0], uniforms.view_proj[1][0], uniforms.view_proj[2][0]);
    let view_up = vec3<f32>(uniforms.view_proj[0][1], uniforms.view_proj[1][1], uniforms.view_proj[2][1]);
    
    // Apply billboard transformation
    let billboard_pos = world_position 
        + view_right * vertex.position.x * scale_x
        + view_up * vertex.position.y * scale_y;
    
    out.clip_position = uniforms.view_proj * vec4<f32>(billboard_pos, 1.0);
    out.color = vertex.color * instance.instance_color;
    
    // UV coordinates
    out.tex_coords = instance.uv_offset_scale.xy + (vertex.position.xy + 0.5) * instance.uv_offset_scale.zw;
    
    return out;
}

// Gradient fragment shader for particles
@fragment
fn fs_particle_gradient(in: VertexOutput) -> @location(0) vec4<f32> {
    // Radial gradient from center
    let center = vec2<f32>(0.5, 0.5);
    let dist = distance(in.tex_coords, center) * 2.0;
    let alpha = smoothstep(1.0, 0.0, dist);
    
    return vec4<f32>(in.color.rgb, in.color.a * alpha);
}

// Text rendering with SDF (Signed Distance Field)
@fragment
fn fs_text_sdf(in: VertexOutput) -> @location(0) vec4<f32> {
    let distance = textureSample(texture, texture_sampler, in.tex_coords).r;
    
    // SDF parameters
    let smoothing = 0.1;
    let threshold = 0.5;
    
    // Calculate alpha with smooth edges
    let alpha = smoothstep(threshold - smoothing, threshold + smoothing, distance);
    
    return vec4<f32>(in.color.rgb, in.color.a * alpha);
}

// Debug visualization for instance bounds
@vertex
fn vs_debug_bounds(
    vertex: VertexInput,
    instance: InstanceInput,
) -> VertexOutput {
    var out: VertexOutput;
    
    let model_matrix = mat4x4<f32>(
        instance.model_matrix_0,
        instance.model_matrix_1,
        instance.model_matrix_2,
        instance.model_matrix_3,
    );
    
    // Create wireframe box
    let world_position = model_matrix * vec4<f32>(vertex.position, 1.0);
    out.clip_position = uniforms.view_proj * world_position;
    
    // Debug color (semi-transparent green)
    out.color = vec4<f32>(0.0, 1.0, 0.0, 0.3);
    out.tex_coords = vec2<f32>(0.0, 0.0);
    
    return out;
}