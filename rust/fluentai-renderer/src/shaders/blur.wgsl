// Blur effect shader for post-processing

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) tex_coords: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = vec4<f32>(in.position, 1.0);
    out.tex_coords = in.tex_coords;
    return out;
}

// Uniforms for blur parameters
struct BlurParams {
    direction: vec2<f32>, // Blur direction (1,0) for horizontal, (0,1) for vertical
    radius: f32,
    _padding: f32,
}

@group(0) @binding(0) var input_texture: texture_2d<f32>;
@group(0) @binding(1) var texture_sampler: sampler;
@group(0) @binding(2) var<uniform> blur_params: BlurParams;

// Gaussian weights for 9-tap filter
const GAUSSIAN_WEIGHTS: array<f32, 9> = array<f32, 9>(
    0.0125786, 0.0296069, 0.0592137, 0.0947416, 0.121281,
    0.121281, 0.0947416, 0.0592137, 0.0296069
);

const GAUSSIAN_OFFSETS: array<f32, 9> = array<f32, 9>(
    -4.0, -3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0
);

@fragment
fn fs_gaussian_blur(in: VertexOutput) -> @location(0) vec4<f32> {
    let texel_size = 1.0 / vec2<f32>(textureDimensions(input_texture));
    var color = vec4<f32>(0.0, 0.0, 0.0, 0.0);
    
    // 9-tap Gaussian blur
    for (var i = 0; i < 9; i = i + 1) {
        let offset = blur_params.direction * GAUSSIAN_OFFSETS[i] * blur_params.radius * texel_size;
        let sample_pos = in.tex_coords + offset;
        color = color + textureSample(input_texture, texture_sampler, sample_pos) * GAUSSIAN_WEIGHTS[i];
    }
    
    return color;
}

// Box blur for faster but lower quality blur
@fragment
fn fs_box_blur(in: VertexOutput) -> @location(0) vec4<f32> {
    let texel_size = 1.0 / vec2<f32>(textureDimensions(input_texture));
    var color = vec4<f32>(0.0, 0.0, 0.0, 0.0);
    
    let radius_int = i32(blur_params.radius);
    let sample_count = radius_int * 2 + 1;
    let weight = 1.0 / f32(sample_count);
    
    for (var i = -radius_int; i <= radius_int; i = i + 1) {
        let offset = blur_params.direction * f32(i) * texel_size;
        let sample_pos = in.tex_coords + offset;
        color = color + textureSample(input_texture, texture_sampler, sample_pos) * weight;
    }
    
    return color;
}

// Drop shadow shader
struct ShadowParams {
    offset: vec2<f32>,
    blur_radius: f32,
    color: vec4<f32>,
}

@group(0) @binding(3) var<uniform> shadow_params: ShadowParams;

@fragment
fn fs_drop_shadow(in: VertexOutput) -> @location(0) vec4<f32> {
    let texel_size = 1.0 / vec2<f32>(textureDimensions(input_texture));
    
    // Sample the original texture
    let original = textureSample(input_texture, texture_sampler, in.tex_coords);
    
    // Sample for shadow (offset position)
    let shadow_coords = in.tex_coords - shadow_params.offset * texel_size;
    var shadow_alpha = 0.0;
    
    // Simple box blur for shadow
    let radius_int = i32(shadow_params.blur_radius);
    let sample_count = (radius_int * 2 + 1) * (radius_int * 2 + 1);
    let weight = 1.0 / f32(sample_count);
    
    for (var x = -radius_int; x <= radius_int; x = x + 1) {
        for (var y = -radius_int; y <= radius_int; y = y + 1) {
            let offset = vec2<f32>(f32(x), f32(y)) * texel_size;
            let sample_pos = shadow_coords + offset;
            shadow_alpha = shadow_alpha + textureSample(input_texture, texture_sampler, sample_pos).a * weight;
        }
    }
    
    // Composite shadow with original
    let shadow_color = vec4<f32>(shadow_params.color.rgb, shadow_params.color.a * shadow_alpha);
    
    // Blend shadow under original
    return mix(shadow_color, original, original.a);
}