// Text rendering shader for FluentAI

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
    @location(2) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
    @location(1) color: vec4<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    
    // Convert from pixel coordinates to clip space
    // Assuming orthographic projection where screen maps to [-1, 1]
    output.clip_position = vec4<f32>(
        input.position.x / 400.0 - 1.0,  // TODO: Pass viewport size as uniform
        1.0 - input.position.y / 300.0,   // Flip Y axis
        0.0,
        1.0
    );
    
    output.tex_coords = input.tex_coords;
    output.color = input.color;
    
    return output;
}

// Texture bindings
@group(0) @binding(0) var glyph_texture: texture_2d<f32>;
@group(0) @binding(1) var glyph_sampler: sampler;

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Sample the glyph texture
    let alpha = textureSample(glyph_texture, glyph_sampler, input.tex_coords).a;
    
    // Multiply color by alpha for proper blending
    return vec4<f32>(input.color.rgb, input.color.a * alpha);
}