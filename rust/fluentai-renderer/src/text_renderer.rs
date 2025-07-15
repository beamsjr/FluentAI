use anyhow::{Result, anyhow};
use cgmath::{Vector2, Matrix4};
use rustc_hash::FxHashMap;
use std::sync::Arc;
// Temporarily disabled until font loading is properly implemented
// use fontdue::{Font as FontdueFont, FontSettings};
// use glyph_brush_layout::{
//     GlyphPositioner, Layout, SectionGeometry, SectionText,
//     FontId as LayoutFontId, SectionGlyph, GlyphChange,
// };

// Text rendering module for FluentAI
// This implementation provides GPU-accelerated text rendering with proper font support

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FontId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextAlign {
    Left,
    Center,
    Right,
    Justify,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextBaseline {
    Top,
    Middle,
    Bottom,
    Alphabetic,
}

#[derive(Debug, Clone)]
pub struct TextStyle {
    pub font_id: FontId,
    pub font_size: f32,
    pub color: [f32; 4],
    pub line_height: Option<f32>,
    pub letter_spacing: f32,
    pub bold: bool,
    pub italic: bool,
}

impl Default for TextStyle {
    fn default() -> Self {
        Self {
            font_id: FontId(0),
            font_size: 16.0,
            color: [0.0, 0.0, 0.0, 1.0],
            line_height: None,
            letter_spacing: 0.0,
            bold: false,
            italic: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TextLayout {
    pub text: String,
    pub position: Vector2<f32>,
    pub max_width: Option<f32>,
    pub align: TextAlign,
    pub baseline: TextBaseline,
    pub style: TextStyle,
}

// Vertex structure for text rendering
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub struct TextVertex {
    pub position: [f32; 2],
    pub tex_coords: [f32; 2],
    pub color: [f32; 4],
}

impl TextVertex {
    pub fn desc<'a>() -> wgpu::VertexBufferLayout<'a> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<TextVertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 0,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 2]>() as wgpu::BufferAddress,
                    shader_location: 1,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 4]>() as wgpu::BufferAddress,
                    shader_location: 2,
                    format: wgpu::VertexFormat::Float32x4,
                },
            ],
        }
    }
}

// Glyph cache entry
#[derive(Debug, Clone)]
struct GlyphCacheEntry {
    texture_coords: [f32; 4], // x, y, width, height in texture space
    size: Vector2<f32>,       // Size in pixels
    bearing: Vector2<f32>,    // Offset from baseline
    advance: f32,             // Horizontal advance
}

pub struct TextRenderer {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    
    // Font management
    fonts: FxHashMap<FontId, Font>,
    default_font: FontId,
    
    // Glyph cache
    glyph_cache: FxHashMap<(FontId, char, u32), GlyphCacheEntry>, // (font, char, size) -> cache entry
    atlas_texture: wgpu::Texture,
    atlas_view: wgpu::TextureView,
    atlas_size: u32,
    atlas_cursor: (u32, u32), // Current position in atlas
    atlas_row_height: u32,
    
    // GPU resources
    pipeline: wgpu::RenderPipeline,
    bind_group_layout: wgpu::BindGroupLayout,
    bind_group: wgpu::BindGroup,
    sampler: wgpu::Sampler,
    
    // Buffers
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    vertex_buffer_size: u64,
    index_buffer_size: u64,
}

// Font wrapper (placeholder until fontdue is integrated)
struct Font {
    name: String,
    // font: FontdueFont,
}

impl TextRenderer {
    // Create a simple fallback font for development
    fn create_fallback_font() -> Result<Font> {
        // Placeholder implementation
        Ok(Font {
            name: "Fallback".to_string(),
        })
    }
    
    pub fn new(device: Arc<wgpu::Device>, queue: Arc<wgpu::Queue>) -> Result<Self> {
        // Create texture atlas for glyph caching
        let atlas_size = 2048;
        let atlas_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Glyph Atlas"),
            size: wgpu::Extent3d {
                width: atlas_size,
                height: atlas_size,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        
        let atlas_view = atlas_texture.create_view(&wgpu::TextureViewDescriptor::default());
        
        // Create sampler
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Text Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });
        
        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Text Bind Group Layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });
        
        // Create bind group
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Text Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&atlas_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });
        
        // Create shader module
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Text Shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("shaders/text.wgsl").into()),
        });
        
        // Create pipeline
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Text Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Text Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[TextVertex::desc()],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: wgpu::TextureFormat::Bgra8UnormSrgb,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: None,
                polygon_mode: wgpu::PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            multiview: None,
        });
        
        // Create initial buffers
        let vertex_buffer_size = 65536; // 64KB initial size
        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Text Vertex Buffer"),
            size: vertex_buffer_size,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        let index_buffer_size = 32768; // 32KB initial size
        let index_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Text Index Buffer"),
            size: index_buffer_size,
            usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        // Load default font
        let mut fonts = FxHashMap::default();
        let default_font = FontId(0);
        
        // For now, create a simple fallback font
        // In production, we'd load from assets/fonts/Roboto-Regular.ttf
        let font = Self::create_fallback_font()?;
        
        fonts.insert(default_font, font);
        
        Ok(Self {
            device,
            queue,
            fonts,
            default_font,
            glyph_cache: FxHashMap::default(),
            atlas_texture,
            atlas_view,
            atlas_size,
            atlas_cursor: (0, 0),
            atlas_row_height: 0,
            pipeline,
            bind_group_layout,
            bind_group,
            sampler,
            vertex_buffer,
            index_buffer,
            vertex_buffer_size,
            index_buffer_size,
        })
    }
    
    // Load a font from file
    pub fn load_font(&mut self, path: &str) -> Result<FontId> {
        // Placeholder implementation
        let id = FontId(self.fonts.len() as u32);
        self.fonts.insert(id, Font {
            name: path.to_string(),
        });
        Ok(id)
    }
    
    // Load a system font
    pub fn load_system_font(&mut self, name: &str) -> Result<FontId> {
        // TODO: Implement system font loading
        self.load_font(name)
    }
    
    // Measure text dimensions
    pub fn measure_text(&self, layout: &TextLayout) -> Vector2<f32> {
        // Simple approximation for now
        let char_width = layout.style.font_size * 0.6;
        let width = layout.text.len() as f32 * char_width;
        let height = layout.style.font_size;
        Vector2::new(width, height)
    }
    
    // Generate vertices for text
    pub fn generate_vertices(&mut self, layouts: &[TextLayout]) -> Result<(Vec<TextVertex>, Vec<u32>)> {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        for layout in layouts {
            self.layout_text(layout, &mut vertices, &mut indices)?;
        }
        
        Ok((vertices, indices))
    }
    
    // Layout a single text string
    fn layout_text(
        &mut self,
        layout: &TextLayout,
        vertices: &mut Vec<TextVertex>,
        indices: &mut Vec<u32>,
    ) -> Result<()> {
        // TODO: Implement actual text layout with proper glyph positioning
        // For now, create a simple rectangle as placeholder
        
        let size = self.measure_text(layout);
        let base_vertex = vertices.len() as u32;
        
        // Calculate position based on alignment
        let x = match layout.align {
            TextAlign::Left => layout.position.x,
            TextAlign::Center => layout.position.x - size.x / 2.0,
            TextAlign::Right => layout.position.x - size.x,
            TextAlign::Justify => layout.position.x,
        };
        
        let y = match layout.baseline {
            TextBaseline::Top => layout.position.y,
            TextBaseline::Middle => layout.position.y - size.y / 2.0,
            TextBaseline::Bottom => layout.position.y - size.y,
            TextBaseline::Alphabetic => layout.position.y - size.y * 0.8,
        };
        
        // Add placeholder rectangle vertices
        vertices.extend_from_slice(&[
            TextVertex {
                position: [x, y],
                tex_coords: [0.0, 0.0],
                color: layout.style.color,
            },
            TextVertex {
                position: [x + size.x, y],
                tex_coords: [1.0, 0.0],
                color: layout.style.color,
            },
            TextVertex {
                position: [x + size.x, y + size.y],
                tex_coords: [1.0, 1.0],
                color: layout.style.color,
            },
            TextVertex {
                position: [x, y + size.y],
                tex_coords: [0.0, 1.0],
                color: layout.style.color,
            },
        ]);
        
        // Add indices
        indices.extend_from_slice(&[
            base_vertex, base_vertex + 1, base_vertex + 2,
            base_vertex, base_vertex + 2, base_vertex + 3,
        ]);
        
        Ok(())
    }
    
    // Render text to a render pass
    pub fn render<'a>(
        &'a mut self,
        render_pass: &mut wgpu::RenderPass<'a>,
        layouts: &[TextLayout],
    ) -> Result<()> {
        // Generate vertices
        let (vertices, indices) = self.generate_vertices(layouts)?;
        
        if vertices.is_empty() {
            return Ok(());
        }
        
        // Update buffers
        self.queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&vertices));
        self.queue.write_buffer(&self.index_buffer, 0, bytemuck::cast_slice(&indices));
        
        // Set pipeline and bind groups
        render_pass.set_pipeline(&self.pipeline);
        render_pass.set_bind_group(0, &self.bind_group, &[]);
        
        // Set vertex and index buffers
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint32);
        
        // Draw
        render_pass.draw_indexed(0..indices.len() as u32, 0, 0..1);
        
        Ok(())
    }
}

// Text rendering utilities
pub mod utils {
    use super::*;
    
    // Create a text layout builder
    pub struct TextLayoutBuilder {
        layout: TextLayout,
    }
    
    impl TextLayoutBuilder {
        pub fn new(text: impl Into<String>) -> Self {
            Self {
                layout: TextLayout {
                    text: text.into(),
                    position: Vector2::new(0.0, 0.0),
                    max_width: None,
                    align: TextAlign::Left,
                    baseline: TextBaseline::Alphabetic,
                    style: TextStyle::default(),
                },
            }
        }
        
        pub fn position(mut self, x: f32, y: f32) -> Self {
            self.layout.position = Vector2::new(x, y);
            self
        }
        
        pub fn max_width(mut self, width: f32) -> Self {
            self.layout.max_width = Some(width);
            self
        }
        
        pub fn align(mut self, align: TextAlign) -> Self {
            self.layout.align = align;
            self
        }
        
        pub fn baseline(mut self, baseline: TextBaseline) -> Self {
            self.layout.baseline = baseline;
            self
        }
        
        pub fn font_size(mut self, size: f32) -> Self {
            self.layout.style.font_size = size;
            self
        }
        
        pub fn color(mut self, r: f32, g: f32, b: f32, a: f32) -> Self {
            self.layout.style.color = [r, g, b, a];
            self
        }
        
        pub fn bold(mut self, bold: bool) -> Self {
            self.layout.style.bold = bold;
            self
        }
        
        pub fn italic(mut self, italic: bool) -> Self {
            self.layout.style.italic = italic;
            self
        }
        
        pub fn build(self) -> TextLayout {
            self.layout
        }
    }
}