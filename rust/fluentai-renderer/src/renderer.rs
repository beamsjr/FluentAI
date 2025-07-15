//! Core renderer implementation

use crate::{Scene, primitives::Renderable};
use anyhow::Result;
use wgpu::util::DeviceExt;
use std::sync::Arc;

/// Main renderer struct
pub struct Renderer {
    device: wgpu::Device,
    queue: wgpu::Queue,
    surface: wgpu::Surface<'static>,
    config: wgpu::SurfaceConfiguration,
    size: winit::dpi::PhysicalSize<u32>,
    
    // Pipeline state
    render_pipeline: wgpu::RenderPipeline,
    
    // Text rendering
    // #[allow(dead_code)]
    // glyph_brush: wgpu_glyph::GlyphBrush<()>,
    
    // Vertex buffer for shapes
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    
    // Uniform buffer for transforms
    uniform_buffer: wgpu::Buffer,
    uniform_bind_group: wgpu::BindGroup,
}

/// Vertex data for rendering
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct Vertex {
    pub position: [f32; 3],
    pub color: [f32; 4],
}

impl Vertex {
    fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 0,
                    format: wgpu::VertexFormat::Float32x3,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 3]>() as wgpu::BufferAddress,
                    shader_location: 1,
                    format: wgpu::VertexFormat::Float32x4,
                },
            ],
        }
    }
}

/// Uniform data for transforms
#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct Uniforms {
    view_proj: [[f32; 4]; 4],
}

impl Renderer {
    /// Create a new renderer
    pub async fn new(window: Arc<winit::window::Window>) -> Result<Self> {
        let size = window.inner_size();
        
        // Create instance
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });
        
        // Create surface
        let surface = instance.create_surface(window.clone())?;
        
        // Get adapter
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await
            .ok_or_else(|| anyhow::anyhow!("Failed to find suitable adapter"))?;
        
        // Create device and queue
        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: Some("Renderer Device"),
                    required_features: wgpu::Features::empty(),
                    required_limits: if cfg!(target_arch = "wasm32") {
                        wgpu::Limits::downlevel_webgl2_defaults()
                    } else {
                        wgpu::Limits::default()
                    },
                },
                None,
            )
            .await?;
        
        // Configure surface
        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = surface_caps
            .formats
            .iter()
            .copied()
            .find(|f| f.is_srgb())
            .unwrap_or(surface_caps.formats[0]);
        
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode: surface_caps.present_modes[0],
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);
        
        // Create shader module
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Basic Shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("shaders/basic.wgsl").into()),
        });
        
        // Create uniform buffer
        let uniforms = Uniforms {
            view_proj: cgmath::Matrix4::from_scale(1.0).into(),
        };
        
        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });
        
        let uniform_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Uniform Bind Group Layout"),
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
        });
        
        let uniform_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Uniform Bind Group"),
            layout: &uniform_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
        });
        
        // Create render pipeline
        let render_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Render Pipeline Layout"),
            bind_group_layouts: &[&uniform_bind_group_layout],
            push_constant_ranges: &[],
        });
        
        let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&render_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[Vertex::desc()],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: Some(wgpu::Face::Back),
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
        
        // Text rendering temporarily disabled due to version mismatch
        // TODO: Re-enable when wgpu_glyph is updated to match wgpu 0.19
        
        // Create initial vertex and index buffers
        let vertices = &[
            Vertex { position: [0.0, 0.0, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
        ];
        let indices = &[0u16];
        
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(vertices),
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        });
        
        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Index Buffer"),
            contents: bytemuck::cast_slice(indices),
            usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
        });
        
        Ok(Self {
            device,
            queue,
            surface,
            config,
            size,
            render_pipeline,
            // glyph_brush,
            vertex_buffer,
            index_buffer,
            uniform_buffer,
            uniform_bind_group,
        })
    }
    
    /// Resize the renderer
    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.size = new_size;
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }
    
    /// Render a scene
    pub fn render(&mut self, scene: &Scene) -> Result<()> {
        let output = self.surface.get_current_texture()?;
        let view = output.texture.create_view(&wgpu::TextureViewDescriptor::default());
        
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Render Encoder"),
        });
        
        // Update uniform buffer with view projection matrix
        let aspect = self.size.width as f32 / self.size.height as f32;
        let proj = cgmath::ortho(-aspect, aspect, -1.0, 1.0, -1.0, 1.0);
        let view_proj = proj * cgmath::Matrix4::from_scale(1.0);
        
        let uniforms = Uniforms {
            view_proj: view_proj.into(),
        };
        self.queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));
        
        // Collect vertices for all renderables
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        
        for renderable in scene.get_renderables() {
            self.add_renderable_vertices(renderable, &mut vertices, &mut indices);
        }
        
        // Update vertex and index buffers if needed
        if !vertices.is_empty() {
            self.queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&vertices));
            self.queue.write_buffer(&self.index_buffer, 0, bytemuck::cast_slice(&indices));
        }
        
        // Begin render pass
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.1,
                            g: 0.1,
                            b: 0.1,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                occlusion_query_set: None,
                timestamp_writes: None,
            });
            
            if !indices.is_empty() {
                render_pass.set_pipeline(&self.render_pipeline);
                render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
                render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
                render_pass.draw_indexed(0..indices.len() as u32, 0, 0..1);
            }
        }
        
        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();
        
        Ok(())
    }
    
    /// Add vertices for a renderable
    fn add_renderable_vertices(&self, renderable: &Renderable, vertices: &mut Vec<Vertex>, indices: &mut Vec<u16>) {
        let base_index = vertices.len() as u16;
        
        match renderable {
            Renderable::Text { transform, content, size: font_size, color, .. } => {
                // Use simple text rendering for now
                use crate::simple_text::SimpleTextLayout;
                
                let layout = SimpleTextLayout::new(
                    content.clone(),
                    transform.position.x,
                    transform.position.y
                )
                .with_size(*font_size)
                .with_color(*color);
                
                // Generate quads for each character
                let quads = layout.generate_quads();
                
                for quad in quads {
                    let quad_vertices = quad.to_vertices();
                    let base = vertices.len() as u16;
                    
                    // Convert primitives::Vertex to renderer::Vertex
                    for v in quad_vertices {
                        vertices.push(Vertex {
                            position: v.position,
                            color: v.color,
                        });
                    }
                    
                    // Add indices for the quad
                    indices.extend_from_slice(&[
                        base, base + 1, base + 2,
                        base, base + 2, base + 3,
                    ]);
                }
            }
            Renderable::Rect { transform: _, size, color, .. } => {
                let half_width = size.width / 2.0;
                let half_height = size.height / 2.0;
                let color_array = color.to_array();
                
                // Add rectangle vertices
                vertices.extend_from_slice(&[
                    Vertex { position: [-half_width, -half_height, 0.0], color: color_array },
                    Vertex { position: [half_width, -half_height, 0.0], color: color_array },
                    Vertex { position: [half_width, half_height, 0.0], color: color_array },
                    Vertex { position: [-half_width, half_height, 0.0], color: color_array },
                ]);
                
                // Add rectangle indices
                indices.extend_from_slice(&[
                    base_index, base_index + 1, base_index + 2,
                    base_index, base_index + 2, base_index + 3,
                ]);
            }
            Renderable::Circle { transform, radius, color } => {
                // Generate vertices for a circle using triangulation
                const SEGMENTS: u16 = 32; // Number of segments for circle approximation
                
                let center_x = transform.position.x;
                let center_y = transform.position.y;
                let color_array = color.to_array();
                
                // Add center vertex
                let center_index = vertices.len() as u16;
                vertices.push(Vertex {
                    position: [center_x, center_y, 0.0],
                    color: color_array,
                });
                
                // Add vertices around the circumference
                for i in 0..=SEGMENTS {
                    let angle = (i as f32 / SEGMENTS as f32) * std::f32::consts::PI * 2.0;
                    let x = center_x + angle.cos() * radius;
                    let y = center_y + angle.sin() * radius;
                    
                    vertices.push(Vertex {
                        position: [x, y, 0.0],
                        color: color_array,
                    });
                }
                
                // Create triangles from center to edge
                for i in 0..SEGMENTS {
                    indices.extend_from_slice(&[
                        center_index,
                        center_index + i + 1,
                        center_index + i + 2,
                    ]);
                }
            }
            Renderable::Line { start, end, width, color } => {
                // Generate vertices for a line with thickness
                let dx = end.x - start.x;
                let dy = end.y - start.y;
                let len = (dx * dx + dy * dy).sqrt();
                
                if len > 0.0 {
                    // Calculate perpendicular vector for line thickness
                    let perp_x = -dy / len * width / 2.0;
                    let perp_y = dx / len * width / 2.0;
                    
                    let color_array = color.to_array();
                    
                    // Create a quad for the line
                    let base = vertices.len() as u16;
                    vertices.extend_from_slice(&[
                        Vertex {
                            position: [start.x - perp_x, start.y - perp_y, start.z],
                            color: color_array,
                        },
                        Vertex {
                            position: [start.x + perp_x, start.y + perp_y, start.z],
                            color: color_array,
                        },
                        Vertex {
                            position: [end.x + perp_x, end.y + perp_y, end.z],
                            color: color_array,
                        },
                        Vertex {
                            position: [end.x - perp_x, end.y - perp_y, end.z],
                            color: color_array,
                        },
                    ]);
                    
                    indices.extend_from_slice(&[
                        base, base + 1, base + 2,
                        base, base + 2, base + 3,
                    ]);
                }
            }
            Renderable::Ellipse { transform, width, height, color } => {
                // Generate vertices for an ellipse using triangulation
                const SEGMENTS: u16 = 32; // Number of segments for ellipse approximation
                
                let center_x = transform.position.x;
                let center_y = transform.position.y;
                let color_array = color.to_array();
                
                // Add center vertex
                let center_index = vertices.len() as u16;
                vertices.push(Vertex {
                    position: [center_x, center_y, 0.0],
                    color: color_array,
                });
                
                // Add vertices around the circumference
                for i in 0..=SEGMENTS {
                    let angle = (i as f32 / SEGMENTS as f32) * std::f32::consts::PI * 2.0;
                    let x = center_x + angle.cos() * width / 2.0;
                    let y = center_y + angle.sin() * height / 2.0;
                    
                    vertices.push(Vertex {
                        position: [x, y, 0.0],
                        color: color_array,
                    });
                }
                
                // Create triangles from center to edge
                for i in 0..SEGMENTS {
                    indices.extend_from_slice(&[
                        center_index,
                        center_index + i + 1,
                        center_index + i + 2,
                    ]);
                }
            }
            Renderable::Path { transform, data, stroke, fill: _ } => {
                // Parse and render path
                use crate::path::PathBuilder;
                
                match PathBuilder::from_svg(data) {
                    Ok(mut path) => {
                        // Apply stroke if specified
                        if let Some((color, width)) = stroke {
                            path = path.stroke(*color, *width);
                        }
                        
                        // Tessellate the path
                        let tess = path.tessellate();
                        
                        // Apply transform to vertices
                        let base = vertices.len() as u16;
                        for vertex in &tess.vertices {
                            vertices.push(Vertex {
                                position: [
                                    vertex.position[0] + transform.position.x,
                                    vertex.position[1] + transform.position.y,
                                    vertex.position[2] + transform.position.z,
                                ],
                                color: vertex.color,
                            });
                        }
                        
                        // Add indices with offset
                        for idx in &tess.indices {
                            indices.push(base + idx);
                        }
                    }
                    Err(e) => {
                        eprintln!("Failed to parse path: {}", e);
                    }
                }
            }
            Renderable::GradientRect { transform: _, size, gradient_id: _, radius: _ } => {
                // For now, render as a solid color rect with a placeholder color
                // TODO: Implement proper gradient rendering with GPU support
                let color_array = [0.5, 0.5, 0.5, 1.0]; // Gray placeholder
                
                let half_width = size.width / 2.0;
                let half_height = size.height / 2.0;
                
                vertices.extend_from_slice(&[
                    Vertex { position: [-half_width, -half_height, 0.0], color: color_array },
                    Vertex { position: [half_width, -half_height, 0.0], color: color_array },
                    Vertex { position: [half_width, half_height, 0.0], color: color_array },
                    Vertex { position: [-half_width, half_height, 0.0], color: color_array },
                ]);
                
                indices.extend_from_slice(&[
                    base_index, base_index + 1, base_index + 2,
                    base_index, base_index + 2, base_index + 3,
                ]);
            }
            Renderable::GradientCircle { transform, radius, gradient_id: _ } => {
                // For now, render as a solid color circle with a placeholder color
                // TODO: Implement proper gradient rendering
                const SEGMENTS: u16 = 32;
                let color_array = [0.5, 0.5, 0.5, 1.0]; // Gray placeholder
                
                let center_x = transform.position.x;
                let center_y = transform.position.y;
                
                // Add center vertex
                let center_index = vertices.len() as u16;
                vertices.push(Vertex {
                    position: [center_x, center_y, 0.0],
                    color: color_array,
                });
                
                // Add vertices around the circumference
                for i in 0..=SEGMENTS {
                    let angle = (i as f32 / SEGMENTS as f32) * std::f32::consts::PI * 2.0;
                    let x = center_x + angle.cos() * radius;
                    let y = center_y + angle.sin() * radius;
                    
                    vertices.push(Vertex {
                        position: [x, y, 0.0],
                        color: color_array,
                    });
                }
                
                // Create triangles from center to edge
                for i in 0..SEGMENTS {
                    indices.extend_from_slice(&[
                        center_index,
                        center_index + i + 1,
                        center_index + i + 2,
                    ]);
                }
            }
            _ => {
                // TODO: Implement Model3D rendering
            }
        }
    }
}