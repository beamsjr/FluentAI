// GPU instancing support for efficient rendering

use wgpu::util::DeviceExt;
use cgmath::{Matrix4, Vector3, Rad};
use crate::batching::InstanceData;

/// Instance buffer manager for GPU instancing
pub struct InstanceBuffer {
    buffer: wgpu::Buffer,
    capacity: usize,
    count: usize,
}

impl InstanceBuffer {
    /// Create a new instance buffer
    pub fn new(device: &wgpu::Device, capacity: usize) -> Self {
        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Instance Buffer"),
            size: (capacity * std::mem::size_of::<InstanceData>()) as wgpu::BufferAddress,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        
        Self {
            buffer,
            capacity,
            count: 0,
        }
    }
    
    /// Update instance data
    pub fn update(&mut self, queue: &wgpu::Queue, instances: &[InstanceData]) {
        self.count = instances.len().min(self.capacity);
        
        if self.count > 0 {
            queue.write_buffer(
                &self.buffer,
                0,
                bytemuck::cast_slice(&instances[..self.count]),
            );
        }
    }
    
    /// Get the buffer for binding
    pub fn buffer(&self) -> &wgpu::Buffer {
        &self.buffer
    }
    
    /// Get the number of instances
    pub fn count(&self) -> u32 {
        self.count as u32
    }
    
    /// Get vertex buffer layout for instances
    pub fn desc<'a>() -> wgpu::VertexBufferLayout<'a> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<InstanceData>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Instance,
            attributes: &[
                // Model matrix (4x4) - takes 4 attribute slots
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 2, // Locations 2-5 for matrix columns
                    format: wgpu::VertexFormat::Float32x4,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 4]>() as wgpu::BufferAddress,
                    shader_location: 3,
                    format: wgpu::VertexFormat::Float32x4,
                },
                wgpu::VertexAttribute {
                    offset: (std::mem::size_of::<[f32; 4]>() * 2) as wgpu::BufferAddress,
                    shader_location: 4,
                    format: wgpu::VertexFormat::Float32x4,
                },
                wgpu::VertexAttribute {
                    offset: (std::mem::size_of::<[f32; 4]>() * 3) as wgpu::BufferAddress,
                    shader_location: 5,
                    format: wgpu::VertexFormat::Float32x4,
                },
                // Color tint
                wgpu::VertexAttribute {
                    offset: (std::mem::size_of::<[f32; 4]>() * 4) as wgpu::BufferAddress,
                    shader_location: 6,
                    format: wgpu::VertexFormat::Float32x4,
                },
                // UV offset and scale
                wgpu::VertexAttribute {
                    offset: (std::mem::size_of::<[f32; 4]>() * 5) as wgpu::BufferAddress,
                    shader_location: 7,
                    format: wgpu::VertexFormat::Float32x4,
                },
            ],
        }
    }
}

/// Particle system using GPU instancing
pub struct ParticleSystem {
    particles: Vec<Particle>,
    instance_buffer: InstanceBuffer,
    base_mesh: BaseMesh,
}

#[derive(Clone)]
pub struct Particle {
    pub position: Vector3<f32>,
    pub velocity: Vector3<f32>,
    pub color: [f32; 4],
    pub size: f32,
    pub lifetime: f32,
    pub age: f32,
}

pub enum BaseMesh {
    Quad,
    Circle,
    Triangle,
}

impl ParticleSystem {
    pub fn new(device: &wgpu::Device, max_particles: usize, base_mesh: BaseMesh) -> Self {
        Self {
            particles: Vec::with_capacity(max_particles),
            instance_buffer: InstanceBuffer::new(device, max_particles),
            base_mesh,
        }
    }
    
    /// Spawn a new particle
    pub fn spawn(&mut self, particle: Particle) {
        if self.particles.len() < self.particles.capacity() {
            self.particles.push(particle);
        }
    }
    
    /// Update all particles
    pub fn update(&mut self, queue: &wgpu::Queue, delta_time: f32) {
        // Update particle physics
        self.particles.retain_mut(|particle| {
            particle.age += delta_time;
            particle.position += particle.velocity * delta_time;
            particle.velocity.y -= 9.8 * delta_time; // Gravity
            
            // Fade out based on age
            let life_ratio = particle.age / particle.lifetime;
            particle.color[3] = (1.0 - life_ratio).max(0.0);
            
            particle.age < particle.lifetime
        });
        
        // Convert particles to instance data
        let instances: Vec<InstanceData> = self.particles.iter()
            .map(|particle| {
                let transform = Transform {
                    position: Position3D {
                        x: particle.position.x,
                        y: particle.position.y,
                        z: particle.position.z,
                    },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (particle.size, particle.size, 1.0),
                };
                
                InstanceData::new(&transform, Color::from_array(particle.color))
            })
            .collect();
        
        self.instance_buffer.update(queue, &instances);
    }
    
    /// Get base mesh vertices and indices
    pub fn get_base_mesh(&self) -> (Vec<crate::renderer::Vertex>, Vec<u16>) {
        match self.base_mesh {
            BaseMesh::Quad => {
                let vertices = vec![
                    crate::renderer::Vertex { position: [-0.5, -0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                    crate::renderer::Vertex { position: [0.5, -0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                    crate::renderer::Vertex { position: [0.5, 0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                    crate::renderer::Vertex { position: [-0.5, 0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                ];
                let indices = vec![0, 1, 2, 0, 2, 3];
                (vertices, indices)
            }
            BaseMesh::Circle => {
                const SEGMENTS: usize = 16; // Lower poly for particles
                let mut vertices = Vec::with_capacity(SEGMENTS + 1);
                let mut indices = Vec::with_capacity(SEGMENTS * 3);
                
                // Center vertex
                vertices.push(crate::renderer::Vertex {
                    position: [0.0, 0.0, 0.0],
                    color: [1.0, 1.0, 1.0, 1.0],
                });
                
                // Edge vertices
                for i in 0..SEGMENTS {
                    let angle = (i as f32 / SEGMENTS as f32) * std::f32::consts::PI * 2.0;
                    vertices.push(crate::renderer::Vertex {
                        position: [angle.cos() * 0.5, angle.sin() * 0.5, 0.0],
                        color: [1.0, 1.0, 1.0, 1.0],
                    });
                }
                
                // Indices
                for i in 0..SEGMENTS {
                    indices.push(0);
                    indices.push((i + 1) as u16);
                    indices.push(((i + 1) % SEGMENTS + 1) as u16);
                }
                
                (vertices, indices)
            }
            BaseMesh::Triangle => {
                let vertices = vec![
                    crate::renderer::Vertex { position: [0.0, 0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                    crate::renderer::Vertex { position: [-0.5, -0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                    crate::renderer::Vertex { position: [0.5, -0.5, 0.0], color: [1.0, 1.0, 1.0, 1.0] },
                ];
                let indices = vec![0, 1, 2];
                (vertices, indices)
            }
        }
    }
}

/// Sprite batch for 2D sprite rendering
pub struct SpriteBatch {
    sprites: Vec<Sprite>,
    instance_buffer: InstanceBuffer,
    texture_atlas: TextureAtlas,
}

pub struct Sprite {
    pub position: Vector2<f32>,
    pub size: Vector2<f32>,
    pub rotation: f32,
    pub color: Color,
    pub texture_region: TextureRegion,
}

pub struct TextureAtlas {
    pub texture_id: u32,
    pub width: u32,
    pub height: u32,
    regions: HashMap<String, TextureRegion>,
}

#[derive(Clone)]
pub struct TextureRegion {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl SpriteBatch {
    pub fn new(device: &wgpu::Device, max_sprites: usize, texture_atlas: TextureAtlas) -> Self {
        Self {
            sprites: Vec::with_capacity(max_sprites),
            instance_buffer: InstanceBuffer::new(device, max_sprites),
            texture_atlas,
        }
    }
    
    /// Add a sprite to the batch
    pub fn add(&mut self, sprite: Sprite) {
        if self.sprites.len() < self.sprites.capacity() {
            self.sprites.push(sprite);
        }
    }
    
    /// Clear all sprites
    pub fn clear(&mut self) {
        self.sprites.clear();
    }
    
    /// Update instance buffer with sprite data
    pub fn update(&mut self, queue: &wgpu::Queue) {
        let instances: Vec<InstanceData> = self.sprites.iter()
            .map(|sprite| {
                let transform = Transform {
                    position: Position3D {
                        x: sprite.position.x,
                        y: sprite.position.y,
                        z: 0.0,
                    },
                    rotation: (0.0, 0.0, sprite.rotation),
                    scale: (sprite.size.x, sprite.size.y, 1.0),
                };
                
                let mut instance = InstanceData::new(&transform, sprite.color);
                
                // Calculate UV coordinates for texture region
                let u = sprite.texture_region.x / self.texture_atlas.width as f32;
                let v = sprite.texture_region.y / self.texture_atlas.height as f32;
                let w = sprite.texture_region.width / self.texture_atlas.width as f32;
                let h = sprite.texture_region.height / self.texture_atlas.height as f32;
                
                instance.uv_offset_scale = [u, v, w, h];
                
                instance
            })
            .collect();
        
        self.instance_buffer.update(queue, &instances);
    }
}

// Import required types
use crate::primitives::{Color, Transform, Position3D};
use cgmath::Vector2;
use std::collections::HashMap;

/// Instanced text renderer for efficient text rendering
pub struct InstancedTextRenderer {
    glyph_instances: Vec<GlyphInstance>,
    instance_buffer: InstanceBuffer,
    font_atlas: FontAtlas,
}

pub struct GlyphInstance {
    pub position: Vector2<f32>,
    pub size: Vector2<f32>,
    pub color: Color,
    pub glyph_id: u32,
}

pub struct FontAtlas {
    pub texture_id: u32,
    pub atlas_size: Vector2<u32>,
    pub glyph_map: HashMap<char, GlyphInfo>,
}

pub struct GlyphInfo {
    pub uv_rect: TextureRegion,
    pub advance: f32,
    pub bearing: Vector2<f32>,
}

impl InstancedTextRenderer {
    pub fn new(device: &wgpu::Device, max_glyphs: usize, font_atlas: FontAtlas) -> Self {
        Self {
            glyph_instances: Vec::with_capacity(max_glyphs),
            instance_buffer: InstanceBuffer::new(device, max_glyphs),
            font_atlas,
        }
    }
    
    /// Add text to render
    pub fn add_text(&mut self, text: &str, position: Vector2<f32>, size: f32, color: Color) {
        let mut cursor_x = position.x;
        let cursor_y = position.y;
        
        for ch in text.chars() {
            if let Some(glyph_info) = self.font_atlas.glyph_map.get(&ch) {
                if self.glyph_instances.len() < self.glyph_instances.capacity() {
                    self.glyph_instances.push(GlyphInstance {
                        position: Vector2::new(
                            cursor_x + glyph_info.bearing.x * size,
                            cursor_y - glyph_info.bearing.y * size,
                        ),
                        size: Vector2::new(
                            glyph_info.uv_rect.width * size,
                            glyph_info.uv_rect.height * size,
                        ),
                        color,
                        glyph_id: ch as u32,
                    });
                }
                
                cursor_x += glyph_info.advance * size;
            }
        }
    }
    
    /// Clear all text
    pub fn clear(&mut self) {
        self.glyph_instances.clear();
    }
    
    /// Update instance buffer
    pub fn update(&mut self, queue: &wgpu::Queue) {
        let instances: Vec<InstanceData> = self.glyph_instances.iter()
            .map(|glyph| {
                let transform = Transform {
                    position: Position3D {
                        x: glyph.position.x,
                        y: glyph.position.y,
                        z: 0.0,
                    },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (glyph.size.x, glyph.size.y, 1.0),
                };
                
                let mut instance = InstanceData::new(&transform, glyph.color);
                
                // Get UV coordinates for this glyph
                if let Some(glyph_info) = self.font_atlas.glyph_map.get(&char::from_u32(glyph.glyph_id).unwrap_or(' ')) {
                    let u = glyph_info.uv_rect.x / self.font_atlas.atlas_size.x as f32;
                    let v = glyph_info.uv_rect.y / self.font_atlas.atlas_size.y as f32;
                    let w = glyph_info.uv_rect.width / self.font_atlas.atlas_size.x as f32;
                    let h = glyph_info.uv_rect.height / self.font_atlas.atlas_size.y as f32;
                    
                    instance.uv_offset_scale = [u, v, w, h];
                }
                
                instance
            })
            .collect();
        
        self.instance_buffer.update(queue, &instances);
    }
}