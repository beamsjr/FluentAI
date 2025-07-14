//! glTF 2.0 model loader

use crate::three_d::{Mesh3D, Material3D, Scene3D, Vertex3D};
use crate::three_d::scene3d::Node3D;
use crate::primitives::Color;
use glam::{Vec2, Vec3, Mat4};
use gltf::Gltf;
use std::path::Path;
use anyhow::{Result, Context};

/// Load a glTF file into a Scene3D
pub fn load_gltf<P: AsRef<Path>>(path: P) -> Result<Scene3D> {
    let path = path.as_ref();
    let gltf = Gltf::open(path)
        .context("Failed to open glTF file")?;
    
    let buffer_data = load_buffers(&gltf, path)?;
    
    let mut scene = Scene3D::new(path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Imported")
        .to_string());
    
    // Load materials
    for gltf_material in gltf.materials() {
        let material = convert_material(gltf_material);
        scene.add_material(material);
    }
    
    // Add a default material if there are none
    if scene.materials.is_empty() {
        scene.add_material(Material3D::default());
    }
    
    // Load meshes
    for gltf_mesh in gltf.meshes() {
        for primitive in gltf_mesh.primitives() {
            let mesh = convert_primitive(&primitive, &buffer_data)?;
            scene.add_mesh(mesh);
        }
    }
    
    // Load scene graph
    if let Some(gltf_scene) = gltf.default_scene().or_else(|| gltf.scenes().next()) {
        for node in gltf_scene.nodes() {
            load_node(&node, &mut scene, None, &buffer_data)?;
        }
    }
    
    Ok(scene)
}

/// Load all buffer data
fn load_buffers(gltf: &Gltf, base_path: &Path) -> Result<Vec<Vec<u8>>> {
    let mut buffer_data = Vec::new();
    
    for buffer in gltf.buffers() {
        match buffer.source() {
            gltf::buffer::Source::Bin => {
                if let Some(blob) = gltf.blob.as_deref() {
                    buffer_data.push(blob.to_vec());
                } else {
                    anyhow::bail!("Missing binary blob");
                }
            }
            gltf::buffer::Source::Uri(uri) => {
                let buffer_path = base_path.parent()
                    .unwrap_or(Path::new("."))
                    .join(uri);
                let data = std::fs::read(&buffer_path)
                    .with_context(|| format!("Failed to read buffer: {}", buffer_path.display()))?;
                buffer_data.push(data);
            }
        }
    }
    
    Ok(buffer_data)
}

/// Convert a glTF material to our Material3D
fn convert_material(gltf_material: gltf::Material) -> Material3D {
    let pbr = gltf_material.pbr_metallic_roughness();
    
    let base_color = pbr.base_color_factor();
    let base_color = Color::new(base_color[0], base_color[1], base_color[2], base_color[3]);
    
    let metallic = pbr.metallic_factor();
    let roughness = pbr.roughness_factor();
    
    let emissive = gltf_material.emissive_factor();
    let emissive_color = Color::new(emissive[0], emissive[1], emissive[2], 1.0);
    
    let mut material = Material3D::new_pbr(
        gltf_material.name().unwrap_or("Material").to_string(),
        base_color,
        metallic,
        roughness,
    );
    
    // Update emissive
    if let crate::three_d::material::MaterialType::PBR { ref mut emissive, .. } = material.material_type {
        *emissive = emissive_color;
    }
    
    // Handle transparency
    match gltf_material.alpha_mode() {
        gltf::material::AlphaMode::Blend => {
            material.opacity = base_color.a;
        }
        _ => {}
    }
    
    material.double_sided = gltf_material.double_sided();
    
    material
}

/// Convert a glTF primitive to our Mesh3D
fn convert_primitive(
    primitive: &gltf::Primitive,
    buffer_data: &[Vec<u8>],
) -> Result<Mesh3D> {
    let reader = primitive.reader(|buffer| Some(&buffer_data[buffer.index()]));
    
    let mut mesh = Mesh3D::new("Primitive".to_string());
    
    // Read positions (required)
    let positions = reader.read_positions()
        .context("Mesh primitive missing positions")?
        .collect::<Vec<_>>();
    
    // Read normals (optional, calculate if missing)
    let normals: Vec<[f32; 3]> = if let Some(normals) = reader.read_normals() {
        normals.collect()
    } else {
        // Calculate flat normals if missing
        vec![[0.0, 1.0, 0.0]; positions.len()]
    };
    
    // Read texture coordinates (optional)
    let tex_coords: Vec<[f32; 2]> = if let Some(coords) = reader.read_tex_coords(0) {
        coords.into_f32().collect()
    } else {
        vec![[0.0, 0.0]; positions.len()]
    };
    
    // Read vertex colors (optional)
    let colors: Option<Vec<[f32; 4]>> = reader.read_colors(0)
        .map(|colors| colors.into_rgba_f32().collect());
    
    // Build vertices
    for i in 0..positions.len() {
        let position = Vec3::from(positions[i]);
        let normal = Vec3::from(normals[i]);
        let tex_coord = Vec2::from(tex_coords[i]);
        
        let vertex = if let Some(ref colors) = colors {
            Vertex3D::with_color(position, normal, tex_coord, colors[i])
        } else {
            Vertex3D::new(position, normal, tex_coord)
        };
        
        mesh.vertices.push(vertex);
    }
    
    // Read indices
    if let Some(indices) = reader.read_indices() {
        mesh.indices = indices.into_u32().collect();
    } else {
        // Generate indices for non-indexed geometry
        mesh.indices = (0..mesh.vertices.len() as u32).collect();
    }
    
    // Set material index
    mesh.material_index = Some(primitive.material().index().unwrap_or(0));
    
    Ok(mesh)
}

/// Load a node and its children
fn load_node(
    gltf_node: &gltf::Node,
    scene: &mut Scene3D,
    parent_index: Option<usize>,
    buffer_data: &[Vec<u8>],
) -> Result<usize> {
    let mut node = Node3D::new(
        gltf_node.name().unwrap_or("Node").to_string()
    );
    
    // Set transform
    let (translation, rotation, scale) = gltf_node.transform().decomposed();
    let translation = Vec3::from(translation);
    let rotation = glam::Quat::from_array(rotation);
    let scale = Vec3::from(scale);
    
    node.transform = Mat4::from_scale_rotation_translation(scale, rotation, translation);
    
    // Set mesh
    if let Some(gltf_mesh) = gltf_node.mesh() {
        // For now, just use the first primitive
        // In a real implementation, we'd handle multiple primitives
        node.mesh_index = Some(gltf_mesh.index());
    }
    
    // Add node to scene
    let node_index = if let Some(parent) = parent_index {
        scene.add_child(parent, node).ok_or_else(|| anyhow::anyhow!("Failed to add child node"))?
    } else {
        scene.add_node(node)
    };
    
    // Load children
    for child in gltf_node.children() {
        load_node(&child, scene, Some(node_index), buffer_data)?;
    }
    
    Ok(node_index)
}