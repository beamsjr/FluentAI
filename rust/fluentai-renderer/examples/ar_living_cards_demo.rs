//! AR Living Cards Demo
//! 
//! This example demonstrates the AR Living Cards dashboard with physics-enabled
//! task cards that can be dragged, flicked, and organized in a Kanban board.
//! 
//! This example must be compiled for wasm32 target:
//! cargo build --example ar_living_cards_demo --target wasm32-unknown-unknown

#[cfg(target_arch = "wasm32")]
use fluentai_renderer::ar::{ARSession, ARFrame, TouchPhase, CardStatus, DebugOverlay};
#[cfg(target_arch = "wasm32")]
use fluentai_renderer::three_d::{Scene3D, Camera3D, Light, LightType, Node3D, Mesh3D, Material3D};
#[cfg(target_arch = "wasm32")]
use fluentai_renderer::webgl_renderer::WebGLRenderer;
#[cfg(target_arch = "wasm32")]
use fluentai_renderer::primitives::Color;
#[cfg(target_arch = "wasm32")]
use glam::{Vec3, Vec2, Quat};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;
#[cfg(target_arch = "wasm32")]
use web_sys::{WebGl2RenderingContext, HtmlCanvasElement};
#[cfg(target_arch = "wasm32")]
use std::cell::RefCell;
#[cfg(target_arch = "wasm32")]
use std::rc::Rc;

/// Demo application state
#[cfg(target_arch = "wasm32")]
struct AppState {
    ar_session: ARSession,
    scene: Scene3D,
    renderer: WebGLRenderer,
    debug_overlay: DebugOverlay,
    canvas_size: (u32, u32),
    show_debug: bool,
}

#[cfg(target_arch = "wasm32")]
thread_local! {
    static APP_STATE: RefCell<Option<Rc<RefCell<AppState>>>> = RefCell::new(None);
}

/// Initialize the AR Living Cards demo
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub async fn init_ar_demo(canvas: HtmlCanvasElement) -> Result<(), JsValue> {
    // Get WebGL context
    let gl = canvas
        .get_context("webgl2")?
        .unwrap()
        .dyn_into::<WebGl2RenderingContext>()?;
    
    // Create renderer
    let renderer = WebGLRenderer::new(gl)?;
    
    // Create AR session
    let mut ar_session = ARSession::new();
    ar_session.initialize().await?;
    
    // Create scene
    let mut scene = Scene3D::new();
    
    // Set up camera
    scene.camera = Camera3D::new(
        Vec3::new(0.0, 1.6, 0.0), // Eye level
        Vec3::new(0.0, 0.0, -2.0), // Looking at dashboard
        Vec3::Y,
        60.0, // FOV
        1.333, // Aspect ratio
        0.1,
        10.0,
    );
    
    // Add lighting
    scene.add_light(Light::ambient(Color::new(0.3, 0.3, 0.3, 1.0)));
    scene.add_light(Light::directional(
        Vec3::new(-0.3, -1.0, -0.5),
        Color::WHITE,
        1.0,
    ));
    
    // Create debug overlay
    let debug_overlay = DebugOverlay::new();
    
    // Store app state
    let app_state = Rc::new(RefCell::new(AppState {
        ar_session,
        scene,
        renderer,
        debug_overlay,
        canvas_size: (canvas.width(), canvas.height()),
        show_debug: true,
    }));
    
    APP_STATE.with(|state| {
        *state.borrow_mut() = Some(app_state.clone());
    });
    
    // Log initialization
    web_sys::console::log_1(&"AR Living Cards demo initialized!".into());
    web_sys::console::log_1(&"Controls:".into());
    web_sys::console::log_1(&"- Drag cards to move them".into());
    web_sys::console::log_1(&"- Flick cards to throw them".into());
    web_sys::console::log_1(&"- Cards snap to columns when close".into());
    web_sys::console::log_1(&"- Press 'D' to toggle debug visualization".into());
    
    Ok(())
}

/// Handle touch/mouse input
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn handle_touch(id: u32, x: f32, y: f32, phase: &str) {
    APP_STATE.with(|state| {
        if let Some(app_state) = state.borrow().as_ref() {
            let mut app = app_state.borrow_mut();
            
            // Convert screen coordinates to world coordinates
            let screen_pos = Vec2::new(x, y);
            let world_pos = screen_to_world(&app.scene.camera, screen_pos, app.canvas_size);
            
            // Convert phase string
            let touch_phase = match phase {
                "began" => TouchPhase::Began,
                "moved" => TouchPhase::Moved,
                "ended" => TouchPhase::Ended,
                _ => return,
            };
            
            // Handle touch in AR session
            app.ar_session.handle_touch(id, screen_pos, world_pos, touch_phase);
        }
    });
}

/// Update and render frame
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn render_frame(time: f64) {
    APP_STATE.with(|state| {
        if let Some(app_state) = state.borrow().as_ref() {
            let mut app = app_state.borrow_mut();
            
            // Create AR frame
            let ar_frame = ARFrame {
                viewer_pose: Some(crate::ar::ar_session::ViewerPose {
                    position: app.scene.camera.position,
                    rotation: Quat::IDENTITY, // Simplified for demo
                    view_matrix: app.scene.camera.view_matrix(),
                    projection_matrix: app.scene.camera.projection_matrix(),
                }),
                planes: Vec::new(),
                hit_tests: Vec::new(),
                timestamp: time,
            };
            
            // Process AR frame
            if let Err(e) = app.ar_session.process_frame(ar_frame) {
                web_sys::console::error_1(&format!("Frame processing error: {:?}", e).into());
                return;
            }
            
            // Update scene with Living Cards
            update_scene(&mut app);
            
            // Render scene
            if let Err(e) = app.renderer.render_scene(&app.scene) {
                web_sys::console::error_1(&format!("Render error: {:?}", e).into());
            }
        }
    });
}

/// Toggle debug visualization
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn toggle_debug() {
    APP_STATE.with(|state| {
        if let Some(app_state) = state.borrow().as_ref() {
            let mut app = app_state.borrow_mut();
            app.show_debug = !app.show_debug;
            web_sys::console::log_1(&format!("Debug visualization: {}", app.show_debug).into());
        }
    });
}

/// Update scene with Living Cards and debug visualization
#[cfg(target_arch = "wasm32")]
fn update_scene(app: &mut AppState) {
    // Clear existing nodes
    app.scene.nodes.clear();
    app.scene.roots.clear();
    app.scene.meshes.clear();
    app.scene.materials.clear();
    
    // Add default material
    app.scene.materials.push(Material3D::default());
    
    // Get Living Cards system
    let living_cards = app.ar_session.get_living_cards();
    
    // Create card meshes
    for (id, card) in living_cards.get_cards() {
        // Create card mesh
        let card_mesh = create_card_mesh(card);
        let mesh_idx = app.scene.add_mesh(card_mesh);
        
        // Create card material
        let mut material = Material3D::pbr(
            card.status.color(),
            0.0, // Non-metallic
            0.8, // Slightly rough
        );
        material.name = format!("card_{}", id);
        let material_idx = app.scene.add_material(material);
        
        // Update mesh material
        if let Some(mesh) = app.scene.meshes.get_mut(mesh_idx) {
            mesh.material_index = Some(material_idx);
        }
        
        // Create node
        let mut node = Node3D::new(format!("card_{}", id));
        node.transform = glam::Mat4::from_translation(card.position);
        node.mesh_index = Some(mesh_idx);
        
        let node_idx = app.scene.add_node(node);
        app.scene.add_root(node_idx);
    }
    
    // Add column visualizations
    for column in &living_cards.columns {
        let column_mesh = create_column_mesh(column);
        let mesh_idx = app.scene.add_mesh(column_mesh);
        
        let mut material = Material3D::unlit(column.status.color());
        material.opacity = 0.1;
        material.blend_mode = crate::three_d::material::BlendMode::Alpha;
        let material_idx = app.scene.add_material(material);
        
        if let Some(mesh) = app.scene.meshes.get_mut(mesh_idx) {
            mesh.material_index = Some(material_idx);
        }
        
        let mut node = Node3D::new(format!("column_{:?}", column.status));
        node.transform = glam::Mat4::from_translation(column.position);
        node.mesh_index = Some(mesh_idx);
        
        let node_idx = app.scene.add_node(node);
        app.scene.add_root(node_idx);
    }
    
    // Update debug overlay
    if app.show_debug {
        let debug_info = living_cards.get_debug_info();
        app.debug_overlay.update(&debug_info);
        
        // Add debug meshes to scene
        for debug_mesh in app.debug_overlay.get_meshes() {
            let mesh_idx = app.scene.meshes.len();
            app.scene.meshes.push(debug_mesh.clone());
            
            let mut node = Node3D::new(format!("debug_{}", mesh_idx));
            node.mesh_index = Some(mesh_idx);
            
            let node_idx = app.scene.add_node(node);
            app.scene.add_root(node_idx);
        }
    }
}

/// Create a mesh for a Living Card
#[cfg(target_arch = "wasm32")]
fn create_card_mesh(card: &fluentai_renderer::ar::LivingCard) -> Mesh3D {
    use fluentai_renderer::three_d::PrimitiveGeometry;
    
    // Create a rounded rectangle for the card
    let size = Vec3::new(card.size.x, card.size.y, 0.02); // 2cm thick
    PrimitiveGeometry::create_box(size)
}

/// Create a mesh for a column
#[cfg(target_arch = "wasm32")]
fn create_column_mesh(column: &fluentai_renderer::ar::CardColumn) -> Mesh3D {
    use fluentai_renderer::three_d::PrimitiveGeometry;
    
    let size = Vec3::new(column.width, column.height, 0.01); // 1cm thick
    PrimitiveGeometry::create_box(size)
}

/// Convert screen coordinates to world coordinates
#[cfg(target_arch = "wasm32")]
fn screen_to_world(camera: &Camera3D, screen_pos: Vec2, canvas_size: (u32, u32)) -> Vec3 {
    // Normalize screen coordinates to [-1, 1]
    let x = (screen_pos.x / canvas_size.0 as f32) * 2.0 - 1.0;
    let y = -((screen_pos.y / canvas_size.1 as f32) * 2.0 - 1.0);
    
    // Create ray from camera
    let view_proj_inv = (camera.projection_matrix() * camera.view_matrix()).inverse();
    let near_point = view_proj_inv * glam::Vec4::new(x, y, -1.0, 1.0);
    let far_point = view_proj_inv * glam::Vec4::new(x, y, 1.0, 1.0);
    
    let near_point = near_point.xyz() / near_point.w;
    let far_point = far_point.xyz() / far_point.w;
    
    // Intersect with z = -2 plane (where dashboard is)
    let ray_dir = (far_point - near_point).normalize();
    let t = (-2.0 - near_point.z) / ray_dir.z;
    
    near_point + ray_dir * t
}

// Re-export required types
#[cfg(target_arch = "wasm32")]
use fluentai_renderer::ar::ar_session::ViewerPose;

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    eprintln!("This example must be compiled for wasm32 target:");
    eprintln!("cargo build --example ar_living_cards_demo --target wasm32-unknown-unknown");
}