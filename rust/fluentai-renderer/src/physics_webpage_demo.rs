// Physics-based webpage demo using FluentAI renderer
use wasm_bindgen::prelude::*;
use web_sys::{console, HtmlCanvasElement, WebGl2RenderingContext};
use crate::webgl_renderer::WebGLRenderer;
use crate::three_d::{Scene3D, Camera3D, Node3D, Mesh3D, Material3D, MaterialType, Light, LightType, CameraType, Vertex3D};
use crate::primitives::Color;
// Text rendering will be added later
use glam::{Vec2, Vec3, Mat4, Quat};
use std::f32::consts::PI;

// Helper math functions for WASM
#[inline]
fn sin_approx(mut x: f32) -> f32 {
    // Normalize to [-PI, PI]
    while x > PI {
        x -= 2.0 * PI;
    }
    while x < -PI {
        x += 2.0 * PI;
    }
    
    // Simple sine approximation using Taylor series
    let x2 = x * x;
    x * (1.0 - x2 / 6.0 + x2 * x2 / 120.0)
}

#[inline]
fn cos_approx(x: f32) -> f32 {
    sin_approx(x + PI / 2.0)
}

// UI Component types
#[derive(Clone, Debug, PartialEq)]
enum ComponentType {
    Header,
    Card,
    Sidebar,
    Button,
    TextBlock,
    Image,
    Footer,
    Chart,
}

// Connection between components
#[derive(Clone)]
struct Connection {
    from_id: String,
    to_id: String,
    rest_length: f32,
    strength: f32,
    visible: bool,
}

// Magnetic docking zone
#[derive(Clone)]
struct DockingZone {
    position: Vec2,
    size: Vec2,
    strength: f32,
    occupied_by: Option<String>,
    zone_type: DockingZoneType,
}

#[derive(Clone, Debug)]
enum DockingZoneType {
    CardSlot,
    ButtonDock,
    SidebarDock,
    HeroZone,
}

// Chart data for interactive charts
#[derive(Clone)]
struct ChartData {
    values: Vec<f32>,
    labels: Vec<String>,
    chart_type: ChartType,
}

#[derive(Clone, Debug)]
enum ChartType {
    Bar,
    Line,
    Pie,
}

// Physics-enabled UI component
#[derive(Clone)]
struct UIComponent {
    id: String,
    component_type: ComponentType,
    position: Vec2,
    velocity: Vec2,
    size: Vec2,
    mass: f32,
    is_dragging: bool,
    drag_offset: Vec2,
    content: String,
    color: Color,
    depth: f32,
    children: Vec<String>, // IDs of child components
    parent: Option<String>,
    is_fixed: bool, // Some components like header might be fixed
    chart_data: Option<ChartData>, // For chart components
}

impl UIComponent {
    fn new(id: String, component_type: ComponentType, position: Vec2, size: Vec2, content: String) -> Self {
        let (mass, color, depth) = match &component_type {
            ComponentType::Header => (3.0, Color::new(1.0, 1.0, 1.0, 0.98), 0.1), // White with slight transparency
            ComponentType::Card => (1.5, Color::new(1.0, 1.0, 1.0, 0.95), 0.2), // Pure white cards
            ComponentType::Sidebar => (2.0, Color::new(0.97, 0.97, 0.99, 0.98), 0.15), // Very light gray
            ComponentType::Button => (0.5, Color::new(0.4, 0.3, 1.0, 0.95), 0.3), // Modern purple
            ComponentType::TextBlock => (0.8, Color::new(0.98, 0.98, 0.98, 0.9), 0.25),
            ComponentType::Image => (1.2, Color::new(0.95, 0.95, 0.97, 1.0), 0.25),
            ComponentType::Footer => (2.5, Color::new(0.05, 0.05, 0.08, 0.98), 0.1), // Dark footer
            ComponentType::Chart => (1.8, Color::new(0.95, 0.95, 1.0, 0.95), 0.2), // Light purple tint
        };

        Self {
            id,
            component_type,
            position,
            velocity: Vec2::ZERO,
            size,
            mass,
            is_dragging: false,
            drag_offset: Vec2::ZERO,
            content,
            color,
            depth,
            children: Vec::new(),
            parent: None,
            is_fixed: false,
            chart_data: None,
        }
    }

    fn contains_point(&self, point: Vec2) -> bool {
        point.x >= self.position.x - self.size.x / 2.0 &&
        point.x <= self.position.x + self.size.x / 2.0 &&
        point.y >= self.position.y - self.size.y / 2.0 &&
        point.y <= self.position.y + self.size.y / 2.0
    }

    fn get_edges(&self) -> (f32, f32, f32, f32) {
        (
            self.position.x - self.size.x / 2.0, // left
            self.position.x + self.size.x / 2.0, // right
            self.position.y - self.size.y / 2.0, // top
            self.position.y + self.size.y / 2.0, // bottom
        )
    }
}

// Touch tracking for multi-touch gestures
#[derive(Clone)]
struct TouchInfo {
    id: i32,
    position: Vec2,
    start_position: Vec2,
    component_id: Option<usize>,
}

// Drag history point for momentum calculation
#[derive(Clone)]
struct DragPoint {
    position: Vec2,
    time: f64,
}

// Motion trail point for visual effect
#[derive(Clone)]
struct TrailPoint {
    position: Vec2,
    age: f32,  // Time since creation
    component_id: String,
    color: Color,
}

#[wasm_bindgen]
pub struct PhysicsWebpageDemo {
    renderer: WebGLRenderer,
    scene: Scene3D,
    camera: Camera3D,
    components: Vec<UIComponent>,
    connections: Vec<Connection>,
    docking_zones: Vec<DockingZone>,
    mouse_pos: Vec2,
    mouse_down: bool,
    selected_component: Option<usize>,
    canvas_width: f32,
    canvas_height: f32,
    time: f32,
    // Physics parameters
    attraction_strength: f32,
    repulsion_strength: f32,
    damping: f32,
    edge_attraction: f32,
    grid_size: f32,              // Grid snapping size
    snap_threshold: f32,         // Distance to snap to grid
    // Visual effects
    hover_component: Option<usize>,
    ambient_motion: f32,
    // Multi-touch tracking
    touches: Vec<TouchInfo>,
    pinch_start_distance: Option<f32>,
    pinch_center: Option<Vec2>,
    grouped_components: Vec<usize>,
    rotation_start_angle: Option<f32>,
    // Performance monitoring
    last_frame_time: f64,
    frame_times: Vec<f64>,
    performance_scale: f32,
    physics_substeps: u32,
    render_quality: RenderQuality,
    // Clustering
    clusters: Vec<ComponentCluster>,
    // Momentum tracking
    drag_history: Vec<DragPoint>,
    momentum_samples: usize,
    // Motion trails
    motion_trails: Vec<TrailPoint>,
    trail_lifetime: f32,  // How long trail points live
    trail_spawn_interval: f32,  // Time between spawning trail points
    last_trail_spawn: f64,
}

// Component cluster for stronger cohesion
#[derive(Clone)]
struct ComponentCluster {
    component_type: ComponentType,
    member_ids: Vec<String>,
    center: Vec2,
    radius: f32,
}

// Render quality levels for performance scaling
#[derive(Clone, Debug)]
enum RenderQuality {
    Low,     // Minimal effects, reduced physics
    Medium,  // Standard quality
    High,    // All effects enabled
}

#[wasm_bindgen]
impl PhysicsWebpageDemo {
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<PhysicsWebpageDemo, JsValue> {
        console::log_1(&"Initializing Physics Webpage Demo...".into());
        
        // Get canvas and context
        let document = web_sys::window().unwrap().document().unwrap();
        let canvas = document.get_element_by_id(canvas_id)
            .ok_or_else(|| JsValue::from_str("Canvas not found"))?;
        let canvas: HtmlCanvasElement = canvas.dyn_into()?;
        
        let gl = canvas
            .get_context("webgl2")?
            .ok_or_else(|| JsValue::from_str("WebGL2 not supported"))?
            .dyn_into::<WebGl2RenderingContext>()?;
        
        let mut renderer = WebGLRenderer::new(gl)?;
        
        // Create scene with orthographic camera for 2D view
        let mut scene = Scene3D::new("Physics Webpage".to_string());
        
        let window = web_sys::window().unwrap();
        let width = window.inner_width().unwrap().as_f64().unwrap() as f32;
        let height = window.inner_height().unwrap().as_f64().unwrap() as f32;
        
        // Orthographic camera for 2D-like rendering
        let camera = Camera3D {
            position: Vec3::new(0.0, 0.0, 10.0),
            target: Vec3::ZERO,
            up: Vec3::Y,
            projection: CameraType::Orthographic {
                left: -width / 2.0,
                right: width / 2.0,
                bottom: -height / 2.0,
                top: height / 2.0,
                near: 0.1,
                far: 100.0,
            },
            aspect_ratio: width / height,
        };
        
        // Set the camera on the scene
        scene.camera = camera.clone();
        
        // Add ambient light
        scene.lights.push(Light {
            id: "ambient".to_string(),
            enabled: true,
            light_type: LightType::Ambient { 
                color: Color::new(0.9, 0.9, 0.95, 1.0)
            },
            cast_shadows: false,
        });
        
        // Add directional light for depth
        scene.lights.push(Light {
            id: "directional".to_string(),
            enabled: true,
            light_type: LightType::Directional {
                direction: Vec3::new(-0.3, -0.5, -1.0).normalize(),
                color: Color::new(1.0, 1.0, 1.0, 1.0),
                intensity: 0.5,
            },
            cast_shadows: true,
        });
        
        // Create materials for different component types - modern, clean design
        let materials = vec![
            // Header material - clean white with subtle shadow
            Material3D {
                name: "header_material".to_string(),
                material_type: MaterialType::PBR {
                    base_color: Color::new(1.0, 1.0, 1.0, 0.98),
                    metallic: 0.0,
                    roughness: 0.95,
                    emissive: Color::new(0.0, 0.0, 0.0, 1.0),
                    base_color_texture: None,
                    metallic_roughness_texture: None,
                    normal_texture: None,
                    emissive_texture: None,
                },
                opacity: 0.98,
                double_sided: true,
            },
            // Card material - pure white with soft shadows
            Material3D {
                name: "card_material".to_string(),
                material_type: MaterialType::PBR {
                    base_color: Color::new(1.0, 1.0, 1.0, 0.95),
                    metallic: 0.0,
                    roughness: 0.95,
                    emissive: Color::new(0.0, 0.0, 0.0, 1.0),
                    base_color_texture: None,
                    metallic_roughness_texture: None,
                    normal_texture: None,
                    emissive_texture: None,
                },
                opacity: 0.95,
                double_sided: true,
            },
            // Sidebar material - light gray
            Material3D {
                name: "sidebar_material".to_string(),
                material_type: MaterialType::PBR {
                    base_color: Color::new(0.97, 0.97, 0.99, 0.98),
                    metallic: 0.0,
                    roughness: 0.95,
                    emissive: Color::new(0.0, 0.0, 0.0, 1.0),
                    base_color_texture: None,
                    metallic_roughness_texture: None,
                    normal_texture: None,
                    emissive_texture: None,
                },
                opacity: 0.98,
                double_sided: true,
            },
            // Button material - modern gradient purple
            Material3D {
                name: "button_material".to_string(),
                material_type: MaterialType::PBR {
                    base_color: Color::new(0.4, 0.3, 1.0, 0.95),
                    metallic: 0.0,
                    roughness: 0.7,
                    emissive: Color::new(0.05, 0.03, 0.15, 1.0),
                    base_color_texture: None,
                    metallic_roughness_texture: None,
                    normal_texture: None,
                    emissive_texture: None,
                },
                opacity: 0.95,
                double_sided: true,
            },
        ];
        
        for material in materials {
            scene.materials.push(material);
        }
        
        // Create meshes for each material type
        // Header mesh with material 0
        let mut header_mesh = create_rounded_box_mesh(1.0, 1.0, 0.1, 0.05);
        header_mesh.material_index = Some(0);
        scene.meshes.push(header_mesh);
        
        // Card mesh with material 1
        let mut card_mesh = create_rounded_box_mesh(1.0, 1.0, 0.1, 0.05);
        card_mesh.material_index = Some(1);
        scene.meshes.push(card_mesh);
        
        // Sidebar mesh with material 2
        let mut sidebar_mesh = create_rounded_box_mesh(1.0, 1.0, 0.1, 0.05);
        sidebar_mesh.material_index = Some(2);
        scene.meshes.push(sidebar_mesh);
        
        // Button mesh with material 3
        let mut button_mesh = create_rounded_box_mesh(1.0, 1.0, 0.1, 0.05);
        button_mesh.material_index = Some(3);
        scene.meshes.push(button_mesh);
        
        // Add shadow material for depth
        scene.materials.push(Material3D {
            name: "shadow_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.0, 0.0, 0.0, 0.1),
                metallic: 0.0,
                roughness: 1.0,
                emissive: Color::new(0.0, 0.0, 0.0, 1.0),
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.1,
            double_sided: true,
        });
        
        // Add footer material
        scene.materials.push(Material3D {
            name: "footer_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.05, 0.05, 0.08, 0.98),
                metallic: 0.0,
                roughness: 0.95,
                emissive: Color::new(0.0, 0.0, 0.0, 1.0),
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.98,
            double_sided: true,
        });
        
        // Add connection line material
        scene.materials.push(Material3D {
            name: "connection_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.4, 0.3, 1.0, 0.3), // Semi-transparent purple
                metallic: 0.0,
                roughness: 1.0,
                emissive: Color::new(0.1, 0.05, 0.3, 1.0), // Slight glow
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.3,
            double_sided: true,
        });
        
        // Add docking zone material
        scene.materials.push(Material3D {
            name: "docking_zone_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.4, 0.5, 0.9, 0.25), // More visible blue
                metallic: 0.0,
                roughness: 1.0,
                emissive: Color::new(0.1, 0.15, 0.3, 1.0), // Add slight glow
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.25,
            double_sided: true,
        });
        
        // Add chart material
        scene.materials.push(Material3D {
            name: "chart_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.95, 0.95, 1.0, 0.95), // Light purple tint
                metallic: 0.0,
                roughness: 0.9,
                emissive: Color::new(0.02, 0.02, 0.05, 1.0), // Subtle glow
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.95,
            double_sided: true,
        });
        
        // Add chart bar material - for individual bars
        scene.materials.push(Material3D {
            name: "chart_bar_material".to_string(),
            material_type: MaterialType::PBR {
                base_color: Color::new(0.4, 0.3, 1.0, 0.9), // Purple bars
                metallic: 0.1,
                roughness: 0.7,
                emissive: Color::new(0.1, 0.05, 0.3, 1.0),
                base_color_texture: None,
                metallic_roughness_texture: None,
                normal_texture: None,
                emissive_texture: None,
            },
            opacity: 0.9,
            double_sided: true,
        });
        
        // Shadow mesh with material 4
        let mut shadow_mesh = create_rounded_box_mesh(1.0, 1.0, 0.02, 0.1);
        shadow_mesh.material_index = Some(4);
        scene.meshes.push(shadow_mesh);
        
        // Footer mesh with material 5
        let mut footer_mesh = create_rounded_box_mesh(1.0, 1.0, 0.1, 0.05);
        footer_mesh.material_index = Some(5);
        scene.meshes.push(footer_mesh);
        
        // Connection line mesh with material 6 - very thin box
        let mut line_mesh = create_rounded_box_mesh(1.0, 0.02, 0.01, 0.0); // Thicker line
        line_mesh.material_index = Some(6);
        scene.meshes.push(line_mesh);
        
        // Docking zone mesh with material 7
        let mut zone_mesh = create_rounded_box_mesh(1.0, 1.0, 0.01, 0.15);
        zone_mesh.material_index = Some(7);
        scene.meshes.push(zone_mesh);
        
        // Chart mesh with material 8
        let mut chart_mesh = create_rounded_box_mesh(1.0, 1.0, 0.1, 0.08);
        chart_mesh.material_index = Some(8);
        scene.meshes.push(chart_mesh);
        
        // Chart bar mesh with material 9
        let mut bar_mesh = create_rounded_box_mesh(1.0, 1.0, 0.05, 0.02);
        bar_mesh.material_index = Some(9);
        scene.meshes.push(bar_mesh);
        
        // Initialize components
        let mut components = Vec::new();
        
        // Header - Professional navigation bar
        let mut header = UIComponent::new(
            "header".to_string(),
            ComponentType::Header,
            Vec2::new(0.0, height / 2.0 - 50.0),
            Vec2::new(width - 60.0, 70.0),
            "FluentAI".to_string()
        );
        header.is_fixed = true;
        components.push(header);
        
        // Hero Section - Large feature card
        components.push(UIComponent::new(
            "hero".to_string(),
            ComponentType::Card,
            Vec2::new(0.0, 180.0),
            Vec2::new(600.0, 300.0),
            "Build Modern Web Experiences".to_string()
        ));
        
        // Sidebar - Navigation menu
        components.push(UIComponent::new(
            "sidebar".to_string(),
            ComponentType::Sidebar,
            Vec2::new(-width / 2.0 + 150.0, -50.0),
            Vec2::new(250.0, 400.0),
            "Dashboard".to_string()
        ));
        
        // Feature cards - Professional content sections
        let features = [
            ("Physics Engine", "Drag and drop UI components with realistic physics"),
            ("Responsive Design", "Automatically adapts to different screen sizes"),
            ("Modern Stack", "Built with Rust and WebAssembly for performance")
        ];
        
        for (i, (title, desc)) in features.iter().enumerate() {
            let x = -200.0 + i as f32 * 200.0;
            let y = -50.0;
            
            components.push(UIComponent::new(
                format!("feature_{}", i),
                ComponentType::Card,
                Vec2::new(x, y),
                Vec2::new(180.0, 220.0),
                title.to_string()
            ));
        }
        
        // Call-to-action buttons
        let buttons = ["Get Started", "Learn More", "View Demo"];
        for (i, label) in buttons.iter().enumerate() {
            components.push(UIComponent::new(
                format!("button_{}", i),
                ComponentType::Button,
                Vec2::new(-150.0 + i as f32 * 150.0, -250.0),
                Vec2::new(130.0, 45.0),
                label.to_string()
            ));
        }
        
        // Interactive chart component
        let mut chart = UIComponent::new(
            "analytics_chart".to_string(),
            ComponentType::Chart,
            Vec2::new(width / 2.0 - 200.0, 0.0),
            Vec2::new(350.0, 250.0),
            "Performance Analytics".to_string()
        );
        chart.chart_data = Some(ChartData {
            values: vec![75.0, 90.0, 65.0, 95.0, 80.0, 70.0],
            labels: vec!["Mon".to_string(), "Tue".to_string(), "Wed".to_string(), 
                        "Thu".to_string(), "Fri".to_string(), "Sat".to_string()],
            chart_type: ChartType::Bar,
        });
        components.push(chart);
        
        // Footer
        let mut footer = UIComponent::new(
            "footer".to_string(),
            ComponentType::Footer,
            Vec2::new(0.0, -height / 2.0 + 40.0),
            Vec2::new(width - 60.0, 60.0),
            "© 2024 FluentAI".to_string()
        );
        footer.is_fixed = true;
        components.push(footer);
        
        // Create connections between related components
        let mut connections = Vec::new();
        
        // Connect hero to feature cards
        connections.push(Connection {
            from_id: "hero".to_string(),
            to_id: "feature_0".to_string(),
            rest_length: 250.0,
            strength: 0.5,
            visible: true,
        });
        connections.push(Connection {
            from_id: "hero".to_string(),
            to_id: "feature_1".to_string(),
            rest_length: 250.0,
            strength: 0.5,
            visible: true,
        });
        connections.push(Connection {
            from_id: "hero".to_string(),
            to_id: "feature_2".to_string(),
            rest_length: 250.0,
            strength: 0.5,
            visible: true,
        });
        
        // Connect feature cards to each other
        connections.push(Connection {
            from_id: "feature_0".to_string(),
            to_id: "feature_1".to_string(),
            rest_length: 200.0,
            strength: 0.3,
            visible: true, // Hidden for cleaner look
        });
        connections.push(Connection {
            from_id: "feature_1".to_string(),
            to_id: "feature_2".to_string(),
            rest_length: 200.0,
            strength: 0.3,
            visible: true,
        });
        
        // Connect buttons to feature cards
        connections.push(Connection {
            from_id: "feature_0".to_string(),
            to_id: "button_0".to_string(),
            rest_length: 200.0,
            strength: 0.4,
            visible: true,
        });
        connections.push(Connection {
            from_id: "feature_1".to_string(),
            to_id: "button_1".to_string(),
            rest_length: 200.0,
            strength: 0.4,
            visible: true,
        });
        connections.push(Connection {
            from_id: "feature_2".to_string(),
            to_id: "button_2".to_string(),
            rest_length: 200.0,
            strength: 0.4,
            visible: true,
        });
        
        // Create docking zones
        let mut docking_zones = Vec::new();
        
        // Hero zone - center top
        docking_zones.push(DockingZone {
            position: Vec2::new(0.0, 180.0),
            size: Vec2::new(650.0, 350.0),
            strength: 200.0,
            occupied_by: Some("hero".to_string()),
            zone_type: DockingZoneType::HeroZone,
        });
        
        // Card slots - arranged in a row
        for i in 0..3 {
            let x = -200.0 + i as f32 * 200.0;
            docking_zones.push(DockingZone {
                position: Vec2::new(x, -50.0),
                size: Vec2::new(190.0, 240.0),
                strength: 150.0,
                occupied_by: Some(format!("feature_{}", i)),
                zone_type: DockingZoneType::CardSlot,
            });
        }
        
        // Button docks - below cards
        for i in 0..3 {
            let x = -150.0 + i as f32 * 150.0;
            docking_zones.push(DockingZone {
                position: Vec2::new(x, -250.0),
                size: Vec2::new(140.0, 50.0),
                strength: 100.0,
                occupied_by: Some(format!("button_{}", i)),
                zone_type: DockingZoneType::ButtonDock,
            });
        }
        
        // Sidebar dock
        docking_zones.push(DockingZone {
            position: Vec2::new(-width / 2.0 + 150.0, -50.0),
            size: Vec2::new(260.0, 420.0),
            strength: 120.0,
            occupied_by: Some("sidebar".to_string()),
            zone_type: DockingZoneType::SidebarDock,
        });
        
        let mut demo = PhysicsWebpageDemo {
            renderer,
            scene,
            camera,
            components,
            connections,
            docking_zones,
            mouse_pos: Vec2::ZERO,
            mouse_down: false,
            selected_component: None,
            canvas_width: width,
            canvas_height: height,
            time: 0.0,
            // Initial physics parameters - will be adapted based on screen size
            attraction_strength: 150.0,
            repulsion_strength: 200.0,
            damping: 0.65,
            edge_attraction: 100.0,
            grid_size: 20.0,
            snap_threshold: 30.0,
            hover_component: None,
            ambient_motion: 0.0,
            touches: Vec::new(),
            pinch_start_distance: None,
            pinch_center: None,
            grouped_components: Vec::new(),
            rotation_start_angle: None,
            // Performance monitoring
            last_frame_time: 0.0,
            frame_times: Vec::with_capacity(60), // Track last 60 frames
            performance_scale: 1.0,
            physics_substeps: 1,
            render_quality: RenderQuality::High,
            // Clustering
            clusters: Vec::new(),
            // Momentum tracking
            drag_history: Vec::with_capacity(10),
            momentum_samples: 5, // Number of samples to use for momentum calculation
            // Motion trails
            motion_trails: Vec::with_capacity(200), // Pre-allocate for performance
            trail_lifetime: 0.8, // Trails fade over 0.8 seconds
            trail_spawn_interval: 0.02, // Spawn new trail point every 20ms
            last_trail_spawn: 0.0,
        };
        
        // Adapt physics and component sizes to initial screen size
        demo.adapt_physics_to_screen_size();
        demo.adapt_component_sizes();
        
        // Initial performance detection
        demo.detect_initial_performance();
        
        // Initialize clusters
        demo.update_clusters();
        
        console::log_1(&format!("Physics webpage initialized for {}x{} viewport", width, height).into());
        
        Ok(demo)
    }
    
    pub fn handle_mouse_down(&mut self, x: f32, y: f32) {
        self.mouse_down = true;
        let mouse_world = self.screen_to_world(Vec2::new(x, y));
        
        // Clear drag history for new drag
        self.drag_history.clear();
        
        // Add initial drag point
        let window = web_sys::window().unwrap();
        let time = window.performance().unwrap().now();
        self.drag_history.push(DragPoint {
            position: mouse_world,
            time,
        });
        
        // Find which component was clicked (iterate in reverse for z-order)
        for (i, component) in self.components.iter_mut().enumerate().rev() {
            if component.contains_point(mouse_world) && !component.is_fixed {
                self.selected_component = Some(i);
                component.is_dragging = true;
                component.drag_offset = component.position - mouse_world;
                break;
            }
        }
    }
    
    pub fn handle_mouse_up(&mut self) {
        self.mouse_down = false;
        if let Some(idx) = self.selected_component {
            self.components[idx].is_dragging = false;
            
            // Check if component is near a compatible docking zone
            let component_id = self.components[idx].id.clone();
            let component_pos = self.components[idx].position;
            let component_type = self.components[idx].component_type.clone();
            
            // Find nearest compatible docking zone
            let mut nearest_zone: Option<usize> = None;
            let mut nearest_distance = f32::MAX;
            
            for (zone_idx, zone) in self.docking_zones.iter().enumerate() {
                // Check if zone is compatible with component type
                let compatible = match (&component_type, &zone.zone_type) {
                    (ComponentType::Card, DockingZoneType::CardSlot) => true,
                    (ComponentType::Button, DockingZoneType::ButtonDock) => true,
                    (ComponentType::Sidebar, DockingZoneType::SidebarDock) => true,
                    (ComponentType::Card, DockingZoneType::HeroZone) => component_id == "hero",
                    _ => false,
                };
                
                if compatible {
                    let distance = (zone.position - component_pos).length();
                    if distance < zone.size.length() * 0.5 && distance < nearest_distance {
                        nearest_zone = Some(zone_idx);
                        nearest_distance = distance;
                    }
                }
            }
            
            // Dock to nearest zone if found
            if let Some(zone_idx) = nearest_zone {
                // Clear old zone occupation
                for zone in &mut self.docking_zones {
                    if zone.occupied_by.as_ref() == Some(&component_id) {
                        zone.occupied_by = None;
                    }
                }
                
                // Occupy new zone
                self.docking_zones[zone_idx].occupied_by = Some(component_id);
                
                // Snap component to zone center with animation
                let target_pos = self.docking_zones[zone_idx].position;
                self.components[idx].velocity = (target_pos - component_pos) * 5.0;
            } else {
                // Calculate momentum from drag history
                let momentum = self.calculate_momentum();
                
                // Apply momentum with some damping for natural feel
                let momentum_scale = match self.render_quality {
                    RenderQuality::High => 0.8,
                    RenderQuality::Medium => 0.6,
                    RenderQuality::Low => 0.4,
                };
                
                self.components[idx].velocity = momentum * momentum_scale;
                
                // Log momentum for debugging
                if momentum.length() > 10.0 {
                    console::log_1(&format!("Applied momentum: {:.1}, {:.1}", momentum.x, momentum.y).into());
                }
            }
        }
        self.selected_component = None;
    }
    
    pub fn handle_mouse_move(&mut self, x: f32, y: f32) {
        self.mouse_pos = Vec2::new(x, y);
        let mouse_world = self.screen_to_world(self.mouse_pos);
        
        // Update hover state
        self.hover_component = None;
        for (i, component) in self.components.iter().enumerate().rev() {
            if component.contains_point(mouse_world) && !component.is_fixed {
                self.hover_component = Some(i);
                break;
            }
        }
        
        if let Some(idx) = self.selected_component {
            if self.components[idx].is_dragging {
                // Update position
                self.components[idx].position = mouse_world + self.components[idx].drag_offset;
                self.components[idx].velocity = Vec2::ZERO; // Stop physics while dragging
                
                // Track drag history for momentum
                let window = web_sys::window().unwrap();
                let time = window.performance().unwrap().now();
                
                // Add to drag history
                self.drag_history.push(DragPoint {
                    position: self.components[idx].position,
                    time,
                });
                
                // Keep only recent samples
                let max_samples = self.momentum_samples * 2;
                if self.drag_history.len() > max_samples {
                    self.drag_history.remove(0);
                }
                
                // Spawn motion trail points
                if time - self.last_trail_spawn > self.trail_spawn_interval as f64 * 1000.0 {
                    self.last_trail_spawn = time;
                    
                    // Only spawn trails in high/medium quality modes
                    if matches!(self.render_quality, RenderQuality::High | RenderQuality::Medium) {
                        let component = &self.components[idx];
                        self.motion_trails.push(TrailPoint {
                            position: component.position,
                            age: 0.0,
                            component_id: component.id.clone(),
                            color: component.color.clone(),
                        });
                    }
                }
            }
        }
    }
    
    // Calculate momentum from drag history
    fn calculate_momentum(&self) -> Vec2 {
        if self.drag_history.len() < 2 {
            return Vec2::ZERO;
        }
        
        // Use only recent samples for momentum calculation
        let samples_to_use = self.momentum_samples.min(self.drag_history.len());
        let start_idx = self.drag_history.len().saturating_sub(samples_to_use);
        
        // Calculate weighted average velocity
        let mut total_velocity = Vec2::ZERO;
        let mut total_weight = 0.0;
        
        for i in start_idx..(self.drag_history.len() - 1) {
            let p1 = &self.drag_history[i];
            let p2 = &self.drag_history[i + 1];
            
            let dt = ((p2.time - p1.time) / 1000.0) as f32; // Convert to seconds
            if dt > 0.0 && dt < 0.1 { // Ignore samples that are too far apart
                let velocity = (p2.position - p1.position) / dt;
                
                // Weight more recent samples higher
                let age = (self.drag_history.len() - 1 - i) as f32;
                let weight = 1.0 / (age + 1.0);
                
                total_velocity += velocity * weight;
                total_weight += weight;
            }
        }
        
        if total_weight > 0.0 {
            let avg_velocity = total_velocity / total_weight;
            
            // Cap maximum velocity to prevent components flying off screen
            let max_velocity = 800.0;
            if avg_velocity.length() > max_velocity {
                avg_velocity.normalize() * max_velocity
            } else {
                avg_velocity
            }
        } else {
            Vec2::ZERO
        }
    }
    
    // Update motion trails - age them and remove old ones
    fn update_motion_trails(&mut self, delta_time: f32) {
        // Update age and remove old trails
        self.motion_trails.retain_mut(|trail| {
            trail.age += delta_time;
            trail.age < self.trail_lifetime
        });
        
        // Limit total trail points for performance
        let max_trails = match self.render_quality {
            RenderQuality::High => 150,
            RenderQuality::Medium => 75,
            RenderQuality::Low => 0, // No trails in low quality
        };
        
        if self.motion_trails.len() > max_trails {
            // Remove oldest trails
            let to_remove = self.motion_trails.len() - max_trails;
            self.motion_trails.drain(0..to_remove);
        }
    }
    
    // Render motion trails as fading circles
    fn render_motion_trails(&mut self) {
        // Skip if no trails or in low quality mode
        if self.motion_trails.is_empty() || matches!(self.render_quality, RenderQuality::Low) {
            return;
        }
        
        // Create trail material if it doesn't exist
        if self.scene.materials.len() <= 10 {
            self.scene.materials.push(Material3D {
                name: "trail_material".to_string(),
                material_type: MaterialType::PBR {
                    base_color: Color::new(1.0, 1.0, 1.0, 0.5),
                    metallic: 0.0,
                    roughness: 1.0,
                    emissive: Color::new(0.1, 0.1, 0.15, 1.0),
                    base_color_texture: None,
                    metallic_roughness_texture: None,
                    normal_texture: None,
                    emissive_texture: None,
                },
                opacity: 0.5,
                double_sided: true,
            });
        }
        
        // Create trail mesh if it doesn't exist (small circle)
        if self.scene.meshes.len() <= 10 {
            let trail_mesh = Mesh3D::create_sphere(1.0, 8, 4); // Low-poly sphere for performance
            self.scene.meshes.push(trail_mesh);
        }
        
        // Render each trail point
        for (idx, trail) in self.motion_trails.iter().enumerate() {
            let mut trail_node = Node3D::new(format!("trail_{}", idx));
            
            // Calculate fade based on age
            let fade = 1.0 - (trail.age / self.trail_lifetime);
            let alpha = fade * fade; // Quadratic fade for smoother appearance
            
            // Position and scale
            let position = Vec3::new(trail.position.x, trail.position.y, -5.0); // Behind components
            let scale_factor = (5.0 + fade * 10.0) * match self.render_quality {
                RenderQuality::High => 1.0,
                RenderQuality::Medium => 0.7,
                _ => 0.0,
            };
            let scale = Vec3::splat(scale_factor);
            
            trail_node.transform = Mat4::from_scale_rotation_translation(
                scale,
                Quat::IDENTITY,
                position
            );
            
            trail_node.mesh_index = Some(10); // Trail mesh
            
            // Update material color to match component with fade
            if let Some(material) = self.scene.materials.get_mut(10) {
                if let MaterialType::PBR { ref mut base_color, ref mut emissive, .. } = material.material_type {
                    *base_color = Color::new(
                        trail.color.r,
                        trail.color.g,
                        trail.color.b,
                        trail.color.a * alpha * 0.3, // Semi-transparent
                    );
                    
                    // Slight glow effect
                    *emissive = Color::new(
                        trail.color.r * 0.1 * fade,
                        trail.color.g * 0.1 * fade,
                        trail.color.b * 0.1 * fade,
                        1.0
                    );
                }
            }
            
            self.scene.add_node(trail_node);
        }
    }
    
    // Adapt physics parameters based on screen size
    fn adapt_physics_to_screen_size(&mut self) {
        // Calculate reference scale based on viewport dimensions
        let reference_width = 1920.0_f32; // Reference desktop width
        let reference_height = 1080.0_f32; // Reference desktop height
        let diagonal_reference = (reference_width * reference_width + reference_height * reference_height).sqrt();
        let diagonal_current = (self.canvas_width * self.canvas_width + self.canvas_height * self.canvas_height).sqrt();
        let scale_factor = diagonal_current / diagonal_reference;
        
        // Detect if this is likely a mobile device
        let is_mobile = self.canvas_width < 768.0 || self.canvas_height < 768.0;
        let mobile_multiplier = if is_mobile { 0.7 } else { 1.0 };
        
        // Adapt physics forces based on screen size
        // Smaller screens need weaker forces to prevent components from flying around
        // Larger screens need stronger forces to maintain visible movement
        self.attraction_strength = 300.0 * scale_factor.clamp(0.5, 2.0) * mobile_multiplier;
        self.repulsion_strength = 5000.0 * scale_factor.clamp(0.5, 2.0) * mobile_multiplier;
        
        // Adapt edge attraction - stronger on smaller screens to keep components in view
        self.edge_attraction = 50.0 / scale_factor.clamp(0.5, 2.0);
        
        // Adapt damping - higher damping on smaller screens for stability
        self.damping = 0.85 + (1.0 - scale_factor.clamp(0.5, 1.5)) * 0.1;
        
        // Adapt grid and snap parameters
        self.grid_size = 10.0 * scale_factor.clamp(0.5, 1.5);
        self.snap_threshold = 20.0 * scale_factor.clamp(0.5, 1.5);
        
        // Adapt connection rest lengths
        for connection in &mut self.connections {
            let base_length = 250.0; // Base connection length
            connection.rest_length = base_length * scale_factor.clamp(0.5, 1.5);
        }
        
        // Adapt docking zone strengths based on screen size
        for zone in &mut self.docking_zones {
            // Stronger magnetic force on smaller screens
            zone.strength = 120.0 * scale_factor.clamp(0.7, 1.5);
        }
        
        console::log_1(&format!("Physics adapted for {}x{} (scale: {:.2})", 
            self.canvas_width, self.canvas_height, scale_factor).into());
    }
    
    // Adapt component sizes for responsive behavior
    fn adapt_component_sizes(&mut self) {
        let width_ratio = self.canvas_width / 1920.0;
        let height_ratio = self.canvas_height / 1080.0;
        let scale_factor = ((width_ratio + height_ratio) / 2.0).clamp(0.5, 1.5);
        
        // Define minimum sizes for different component types
        let min_sizes = |comp_type: &ComponentType| match comp_type {
            ComponentType::Header => Vec2::new(300.0, 50.0),
            ComponentType::Card => Vec2::new(120.0, 150.0),
            ComponentType::Sidebar => Vec2::new(180.0, 300.0),
            ComponentType::Button => Vec2::new(80.0, 35.0),
            ComponentType::TextBlock => Vec2::new(100.0, 60.0),
            ComponentType::Image => Vec2::new(100.0, 100.0),
            ComponentType::Footer => Vec2::new(300.0, 40.0),
            ComponentType::Chart => Vec2::new(200.0, 150.0),
        };
        
        // Define base sizes for different component types (at 1920x1080)
        let base_sizes = |comp_type: &ComponentType| match comp_type {
            ComponentType::Header => Vec2::new(1860.0, 70.0), // Full width minus margin
            ComponentType::Card => Vec2::new(180.0, 220.0),
            ComponentType::Sidebar => Vec2::new(250.0, 400.0),
            ComponentType::Button => Vec2::new(130.0, 45.0),
            ComponentType::TextBlock => Vec2::new(200.0, 100.0),
            ComponentType::Image => Vec2::new(150.0, 150.0),
            ComponentType::Footer => Vec2::new(1860.0, 60.0), // Full width minus margin
            ComponentType::Chart => Vec2::new(350.0, 250.0),
        };
        
        for component in &mut self.components {
            // Skip fixed-width components like header and footer for width scaling
            let is_full_width = matches!(component.component_type, ComponentType::Header | ComponentType::Footer);
            
            if is_full_width {
                // Header and footer stretch to canvas width
                component.size.x = self.canvas_width - 60.0;
                component.size.y = base_sizes(&component.component_type).y * scale_factor;
            } else {
                // Scale other components proportionally
                let base = base_sizes(&component.component_type);
                let min = min_sizes(&component.component_type);
                
                component.size = Vec2::new(
                    (base.x * scale_factor).max(min.x),
                    (base.y * scale_factor).max(min.y)
                );
            }
            
            // Adjust positions for fixed components
            match &component.component_type {
                ComponentType::Header => {
                    component.position.x = 0.0;
                    component.position.y = self.canvas_height / 2.0 - 50.0;
                }
                ComponentType::Footer => {
                    component.position.x = 0.0;
                    component.position.y = -self.canvas_height / 2.0 + 40.0;
                }
                _ => {
                    // For non-fixed components, ensure they stay within bounds
                    let half_width = self.canvas_width / 2.0;
                    let half_height = self.canvas_height / 2.0;
                    let margin = 50.0;
                    
                    component.position.x = component.position.x.clamp(
                        -half_width + component.size.x / 2.0 + margin,
                        half_width - component.size.x / 2.0 - margin
                    );
                    component.position.y = component.position.y.clamp(
                        -half_height + component.size.y / 2.0 + margin,
                        half_height - component.size.y / 2.0 - margin
                    );
                }
            }
        }
        
        console::log_1(&format!("Component sizes adapted (scale: {:.2})", scale_factor).into());
    }
    
    // Update component clusters for stronger cohesion
    fn update_clusters(&mut self) {
        self.clusters.clear();
        
        // Group components by type
        let mut type_groups: Vec<(ComponentType, Vec<usize>)> = Vec::new();
        
        for component_type in [
            ComponentType::Card,
            ComponentType::Button,
            ComponentType::TextBlock,
            ComponentType::Image,
        ].iter() {
            let indices: Vec<usize> = self.components.iter()
                .enumerate()
                .filter(|(_, c)| matches!(&c.component_type, t if t == component_type))
                .map(|(i, _)| i)
                .collect();
                
            if indices.len() >= 2 { // Only cluster if there are at least 2 components
                type_groups.push((component_type.clone(), indices));
            }
        }
        
        // Create clusters using proximity-based grouping
        for (component_type, indices) in type_groups {
            let mut unvisited: Vec<usize> = indices;
            
            while !unvisited.is_empty() {
                let mut cluster_indices = vec![unvisited.remove(0)];
                let cluster_threshold = 300.0; // Maximum distance to be in same cluster
                
                // Find all components within threshold distance
                let mut i = 0;
                while i < cluster_indices.len() {
                    let current_idx = cluster_indices[i];
                    let current_pos = self.components[current_idx].position;
                    
                    let mut j = 0;
                    while j < unvisited.len() {
                        let test_idx = unvisited[j];
                        let test_pos = self.components[test_idx].position;
                        
                        if (current_pos - test_pos).length() < cluster_threshold {
                            cluster_indices.push(unvisited.remove(j));
                        } else {
                            j += 1;
                        }
                    }
                    i += 1;
                }
                
                // Only create cluster if it has at least 2 members
                if cluster_indices.len() >= 2 {
                    // Calculate cluster center and radius
                    let mut center = Vec2::ZERO;
                    for &idx in &cluster_indices {
                        center += self.components[idx].position;
                    }
                    center /= cluster_indices.len() as f32;
                    
                    let mut max_radius = 0.0_f32;
                    for &idx in &cluster_indices {
                        let dist = (self.components[idx].position - center).length();
                        max_radius = max_radius.max(dist + self.components[idx].size.length() / 2.0);
                    }
                    
                    let member_ids: Vec<String> = cluster_indices.iter()
                        .map(|&idx| self.components[idx].id.clone())
                        .collect();
                    
                    self.clusters.push(ComponentCluster {
                        component_type: component_type.clone(),
                        member_ids,
                        center,
                        radius: max_radius,
                    });
                }
            }
        }
    }
    
    // Detect initial device performance capabilities
    fn detect_initial_performance(&mut self) {
        // Check for mobile or low-end device indicators
        let is_mobile = self.canvas_width < 768.0 || self.canvas_height < 768.0;
        
        // Get device pixel ratio as a performance indicator
        let window = web_sys::window().unwrap();
        let device_pixel_ratio = window.device_pixel_ratio();
        
        // Check for high resolution displays that might impact performance
        let high_res = device_pixel_ratio > 2.0;
        
        // Set initial quality based on device characteristics
        if is_mobile {
            self.render_quality = RenderQuality::Low;
            self.physics_substeps = 1;
            console::log_1(&"Mobile device detected - starting with Low quality".into());
        } else if high_res {
            self.render_quality = RenderQuality::Medium;
            self.physics_substeps = 1;
            console::log_1(&"High DPI display detected - starting with Medium quality".into());
        } else {
            self.render_quality = RenderQuality::High;
            self.physics_substeps = 2;
            console::log_1(&"Desktop device detected - starting with High quality".into());
        }
    }
    
    // Monitor and adapt performance based on frame times
    fn update_performance_metrics(&mut self, timestamp: f64) {
        // Calculate frame time
        let frame_time = if self.last_frame_time > 0.0 {
            timestamp - self.last_frame_time
        } else {
            16.0 // Initial assumption of 60 FPS
        };
        
        self.last_frame_time = timestamp;
        
        // Track frame times (keep last 60 frames)
        self.frame_times.push(frame_time);
        if self.frame_times.len() > 60 {
            self.frame_times.remove(0);
        }
        
        // Only adapt after collecting enough samples
        if self.frame_times.len() >= 30 {
            // Calculate average frame time
            let avg_frame_time = self.frame_times.iter().sum::<f64>() / self.frame_times.len() as f64;
            
            // Target 60 FPS (16.67ms) with thresholds
            match self.render_quality {
                RenderQuality::High => {
                    if avg_frame_time > 25.0 { // Below 40 FPS
                        self.downgrade_quality();
                        console::log_1(&format!("Performance degraded to Medium (avg frame time: {:.1}ms)", avg_frame_time).into());
                    }
                }
                RenderQuality::Medium => {
                    if avg_frame_time > 33.0 { // Below 30 FPS
                        self.downgrade_quality();
                        console::log_1(&format!("Performance degraded to Low (avg frame time: {:.1}ms)", avg_frame_time).into());
                    } else if avg_frame_time < 14.0 { // Above 70 FPS for 5 seconds
                        self.upgrade_quality();
                        console::log_1(&format!("Performance upgraded to High (avg frame time: {:.1}ms)", avg_frame_time).into());
                    }
                }
                RenderQuality::Low => {
                    if avg_frame_time < 14.0 { // Above 70 FPS
                        self.upgrade_quality();
                        console::log_1(&format!("Performance upgraded to Medium (avg frame time: {:.1}ms)", avg_frame_time).into());
                    }
                }
            }
        }
    }
    
    // Downgrade rendering quality for better performance
    fn downgrade_quality(&mut self) {
        match self.render_quality {
            RenderQuality::High => {
                self.render_quality = RenderQuality::Medium;
                self.physics_substeps = 1;
                self.performance_scale = 0.8;
            }
            RenderQuality::Medium => {
                self.render_quality = RenderQuality::Low;
                self.physics_substeps = 1;
                self.performance_scale = 0.6;
                // Reduce number of connections shown
                for connection in &mut self.connections {
                    connection.visible = false;
                }
            }
            RenderQuality::Low => {
                // Already at lowest quality
                self.performance_scale = 0.5;
            }
        }
        
        // Clear frame time history to avoid rapid switching
        self.frame_times.clear();
    }
    
    // Upgrade rendering quality when performance allows
    fn upgrade_quality(&mut self) {
        match self.render_quality {
            RenderQuality::Low => {
                self.render_quality = RenderQuality::Medium;
                self.physics_substeps = 1;
                self.performance_scale = 0.8;
                // Re-enable some connections
                for (i, connection) in self.connections.iter_mut().enumerate() {
                    if i % 2 == 0 { // Show every other connection
                        connection.visible = true;
                    }
                }
            }
            RenderQuality::Medium => {
                self.render_quality = RenderQuality::High;
                self.physics_substeps = 2;
                self.performance_scale = 1.0;
                // Re-enable all connections
                for connection in &mut self.connections {
                    connection.visible = true;
                }
            }
            RenderQuality::High => {
                // Already at highest quality
                self.performance_scale = 1.0;
            }
        }
        
        // Clear frame time history to avoid rapid switching
        self.frame_times.clear();
    }
    
    pub fn update_canvas_size(&mut self, width: f32, height: f32) {
        self.canvas_width = width;
        self.canvas_height = height;
        self.camera.aspect_ratio = width / height;
        
        // Update camera projection
        if let CameraType::Orthographic { .. } = self.camera.projection {
            self.camera.projection = CameraType::Orthographic {
                left: -width / 2.0,
                right: width / 2.0,
                bottom: -height / 2.0,
                top: height / 2.0,
                near: 0.1,
                far: 100.0,
            };
        }
        
        self.renderer.set_viewport(width as i32, height as i32);
        
        // Adapt physics parameters based on screen size
        self.adapt_physics_to_screen_size();
        
        // Adapt component sizes for better responsive behavior
        self.adapt_component_sizes();
    }
    
    pub fn render(&mut self, timestamp: f64) -> Result<(), JsValue> {
        // Update performance metrics
        self.update_performance_metrics(timestamp);
        
        let delta_time = 0.016; // Assume 60 FPS
        self.time = (timestamp * 0.001) as f32;
        
        // Debug log first frame
        if self.time < 0.1 {
            console::log_1(&format!("Rendering at time: {}, components: {}", self.time, self.components.len()).into());
        }
        
        // Update physics with substeps for better stability
        let substep_dt = delta_time / self.physics_substeps as f32;
        for _ in 0..self.physics_substeps {
            self.update_physics(substep_dt);
        }
        
        // Update clusters periodically (every 30 frames)
        if self.time as u32 % 30 == 0 {
            self.update_clusters();
        }
        
        // Clear scene
        self.scene.nodes.clear();
        self.scene.roots.clear();
        
        // Update scene camera
        self.scene.camera = self.camera.clone();
        
        // Update and render motion trails
        self.update_motion_trails(delta_time);
        self.render_motion_trails();
        
        // Update ambient motion (reduced for low quality)
        let motion_scale = match self.render_quality {
            RenderQuality::High => 1.0,
            RenderQuality::Medium => 0.5,
            RenderQuality::Low => 0.0,
        };
        self.ambient_motion = (self.ambient_motion + delta_time * 0.5 * motion_scale) % (2.0 * PI);
        
        // Render components
        for (i, component) in self.components.iter().enumerate() {
            let is_hovered = self.hover_component == Some(i);
            let is_dragging = component.is_dragging;
            
            // Base transform
            let mut node = Node3D::new(component.id.clone());
            
            // Much reduced floating motion - only slight hover effect
            let float_offset = if !component.is_fixed && !is_dragging && is_hovered {
                sin_approx(self.time * 2.0) * 0.5 // Very subtle hover float
            } else {
                0.0
            };
            
            // Position in 3D space (convert 2D position to 3D)
            let position = Vec3::new(
                component.position.x,
                component.position.y + float_offset,
                component.depth
            );
            
            // Add rotation effects
            let rotation = if is_dragging {
                Quat::from_rotation_z(sin_approx(self.time * 3.0) * 0.05)
            } else if is_hovered {
                Quat::from_rotation_z(sin_approx(self.time * 2.0) * 0.02) *
                Quat::from_rotation_x(cos_approx(self.time * 1.5) * 0.01)
            } else {
                Quat::from_rotation_y(sin_approx(self.ambient_motion + i as f32) * 0.01)
            };
            
            // Scale based on component size with hover effect
            let hover_scale = if is_hovered { 1.05 } else { 1.0 };
            let drag_scale = if is_dragging { 1.1 } else { 1.0 };
            let scale = Vec3::new(
                component.size.x * hover_scale * drag_scale,
                component.size.y * hover_scale * drag_scale,
                1.0
            );
            
            node.transform = Mat4::from_scale_rotation_translation(scale, rotation, position);
            
            // Set mesh based on component type
            // Each mesh index corresponds to a mesh with the appropriate material
            let mesh_idx = match component.component_type {
                ComponentType::Header => 0,      // Header mesh with material 0
                ComponentType::Card => 1,        // Card mesh with material 1
                ComponentType::Sidebar => 2,     // Sidebar mesh with material 2
                ComponentType::Button => 3,      // Button mesh with material 3
                ComponentType::Footer => 5,      // Footer mesh with material 5
                ComponentType::Chart => 8,       // Chart mesh with material 8
                _ => 1, // Default to card mesh
            };
            
            node.mesh_index = Some(mesh_idx);
            
            // Update material with current color and effects
            let material_idx = mesh_idx; // Material index matches mesh index
            if let Some(material) = self.scene.materials.get_mut(material_idx) {
                if let MaterialType::PBR { ref mut base_color, ref mut emissive, .. } = material.material_type {
                    *base_color = component.color;
                    
                    // Add glow for hovered/dragged items
                    if is_hovered || is_dragging {
                        let glow_intensity = if is_dragging { 0.3 } else { 0.1 };
                        *emissive = Color::new(
                            component.color.r * glow_intensity,
                            component.color.g * glow_intensity,
                            component.color.b * glow_intensity,
                            1.0
                        );
                    } else {
                        *emissive = Color::new(0.0, 0.0, 0.0, 1.0);
                    }
                }
            }
            
            // Add shadow for cards and buttons for depth (skip in low quality)
            if matches!(self.render_quality, RenderQuality::High | RenderQuality::Medium) &&
               matches!(component.component_type, ComponentType::Card | ComponentType::Button) && 
               !is_dragging {
                let mut shadow_node = Node3D::new(format!("{}_shadow", component.id));
                let shadow_offset = if is_hovered { 8.0 } else { 4.0 };
                let shadow_position = Vec3::new(
                    component.position.x + shadow_offset * 0.2,
                    component.position.y - shadow_offset,
                    component.depth - 0.1
                );
                let shadow_scale = Vec3::new(
                    component.size.x * 1.02,
                    component.size.y * 1.02,
                    1.0
                );
                shadow_node.transform = Mat4::from_scale_rotation_translation(
                    shadow_scale,
                    Quat::IDENTITY,
                    shadow_position
                );
                shadow_node.mesh_index = Some(4); // Shadow mesh
                self.scene.add_node(shadow_node);
            }
            
            // Add the main component node
            self.scene.add_node(node);
            
            // Render chart bars if this is a chart component (skip animation in low quality)
            if matches!(component.component_type, ComponentType::Chart) {
                if let Some(chart_data) = &component.chart_data {
                    let bar_count = chart_data.values.len();
                    let bar_width = component.size.x / (bar_count as f32 * 1.5);
                    let max_height = component.size.y * 0.8;
                    let max_value = chart_data.values.iter().cloned().fold(0.0, f32::max);
                    
                    // Render fewer bars in low quality mode
                    let bar_step = match self.render_quality {
                        RenderQuality::Low => 2,  // Skip every other bar
                        _ => 1,
                    };
                    
                    for (bar_idx, &value) in chart_data.values.iter().enumerate().step_by(bar_step) {
                        let bar_height = (value / max_value) * max_height;
                        let bar_x = component.position.x - component.size.x / 2.0 + 
                                   bar_width * 0.75 + (bar_idx as f32) * bar_width * 1.5;
                        let bar_y = component.position.y - component.size.y / 2.0 + bar_height / 2.0 + 20.0;
                        
                        let mut bar_node = Node3D::new(format!("{}_bar_{}", component.id, bar_idx));
                        
                        // Physics-reactive bars - they wiggle based on velocity and time
                        let wiggle = if !component.is_fixed && matches!(self.render_quality, RenderQuality::High) {
                            let vel_factor = component.velocity.length() * 0.01;
                            let time_offset = self.time + bar_idx as f32 * 0.5;
                            sin_approx(time_offset * 3.0) * vel_factor +
                            sin_approx(time_offset * 5.0 + bar_idx as f32) * 0.002
                        } else {
                            0.0
                        };
                        
                        let bar_position = Vec3::new(
                            bar_x + wiggle * 5.0,
                            bar_y + wiggle * 3.0,
                            component.depth + 0.05
                        );
                        
                        // Scale bars with hover effect
                        let hover_scale = if is_hovered {
                            1.0 + sin_approx(self.time * 4.0 + bar_idx as f32) * 0.05
                        } else {
                            1.0
                        };
                        
                        let bar_scale = Vec3::new(
                            bar_width * 0.8 * hover_scale,
                            bar_height * hover_scale,
                            0.05
                        );
                        
                        bar_node.transform = Mat4::from_scale_rotation_translation(
                            bar_scale,
                            Quat::IDENTITY,
                            bar_position
                        );
                        
                        bar_node.mesh_index = Some(9); // Bar mesh
                        self.scene.add_node(bar_node);
                    }
                }
            }
        }
        
        // Render visible connections as lines
        for connection in &self.connections {
            if !connection.visible {
                continue;
            }
            
            // Find component positions
            let from_pos = self.components.iter()
                .find(|c| c.id == connection.from_id)
                .map(|c| c.position);
            let to_pos = self.components.iter()
                .find(|c| c.id == connection.to_id)
                .map(|c| c.position);
                
            if let (Some(from), Some(to)) = (from_pos, to_pos) {
                // Create a line mesh between components
                let diff = to - from;
                let distance = diff.length();
                let angle = diff.y.atan2(diff.x);
                
                let mut line_node = Node3D::new(format!("connection_{}_{}", connection.from_id, connection.to_id));
                
                // Position at midpoint
                let midpoint = Vec3::new(
                    (from.x + to.x) / 2.0,
                    (from.y + to.y) / 2.0,
                    -0.5  // Behind components
                );
                
                // Scale to match distance
                let scale = Vec3::new(distance, 2.0, 1.0);
                
                // Rotate to align with connection
                let rotation = Quat::from_rotation_z(angle);
                
                line_node.transform = Mat4::from_scale_rotation_translation(scale, rotation, midpoint);
                
                // Use the connection line mesh
                line_node.mesh_index = Some(6); // Connection line mesh
                
                self.scene.add_node(line_node);
            }
        }
        
        // Render cluster boundaries in debug mode (high quality only)
        if matches!(self.render_quality, RenderQuality::High) && self.clusters.len() > 0 {
            for (cluster_idx, cluster) in self.clusters.iter().enumerate() {
                // Create a subtle circle to show cluster boundary
                let segments = 32;
                for i in 0..segments {
                    let angle1 = (i as f32 / segments as f32) * 2.0 * PI;
                    let angle2 = ((i + 1) as f32 / segments as f32) * 2.0 * PI;
                    
                    let p1 = cluster.center + Vec2::new(cos_approx(angle1), sin_approx(angle1)) * cluster.radius;
                    let p2 = cluster.center + Vec2::new(cos_approx(angle2), sin_approx(angle2)) * cluster.radius;
                    
                    // Create thin line segment
                    let mut line_node = Node3D::new(format!("cluster_{}_seg_{}", cluster_idx, i));
                    
                    let mid = (p1 + p2) / 2.0;
                    let diff = p2 - p1;
                    let length = diff.length();
                    let angle = diff.y.atan2(diff.x);
                    
                    let position = Vec3::new(mid.x, mid.y, -2.0); // Behind everything
                    let scale = Vec3::new(length, 1.0, 1.0);
                    let rotation = Quat::from_rotation_z(angle);
                    
                    line_node.transform = Mat4::from_scale_rotation_translation(scale, rotation, position);
                    line_node.mesh_index = Some(6); // Connection line mesh
                    
                    // Make cluster lines very subtle
                    if let Some(material) = self.scene.materials.get_mut(6) {
                        if let MaterialType::PBR { ref mut base_color, .. } = material.material_type {
                            *base_color = Color::new(0.5, 0.5, 0.6, 0.1); // Very transparent
                        }
                    }
                    
                    self.scene.add_node(line_node);
                }
            }
        }
        
        // Render docking zones as semi-transparent areas
        for (zone_idx, zone) in self.docking_zones.iter().enumerate() {
            let mut zone_node = Node3D::new(format!("docking_zone_{}", zone_idx));
            
            // Position at zone center
            let position = Vec3::new(zone.position.x, zone.position.y, -1.0); // Behind components
            
            // Scale to zone size
            let scale = Vec3::new(zone.size.x, zone.size.y, 1.0);
            
            // Subtle pulsing effect for empty zones
            let pulse = if zone.occupied_by.is_none() {
                1.0 + sin_approx(self.time * 2.0) * 0.02
            } else {
                1.0
            };
            
            zone_node.transform = Mat4::from_scale_rotation_translation(
                scale * pulse,
                Quat::IDENTITY,
                position
            );
            
            // Use docking zone mesh
            zone_node.mesh_index = Some(7); // Docking zone mesh
            
            self.scene.add_node(zone_node);
        }
        
        // Debug log scene info
        if self.time < 0.1 {
            console::log_1(&format!("Scene has {} nodes, {} roots, {} meshes, {} materials", 
                self.scene.nodes.len(), 
                self.scene.roots.len(),
                self.scene.meshes.len(),
                self.scene.materials.len()
            ).into());
        }
        
        // Render the scene
        self.renderer.render_scene(&self.scene)?;
        
        Ok(())
    }
    
    // Physics update
    fn update_physics(&mut self, delta_time: f32) {
        let component_count = self.components.len();
        
        // Skip expensive calculations in low quality mode
        let calculate_repulsion = matches!(self.render_quality, RenderQuality::High | RenderQuality::Medium);
        let calculate_all_attractions = matches!(self.render_quality, RenderQuality::High);
        
        // Calculate forces between components
        for i in 0..component_count {
            if self.components[i].is_dragging || self.components[i].is_fixed {
                continue;
            }
            
            let mut force = Vec2::ZERO;
            let pos_i = self.components[i].position;
            let size_i = self.components[i].size;
            
            // Attraction/repulsion with other components
            for j in 0..component_count {
                if i == j {
                    continue;
                }
                
                let pos_j = self.components[j].position;
                let size_j = self.components[j].size;
                
                let diff = pos_j - pos_i;
                let distance = diff.length();
                
                if distance > 0.01 {
                    let direction = diff / distance;
                    
                    // Calculate ideal distance based on component sizes
                    let ideal_distance = (size_i.length() + size_j.length()) * 0.6;
                    
                    // Attraction force (pulls components together)
                    if calculate_all_attractions && distance > ideal_distance {
                        let attraction = self.attraction_strength * (distance - ideal_distance) / distance * self.performance_scale;
                        force += direction * attraction;
                    }
                    
                    // Repulsion force (prevents overlap)
                    if calculate_repulsion && distance < ideal_distance {
                        let repulsion = self.repulsion_strength * (ideal_distance - distance) / distance * self.performance_scale;
                        force -= direction * repulsion;
                    }
                    
                    // Special case: components of same type attract more
                    let same_type = match (&self.components[i].component_type, &self.components[j].component_type) {
                        (ComponentType::Card, ComponentType::Card) => true,
                        (ComponentType::Button, ComponentType::Button) => true,
                        (ComponentType::Header, ComponentType::Header) => true,
                        (ComponentType::Sidebar, ComponentType::Sidebar) => true,
                        (ComponentType::TextBlock, ComponentType::TextBlock) => true,
                        (ComponentType::Image, ComponentType::Image) => true,
                        _ => false,
                    };
                    if same_type && calculate_all_attractions {
                        // Stronger attraction for same-type components
                        let type_attraction = 30.0 * self.performance_scale;
                        force += direction * type_attraction;
                    }
                }
            }
            
            // Spring forces from connections
            for connection in &self.connections {
                // Find component indices
                let from_idx = self.components.iter().position(|c| c.id == connection.from_id);
                let to_idx = self.components.iter().position(|c| c.id == connection.to_id);
                
                if let (Some(from_i), Some(to_i)) = (from_idx, to_idx) {
                    if from_i == i || to_i == i {
                        let other_idx = if from_i == i { to_i } else { from_i };
                        let other_pos = self.components[other_idx].position;
                        
                        let diff = other_pos - pos_i;
                        let distance = diff.length();
                        
                        if distance > 0.01 {
                            let direction = diff / distance;
                            let spring_force = connection.strength * (distance - connection.rest_length);
                            force += direction * spring_force;
                        }
                    }
                }
            }
            
            // Magnetic docking zone forces
            let component_id = &self.components[i].id;
            let component_type = &self.components[i].component_type;
            
            for zone in &self.docking_zones {
                // Check if this zone already contains this component
                let is_occupant = zone.occupied_by.as_ref() == Some(component_id);
                
                // Check if zone is compatible
                let compatible = match (component_type, &zone.zone_type) {
                    (ComponentType::Card, DockingZoneType::CardSlot) => true,
                    (ComponentType::Button, DockingZoneType::ButtonDock) => true,
                    (ComponentType::Sidebar, DockingZoneType::SidebarDock) => true,
                    (ComponentType::Card, DockingZoneType::HeroZone) => component_id == "hero",
                    _ => false,
                };
                
                if compatible || is_occupant {
                    let diff = zone.position - pos_i;
                    let distance = diff.length();
                    
                    // Strong magnetic attraction when close to empty zone or when occupant
                    if (zone.occupied_by.is_none() || is_occupant) && distance > 0.01 {
                        let direction = diff / distance;
                        
                        // Exponential falloff for magnetic force
                        let magnetic_range = zone.size.length() * 0.8;
                        if distance < magnetic_range {
                            let strength_multiplier = if is_occupant { 3.0 } else { 1.0 };
                            let magnetic_force = zone.strength * strength_multiplier * 
                                                 (1.0 - distance / magnetic_range).powf(2.0);
                            force += direction * magnetic_force;
                        }
                    }
                }
            }
            
            // Cluster cohesion forces (skip in low quality)
            if matches!(self.render_quality, RenderQuality::High | RenderQuality::Medium) {
                let component_id = &self.components[i].id;
                
                for cluster in &self.clusters {
                    if cluster.member_ids.contains(component_id) {
                        // Pull towards cluster center
                        let diff = cluster.center - pos_i;
                        let distance = diff.length();
                        
                        if distance > 0.01 {
                            let direction = diff / distance;
                            
                            // Cohesion force increases with distance from center
                            let cohesion_strength = 40.0 * self.performance_scale;
                            let normalized_distance = (distance / cluster.radius).min(1.0);
                            let cohesion_force = cohesion_strength * normalized_distance;
                            
                            force += direction * cohesion_force;
                        }
                        
                        // Additional force to maintain cluster shape
                        // Push away from cluster boundary if too close to edge
                        let distance_from_center = (pos_i - cluster.center).length();
                        let component_radius = self.components[i].size.length() / 2.0;
                        let boundary_distance = cluster.radius - distance_from_center - component_radius;
                        
                        if boundary_distance < 20.0 && distance_from_center > 0.01 {
                            let inward_direction = (cluster.center - pos_i) / distance_from_center;
                            let boundary_force = 30.0 * (1.0 - boundary_distance / 20.0);
                            force += inward_direction * boundary_force;
                        }
                        
                        break; // Component can only be in one cluster
                    }
                }
            }
            
            // Edge alignment forces
            for j in 0..component_count {
                if i == j {
                    continue;
                }
                
                let pos_j = self.components[j].position;
                
                let (left_i, right_i, top_i, bottom_i) = self.components[i].get_edges();
                let (left_j, right_j, top_j, bottom_j) = self.components[j].get_edges();
                
                // Horizontal alignment - much stronger forces for snapping
                let y_diff = if pos_i.y > pos_j.y { pos_i.y - pos_j.y } else { pos_j.y - pos_i.y };
                if y_diff < 150.0 { // Larger alignment range
                    // Right edge to left edge alignment
                    let gap = 15.0; // Desired gap between components
                    let right_to_left = left_j - right_i;
                    if right_to_left > 0.0 && right_to_left < 100.0 {
                        // Strong snapping force when close
                        let snap_force = if right_to_left < 50.0 { 3.0 } else { 1.0 };
                        force.x += self.edge_attraction * snap_force * (gap - right_to_left);
                    }
                    
                    // Left edge to right edge alignment
                    let left_to_right = left_i - right_j;
                    if left_to_right > 0.0 && left_to_right < 100.0 {
                        let snap_force = if left_to_right < 50.0 { 3.0 } else { 1.0 };
                        force.x -= self.edge_attraction * snap_force * (gap - left_to_right);
                    }
                    
                    // Align tops and bottoms when horizontally aligned
                    if (right_to_left > 0.0 && right_to_left < 50.0) || (left_to_right > 0.0 && left_to_right < 50.0) {
                        // Align tops
                        let top_diff = top_i - top_j;
                        if top_diff.abs() < 30.0 {
                            force.y -= top_diff * self.edge_attraction * 2.0;
                        }
                    }
                }
                
                // Vertical alignment - stronger snapping
                let x_diff = if pos_i.x > pos_j.x { pos_i.x - pos_j.x } else { pos_j.x - pos_i.x };
                if x_diff < 150.0 { // Larger alignment range
                    // Bottom edge to top edge alignment
                    let gap = 15.0;
                    let bottom_to_top = top_j - bottom_i;
                    if bottom_to_top > 0.0 && bottom_to_top < 100.0 {
                        let snap_force = if bottom_to_top < 50.0 { 3.0 } else { 1.0 };
                        force.y += self.edge_attraction * snap_force * (gap - bottom_to_top);
                    }
                    
                    // Top edge to bottom edge alignment
                    let top_to_bottom = top_i - bottom_j;
                    if top_to_bottom > 0.0 && top_to_bottom < 100.0 {
                        let snap_force = if top_to_bottom < 50.0 { 3.0 } else { 1.0 };
                        force.y -= self.edge_attraction * snap_force * (gap - top_to_bottom);
                    }
                    
                    // Align left and right edges when vertically aligned
                    if (bottom_to_top > 0.0 && bottom_to_top < 50.0) || (top_to_bottom > 0.0 && top_to_bottom < 50.0) {
                        // Align left edges
                        let left_diff = left_i - left_j;
                        if left_diff.abs() < 30.0 {
                            force.x -= left_diff * self.edge_attraction * 2.0;
                        }
                    }
                }
            }
            
            // Apply force to velocity
            let mass = self.components[i].mass;
            self.components[i].velocity += force / mass * delta_time;
        }
        
        // Update positions and apply damping
        for component in &mut self.components {
            if !component.is_dragging && !component.is_fixed {
                component.position += component.velocity * delta_time;
                component.velocity *= self.damping;
                
                // Grid snapping when velocity is low
                let speed = component.velocity.length();
                if speed < 50.0 { // When moving slowly, snap to grid
                    // Snap X position
                    let grid_x = (component.position.x / self.grid_size).round() * self.grid_size;
                    let diff_x = grid_x - component.position.x;
                    if diff_x.abs() < self.snap_threshold {
                        component.position.x += diff_x * 0.2; // Smooth snapping
                        if diff_x.abs() < 2.0 {
                            component.position.x = grid_x; // Final snap
                            component.velocity.x *= 0.5; // Reduce velocity
                        }
                    }
                    
                    // Snap Y position
                    let grid_y = (component.position.y / self.grid_size).round() * self.grid_size;
                    let diff_y = grid_y - component.position.y;
                    if diff_y.abs() < self.snap_threshold {
                        component.position.y += diff_y * 0.2; // Smooth snapping
                        if diff_y.abs() < 2.0 {
                            component.position.y = grid_y; // Final snap
                            component.velocity.y *= 0.5; // Reduce velocity
                        }
                    }
                }
                
                // Keep components on screen
                let half_width = self.canvas_width / 2.0;
                let half_height = self.canvas_height / 2.0;
                let margin = 20.0;
                
                let min_x = -half_width + component.size.x / 2.0 + margin;
                let max_x = half_width - component.size.x / 2.0 - margin;
                if component.position.x < min_x {
                    component.position.x = min_x;
                } else if component.position.x > max_x {
                    component.position.x = max_x;
                }
                
                let min_y = -half_height + component.size.y / 2.0 + margin;
                let max_y = half_height - component.size.y / 2.0 - margin;
                if component.position.y < min_y {
                    component.position.y = min_y;
                } else if component.position.y > max_y {
                    component.position.y = max_y;
                }
            }
        }
    }
    
    fn screen_to_world(&self, screen_pos: Vec2) -> Vec2 {
        Vec2::new(
            screen_pos.x - self.canvas_width / 2.0,
            self.canvas_height / 2.0 - screen_pos.y
        )
    }
    
    // Multi-touch handling
    pub fn handle_touch_start(&mut self, touch_id: i32, x: f32, y: f32) {
        let touch_world = self.screen_to_world(Vec2::new(x, y));
        
        // Find which component was touched
        let mut touched_component = None;
        for (i, component) in self.components.iter().enumerate().rev() {
            if component.contains_point(touch_world) && !component.is_fixed {
                touched_component = Some(i);
                break;
            }
        }
        
        let touch_info = TouchInfo {
            id: touch_id,
            position: touch_world,
            start_position: touch_world,
            component_id: touched_component,
        };
        
        self.touches.push(touch_info);
        
        // Handle pinch gesture initialization
        if self.touches.len() == 2 {
            let pos1 = self.touches[0].position;
            let pos2 = self.touches[1].position;
            self.pinch_start_distance = Some((pos2 - pos1).length());
            self.pinch_center = Some((pos1 + pos2) * 0.5);
            
            // Find components near the pinch center to group
            self.grouped_components.clear();
            let pinch_radius = 200.0;
            for (i, component) in self.components.iter().enumerate() {
                if !component.is_fixed {
                    let dist = (component.position - self.pinch_center.unwrap()).length();
                    if dist < pinch_radius {
                        self.grouped_components.push(i);
                    }
                }
            }
            
            // Calculate initial rotation angle
            let diff = pos2 - pos1;
            self.rotation_start_angle = Some(diff.y.atan2(diff.x));
        }
    }
    
    pub fn handle_touch_move(&mut self, touch_id: i32, x: f32, y: f32) {
        let touch_world = self.screen_to_world(Vec2::new(x, y));
        
        // Update touch position
        if let Some(touch) = self.touches.iter_mut().find(|t| t.id == touch_id) {
            touch.position = touch_world;
        }
        
        // Handle single touch drag
        if self.touches.len() == 1 {
            if let Some(touch) = self.touches.first() {
                if let Some(comp_idx) = touch.component_id {
                    if comp_idx < self.components.len() {
                        let offset = touch_world - touch.start_position;
                        self.components[comp_idx].position = self.components[comp_idx].position + offset;
                        self.components[comp_idx].velocity = Vec2::ZERO;
                        self.components[comp_idx].is_dragging = true;
                        
                        // Track drag history for momentum
                        let window = web_sys::window().unwrap();
                        let time = window.performance().unwrap().now();
                        
                        self.drag_history.push(DragPoint {
                            position: self.components[comp_idx].position,
                            time,
                        });
                        
                        // Keep only recent samples
                        let max_samples = self.momentum_samples * 2;
                        if self.drag_history.len() > max_samples {
                            self.drag_history.remove(0);
                        }
                        
                        // Spawn motion trail points for touch drag
                        if time - self.last_trail_spawn > self.trail_spawn_interval as f64 * 1000.0 {
                            self.last_trail_spawn = time;
                            
                            // Only spawn trails in high/medium quality modes
                            if matches!(self.render_quality, RenderQuality::High | RenderQuality::Medium) {
                                let component = &self.components[comp_idx];
                                self.motion_trails.push(TrailPoint {
                                    position: component.position,
                                    age: 0.0,
                                    component_id: component.id.clone(),
                                    color: component.color.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }
        // Handle pinch and rotate gestures
        else if self.touches.len() == 2 {
            let pos1 = self.touches[0].position;
            let pos2 = self.touches[1].position;
            let current_center = (pos1 + pos2) * 0.5;
            let current_distance = (pos2 - pos1).length();
            
            if let (Some(start_dist), Some(start_center)) = (self.pinch_start_distance, self.pinch_center) {
                // Calculate scale factor
                let scale_factor = current_distance / start_dist;
                
                // Calculate rotation
                let current_angle = (pos2 - pos1).y.atan2((pos2 - pos1).x);
                let angle_delta = if let Some(start_angle) = self.rotation_start_angle {
                    current_angle - start_angle
                } else {
                    0.0
                };
                
                // Apply transformations to grouped components
                for &comp_idx in &self.grouped_components {
                    if comp_idx < self.components.len() {
                        let comp = &mut self.components[comp_idx];
                        
                        // Translate to origin, scale, rotate, translate back
                        let relative_pos = comp.position - start_center;
                        
                        // Apply scale
                        let scaled_pos = relative_pos * scale_factor;
                        
                        // Apply rotation
                        let cos_a = cos_approx(angle_delta);
                        let sin_a = sin_approx(angle_delta);
                        let rotated_pos = Vec2::new(
                            scaled_pos.x * cos_a - scaled_pos.y * sin_a,
                            scaled_pos.x * sin_a + scaled_pos.y * cos_a
                        );
                        
                        // Apply translation
                        comp.position = current_center + rotated_pos;
                        comp.velocity = Vec2::ZERO;
                        
                        // Scale component size for pinch
                        if scale_factor > 0.5 && scale_factor < 2.0 {
                            comp.size = comp.size * scale_factor.powf(0.1); // Subtle size change
                        }
                    }
                }
                
                // Update centers for next frame
                self.pinch_center = Some(current_center);
                self.rotation_start_angle = Some(current_angle);
            }
        }
    }
    
    pub fn handle_touch_end(&mut self, touch_id: i32) {
        // Find the touch that ended
        if let Some(touch_idx) = self.touches.iter().position(|t| t.id == touch_id) {
            let touch = &self.touches[touch_idx];
            
            // Apply momentum to the component that was being dragged
            if let Some(component_idx) = touch.component_id {
                if component_idx < self.components.len() && self.components[component_idx].is_dragging {
                    // Calculate momentum from drag history
                    let momentum = self.calculate_momentum();
                    
                    // Apply momentum with touch-specific scaling
                    let momentum_scale = match self.render_quality {
                        RenderQuality::High => 0.7,  // Slightly less than mouse for touch
                        RenderQuality::Medium => 0.5,
                        RenderQuality::Low => 0.3,
                    };
                    
                    self.components[component_idx].velocity = momentum * momentum_scale;
                    self.components[component_idx].is_dragging = false;
                    
                    if momentum.length() > 10.0 {
                        console::log_1(&format!("Touch momentum: {:.1}, {:.1}", momentum.x, momentum.y).into());
                    }
                }
            }
        }
        
        // Remove the touch
        self.touches.retain(|t| t.id != touch_id);
        
        // Reset dragging state for any remaining components
        for component in &mut self.components {
            if self.touches.is_empty() {
                component.is_dragging = false;
            }
        }
        
        // Clear pinch/rotate state if less than 2 touches
        if self.touches.len() < 2 {
            self.pinch_start_distance = None;
            self.pinch_center = None;
            self.grouped_components.clear();
            self.rotation_start_angle = None;
        }
    }
}

// Create a rounded box mesh with beveled edges
fn create_rounded_box_mesh(width: f32, height: f32, depth: f32, _radius: f32) -> Mesh3D {
    let mut mesh = Mesh3D::new("rounded_box".to_string());
    
    // Create a box with slightly beveled edges for a softer look
    let hw = width / 2.0;
    let hh = height / 2.0;
    let hd = depth / 2.0;
    
    // Front face
    let vertices = vec![
        // Front
        Vertex3D::new(Vec3::new(-hw, -hh, hd), Vec3::new(0.0, 0.0, 1.0), Vec2::new(0.0, 0.0)),
        Vertex3D::new(Vec3::new(hw, -hh, hd), Vec3::new(0.0, 0.0, 1.0), Vec2::new(1.0, 0.0)),
        Vertex3D::new(Vec3::new(hw, hh, hd), Vec3::new(0.0, 0.0, 1.0), Vec2::new(1.0, 1.0)),
        Vertex3D::new(Vec3::new(-hw, hh, hd), Vec3::new(0.0, 0.0, 1.0), Vec2::new(0.0, 1.0)),
        
        // Back
        Vertex3D::new(Vec3::new(hw, -hh, -hd), Vec3::new(0.0, 0.0, -1.0), Vec2::new(0.0, 0.0)),
        Vertex3D::new(Vec3::new(-hw, -hh, -hd), Vec3::new(0.0, 0.0, -1.0), Vec2::new(1.0, 0.0)),
        Vertex3D::new(Vec3::new(-hw, hh, -hd), Vec3::new(0.0, 0.0, -1.0), Vec2::new(1.0, 1.0)),
        Vertex3D::new(Vec3::new(hw, hh, -hd), Vec3::new(0.0, 0.0, -1.0), Vec2::new(0.0, 1.0)),
    ];
    
    mesh.vertices = vertices;
    
    // Indices for the box
    mesh.indices = vec![
        // Front
        0, 1, 2, 0, 2, 3,
        // Back
        4, 5, 6, 4, 6, 7,
        // Top
        3, 2, 7, 3, 7, 6,
        // Bottom
        5, 4, 1, 5, 1, 0,
        // Right
        1, 4, 7, 1, 7, 2,
        // Left
        5, 0, 3, 5, 3, 6,
    ];
    
    mesh
}

