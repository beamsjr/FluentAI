use fluentai_renderer::prelude::*;
use fluentai_renderer::devtools::{DevTools, KeyCode, ModifiersState};
use fluentai_renderer::components::{Component, ComponentId, ComponentContext};
use fluentai_renderer::primitives::{Renderable, Color, Transform, Position3D, Size2D};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;
use web_sys::{window, console};

/// Interactive DevTools showcase application
#[wasm_bindgen]
pub struct DevToolsShowcase {
    renderer: Renderer,
    devtools: DevTools,
    components: Vec<Box<dyn Component>>,
    particles: Vec<Particle>,
    animation_enabled: bool,
    selected_component: Option<ComponentId>,
}

#[wasm_bindgen]
impl DevToolsShowcase {
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &str) -> Result<DevToolsShowcase, JsValue> {
        console::log_1(&"Initializing DevTools Showcase...".into());
        
        let renderer = Renderer::new(canvas_id)
            .map_err(|e| JsValue::from_str(&format!("Failed to create renderer: {:?}", e)))?;
        
        let mut devtools = DevTools::new();
        devtools.enable();
        
        Ok(DevToolsShowcase {
            renderer,
            devtools,
            components: Vec::new(),
            particles: Vec::new(),
            animation_enabled: true,
            selected_component: None,
        })
    }
    
    /// Initialize the demo components
    pub fn init_demo(&mut self) {
        // Create demo components
        self.components.push(Box::new(HeaderComponent::new()));
        self.components.push(Box::new(ParticleSystemComponent::new()));
        self.components.push(Box::new(ComponentTreeComponent::new()));
        
        // Set up hot reload callbacks
        self.devtools.hot_reload.on_reload(|event| {
            console::log_1(&format!("Hot reload event: {:?}", event.path).into());
        });
        
        console::log_1(&"Demo initialized with components".into());
    }
    
    /// Add particles for performance testing
    pub fn add_particles(&mut self, count: u32) {
        for _ in 0..count {
            self.particles.push(Particle::new());
        }
        
        self.update_profiler_stats();
    }
    
    /// Clear all particles
    pub fn clear_particles(&mut self) {
        self.particles.clear();
        self.update_profiler_stats();
    }
    
    /// Toggle animation
    pub fn toggle_animation(&mut self) {
        self.animation_enabled = !self.animation_enabled;
    }
    
    /// Handle keyboard input
    pub fn handle_key(&mut self, key: &str, ctrl: bool, shift: bool, alt: bool) {
        let modifiers = ModifiersState {
            ctrl,
            shift,
            alt,
            meta: false,
        };
        
        let keycode = match key {
            "F12" => Some(KeyCode::F12),
            "i" | "I" => Some(KeyCode::I),
            "p" | "P" => Some(KeyCode::P),
            "r" | "R" => Some(KeyCode::R),
            _ => None,
        };
        
        if let Some(code) = keycode {
            if self.devtools.handle_key(code, modifiers) {
                console::log_1(&format!("DevTools handled key: {}", key).into());
            }
        }
        
        // Handle debug overlay toggle
        if key == "d" || key == "D" {
            self.toggle_debug_overlay();
        }
    }
    
    /// Main render loop
    pub fn render(&mut self, timestamp: f64) {
        // Update particle physics
        if self.animation_enabled {
            for particle in &mut self.particles {
                particle.update(timestamp);
            }
        }
        
        // Check for hot reload changes
        if let Some(event) = self.devtools.check_hot_reload() {
            console::log_1(&format!("Reloading due to: {:?}", event.path).into());
            // In a real app, we would reload the affected components
        }
        
        // Update component tree in inspector
        if let Some(root) = self.components.first() {
            let ctx = ComponentContext::default();
            self.devtools.inspector().update_tree(root.as_ref(), &ctx);
        }
        
        // Collect renderables
        let mut renderables = Vec::new();
        
        // Render components
        for component in &self.components {
            let ctx = ComponentContext::default();
            renderables.extend(component.render(&ctx));
        }
        
        // Render particles
        for particle in &self.particles {
            renderables.push(particle.to_renderable());
        }
        
        // Add DevTools overlay
        renderables.extend(self.devtools.render_overlay());
        
        // Update profiler
        self.devtools.profiler().frame_start();
        
        // Submit to renderer
        self.renderer.render(&renderables);
        
        self.devtools.profiler().frame_end();
        
        // Update stats display
        self.update_stats_display();
    }
    
    /// Get current stats as JSON
    pub fn get_stats(&self) -> JsValue {
        let stats = self.devtools.profiler().get_stats();
        
        serde_wasm_bindgen::to_value(&serde_json::json!({
            "fps": stats.fps,
            "frameTime": stats.frame_time_ms,
            "drawCalls": stats.draw_calls,
            "memory": stats.memory_mb,
            "particleCount": self.particles.len(),
            "componentCount": self.components.len(),
        })).unwrap_or(JsValue::NULL)
    }
    
    /// Toggle debug overlay
    fn toggle_debug_overlay(&mut self) {
        // This would toggle physics visualization, bounding boxes, etc.
        console::log_1(&"Toggling debug overlay".into());
    }
    
    /// Update profiler statistics
    fn update_profiler_stats(&mut self) {
        let stats = self.devtools.profiler().get_stats_mut();
        stats.draw_calls = self.particles.len() as u32 + 10; // Base draw calls
        stats.memory_mb = 20.0 + (self.particles.len() as f32 * 0.1);
    }
    
    /// Update stats display in the DOM
    fn update_stats_display(&self) {
        if let Some(window) = window() {
            if let Some(document) = window.document() {
                let stats = self.devtools.profiler().get_stats();
                
                // Update FPS
                if let Some(element) = document.get_element_by_id("fps") {
                    element.set_text_content(Some(&format!("{:.0}", stats.fps)));
                }
                
                // Update draw calls
                if let Some(element) = document.get_element_by_id("drawCalls") {
                    element.set_text_content(Some(&format!("{}", stats.draw_calls)));
                }
                
                // Update memory
                if let Some(element) = document.get_element_by_id("memory") {
                    element.set_text_content(Some(&format!("{:.1} MB", stats.memory_mb)));
                }
                
                // Update component count
                if let Some(element) = document.get_element_by_id("components") {
                    element.set_text_content(Some(&format!("{}", self.components.len())));
                }
            }
        }
    }
}

/// Simple particle for performance testing
struct Particle {
    position: Position3D,
    velocity: (f32, f32),
    color: Color,
    size: f32,
    phase: f32,
}

impl Particle {
    fn new() -> Self {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        
        Self {
            position: Position3D {
                x: rng.gen_range(0.0..800.0),
                y: rng.gen_range(0.0..600.0),
                z: 0.0,
            },
            velocity: (
                rng.gen_range(-50.0..50.0),
                rng.gen_range(-30.0..30.0),
            ),
            color: Color::new(
                rng.gen_range(0.4..0.8),
                rng.gen_range(0.5..0.9),
                rng.gen_range(0.8..1.0),
                0.8,
            ),
            size: rng.gen_range(2.0..6.0),
            phase: rng.gen_range(0.0..std::f32::consts::TAU),
        }
    }
    
    fn update(&mut self, timestamp: f64) {
        let dt = 0.016; // Assume 60fps
        
        // Update position
        self.position.x += self.velocity.0 * dt;
        self.position.y += self.velocity.1 * dt;
        
        // Add some floating motion
        self.position.y += (timestamp * 0.001 + self.phase as f64).sin() as f32 * 0.5;
        
        // Wrap around screen
        if self.position.x < 0.0 {
            self.position.x = 800.0;
        } else if self.position.x > 800.0 {
            self.position.x = 0.0;
        }
        
        if self.position.y < 0.0 {
            self.position.y = 600.0;
        } else if self.position.y > 600.0 {
            self.position.y = 0.0;
        }
    }
    
    fn to_renderable(&self) -> Renderable {
        Renderable::Circle {
            transform: Transform {
                position: self.position,
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            radius: self.size,
            color: self.color,
        }
    }
}

/// Demo component: Header
struct HeaderComponent {
    id: ComponentId,
}

impl HeaderComponent {
    fn new() -> Self {
        Self {
            id: ComponentId::new(),
        }
    }
}

impl Component for HeaderComponent {
    fn id(&self) -> ComponentId {
        self.id
    }
    
    fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
        vec![
            Renderable::Rect {
                transform: Transform {
                    position: Position3D { x: 0.0, y: 0.0, z: 0.0 },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                size: Size2D::new(800.0, 60.0),
                color: Color::new(0.1, 0.1, 0.1, 0.9),
                radius: 0.0,
            },
            Renderable::Text {
                transform: Transform {
                    position: Position3D { x: 20.0, y: 20.0, z: 1.0 },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                content: "FluentAI DevTools Demo".to_string(),
                size: 24.0,
                color: Color::new(1.0, 1.0, 1.0, 1.0),
                font: Some("system".to_string()),
            },
        ]
    }
}

/// Demo component: Particle System
struct ParticleSystemComponent {
    id: ComponentId,
}

impl ParticleSystemComponent {
    fn new() -> Self {
        Self {
            id: ComponentId::new(),
        }
    }
}

impl Component for ParticleSystemComponent {
    fn id(&self) -> ComponentId {
        self.id
    }
    
    fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
        // Just a container, particles are rendered separately
        vec![
            Renderable::Rect {
                transform: Transform {
                    position: Position3D { x: 20.0, y: 80.0, z: 0.0 },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                size: Size2D::new(760.0, 200.0),
                color: Color::new(0.0, 0.0, 0.0, 0.3),
                radius: 8.0,
            },
        ]
    }
}

/// Demo component: Component Tree
struct ComponentTreeComponent {
    id: ComponentId,
}

impl ComponentTreeComponent {
    fn new() -> Self {
        Self {
            id: ComponentId::new(),
        }
    }
}

impl Component for ComponentTreeComponent {
    fn id(&self) -> ComponentId {
        self.id
    }
    
    fn render(&self, _ctx: &ComponentContext) -> Vec<Renderable> {
        let mut renderables = vec![];
        
        // Tree items
        let items = vec![
            ("📁 App Root", 0),
            ("  📦 Header", 1),
            ("  🎨 ParticleSystem", 1),
            ("  🌳 ComponentTree", 1),
        ];
        
        for (i, (text, _indent)) in items.iter().enumerate() {
            renderables.push(Renderable::Text {
                transform: Transform {
                    position: Position3D {
                        x: 30.0,
                        y: 300.0 + (i as f32 * 20.0),
                        z: 1.0,
                    },
                    rotation: (0.0, 0.0, 0.0),
                    scale: (1.0, 1.0, 1.0),
                },
                content: text.to_string(),
                size: 14.0,
                color: Color::new(0.8, 0.8, 0.8, 1.0),
                font: Some("monospace".to_string()),
            });
        }
        
        renderables
    }
}

/// Export initialization function for JavaScript
#[wasm_bindgen]
pub fn init_showcase() -> DevToolsShowcase {
    console_error_panic_hook::set_once();
    
    let mut showcase = DevToolsShowcase::new("render-canvas")
        .expect("Failed to create showcase");
    
    showcase.init_demo();
    showcase
}