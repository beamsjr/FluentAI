/// Developer tools for FluentAI renderer
use crate::components::{Component, ComponentId};
use crate::primitives::{Renderable, Color};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

pub mod inspector;
pub mod profiler;
pub mod hot_reload;
pub mod debugger;

/// Developer tools state
pub struct DevTools {
    enabled: bool,
    inspector: inspector::ComponentInspector,
    profiler: profiler::RenderProfiler,
    hot_reload: hot_reload::HotReloadServer,
    debugger: debugger::UIDebugger,
    overlay: DevOverlay,
}

impl DevTools {
    pub fn new() -> Self {
        Self {
            enabled: false,
            inspector: inspector::ComponentInspector::new(),
            profiler: profiler::RenderProfiler::new(),
            hot_reload: hot_reload::HotReloadServer::new(),
            debugger: debugger::UIDebugger::new(),
            overlay: DevOverlay::new(),
        }
    }
    
    /// Enable developer tools
    pub fn enable(&mut self) {
        self.enabled = true;
        self.hot_reload.start();
        println!("🛠️  DevTools enabled");
        println!("   Press F12 to toggle overlay");
        println!("   Hot reload server on http://localhost:3030");
    }
    
    /// Disable developer tools
    pub fn disable(&mut self) {
        self.enabled = false;
        self.hot_reload.stop();
    }
    
    /// Toggle developer tools
    pub fn toggle(&mut self) {
        if self.enabled {
            self.disable();
        } else {
            self.enable();
        }
    }
    
    /// Check if enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
    
    /// Get the inspector
    pub fn inspector(&mut self) -> &mut inspector::ComponentInspector {
        &mut self.inspector
    }
    
    /// Get the profiler
    pub fn profiler(&mut self) -> &mut profiler::RenderProfiler {
        &mut self.profiler
    }
    
    /// Get the debugger
    pub fn debugger(&mut self) -> &mut debugger::UIDebugger {
        &mut self.debugger
    }
    
    /// Handle hot reload events
    pub fn check_hot_reload(&mut self) -> Option<hot_reload::ReloadEvent> {
        self.hot_reload.check_for_changes()
    }
    
    /// Render developer overlay
    pub fn render_overlay(&self) -> Vec<Renderable> {
        if self.enabled && self.overlay.visible {
            self.overlay.render(&self.inspector, &self.profiler)
        } else {
            Vec::new()
        }
    }
    
    /// Toggle overlay visibility
    pub fn toggle_overlay(&mut self) {
        self.overlay.visible = !self.overlay.visible;
    }
    
    /// Handle keyboard shortcuts
    pub fn handle_key(&mut self, key: KeyCode, modifiers: ModifiersState) -> bool {
        if !self.enabled {
            return false;
        }
        
        match (key, modifiers) {
            // F12 - Toggle overlay
            (KeyCode::F12, _) => {
                self.toggle_overlay();
                true
            }
            // Ctrl+Shift+I - Toggle inspector
            (KeyCode::I, ModifiersState::CTRL | ModifiersState::SHIFT) => {
                self.overlay.show_inspector = !self.overlay.show_inspector;
                true
            }
            // Ctrl+Shift+P - Toggle profiler
            (KeyCode::P, ModifiersState::CTRL | ModifiersState::SHIFT) => {
                self.overlay.show_profiler = !self.overlay.show_profiler;
                true
            }
            // Ctrl+R - Force reload
            (KeyCode::R, ModifiersState::CTRL) => {
                self.hot_reload.force_reload();
                true
            }
            _ => false,
        }
    }
}

/// Developer overlay UI
struct DevOverlay {
    visible: bool,
    show_inspector: bool,
    show_profiler: bool,
    show_console: bool,
    position: OverlayPosition,
}

#[derive(Clone, Copy)]
enum OverlayPosition {
    Right,
    Bottom,
    Floating(f32, f32),
}

impl DevOverlay {
    fn new() -> Self {
        Self {
            visible: false,
            show_inspector: true,
            show_profiler: true,
            show_console: true,
            position: OverlayPosition::Right,
        }
    }
    
    fn render(
        &self,
        inspector: &inspector::ComponentInspector,
        profiler: &profiler::RenderProfiler,
    ) -> Vec<Renderable> {
        let mut renderables = Vec::new();
        
        // Background panel
        let (x, y, width, height) = match self.position {
            OverlayPosition::Right => {
                let width = 400.0;
                let screen_width = 1920.0; // TODO: Get actual screen width
                let screen_height = 1080.0; // TODO: Get actual screen height
                (screen_width - width, 0.0, width, screen_height)
            }
            OverlayPosition::Bottom => {
                let height = 300.0;
                let screen_width = 1920.0;
                let screen_height = 1080.0;
                (0.0, screen_height - height, screen_width, height)
            }
            OverlayPosition::Floating(x, y) => (x, y, 400.0, 600.0),
        };
        
        // Semi-transparent background
        renderables.push(Renderable::Rect {
            transform: Transform {
                position: Position3D { x, y, z: 1000.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            size: Size2D::new(width, height),
            color: Color::new(0.1, 0.1, 0.1, 0.95),
            radius: 0.0,
        });
        
        // Title bar
        renderables.push(Renderable::Rect {
            transform: Transform {
                position: Position3D { x, y, z: 1001.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            size: Size2D::new(width, 40.0),
            color: Color::new(0.2, 0.2, 0.2, 1.0),
            radius: 0.0,
        });
        
        // Title text
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x: x + 10.0, y: y + 10.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: "FluentAI DevTools".to_string(),
            size: 16.0,
            color: Color::new(1.0, 1.0, 1.0, 1.0),
            font: Some("system".to_string()),
        });
        
        let mut content_y = y + 50.0;
        
        // Inspector section
        if self.show_inspector {
            renderables.extend(self.render_inspector_section(
                inspector,
                x + 10.0,
                content_y,
                width - 20.0,
            ));
            content_y += 200.0;
        }
        
        // Profiler section
        if self.show_profiler {
            renderables.extend(self.render_profiler_section(
                profiler,
                x + 10.0,
                content_y,
                width - 20.0,
            ));
            content_y += 200.0;
        }
        
        renderables
    }
    
    fn render_inspector_section(
        &self,
        inspector: &inspector::ComponentInspector,
        x: f32,
        y: f32,
        width: f32,
    ) -> Vec<Renderable> {
        let mut renderables = Vec::new();
        
        // Section header
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: "Component Inspector".to_string(),
            size: 14.0,
            color: Color::new(0.8, 0.8, 1.0, 1.0),
            font: Some("system".to_string()),
        });
        
        // Component tree
        if let Some(tree) = inspector.get_component_tree() {
            renderables.extend(self.render_component_tree(&tree, x, y + 20.0, 0));
        }
        
        renderables
    }
    
    fn render_component_tree(
        &self,
        node: &inspector::ComponentNode,
        x: f32,
        y: f32,
        depth: usize,
    ) -> Vec<Renderable> {
        let mut renderables = Vec::new();
        let indent = depth as f32 * 20.0;
        
        // Component name
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x: x + indent, y, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("• {} ({})", node.name, node.id),
            size: 12.0,
            color: if node.selected {
                Color::new(1.0, 0.8, 0.2, 1.0)
            } else {
                Color::new(0.8, 0.8, 0.8, 1.0)
            },
            font: Some("system".to_string()),
        });
        
        // Render children
        let mut child_y = y + 16.0;
        for child in &node.children {
            let child_renderables = self.render_component_tree(child, x, child_y, depth + 1);
            child_y += 16.0 * (1 + child.count_descendants()) as f32;
            renderables.extend(child_renderables);
        }
        
        renderables
    }
    
    fn render_profiler_section(
        &self,
        profiler: &profiler::RenderProfiler,
        x: f32,
        y: f32,
        width: f32,
    ) -> Vec<Renderable> {
        let mut renderables = Vec::new();
        
        // Section header
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: "Performance Profiler".to_string(),
            size: 14.0,
            color: Color::new(1.0, 0.8, 0.8, 1.0),
            font: Some("system".to_string()),
        });
        
        let stats = profiler.get_stats();
        
        // FPS
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y: y + 20.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("FPS: {:.1} (target: 60)", stats.fps),
            size: 12.0,
            color: if stats.fps >= 55.0 {
                Color::new(0.2, 1.0, 0.2, 1.0)
            } else if stats.fps >= 30.0 {
                Color::new(1.0, 1.0, 0.2, 1.0)
            } else {
                Color::new(1.0, 0.2, 0.2, 1.0)
            },
            font: Some("system".to_string()),
        });
        
        // Frame time
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y: y + 36.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("Frame time: {:.2}ms", stats.frame_time_ms),
            size: 12.0,
            color: Color::new(0.8, 0.8, 0.8, 1.0),
            font: Some("system".to_string()),
        });
        
        // Draw calls
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y: y + 52.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("Draw calls: {}", stats.draw_calls),
            size: 12.0,
            color: Color::new(0.8, 0.8, 0.8, 1.0),
            font: Some("system".to_string()),
        });
        
        // Memory usage
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y: y + 68.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("Memory: {:.1} MB", stats.memory_mb),
            size: 12.0,
            color: Color::new(0.8, 0.8, 0.8, 1.0),
            font: Some("system".to_string()),
        });
        
        // Render time breakdown
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x, y: y + 92.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: "Time breakdown:".to_string(),
            size: 12.0,
            color: Color::new(0.8, 0.8, 0.8, 1.0),
            font: Some("system".to_string()),
        });
        
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x: x + 10.0, y: y + 108.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("• Layout: {:.2}ms", stats.layout_time_ms),
            size: 11.0,
            color: Color::new(0.7, 0.7, 0.7, 1.0),
            font: Some("system".to_string()),
        });
        
        renderables.push(Renderable::Text {
            transform: Transform {
                position: Position3D { x: x + 10.0, y: y + 122.0, z: 1002.0 },
                rotation: (0.0, 0.0, 0.0),
                scale: (1.0, 1.0, 1.0),
            },
            content: format!("• Render: {:.2}ms", stats.render_time_ms),
            size: 11.0,
            color: Color::new(0.7, 0.7, 0.7, 1.0),
            font: Some("system".to_string()),
        });
        
        renderables
    }
}

// Keyboard support
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeyCode {
    F12,
    I,
    P,
    R,
    // Add more as needed
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ModifiersState {
    pub shift: bool,
    pub ctrl: bool,
    pub alt: bool,
    pub meta: bool,
}

impl ModifiersState {
    pub const NONE: Self = Self {
        shift: false,
        ctrl: false,
        alt: false,
        meta: false,
    };
    
    pub const SHIFT: Self = Self {
        shift: true,
        ctrl: false,
        alt: false,
        meta: false,
    };
    
    pub const CTRL: Self = Self {
        shift: false,
        ctrl: true,
        alt: false,
        meta: false,
    };
}

impl std::ops::BitOr for ModifiersState {
    type Output = Self;
    
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            shift: self.shift || rhs.shift,
            ctrl: self.ctrl || rhs.ctrl,
            alt: self.alt || rhs.alt,
            meta: self.meta || rhs.meta,
        }
    }
}

// Re-export common types
use crate::primitives::{Transform, Position3D, Size2D};