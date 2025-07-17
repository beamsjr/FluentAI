/// Performance profiler for rendering
use std::collections::VecDeque;
use web_time::{Duration, Instant};
use std::sync::{Arc, Mutex};

/// Render profiler for performance analysis
pub struct RenderProfiler {
    enabled: bool,
    frame_times: VecDeque<FrameTime>,
    current_frame: Option<FrameTime>,
    max_frames: usize,
    stats: Arc<Mutex<ProfilerStats>>,
}

#[derive(Debug, Clone)]
struct FrameTime {
    start: Instant,
    end: Option<Instant>,
    layout_start: Option<Instant>,
    layout_end: Option<Instant>,
    render_start: Option<Instant>,
    render_end: Option<Instant>,
    draw_calls: u32,
    vertices: u32,
    memory_allocated: usize,
}

#[derive(Debug, Clone)]
pub struct ProfilerStats {
    pub fps: f32,
    pub frame_time_ms: f32,
    pub layout_time_ms: f32,
    pub render_time_ms: f32,
    pub draw_calls: u32,
    pub vertices: u32,
    pub memory_mb: f32,
    pub frame_time_history: Vec<f32>,
    pub fps_history: Vec<f32>,
}

impl RenderProfiler {
    pub fn new() -> Self {
        Self {
            enabled: true,
            frame_times: VecDeque::with_capacity(120),
            current_frame: None,
            max_frames: 120, // 2 seconds at 60 FPS
            stats: Arc::new(Mutex::new(ProfilerStats {
                fps: 0.0,
                frame_time_ms: 0.0,
                layout_time_ms: 0.0,
                render_time_ms: 0.0,
                draw_calls: 0,
                vertices: 0,
                memory_mb: 0.0,
                frame_time_history: Vec::with_capacity(120),
                fps_history: Vec::with_capacity(120),
            })),
        }
    }
    
    /// Enable profiling
    pub fn enable(&mut self) {
        self.enabled = true;
    }
    
    /// Disable profiling
    pub fn disable(&mut self) {
        self.enabled = false;
    }
    
    /// Start a new frame
    pub fn begin_frame(&mut self) {
        if !self.enabled {
            return;
        }
        
        // Complete previous frame if exists
        if let Some(mut frame) = self.current_frame.take() {
            frame.end = Some(Instant::now());
            self.add_frame(frame);
        }
        
        self.current_frame = Some(FrameTime {
            start: Instant::now(),
            end: None,
            layout_start: None,
            layout_end: None,
            render_start: None,
            render_end: None,
            draw_calls: 0,
            vertices: 0,
            memory_allocated: 0,
        });
    }
    
    /// End current frame
    pub fn end_frame(&mut self) {
        if !self.enabled {
            return;
        }
        
        if let Some(mut frame) = self.current_frame.take() {
            frame.end = Some(Instant::now());
            self.add_frame(frame);
        }
        
        self.update_stats();
    }
    
    /// Mark layout phase start
    pub fn begin_layout(&mut self) {
        if !self.enabled {
            return;
        }
        
        if let Some(ref mut frame) = self.current_frame {
            frame.layout_start = Some(Instant::now());
        }
    }
    
    /// Mark layout phase end
    pub fn end_layout(&mut self) {
        if !self.enabled {
            return;
        }
        
        if let Some(ref mut frame) = self.current_frame {
            frame.layout_end = Some(Instant::now());
        }
    }
    
    /// Mark render phase start
    pub fn begin_render(&mut self) {
        if !self.enabled {
            return;
        }
        
        if let Some(ref mut frame) = self.current_frame {
            frame.render_start = Some(Instant::now());
        }
    }
    
    /// Mark render phase end
    pub fn end_render(&mut self) {
        if !self.enabled {
            return;
        }
        
        if let Some(ref mut frame) = self.current_frame {
            frame.render_end = Some(Instant::now());
        }
    }
    
    /// Record a draw call
    pub fn record_draw_call(&mut self, vertices: u32) {
        if !self.enabled {
            return;
        }
        
        if let Some(ref mut frame) = self.current_frame {
            frame.draw_calls += 1;
            frame.vertices += vertices;
        }
    }
    
    /// Record memory allocation
    pub fn record_memory_allocation(&mut self, bytes: usize) {
        if !self.enabled {
            return;
        }
        
        if let Some(ref mut frame) = self.current_frame {
            frame.memory_allocated += bytes;
        }
    }
    
    /// Add completed frame
    fn add_frame(&mut self, frame: FrameTime) {
        self.frame_times.push_back(frame);
        
        // Keep only recent frames
        while self.frame_times.len() > self.max_frames {
            self.frame_times.pop_front();
        }
    }
    
    /// Update statistics
    fn update_stats(&mut self) {
        if self.frame_times.is_empty() {
            return;
        }
        
        let mut stats = self.stats.lock().unwrap();
        
        // Calculate average frame time
        let frame_times: Vec<f32> = self.frame_times.iter()
            .filter_map(|frame| {
                frame.end.and_then(|end| {
                    Some(end.duration_since(frame.start).as_secs_f32() * 1000.0)
                })
            })
            .collect();
        
        if !frame_times.is_empty() {
            let avg_frame_time = frame_times.iter().sum::<f32>() / frame_times.len() as f32;
            stats.frame_time_ms = avg_frame_time;
            stats.fps = if avg_frame_time > 0.0 { 1000.0 / avg_frame_time } else { 0.0 };
        }
        
        // Calculate layout and render times
        let layout_times: Vec<f32> = self.frame_times.iter()
            .filter_map(|frame| {
                match (frame.layout_start, frame.layout_end) {
                    (Some(start), Some(end)) => Some(end.duration_since(start).as_secs_f32() * 1000.0),
                    _ => None,
                }
            })
            .collect();
        
        if !layout_times.is_empty() {
            stats.layout_time_ms = layout_times.iter().sum::<f32>() / layout_times.len() as f32;
        }
        
        let render_times: Vec<f32> = self.frame_times.iter()
            .filter_map(|frame| {
                match (frame.render_start, frame.render_end) {
                    (Some(start), Some(end)) => Some(end.duration_since(start).as_secs_f32() * 1000.0),
                    _ => None,
                }
            })
            .collect();
        
        if !render_times.is_empty() {
            stats.render_time_ms = render_times.iter().sum::<f32>() / render_times.len() as f32;
        }
        
        // Average draw calls and vertices
        let total_frames = self.frame_times.len() as f32;
        stats.draw_calls = (self.frame_times.iter().map(|f| f.draw_calls).sum::<u32>() as f32 / total_frames) as u32;
        stats.vertices = (self.frame_times.iter().map(|f| f.vertices).sum::<u32>() as f32 / total_frames) as u32;
        
        // Memory usage
        let total_memory = self.frame_times.iter().map(|f| f.memory_allocated).sum::<usize>();
        stats.memory_mb = total_memory as f32 / 1024.0 / 1024.0;
        
        // Update history
        let frame_time_ms = stats.frame_time_ms;
        stats.frame_time_history.push(frame_time_ms);
        if stats.frame_time_history.len() > self.max_frames {
            stats.frame_time_history.remove(0);
        }
        
        let fps = stats.fps;
        stats.fps_history.push(fps);
        if stats.fps_history.len() > self.max_frames {
            stats.fps_history.remove(0);
        }
    }
    
    /// Get current statistics
    pub fn get_stats(&self) -> ProfilerStats {
        self.stats.lock().unwrap().clone()
    }
    
    /// Create a scoped timer
    pub fn scoped_timer(&mut self, name: &'static str) -> ScopedTimer {
        ScopedTimer::new(name, self.enabled)
    }
    
    /// Export profiling data
    pub fn export_data(&self) -> serde_json::Value {
        let stats = self.stats.lock().unwrap();
        
        serde_json::json!({
            "summary": {
                "fps": stats.fps,
                "frameTimeMs": stats.frame_time_ms,
                "layoutTimeMs": stats.layout_time_ms,
                "renderTimeMs": stats.render_time_ms,
                "drawCalls": stats.draw_calls,
                "vertices": stats.vertices,
                "memoryMB": stats.memory_mb,
            },
            "history": {
                "frameTime": stats.frame_time_history,
                "fps": stats.fps_history,
            },
            "frames": self.frame_times.iter().map(|frame| {
                serde_json::json!({
                    "totalMs": frame.end.and_then(|end| {
                        Some(end.duration_since(frame.start).as_secs_f32() * 1000.0)
                    }),
                    "layoutMs": match (frame.layout_start, frame.layout_end) {
                        (Some(start), Some(end)) => Some(end.duration_since(start).as_secs_f32() * 1000.0),
                        _ => None,
                    },
                    "renderMs": match (frame.render_start, frame.render_end) {
                        (Some(start), Some(end)) => Some(end.duration_since(start).as_secs_f32() * 1000.0),
                        _ => None,
                    },
                    "drawCalls": frame.draw_calls,
                    "vertices": frame.vertices,
                })
            }).collect::<Vec<_>>()
        })
    }
}

/// Scoped timer for profiling specific code sections
pub struct ScopedTimer {
    name: &'static str,
    start: Instant,
    enabled: bool,
}

impl ScopedTimer {
    fn new(name: &'static str, enabled: bool) -> Self {
        Self {
            name,
            start: Instant::now(),
            enabled,
        }
    }
}

impl Drop for ScopedTimer {
    fn drop(&mut self) {
        if self.enabled {
            let duration = self.start.elapsed();
            println!("⏱️  {}: {:.2}ms", self.name, duration.as_secs_f32() * 1000.0);
        }
    }
}

/// GPU profiler for detailed GPU timing
pub struct GPUProfiler {
    queries: Vec<GPUTimerQuery>,
}

struct GPUTimerQuery {
    name: String,
    start_query: Option<u32>, // GPU query object
    end_query: Option<u32>,
}

impl GPUProfiler {
    pub fn new() -> Self {
        Self {
            queries: Vec::new(),
        }
    }
    
    /// Begin GPU timer
    pub fn begin_gpu_timer(&mut self, name: &str) {
        // TODO: Create GPU query object
        self.queries.push(GPUTimerQuery {
            name: name.to_string(),
            start_query: Some(0), // Placeholder
            end_query: None,
        });
    }
    
    /// End GPU timer
    pub fn end_gpu_timer(&mut self, name: &str) {
        if let Some(query) = self.queries.iter_mut().find(|q| q.name == name) {
            query.end_query = Some(0); // Placeholder
        }
    }
    
    /// Get GPU timing results
    pub fn get_results(&self) -> Vec<(String, f32)> {
        // TODO: Query GPU for timing results
        Vec::new()
    }
}

/// Memory profiler
pub struct MemoryProfiler {
    allocations: HashMap<String, AllocationInfo>,
}

use std::collections::HashMap;

#[derive(Debug, Clone)]
struct AllocationInfo {
    count: usize,
    total_bytes: usize,
    peak_bytes: usize,
}

impl MemoryProfiler {
    pub fn new() -> Self {
        Self {
            allocations: HashMap::new(),
        }
    }
    
    /// Record allocation
    pub fn record_allocation(&mut self, category: &str, bytes: usize) {
        let info = self.allocations.entry(category.to_string()).or_insert(AllocationInfo {
            count: 0,
            total_bytes: 0,
            peak_bytes: 0,
        });
        
        info.count += 1;
        info.total_bytes += bytes;
        info.peak_bytes = info.peak_bytes.max(info.total_bytes);
    }
    
    /// Record deallocation
    pub fn record_deallocation(&mut self, category: &str, bytes: usize) {
        if let Some(info) = self.allocations.get_mut(category) {
            info.total_bytes = info.total_bytes.saturating_sub(bytes);
        }
    }
    
    /// Get memory report
    pub fn get_report(&self) -> Vec<(String, AllocationInfo)> {
        let mut report: Vec<_> = self.allocations.iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        
        report.sort_by(|a, b| b.1.total_bytes.cmp(&a.1.total_bytes));
        report
    }
}