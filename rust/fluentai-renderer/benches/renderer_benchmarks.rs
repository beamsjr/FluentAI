/// Performance benchmarks for critical rendering paths
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fluentai_renderer::{
    simple_text::render_text,
    gradient::Gradient,
    path::{PathBuilder, Path},
    batching::RenderBatcher,
    animation::{Animation, AnimationEngine, Easing},
    primitives::{Color, Point2D, Rect, Renderable},
};

fn benchmark_text_rendering(c: &mut Criterion) {
    c.bench_function("render_text_short", |b| {
        let text = "Hello";
        let position = Point2D::new(0.0, 0.0);
        let color = Color::white();
        let scale = 1.0;
        
        b.iter(|| {
            black_box(render_text(
                black_box(text),
                black_box(position),
                black_box(color),
                black_box(scale),
            ))
        });
    });
    
    c.bench_function("render_text_long", |b| {
        let text = "The quick brown fox jumps over the lazy dog. 1234567890!@#$%^&*()";
        let position = Point2D::new(0.0, 0.0);
        let color = Color::white();
        let scale = 1.0;
        
        b.iter(|| {
            black_box(render_text(
                black_box(text),
                black_box(position),
                black_box(color),
                black_box(scale),
            ))
        });
    });
    
    c.bench_function("render_text_scaled", |b| {
        let text = "Scaled Text";
        let position = Point2D::new(0.0, 0.0);
        let color = Color::white();
        let scale = 2.5;
        
        b.iter(|| {
            black_box(render_text(
                black_box(text),
                black_box(position),
                black_box(color),
                black_box(scale),
            ))
        });
    });
}

fn benchmark_gradient_generation(c: &mut Criterion) {
    c.bench_function("gradient_linear_2_stops", |b| {
        let stops = vec![
            (0.0, Color::red()),
            (1.0, Color::blue()),
        ];
        
        b.iter(|| {
            let gradient = Gradient::linear(black_box(45.0), black_box(stops.clone()));
            black_box(gradient.to_mesh(800.0, 600.0))
        });
    });
    
    c.bench_function("gradient_linear_5_stops", |b| {
        let stops = vec![
            (0.0, Color::red()),
            (0.25, Color::orange()),
            (0.5, Color::yellow()),
            (0.75, Color::green()),
            (1.0, Color::blue()),
        ];
        
        b.iter(|| {
            let gradient = Gradient::linear(black_box(90.0), black_box(stops.clone()));
            black_box(gradient.to_mesh(800.0, 600.0))
        });
    });
    
    c.bench_function("gradient_radial", |b| {
        let center = Point2D::new(400.0, 300.0);
        let radius = 200.0;
        let stops = vec![
            (0.0, Color::white()),
            (0.5, Color::gray()),
            (1.0, Color::black()),
        ];
        
        b.iter(|| {
            let gradient = Gradient::radial(
                black_box(center),
                black_box(radius),
                black_box(stops.clone())
            );
            black_box(gradient.to_mesh(800.0, 600.0))
        });
    });
}

fn benchmark_path_tessellation(c: &mut Criterion) {
    c.bench_function("path_simple_triangle", |b| {
        let mut builder = PathBuilder::new();
        builder.move_to(Point2D::new(0.0, 0.0));
        builder.line_to(Point2D::new(100.0, 0.0));
        builder.line_to(Point2D::new(50.0, 100.0));
        builder.close();
        let path = builder.build();
        
        b.iter(|| {
            black_box(path.tessellate(black_box(Color::red())))
        });
    });
    
    c.bench_function("path_complex_shape", |b| {
        let mut builder = PathBuilder::new();
        // Create a star shape
        let center = Point2D::new(50.0, 50.0);
        let outer_radius = 50.0;
        let inner_radius = 20.0;
        let points = 5;
        
        builder.move_to(Point2D::new(center.x, center.y - outer_radius));
        
        for i in 0..points * 2 {
            let angle = i as f32 * std::f32::consts::PI / points as f32;
            let radius = if i % 2 == 0 { outer_radius } else { inner_radius };
            let x = center.x + angle.sin() * radius;
            let y = center.y - angle.cos() * radius;
            builder.line_to(Point2D::new(x, y));
        }
        
        builder.close();
        let path = builder.build();
        
        b.iter(|| {
            black_box(path.tessellate(black_box(Color::yellow())))
        });
    });
    
    c.bench_function("path_with_curves", |b| {
        let mut builder = PathBuilder::new();
        builder.move_to(Point2D::new(0.0, 50.0));
        
        // Add multiple bezier curves
        for i in 0..5 {
            let x = i as f32 * 40.0;
            builder.quadratic_to(
                Point2D::new(x + 20.0, 0.0),
                Point2D::new(x + 40.0, 50.0),
            );
        }
        
        let path = builder.build();
        
        b.iter(|| {
            black_box(path.tessellate(black_box(Color::green())))
        });
    });
}

fn benchmark_render_batching(c: &mut Criterion) {
    c.bench_function("batch_100_rectangles", |b| {
        b.iter(|| {
            let mut batcher = RenderBatcher::new();
            
            for i in 0..100 {
                batcher.add_renderable(Renderable::Rectangle {
                    rect: Rect::new(i as f32 * 10.0, 0.0, 8.0, 8.0),
                    color: Color::blue(),
                    border_radius: 0.0,
                });
            }
            
            black_box(batcher.create_batches())
        });
    });
    
    c.bench_function("batch_mixed_shapes", |b| {
        b.iter(|| {
            let mut batcher = RenderBatcher::new();
            
            // Add rectangles
            for i in 0..50 {
                batcher.add_renderable(Renderable::Rectangle {
                    rect: Rect::new(i as f32 * 20.0, 0.0, 15.0, 15.0),
                    color: Color::red(),
                    border_radius: 2.0,
                });
            }
            
            // Add circles
            for i in 0..50 {
                batcher.add_renderable(Renderable::Circle {
                    center: Point2D::new(i as f32 * 20.0, 50.0),
                    radius: 8.0,
                    color: Color::green(),
                });
            }
            
            black_box(batcher.create_batches())
        });
    });
}

fn benchmark_animation_engine(c: &mut Criterion) {
    c.bench_function("animation_update_10", |b| {
        b.iter(|| {
            let mut engine = AnimationEngine::new();
            let mut values = vec![0.0; 10];
            
            // Add 10 animations
            for i in 0..10 {
                engine.add_animation(Animation::new(
                    &mut values[i],
                    100.0,
                    1000,
                    Easing::EaseInOut,
                ));
            }
            
            // Update at various times
            for t in 0..10 {
                engine.update(black_box(t as f32 * 100.0));
            }
            
            black_box(values)
        });
    });
    
    c.bench_function("animation_update_100", |b| {
        b.iter(|| {
            let mut engine = AnimationEngine::new();
            let mut values = vec![0.0; 100];
            
            // Add 100 animations with different durations
            for i in 0..100 {
                engine.add_animation(Animation::new(
                    &mut values[i],
                    i as f32,
                    500 + i * 10,
                    Easing::Linear,
                ));
            }
            
            // Single update
            engine.update(black_box(750.0));
            
            black_box(values)
        });
    });
}

fn benchmark_color_operations(c: &mut Criterion) {
    c.bench_function("color_interpolation", |b| {
        let c1 = Color::red();
        let c2 = Color::blue();
        
        b.iter(|| {
            let mut result = Color::black();
            for i in 0..100 {
                let t = i as f32 / 100.0;
                result = Color::new(
                    c1.r + (c2.r - c1.r) * t,
                    c1.g + (c2.g - c1.g) * t,
                    c1.b + (c2.b - c1.b) * t,
                    c1.a + (c2.a - c1.a) * t,
                );
            }
            black_box(result)
        });
    });
    
    c.bench_function("color_hsv_conversion", |b| {
        let colors = vec![
            Color::red(),
            Color::green(),
            Color::blue(),
            Color::yellow(),
            Color::cyan(),
            Color::magenta(),
        ];
        
        b.iter(|| {
            for color in &colors {
                let hsv = color.to_hsv();
                let rgb = Color::from_hsv(hsv.0, hsv.1, hsv.2);
                black_box(rgb);
            }
        });
    });
}

criterion_group!(
    benches,
    benchmark_text_rendering,
    benchmark_gradient_generation,
    benchmark_path_tessellation,
    benchmark_render_batching,
    benchmark_animation_engine,
    benchmark_color_operations
);

criterion_main!(benches);

// Mock implementations for benchmarking
impl Gradient {
    fn to_mesh(&self, width: f32, height: f32) -> Vec<f32> {
        // Mock implementation returning vertex data
        vec![0.0; 24] // 4 vertices * 6 attributes each
    }
}

impl Path {
    fn tessellate(&self, _color: Color) -> Vec<f32> {
        // Mock implementation
        vec![0.0; self.commands.len() * 18] // Approximate vertices
    }
}

impl RenderBatcher {
    fn create_batches(&mut self) -> Vec<RenderBatch> {
        // Mock implementation
        vec![RenderBatch { instances: vec![] }]
    }
}

struct RenderBatch {
    instances: Vec<()>,
}

impl Color {
    fn to_hsv(&self) -> (f32, f32, f32) {
        // Simple RGB to HSV conversion
        let max = self.r.max(self.g).max(self.b);
        let min = self.r.min(self.g).min(self.b);
        let delta = max - min;
        
        let v = max;
        let s = if max > 0.0 { delta / max } else { 0.0 };
        let h = if delta == 0.0 {
            0.0
        } else if max == self.r {
            60.0 * (((self.g - self.b) / delta) % 6.0)
        } else if max == self.g {
            60.0 * (((self.b - self.r) / delta) + 2.0)
        } else {
            60.0 * (((self.r - self.g) / delta) + 4.0)
        };
        
        (h, s, v)
    }
    
    fn from_hsv(h: f32, s: f32, v: f32) -> Self {
        let c = v * s;
        let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
        let m = v - c;
        
        let (r, g, b) = match (h / 60.0) as i32 {
            0 => (c, x, 0.0),
            1 => (x, c, 0.0),
            2 => (0.0, c, x),
            3 => (0.0, x, c),
            4 => (x, 0.0, c),
            _ => (c, 0.0, x),
        };
        
        Color::new(r + m, g + m, b + m, 1.0)
    }
    
    fn cyan() -> Self { Color::new(0.0, 1.0, 1.0, 1.0) }
    fn magenta() -> Self { Color::new(1.0, 0.0, 1.0, 1.0) }
    fn yellow() -> Self { Color::new(1.0, 1.0, 0.0, 1.0) }
    fn orange() -> Self { Color::new(1.0, 0.5, 0.0, 1.0) }
    fn gray() -> Self { Color::new(0.5, 0.5, 0.5, 1.0) }
}