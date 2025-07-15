// Path rendering with bezier curve support

use cgmath::Vector2;
use crate::primitives::Color;

/// A point in 2D space
pub type Point2D = Vector2<f32>;

/// Path command for building complex shapes
#[derive(Debug, Clone)]
pub enum PathCommand {
    /// Move to a point without drawing
    MoveTo(Point2D),
    /// Draw a straight line to a point
    LineTo(Point2D),
    /// Draw a quadratic bezier curve
    QuadraticTo(Point2D, Point2D), // control point, end point
    /// Draw a cubic bezier curve
    CubicTo(Point2D, Point2D, Point2D), // control point 1, control point 2, end point
    /// Close the current path
    Close,
}

/// A path that can contain multiple subpaths
#[derive(Debug, Clone)]
pub struct Path {
    commands: Vec<PathCommand>,
    stroke_color: Option<Color>,
    stroke_width: Option<f32>,
    fill_color: Option<Color>,
}

impl Path {
    /// Create a new empty path
    pub fn new() -> Self {
        Self {
            commands: Vec::new(),
            stroke_color: None,
            stroke_width: None,
            fill_color: None,
        }
    }
    
    /// Move to a point without drawing
    pub fn move_to(mut self, x: f32, y: f32) -> Self {
        self.commands.push(PathCommand::MoveTo(Point2D::new(x, y)));
        self
    }
    
    /// Draw a line to a point
    pub fn line_to(mut self, x: f32, y: f32) -> Self {
        self.commands.push(PathCommand::LineTo(Point2D::new(x, y)));
        self
    }
    
    /// Draw a quadratic bezier curve
    pub fn quadratic_to(mut self, cx: f32, cy: f32, x: f32, y: f32) -> Self {
        self.commands.push(PathCommand::QuadraticTo(
            Point2D::new(cx, cy),
            Point2D::new(x, y)
        ));
        self
    }
    
    /// Draw a cubic bezier curve
    pub fn cubic_to(mut self, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32) -> Self {
        self.commands.push(PathCommand::CubicTo(
            Point2D::new(c1x, c1y),
            Point2D::new(c2x, c2y),
            Point2D::new(x, y)
        ));
        self
    }
    
    /// Close the current subpath
    pub fn close(mut self) -> Self {
        self.commands.push(PathCommand::Close);
        self
    }
    
    /// Set stroke color
    pub fn stroke(mut self, color: Color, width: f32) -> Self {
        self.stroke_color = Some(color);
        self.stroke_width = Some(width);
        self
    }
    
    /// Set fill color
    pub fn fill(mut self, color: Color) -> Self {
        self.fill_color = Some(color);
        self
    }
    
    /// Get the commands
    pub fn commands(&self) -> &[PathCommand] {
        &self.commands
    }
    
    /// Get stroke properties
    pub fn stroke_props(&self) -> Option<(Color, f32)> {
        match (self.stroke_color, self.stroke_width) {
            (Some(color), Some(width)) => Some((color, width)),
            _ => None,
        }
    }
    
    /// Get fill color
    pub fn fill_color(&self) -> Option<Color> {
        self.fill_color
    }
    
    /// Tessellate the path into triangles for rendering
    pub fn tessellate(&self) -> PathTessellation {
        let mut tessellation = PathTessellation::new();
        let mut current_pos = Point2D::new(0.0, 0.0);
        let mut subpath_start = Point2D::new(0.0, 0.0);
        
        for command in &self.commands {
            match command {
                PathCommand::MoveTo(p) => {
                    current_pos = *p;
                    subpath_start = *p;
                }
                PathCommand::LineTo(p) => {
                    if let Some((color, width)) = self.stroke_props() {
                        tessellation.add_line(current_pos, *p, width, color);
                    }
                    current_pos = *p;
                }
                PathCommand::QuadraticTo(control, end) => {
                    // Tessellate quadratic bezier curve
                    const SEGMENTS: usize = 16;
                    let mut prev = current_pos;
                    
                    for i in 1..=SEGMENTS {
                        let t = i as f32 / SEGMENTS as f32;
                        let point = Self::quadratic_bezier(current_pos, *control, *end, t);
                        
                        if let Some((color, width)) = self.stroke_props() {
                            tessellation.add_line(prev, point, width, color);
                        }
                        prev = point;
                    }
                    current_pos = *end;
                }
                PathCommand::CubicTo(c1, c2, end) => {
                    // Tessellate cubic bezier curve
                    const SEGMENTS: usize = 24;
                    let mut prev = current_pos;
                    
                    for i in 1..=SEGMENTS {
                        let t = i as f32 / SEGMENTS as f32;
                        let point = Self::cubic_bezier(current_pos, *c1, *c2, *end, t);
                        
                        if let Some((color, width)) = self.stroke_props() {
                            tessellation.add_line(prev, point, width, color);
                        }
                        prev = point;
                    }
                    current_pos = *end;
                }
                PathCommand::Close => {
                    if let Some((color, width)) = self.stroke_props() {
                        tessellation.add_line(current_pos, subpath_start, width, color);
                    }
                    current_pos = subpath_start;
                }
            }
        }
        
        tessellation
    }
    
    /// Calculate point on quadratic bezier curve
    fn quadratic_bezier(p0: Point2D, p1: Point2D, p2: Point2D, t: f32) -> Point2D {
        let t2 = t * t;
        let one_minus_t = 1.0 - t;
        let one_minus_t2 = one_minus_t * one_minus_t;
        
        p0 * one_minus_t2 + p1 * (2.0 * one_minus_t * t) + p2 * t2
    }
    
    /// Calculate point on cubic bezier curve
    fn cubic_bezier(p0: Point2D, p1: Point2D, p2: Point2D, p3: Point2D, t: f32) -> Point2D {
        let t2 = t * t;
        let t3 = t2 * t;
        let one_minus_t = 1.0 - t;
        let one_minus_t2 = one_minus_t * one_minus_t;
        let one_minus_t3 = one_minus_t2 * one_minus_t;
        
        p0 * one_minus_t3 + 
        p1 * (3.0 * one_minus_t2 * t) + 
        p2 * (3.0 * one_minus_t * t2) + 
        p3 * t3
    }
}

/// Tessellated path ready for rendering
pub struct PathTessellation {
    pub vertices: Vec<PathVertex>,
    pub indices: Vec<u16>,
}

/// Vertex data for path rendering
#[derive(Debug, Clone, Copy)]
pub struct PathVertex {
    pub position: [f32; 3],
    pub color: [f32; 4],
}

impl PathTessellation {
    fn new() -> Self {
        Self {
            vertices: Vec::new(),
            indices: Vec::new(),
        }
    }
    
    /// Add a line segment to the tessellation
    fn add_line(&mut self, start: Point2D, end: Point2D, width: f32, color: Color) {
        let dx = end.x - start.x;
        let dy = end.y - start.y;
        let len = (dx * dx + dy * dy).sqrt();
        
        if len > 0.0 {
            // Calculate perpendicular vector for line thickness
            let perp_x = -dy / len * width / 2.0;
            let perp_y = dx / len * width / 2.0;
            
            let color_array = color.to_array();
            let base = self.vertices.len() as u16;
            
            // Create a quad for the line segment
            self.vertices.extend_from_slice(&[
                PathVertex {
                    position: [start.x - perp_x, start.y - perp_y, 0.0],
                    color: color_array,
                },
                PathVertex {
                    position: [start.x + perp_x, start.y + perp_y, 0.0],
                    color: color_array,
                },
                PathVertex {
                    position: [end.x + perp_x, end.y + perp_y, 0.0],
                    color: color_array,
                },
                PathVertex {
                    position: [end.x - perp_x, end.y - perp_y, 0.0],
                    color: color_array,
                },
            ]);
            
            self.indices.extend_from_slice(&[
                base, base + 1, base + 2,
                base, base + 2, base + 3,
            ]);
        }
    }
}

/// Builder for creating SVG-like paths from string commands
pub struct PathBuilder;

impl PathBuilder {
    /// Parse SVG path data string
    pub fn from_svg(data: &str) -> Result<Path, String> {
        // This is a simplified version - full SVG path parsing would be more complex
        let mut path = Path::new();
        let mut chars = data.chars().peekable();
        let mut current_x = 0.0;
        let mut current_y = 0.0;
        
        while let Some(&cmd) = chars.peek() {
            chars.next(); // consume command
            
            match cmd {
                'M' | 'm' => {
                    let (x, y) = Self::parse_point(&mut chars)?;
                    if cmd == 'M' {
                        path = path.move_to(x, y);
                        current_x = x;
                        current_y = y;
                    } else {
                        path = path.move_to(current_x + x, current_y + y);
                        current_x += x;
                        current_y += y;
                    }
                }
                'L' | 'l' => {
                    let (x, y) = Self::parse_point(&mut chars)?;
                    if cmd == 'L' {
                        path = path.line_to(x, y);
                        current_x = x;
                        current_y = y;
                    } else {
                        path = path.line_to(current_x + x, current_y + y);
                        current_x += x;
                        current_y += y;
                    }
                }
                'Q' | 'q' => {
                    let (cx, cy) = Self::parse_point(&mut chars)?;
                    let (x, y) = Self::parse_point(&mut chars)?;
                    if cmd == 'Q' {
                        path = path.quadratic_to(cx, cy, x, y);
                        current_x = x;
                        current_y = y;
                    } else {
                        path = path.quadratic_to(
                            current_x + cx, current_y + cy,
                            current_x + x, current_y + y
                        );
                        current_x += x;
                        current_y += y;
                    }
                }
                'C' | 'c' => {
                    let (c1x, c1y) = Self::parse_point(&mut chars)?;
                    let (c2x, c2y) = Self::parse_point(&mut chars)?;
                    let (x, y) = Self::parse_point(&mut chars)?;
                    if cmd == 'C' {
                        path = path.cubic_to(c1x, c1y, c2x, c2y, x, y);
                        current_x = x;
                        current_y = y;
                    } else {
                        path = path.cubic_to(
                            current_x + c1x, current_y + c1y,
                            current_x + c2x, current_y + c2y,
                            current_x + x, current_y + y
                        );
                        current_x += x;
                        current_y += y;
                    }
                }
                'Z' | 'z' => {
                    path = path.close();
                }
                ' ' | ',' | '\n' | '\r' | '\t' => {
                    // Skip whitespace
                }
                _ => {
                    return Err(format!("Unknown path command: {}", cmd));
                }
            }
        }
        
        Ok(path)
    }
    
    /// Parse a coordinate pair from the character stream
    fn parse_point(chars: &mut std::iter::Peekable<std::str::Chars>) -> Result<(f32, f32), String> {
        let x = Self::parse_number(chars)?;
        let y = Self::parse_number(chars)?;
        Ok((x, y))
    }
    
    /// Parse a number from the character stream
    fn parse_number(chars: &mut std::iter::Peekable<std::str::Chars>) -> Result<f32, String> {
        // Skip whitespace
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() || ch == ',' {
                chars.next();
            } else {
                break;
            }
        }
        
        let mut num_str = String::new();
        let mut has_dot = false;
        
        // Handle negative sign
        if chars.peek() == Some(&'-') {
            num_str.push('-');
            chars.next();
        }
        
        // Parse digits
        while let Some(&ch) = chars.peek() {
            if ch.is_digit(10) {
                num_str.push(ch);
                chars.next();
            } else if ch == '.' && !has_dot {
                num_str.push(ch);
                has_dot = true;
                chars.next();
            } else {
                break;
            }
        }
        
        num_str.parse()
            .map_err(|_| format!("Invalid number: {}", num_str))
    }
}