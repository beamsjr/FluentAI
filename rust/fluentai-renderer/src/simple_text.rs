// Simple text rendering without external font dependencies
// This provides basic text rendering capabilities using a built-in bitmap font

use crate::primitives::Color;
use cgmath::Vector2;

/// Horizontal text alignment
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HorizontalAlign {
    Left,
    Center,
    Right,
    Justify,
}

/// Vertical text alignment
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum VerticalAlign {
    Top,
    Middle,
    Bottom,
}

/// Simple bitmap font for basic text rendering
/// Each character is 8x8 pixels
pub struct SimpleBitmapFont;

impl SimpleBitmapFont {
    /// Get bitmap data for a character (8x8 pixels, 1 bit per pixel)
    pub fn get_char_bitmap(ch: char) -> Option<[u8; 8]> {
        // Basic ASCII characters (32-126)
        match ch {
            ' ' => Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            '!' => Some([0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 0x18, 0x00]),
            '"' => Some([0x66, 0x66, 0x66, 0x00, 0x00, 0x00, 0x00, 0x00]),
            '#' => Some([0x66, 0x66, 0xFF, 0x66, 0xFF, 0x66, 0x66, 0x00]),
            '$' => Some([0x18, 0x3E, 0x60, 0x3C, 0x06, 0x7C, 0x18, 0x00]),
            '%' => Some([0x62, 0x66, 0x0C, 0x18, 0x30, 0x66, 0x46, 0x00]),
            '&' => Some([0x3C, 0x66, 0x3C, 0x38, 0x67, 0x66, 0x3F, 0x00]),
            '\'' => Some([0x06, 0x0C, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00]),
            '(' => Some([0x0C, 0x18, 0x30, 0x30, 0x30, 0x18, 0x0C, 0x00]),
            ')' => Some([0x30, 0x18, 0x0C, 0x0C, 0x0C, 0x18, 0x30, 0x00]),
            '*' => Some([0x00, 0x66, 0x3C, 0xFF, 0x3C, 0x66, 0x00, 0x00]),
            '+' => Some([0x00, 0x18, 0x18, 0x7E, 0x18, 0x18, 0x00, 0x00]),
            ',' => Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18, 0x30]),
            '-' => Some([0x00, 0x00, 0x00, 0x7E, 0x00, 0x00, 0x00, 0x00]),
            '.' => Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18, 0x00]),
            '/' => Some([0x00, 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0x00]),
            '0' => Some([0x3C, 0x66, 0x6E, 0x76, 0x66, 0x66, 0x3C, 0x00]),
            '1' => Some([0x18, 0x18, 0x38, 0x18, 0x18, 0x18, 0x7E, 0x00]),
            '2' => Some([0x3C, 0x66, 0x06, 0x0C, 0x30, 0x60, 0x7E, 0x00]),
            '3' => Some([0x3C, 0x66, 0x06, 0x1C, 0x06, 0x66, 0x3C, 0x00]),
            '4' => Some([0x06, 0x0E, 0x1E, 0x66, 0x7F, 0x06, 0x06, 0x00]),
            '5' => Some([0x7E, 0x60, 0x7C, 0x06, 0x06, 0x66, 0x3C, 0x00]),
            '6' => Some([0x3C, 0x66, 0x60, 0x7C, 0x66, 0x66, 0x3C, 0x00]),
            '7' => Some([0x7E, 0x66, 0x0C, 0x18, 0x18, 0x18, 0x18, 0x00]),
            '8' => Some([0x3C, 0x66, 0x66, 0x3C, 0x66, 0x66, 0x3C, 0x00]),
            '9' => Some([0x3C, 0x66, 0x66, 0x3E, 0x06, 0x66, 0x3C, 0x00]),
            ':' => Some([0x00, 0x00, 0x18, 0x00, 0x00, 0x18, 0x00, 0x00]),
            ';' => Some([0x00, 0x00, 0x18, 0x00, 0x00, 0x18, 0x18, 0x30]),
            '<' => Some([0x0E, 0x18, 0x30, 0x60, 0x30, 0x18, 0x0E, 0x00]),
            '=' => Some([0x00, 0x00, 0x7E, 0x00, 0x7E, 0x00, 0x00, 0x00]),
            '>' => Some([0x70, 0x18, 0x0C, 0x06, 0x0C, 0x18, 0x70, 0x00]),
            '?' => Some([0x3C, 0x66, 0x06, 0x0C, 0x18, 0x00, 0x18, 0x00]),
            '@' => Some([0x3C, 0x66, 0x6E, 0x6E, 0x60, 0x62, 0x3C, 0x00]),
            'A' => Some([0x18, 0x3C, 0x66, 0x7E, 0x66, 0x66, 0x66, 0x00]),
            'B' => Some([0x7C, 0x66, 0x66, 0x7C, 0x66, 0x66, 0x7C, 0x00]),
            'C' => Some([0x3C, 0x66, 0x60, 0x60, 0x60, 0x66, 0x3C, 0x00]),
            'D' => Some([0x78, 0x6C, 0x66, 0x66, 0x66, 0x6C, 0x78, 0x00]),
            'E' => Some([0x7E, 0x60, 0x60, 0x78, 0x60, 0x60, 0x7E, 0x00]),
            'F' => Some([0x7E, 0x60, 0x60, 0x78, 0x60, 0x60, 0x60, 0x00]),
            'G' => Some([0x3C, 0x66, 0x60, 0x6E, 0x66, 0x66, 0x3C, 0x00]),
            'H' => Some([0x66, 0x66, 0x66, 0x7E, 0x66, 0x66, 0x66, 0x00]),
            'I' => Some([0x3C, 0x18, 0x18, 0x18, 0x18, 0x18, 0x3C, 0x00]),
            'J' => Some([0x1E, 0x0C, 0x0C, 0x0C, 0x0C, 0x6C, 0x38, 0x00]),
            'K' => Some([0x66, 0x6C, 0x78, 0x70, 0x78, 0x6C, 0x66, 0x00]),
            'L' => Some([0x60, 0x60, 0x60, 0x60, 0x60, 0x60, 0x7E, 0x00]),
            'M' => Some([0x63, 0x77, 0x7F, 0x6B, 0x63, 0x63, 0x63, 0x00]),
            'N' => Some([0x66, 0x76, 0x7E, 0x7E, 0x6E, 0x66, 0x66, 0x00]),
            'O' => Some([0x3C, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x00]),
            'P' => Some([0x7C, 0x66, 0x66, 0x7C, 0x60, 0x60, 0x60, 0x00]),
            'Q' => Some([0x3C, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x0E, 0x00]),
            'R' => Some([0x7C, 0x66, 0x66, 0x7C, 0x78, 0x6C, 0x66, 0x00]),
            'S' => Some([0x3C, 0x66, 0x60, 0x3C, 0x06, 0x66, 0x3C, 0x00]),
            'T' => Some([0x7E, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00]),
            'U' => Some([0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x00]),
            'V' => Some([0x66, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x18, 0x00]),
            'W' => Some([0x63, 0x63, 0x63, 0x6B, 0x7F, 0x77, 0x63, 0x00]),
            'X' => Some([0x66, 0x66, 0x3C, 0x18, 0x3C, 0x66, 0x66, 0x00]),
            'Y' => Some([0x66, 0x66, 0x66, 0x3C, 0x18, 0x18, 0x18, 0x00]),
            'Z' => Some([0x7E, 0x06, 0x0C, 0x18, 0x30, 0x60, 0x7E, 0x00]),
            '[' => Some([0x3C, 0x30, 0x30, 0x30, 0x30, 0x30, 0x3C, 0x00]),
            '\\' => Some([0x00, 0x60, 0x30, 0x18, 0x0C, 0x06, 0x03, 0x00]),
            ']' => Some([0x3C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x3C, 0x00]),
            '^' => Some([0x18, 0x3C, 0x66, 0x00, 0x00, 0x00, 0x00, 0x00]),
            '_' => Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF]),
            '`' => Some([0x30, 0x18, 0x0C, 0x00, 0x00, 0x00, 0x00, 0x00]),
            'a' => Some([0x00, 0x00, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00]),
            'b' => Some([0x00, 0x60, 0x60, 0x7C, 0x66, 0x66, 0x7C, 0x00]),
            'c' => Some([0x00, 0x00, 0x3C, 0x60, 0x60, 0x60, 0x3C, 0x00]),
            'd' => Some([0x00, 0x06, 0x06, 0x3E, 0x66, 0x66, 0x3E, 0x00]),
            'e' => Some([0x00, 0x00, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00]),
            'f' => Some([0x00, 0x0E, 0x18, 0x3E, 0x18, 0x18, 0x18, 0x00]),
            'g' => Some([0x00, 0x00, 0x3E, 0x66, 0x66, 0x3E, 0x06, 0x7C]),
            'h' => Some([0x00, 0x60, 0x60, 0x7C, 0x66, 0x66, 0x66, 0x00]),
            'i' => Some([0x00, 0x18, 0x00, 0x38, 0x18, 0x18, 0x3C, 0x00]),
            'j' => Some([0x00, 0x06, 0x00, 0x06, 0x06, 0x06, 0x06, 0x3C]),
            'k' => Some([0x00, 0x60, 0x60, 0x6C, 0x78, 0x6C, 0x66, 0x00]),
            'l' => Some([0x00, 0x38, 0x18, 0x18, 0x18, 0x18, 0x3C, 0x00]),
            'm' => Some([0x00, 0x00, 0x66, 0x7F, 0x7F, 0x6B, 0x63, 0x00]),
            'n' => Some([0x00, 0x00, 0x7C, 0x66, 0x66, 0x66, 0x66, 0x00]),
            'o' => Some([0x00, 0x00, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00]),
            'p' => Some([0x00, 0x00, 0x7C, 0x66, 0x66, 0x7C, 0x60, 0x60]),
            'q' => Some([0x00, 0x00, 0x3E, 0x66, 0x66, 0x3E, 0x06, 0x06]),
            'r' => Some([0x00, 0x00, 0x7C, 0x66, 0x60, 0x60, 0x60, 0x00]),
            's' => Some([0x00, 0x00, 0x3E, 0x60, 0x3C, 0x06, 0x7C, 0x00]),
            't' => Some([0x00, 0x18, 0x7E, 0x18, 0x18, 0x18, 0x0E, 0x00]),
            'u' => Some([0x00, 0x00, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00]),
            'v' => Some([0x00, 0x00, 0x66, 0x66, 0x66, 0x3C, 0x18, 0x00]),
            'w' => Some([0x00, 0x00, 0x63, 0x6B, 0x7F, 0x3E, 0x36, 0x00]),
            'x' => Some([0x00, 0x00, 0x66, 0x3C, 0x18, 0x3C, 0x66, 0x00]),
            'y' => Some([0x00, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x0C, 0x78]),
            'z' => Some([0x00, 0x00, 0x7E, 0x0C, 0x18, 0x30, 0x7E, 0x00]),
            '{' => Some([0x0E, 0x18, 0x18, 0x70, 0x18, 0x18, 0x0E, 0x00]),
            '|' => Some([0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00]),
            '}' => Some([0x70, 0x18, 0x18, 0x0E, 0x18, 0x18, 0x70, 0x00]),
            '~' => Some([0x76, 0xDC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            _ => None,
        }
    }
}

/// Simple text layout for basic rendering
pub struct SimpleTextLayout {
    pub text: String,
    pub position: Vector2<f32>,
    pub font_size: f32,
    pub color: Color,
    pub max_width: Option<f32>,
    pub horizontal_align: Option<HorizontalAlign>,
    pub vertical_align: Option<VerticalAlign>,
}

impl SimpleTextLayout {
    pub fn new(text: impl Into<String>, x: f32, y: f32) -> Self {
        Self {
            text: text.into(),
            position: Vector2::new(x, y),
            font_size: 16.0,
            color: Color::new(1.0, 1.0, 1.0, 1.0),
            max_width: None,
            horizontal_align: None,
            vertical_align: None,
        }
    }
    
    pub fn with_size(mut self, size: f32) -> Self {
        self.font_size = size;
        self
    }
    
    pub fn with_color(mut self, color: Color) -> Self {
        self.color = color;
        self
    }
    
    pub fn with_max_width(mut self, width: f32) -> Self {
        self.max_width = Some(width);
        self
    }
    
    pub fn with_horizontal_align(mut self, align: HorizontalAlign) -> Self {
        self.horizontal_align = Some(align);
        self
    }
    
    pub fn with_vertical_align(mut self, align: VerticalAlign) -> Self {
        self.vertical_align = Some(align);
        self
    }
    
    /// Generate quads for rendering this text
    pub fn generate_quads(&self) -> Vec<TextQuad> {
        let mut quads = Vec::new();
        let char_width = self.font_size * 0.75; // Approximate character width
        let char_height = self.font_size;
        let line_height = char_height * 1.2; // Line spacing
        
        // Split text into lines
        let lines: Vec<&str> = self.text.lines().collect();
        
        // Calculate total text height for vertical alignment
        let total_height = lines.len() as f32 * line_height;
        
        // Calculate starting Y position based on vertical alignment
        let start_y = match &self.vertical_align {
            Some(VerticalAlign::Top) => self.position.y,
            Some(VerticalAlign::Middle) => self.position.y - total_height / 2.0,
            Some(VerticalAlign::Bottom) => self.position.y - total_height,
            None => self.position.y, // Default to top
        };
        
        let mut y = start_y;
        
        for line in lines {
            // Handle word wrapping if max_width is set
            let wrapped_lines = if let Some(max_w) = self.max_width {
                self.wrap_line(line, max_w, char_width)
            } else {
                vec![line.to_string()]
            };
            
            for wrapped_line in wrapped_lines {
                // Calculate line width for horizontal alignment
                let line_width = wrapped_line.chars().count() as f32 * char_width;
                
                // Calculate starting X position based on alignment
                let start_x = match &self.horizontal_align {
                    Some(HorizontalAlign::Left) => self.position.x,
                    Some(HorizontalAlign::Center) => {
                        if let Some(max_w) = self.max_width {
                            self.position.x + (max_w - line_width) / 2.0
                        } else {
                            self.position.x - line_width / 2.0
                        }
                    },
                    Some(HorizontalAlign::Right) => {
                        if let Some(max_w) = self.max_width {
                            self.position.x + max_w - line_width
                        } else {
                            self.position.x - line_width
                        }
                    },
                    Some(HorizontalAlign::Justify) => self.position.x,
                    None => self.position.x, // Default to left
                };
                
                let mut x = start_x;
                let chars: Vec<char> = wrapped_line.chars().collect();
                let char_count = chars.len();
                
                // Calculate spacing for justified text
                let extra_spacing = if matches!(&self.horizontal_align, Some(HorizontalAlign::Justify)) 
                    && char_count > 1 
                    && self.max_width.is_some() {
                    let spaces_count = wrapped_line.matches(' ').count() as f32;
                    if spaces_count > 0.0 {
                        (self.max_width.unwrap() - line_width) / spaces_count
                    } else {
                        0.0
                    }
                } else {
                    0.0
                };
                
                for ch in chars {
                    if let Some(bitmap) = SimpleBitmapFont::get_char_bitmap(ch) {
                        quads.push(TextQuad {
                            position: Vector2::new(x, y),
                            size: Vector2::new(char_width, char_height),
                            color: self.color,
                            character: ch,
                            bitmap,
                        });
                    }
                    
                    x += char_width;
                    if ch == ' ' && extra_spacing > 0.0 {
                        x += extra_spacing;
                    }
                }
                
                y += line_height;
            }
        }
        
        quads
    }
    
    /// Wrap a line of text to fit within max_width
    fn wrap_line(&self, line: &str, max_width: f32, char_width: f32) -> Vec<String> {
        let mut wrapped = Vec::new();
        let words: Vec<&str> = line.split_whitespace().collect();
        
        if words.is_empty() {
            wrapped.push(String::new());
            return wrapped;
        }
        
        let mut current_line = String::new();
        let mut current_width = 0.0;
        
        for word in words {
            let word_width = word.chars().count() as f32 * char_width;
            let space_width = char_width;
            
            if current_width + word_width <= max_width || current_line.is_empty() {
                if !current_line.is_empty() {
                    current_line.push(' ');
                    current_width += space_width;
                }
                current_line.push_str(word);
                current_width += word_width;
            } else {
                wrapped.push(current_line);
                current_line = word.to_string();
                current_width = word_width;
            }
        }
        
        if !current_line.is_empty() {
            wrapped.push(current_line);
        }
        
        wrapped
    }
}

/// A single character quad for rendering
#[derive(Debug, Clone)]
pub struct TextQuad {
    pub position: Vector2<f32>,
    pub size: Vector2<f32>,
    pub color: Color,
    pub character: char,
    pub bitmap: [u8; 8],
}

impl TextQuad {
    /// Convert to rectangle vertices for rendering
    /// Each character is rendered as a textured quad
    pub fn to_vertices(&self) -> Vec<crate::primitives::Vertex> {
        
        let x = self.position.x;
        let y = self.position.y;
        let w = self.size.x;
        let h = self.size.y;
        let color = self.color.to_array();
        
        vec![
            crate::primitives::Vertex { position: [x, y, 0.0], color },
            crate::primitives::Vertex { position: [x + w, y, 0.0], color },
            crate::primitives::Vertex { position: [x + w, y + h, 0.0], color },
            crate::primitives::Vertex { position: [x, y + h, 0.0], color },
        ]
    }
}