/// Tests for text rendering functionality
use fluentai_renderer::simple_text::{get_char_bitmap, render_text};
use fluentai_renderer::primitives::{Color, Point2D};

mod test_helpers;
use test_helpers::*;

#[test]
fn test_bitmap_font_ascii_characters() {
    // Test that we have bitmaps for all printable ASCII characters
    for ch in 32u8..127u8 {
        let character = ch as char;
        let bitmap = get_char_bitmap(character);
        
        if character == ' ' {
            // Space should return None
            assert!(bitmap.is_none(), "Space character should return None");
        } else {
            // All other printable ASCII should have bitmaps
            assert!(bitmap.is_some(), "Missing bitmap for character: {} ({})", character, ch);
            
            // Verify bitmap is 8 bytes (8x8 pixels)
            let bitmap_data = bitmap.unwrap();
            assert_eq!(bitmap_data.len(), 8, "Bitmap should be 8 bytes for 8x8 font");
        }
    }
}

#[test]
fn test_bitmap_font_special_characters() {
    // Test some specific characters have expected patterns
    let test_cases = vec![
        ('A', true),   // Should have bitmap
        ('0', true),   // Should have bitmap
        ('!', true),   // Should have bitmap
        ('@', true),   // Should have bitmap
        (' ', false),  // Space should not have bitmap
        ('\n', false), // Newline should not have bitmap
        ('€', false),  // Non-ASCII should not have bitmap
    ];
    
    for (ch, should_have_bitmap) in test_cases {
        let bitmap = get_char_bitmap(ch);
        assert_eq!(
            bitmap.is_some(),
            should_have_bitmap,
            "Character '{}' bitmap presence mismatch",
            ch
        );
    }
}

#[test]
fn test_render_text_simple() {
    let text = "Hello";
    let position = Point2D::new(10.0, 20.0);
    let color = Color::white();
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // Each character should generate 6 vertices (2 triangles)
    let expected_vertices = text.len() * 6;
    assert_eq!(
        vertices.len(),
        expected_vertices,
        "Should have {} vertices for {} characters",
        expected_vertices,
        text.len()
    );
    
    // Verify first vertex position (top-left of 'H')
    assert_eq!(vertices[0].position[0], 10.0);
    assert_eq!(vertices[0].position[1], 20.0);
    
    // Verify color is set correctly
    assert_eq!(vertices[0].color[0], 1.0); // R
    assert_eq!(vertices[0].color[1], 1.0); // G
    assert_eq!(vertices[0].color[2], 1.0); // B
    assert_eq!(vertices[0].color[3], 1.0); // A
}

#[test]
fn test_render_text_with_spacing() {
    let text = "AB";
    let position = Point2D::new(0.0, 0.0);
    let color = Color::red();
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // Check that second character is positioned after first
    // First char vertices: 0-5, Second char vertices: 6-11
    let first_char_x = vertices[0].position[0];
    let second_char_x = vertices[6].position[0];
    
    // Second character should be 10 pixels to the right (8 pixel char + 2 pixel spacing)
    assert_eq!(second_char_x, first_char_x + 10.0);
}

#[test]
fn test_render_text_with_scale() {
    let text = "X";
    let position = Point2D::new(0.0, 0.0);
    let color = Color::blue();
    let scale = 2.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // With scale 2.0, character should be 16x16 instead of 8x8
    // Check the bottom-right vertex (index 4)
    assert_eq!(vertices[4].position[0], 16.0); // x = 0 + 16
    assert_eq!(vertices[4].position[1], 16.0); // y = 0 + 16
}

#[test]
fn test_render_text_empty_string() {
    let text = "";
    let position = Point2D::new(0.0, 0.0);
    let color = Color::black();
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    assert_eq!(vertices.len(), 0, "Empty string should produce no vertices");
}

#[test]
fn test_render_text_with_spaces() {
    let text = "A B";
    let position = Point2D::new(0.0, 0.0);
    let color = Color::green();
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // Should have vertices for 'A' and 'B' only (not space)
    assert_eq!(vertices.len(), 12, "Should have 12 vertices for 2 characters");
    
    // Check that 'B' is positioned correctly (after 'A' and space)
    let b_x = vertices[6].position[0];
    assert_eq!(b_x, 20.0, "'B' should be at x=20 (0 + 10 for 'A' + 10 for space)");
}

#[test]
fn test_render_text_texture_coordinates() {
    let text = "A";
    let position = Point2D::new(0.0, 0.0);
    let color = Color::white();
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // Check texture coordinates for the quad
    // Top-left
    assert_eq!(vertices[0].tex_coords[0], 0.0);
    assert_eq!(vertices[0].tex_coords[1], 0.0);
    
    // Top-right
    assert_eq!(vertices[1].tex_coords[0], 1.0);
    assert_eq!(vertices[1].tex_coords[1], 0.0);
    
    // Bottom-left
    assert_eq!(vertices[2].tex_coords[0], 0.0);
    assert_eq!(vertices[2].tex_coords[1], 1.0);
    
    // Bottom-right (vertex 4 in the quad)
    assert_eq!(vertices[4].tex_coords[0], 1.0);
    assert_eq!(vertices[4].tex_coords[1], 1.0);
}

#[test]
fn test_render_text_multi_line_simulation() {
    // Simulate multi-line by rendering at different Y positions
    let line1 = "Line 1";
    let line2 = "Line 2";
    let color = Color::white();
    let scale = 1.0;
    let line_height = 10.0;
    
    let vertices1 = render_text(line1, Point2D::new(0.0, 0.0), color, scale);
    let vertices2 = render_text(line2, Point2D::new(0.0, line_height), color, scale);
    
    // Verify line 2 is below line 1
    assert_eq!(vertices2[0].position[1], line_height);
}

#[test]
fn test_character_bitmap_data_integrity() {
    // Test a few known characters have non-zero bitmap data
    let test_chars = vec!['A', 'B', '1', '!', '#'];
    
    for ch in test_chars {
        let bitmap = get_char_bitmap(ch).expect(&format!("Should have bitmap for '{}'", ch));
        
        // Check that bitmap has some non-zero data (not blank)
        let has_data = bitmap.iter().any(|&byte| byte != 0);
        assert!(has_data, "Character '{}' bitmap should have non-zero data", ch);
    }
}

#[test]
fn test_render_text_color_consistency() {
    let text = "RGB";
    let position = Point2D::new(0.0, 0.0);
    let color = Color::new(0.25, 0.5, 0.75, 0.8);
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // All vertices should have the same color
    for vertex in &vertices {
        assert!(approx_equal(vertex.color[0], 0.25, 0.001), "Red component mismatch");
        assert!(approx_equal(vertex.color[1], 0.5, 0.001), "Green component mismatch");
        assert!(approx_equal(vertex.color[2], 0.75, 0.001), "Blue component mismatch");
        assert!(approx_equal(vertex.color[3], 0.8, 0.001), "Alpha component mismatch");
    }
}

#[test]
fn test_render_text_special_characters_handling() {
    // Test that non-ASCII characters are skipped gracefully
    let text = "Hello🌟World"; // Contains emoji
    let position = Point2D::new(0.0, 0.0);
    let color = Color::white();
    let scale = 1.0;
    
    let vertices = render_text(text, position, color, scale);
    
    // Should only render "HelloWorld" (10 characters * 6 vertices)
    // The emoji should be skipped
    let expected_vertex_count = 10 * 6;
    assert_eq!(
        vertices.len(),
        expected_vertex_count,
        "Should skip non-ASCII characters"
    );
}