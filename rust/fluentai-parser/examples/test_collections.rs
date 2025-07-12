use fluentai_parser::parse_flc;

fn main() {
    println!("Testing collection literals...\n");
    
    // Test list literal
    match parse_flc("[1, 2, 3]") {
        Ok(_) => println!("✓ List literal works"),
        Err(e) => println!("✗ List literal failed: {}", e),
    }
    
    // Test empty map
    match parse_flc("{}") {
        Ok(_) => println!("✓ Empty map works"),
        Err(e) => println!("✗ Empty map failed: {}", e),
    }
    
    // Test map with entries
    match parse_flc(r#"{"key": "value"}"#) {
        Ok(_) => println!("✓ Map literal works"),
        Err(e) => println!("✗ Map literal failed: {}", e),
    }
    
    // Test empty set
    match parse_flc("#{}") {
        Ok(_) => println!("✓ Empty set works"),
        Err(e) => println!("✗ Empty set failed: {}", e),
    }
    
    // Test set with items
    match parse_flc("#{1, 2, 3}") {
        Ok(_) => println!("✓ Set literal works"),
        Err(e) => println!("✗ Set literal failed: {}", e),
    }
}