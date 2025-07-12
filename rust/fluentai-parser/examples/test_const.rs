use fluentai_parser::parse_flc;

fn main() {
    println!("Testing const definitions...\n");
    
    // Test basic const
    match parse_flc("private const MAX_SIZE = 100") {
        Ok(_) => println!("✓ Basic const definition works"),
        Err(e) => println!("✗ Basic const definition failed: {}", e),
    }
    
    // Test public const
    match parse_flc("public const API_KEY = \"secret123\"") {
        Ok(_) => println!("✓ Public const definition works"),
        Err(e) => println!("✗ Public const definition failed: {}", e),
    }
    
    // Test const with expression
    match parse_flc("private const DEFAULT_TIMEOUT = 60 * 1000") {
        Ok(_) => println!("✓ Const with expression works"),
        Err(e) => println!("✗ Const with expression failed: {}", e),
    }
    
    // Test const with function call
    match parse_flc("private const VERSION = get_version()") {
        Ok(_) => println!("✓ Const with function call works"),
        Err(e) => println!("✗ Const with function call failed: {}", e),
    }
}