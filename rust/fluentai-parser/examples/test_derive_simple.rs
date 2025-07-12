use fluentai_parser::parse_flc;

fn main() {
    println!("Testing derive attributes (simple cases)...\n");

    // Test basic struct derive
    match parse_flc("private struct Point { x: int, y: int }.derive(Debug)") {
        Ok(_) => println!("✓ Basic struct derive works"),
        Err(e) => println!("✗ Basic struct derive failed: {}", e),
    }

    // Test enum with simple variants
    match parse_flc("private enum Color { Red, Green, Blue }.derive(Debug)") {
        Ok(_) => println!("✓ Simple enum derive works"),
        Err(e) => println!("✗ Simple enum derive failed: {}", e),
    }

    // Test empty struct with derive
    match parse_flc("private struct Empty { }.derive(Default)") {
        Ok(_) => println!("✓ Empty struct derive works"),
        Err(e) => println!("✗ Empty struct derive failed: {}", e),
    }
}
