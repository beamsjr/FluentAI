use fluentai_parser::parse_flc;

fn main() {
    println!("Testing derive attributes...\n");

    // Test basic struct derive
    match parse_flc("private struct Point { x: int, y: int }.derive(Debug)") {
        Ok(_) => println!("✓ Basic struct derive works"),
        Err(e) => println!("✗ Basic struct derive failed: {}", e),
    }

    // Test multiple derives
    match parse_flc("public struct User { name: string, age: int }.derive(Debug, Clone)") {
        Ok(_) => println!("✓ Multiple derive traits work"),
        Err(e) => println!("✗ Multiple derive traits failed: {}", e),
    }

    // Test enum derive
    match parse_flc("private enum Status { Active, Inactive }.derive(Debug)") {
        Ok(_) => println!("✓ Enum derive works"),
        Err(e) => println!("✗ Enum derive failed: {}", e),
    }

    // Test complex enum with derive
    match parse_flc("private enum Result { Ok(value), Err(error) }.derive(Debug, Clone, PartialEq)")
    {
        Ok(_) => println!("✓ Complex enum with multiple derives works"),
        Err(e) => println!("✗ Complex enum with derives failed: {}", e),
    }

    // Test struct without derive
    match parse_flc("private struct Simple { field: string }") {
        Ok(_) => println!("✓ Struct without derive still works"),
        Err(e) => println!("✗ Struct without derive failed: {}", e),
    }
}
