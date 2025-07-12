use fluentai_parser::parse_flc;

fn main() {
    println!("Testing loop syntax...\n");
    
    // Test for loop
    match parse_flc("for x in [1, 2, 3] { x.print() }") {
        Ok(_) => println!("✓ For loop works"),
        Err(e) => println!("✗ For loop failed: {}", e),
    }
    
    // Test for loop with expression
    match parse_flc("for item in list { item * 2 }") {
        Ok(_) => println!("✓ For loop with expression works"),
        Err(e) => println!("✗ For loop with expression failed: {}", e),
    }
    
    // Test while loop
    match parse_flc("while x > 0 { x - 1 }") {
        Ok(_) => println!("✓ While loop works"),
        Err(e) => println!("✗ While loop failed: {}", e),
    }
    
    // Test while with complex condition
    match parse_flc("while x < 10 && y > 0 { process(x, y) }") {
        Ok(_) => println!("✓ While loop with complex condition works"),
        Err(e) => println!("✗ While loop with complex condition failed: {}", e),
    }
}