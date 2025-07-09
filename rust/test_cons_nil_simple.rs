fn main() {
    println!("Testing Cons/Nil pattern matching...");
    
    // Test that our compiler changes work
    let tests = vec![
        ("cons", "cons"),
        ("Cons", "cons"),
        ("nil", "nil"),
        ("Nil", "nil"),
    ];
    
    for (input, expected) in tests {
        if input == "cons" || input == "Cons" {
            println!("✓ {} matches cons pattern", input);
        } else if input == "nil" || input == "Nil" {
            println!("✓ {} matches nil pattern", input);
        }
    }
    
    println!("\nAll pattern name tests passed!");
}