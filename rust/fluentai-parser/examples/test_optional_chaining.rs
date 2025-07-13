use fluentai_parser::parse_flc;

fn main() {
    println!("Testing optional chaining...\n");

    // Test basic optional chaining
    match parse_flc("user.?name") {
        Ok(_) => println!("✓ Basic optional chaining works"),
        Err(e) => println!("✗ Basic optional chaining failed: {}", e),
    }

    // Test optional method call
    match parse_flc("obj.?method()") {
        Ok(_) => println!("✓ Optional method call works"),
        Err(e) => println!("✗ Optional method call failed: {}", e),
    }

    // Test optional method with args
    match parse_flc("user.?send(message)") {
        Ok(_) => println!("✓ Optional method with args works"),
        Err(e) => println!("✗ Optional method with args failed: {}", e),
    }

    // Test chained optional
    match parse_flc("company.?department.?manager") {
        Ok(_) => println!("✓ Chained optional access works"),
        Err(e) => println!("✗ Chained optional access failed: {}", e),
    }

    // Test mixed chaining
    match parse_flc("user.profile.?address.?city") {
        Ok(_) => println!("✓ Mixed regular and optional chaining works"),
        Err(e) => println!("✗ Mixed chaining failed: {}", e),
    }
}
