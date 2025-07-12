use fluentai_parser::parse_flc;

fn main() {
    println!("Testing destructuring in let bindings...\n");

    // Test basic destructuring
    match parse_flc("let {x, y} = point; x + y") {
        Ok(_) => println!("✓ Basic destructuring works"),
        Err(e) => println!("✗ Basic destructuring failed: {}", e),
    }

    // Test single field destructuring
    match parse_flc("let {name} = user; name") {
        Ok(_) => println!("✓ Single field destructuring works"),
        Err(e) => println!("✗ Single field destructuring failed: {}", e),
    }

    // Test multiple field destructuring
    match parse_flc("let {width, height, depth} = dimensions; width * height * depth") {
        Ok(_) => println!("✓ Multiple field destructuring works"),
        Err(e) => println!("✗ Multiple field destructuring failed: {}", e),
    }

    // Test nested expression
    match parse_flc("let {x, y} = get_position(); x") {
        Ok(_) => println!("✓ Destructuring with function call works"),
        Err(e) => println!("✗ Destructuring with function call failed: {}", e),
    }
}
