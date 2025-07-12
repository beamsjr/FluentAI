use fluentai_parser::parse_flc;

fn main() {
    println!("Testing import aliases...\n");

    // Test basic alias
    match parse_flc("use std::collections::HashMap as Map;") {
        Ok(_) => println!("✓ Basic import alias works"),
        Err(e) => println!("✗ Basic import alias failed: {}", e),
    }

    // Test simple module alias
    match parse_flc("use http::client as http_client;") {
        Ok(_) => println!("✓ Module alias works"),
        Err(e) => println!("✗ Module alias failed: {}", e),
    }

    // Test import with items and aliases
    match parse_flc("use math::{sin as sine, cos as cosine};") {
        Ok(_) => println!("✓ Import items with aliases works"),
        Err(e) => println!("✗ Import items with aliases failed: {}", e),
    }

    // Test mixed imports
    match parse_flc("use utils::{Logger, Timer as StopWatch};") {
        Ok(_) => println!("✓ Mixed imports (with and without alias) works"),
        Err(e) => println!("✗ Mixed imports failed: {}", e),
    }
}
