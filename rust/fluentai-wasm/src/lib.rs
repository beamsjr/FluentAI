//! Minimal WASM bindings for FluentAI

use wasm_bindgen::prelude::*;
use serde_json::json;

// Set up panic hook for better error messages
pub fn set_panic_hook() {
    console_error_panic_hook::set_once();
}

/// Initialize WASM module
#[wasm_bindgen(start)]
pub fn init() {
    set_panic_hook();
}

/// Simple test function
#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}! FluentAI WASM is working.", name)
}

/// Parse FluentAI code and return result
#[wasm_bindgen]
pub fn parse_fluentai(source: &str) -> Result<String, JsValue> {
    use fluentai_parser::flc_parser::Parser;
    
    let parser = Parser::new(source);
    match parser.parse() {
        Ok(ast) => {
            Ok(json!({
                "success": true,
                "node_count": ast.nodes.len(),
                "message": "Parse successful"
            }).to_string())
        }
        Err(e) => {
            Err(JsValue::from_str(&format!("Parse error: {}", e)))
        }
    }
}

/// Validate FluentAI code
#[wasm_bindgen]
pub fn validate_fluentai(source: &str) -> bool {
    use fluentai_parser::flc_parser::Parser;
    
    let parser = Parser::new(source);
    parser.parse().is_ok()
}