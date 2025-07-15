//! Minimal FluentAI VM WASM bindings
//! 
//! This module provides minimal WASM bindings for compiling FluentAI code
//! without heavy dependencies like tokio or effects.

use wasm_bindgen::prelude::*;

/// Simple compiler interface for WASM
#[wasm_bindgen]
pub struct FluentAICompiler;

#[wasm_bindgen]
impl FluentAICompiler {
    /// Create a new compiler instance
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        console_error_panic_hook::set_once();
        FluentAICompiler
    }
    
    /// Parse FluentAI source code and return AST as JSON
    #[wasm_bindgen]
    pub fn parse(&self, source: &str) -> Result<String, JsValue> {
        use fluentai_parser::flc_parser::Parser;
        
        let parser = Parser::new(source);
        match parser.parse() {
            Ok(ast) => {
                // Convert AST to a simple JSON representation
                Ok(format!("{{ \"status\": \"success\", \"nodes\": {} }}", ast.nodes.len()))
            }
            Err(e) => {
                Err(JsValue::from_str(&format!("Parse error: {}", e)))
            }
        }
    }
    
    /// Check if source code is valid FluentAI
    #[wasm_bindgen]
    pub fn validate(&self, source: &str) -> bool {
        use fluentai_parser::flc_parser::Parser;
        
        let parser = Parser::new(source);
        parser.parse().is_ok()
    }
    
    /// Get parser version
    #[wasm_bindgen]
    pub fn version(&self) -> String {
        "0.1.0".to_string()
    }
}

/// Simple test function to verify WASM is working
#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}! FluentAI WASM is working.", name)
}