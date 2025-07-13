//! Tests for automatic export collection in modules

use fluentai_parser::parse_flc;
use fluentai_core::ast::ExportItem;

#[test]
fn test_automatic_exports() {
    let source = r#"
        mod MyModule;
        
        public function greet(name: string) -> string {
            f"Hello, {name}!"
        }
        
        private function internal_helper() -> int {
            42
        }
        
        public struct User {}
        
        public const MAX_USERS = 100;
        
        private const SECRET_KEY = "secret";
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Check module name was stored
    assert_eq!(
        graph.graph_metadata.get("module_name"),
        Some(&"MyModule".to_string())
    );
    
    // Check exports were collected
    let exports_json = graph.graph_metadata.get("exports").unwrap();
    let exports: Vec<ExportItem> = serde_json::from_str(exports_json).unwrap();
    
    // Should have 3 public exports
    assert_eq!(exports.len(), 3);
    
    // Check export names (order may vary)
    let export_names: Vec<&str> = exports.iter().map(|e| e.name.as_str()).collect();
    assert!(export_names.contains(&"greet"));
    assert!(export_names.contains(&"User"));
    assert!(export_names.contains(&"MAX_USERS"));
    
    // Check that private items were not exported
    assert!(!export_names.contains(&"internal_helper"));
    assert!(!export_names.contains(&"SECRET_KEY"));
}

#[test]
fn test_no_module_declaration() {
    let source = r#"
        public function main() -> int {
            42
        }
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // No module name should be set
    assert_eq!(graph.graph_metadata.get("module_name"), None);
    
    // But exports should still be collected
    let exports_json = graph.graph_metadata.get("exports").unwrap();
    let exports: Vec<ExportItem> = serde_json::from_str(exports_json).unwrap();
    
    assert_eq!(exports.len(), 1);
    assert_eq!(exports[0].name, "main");
}

#[test]
fn test_mixed_visibility() {
    let source = r#"
        mod DataModule;
        
        public enum Status {
            Active,
            Inactive,
        }
        
        private trait Internal {}
        
        public trait Serializable {}
        
        public type UserId = int;
        
        private actor InternalActor {
            private handle ping() {
                "pong"
            }
        }
        
        public effect Logger {}
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Check module name
    assert_eq!(
        graph.graph_metadata.get("module_name"),
        Some(&"DataModule".to_string())
    );
    
    // Check exports
    let exports_json = graph.graph_metadata.get("exports").unwrap();
    let exports: Vec<ExportItem> = serde_json::from_str(exports_json).unwrap();
    
    // Should have 4 public exports
    assert_eq!(exports.len(), 4);
    
    let export_names: Vec<&str> = exports.iter().map(|e| e.name.as_str()).collect();
    assert!(export_names.contains(&"Status"));
    assert!(export_names.contains(&"Serializable"));
    assert!(export_names.contains(&"UserId"));
    assert!(export_names.contains(&"Logger"));
    
    // Private items should not be exported
    assert!(!export_names.contains(&"Internal"));
    assert!(!export_names.contains(&"InternalActor"));
}