use fluentai_core::documentation::DocumentationRegistry;

#[test]
fn test_no_duplicate_operators() {
    let registry = DocumentationRegistry::new();
    
    // Test subtraction operator
    let results = registry.search("subtraction");
    assert_eq!(results.len(), 1, "Subtraction should only appear once, got: {}", results.len());
    if results.len() > 0 {
        assert_eq!(results[0].name, "Subtraction");
    }
    
    // Test and operator
    let results = registry.search("and");
    let and_results: Vec<_> = results.iter().filter(|d| d.name == "Logical AND").collect();
    assert_eq!(and_results.len(), 1, "Logical AND should only appear once, got: {}", and_results.len());
    
    // Test = operator
    let results = registry.search("=");
    let eq_results: Vec<_> = results.iter().filter(|d| d.name == "Equality" || d.name == "=").collect();
    assert_eq!(eq_results.len(), 1, "Equality operator should only appear once, got: {}", eq_results.len());
    
    println!("All duplicate tests passed!");
}