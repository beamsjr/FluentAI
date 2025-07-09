//! Tests for the documentation system

#[cfg(test)]
mod tests {
    use crate::documentation::impls::*;
    use crate::documentation::{DocumentationCategory, DocumentationRegistry, DocumentedNode};

    #[test]
    fn test_documented_node_trait() {
        // Test that all nodes implement DocumentedNode properly
        let integer_doc = IntegerDoc::get_docs();
        assert_eq!(integer_doc.name, "Integer");
        assert!(!integer_doc.syntax.is_empty());
        assert!(!integer_doc.description.is_empty());
        assert!(!integer_doc.examples.is_empty());
        assert_eq!(integer_doc.category, DocumentationCategory::Literal);

        let lambda_doc = LambdaDoc::get_docs();
        assert_eq!(lambda_doc.name, "Lambda");
        assert_eq!(lambda_doc.category, DocumentationCategory::Function);
        assert!(!lambda_doc.see_also.is_empty());
    }

    #[test]
    fn test_documentation_registry() {
        let registry = DocumentationRegistry::new();

        // Test get by name
        let integer_doc = registry.get("Integer");
        assert!(integer_doc.is_some());
        assert_eq!(integer_doc.unwrap().name, "Integer");

        // Test that all major constructs are registered
        assert!(registry.get("Lambda").is_some());
        assert!(registry.get("If").is_some());
        assert!(registry.get("Let").is_some());
        assert!(registry.get("Match").is_some());
        assert!(registry.get("List").is_some());
        assert!(registry.get("Async").is_some());

        // Test non-existent construct
        assert!(registry.get("NonExistent").is_none());
    }

    #[test]
    fn test_search_functionality() {
        let registry = DocumentationRegistry::new();

        // Search for "function"
        let results = registry.search("function");
        assert!(!results.is_empty());

        // Should find Lambda, Application, and others
        let names: Vec<_> = results.iter().map(|d| d.name.as_str()).collect();
        assert!(names.contains(&"Lambda"));
        assert!(names.contains(&"Application"));

        // Search for operators
        let add_results = registry.search("+");
        assert!(!add_results.is_empty());
        assert!(add_results.iter().any(|d| d.name == "Addition"));

        // Case insensitive search
        let if_results = registry.search("IF");
        assert!(!if_results.is_empty());
        assert!(if_results.iter().any(|d| d.name == "If"));
    }

    #[test]
    fn test_list_all_features() {
        let registry = DocumentationRegistry::new();
        let all_docs = registry.list_all();

        // Should have a reasonable number of features
        assert!(all_docs.len() > 20);

        // Check that different categories are present
        let categories: std::collections::HashSet<_> =
            all_docs.iter().map(|d| d.category).collect();

        assert!(categories.contains(&DocumentationCategory::Literal));
        assert!(categories.contains(&DocumentationCategory::Function));
        assert!(categories.contains(&DocumentationCategory::ControlFlow));
        assert!(categories.contains(&DocumentationCategory::Async));
        assert!(categories.contains(&DocumentationCategory::Operator));
        assert!(categories.contains(&DocumentationCategory::Keyword));
    }

    #[test]
    fn test_operators_documentation() {
        let registry = DocumentationRegistry::new();
        let operators = registry.get_operators();

        assert!(!operators.is_empty());

        // Check specific operators
        assert!(operators.iter().any(|op| op.symbol == "+"));
        assert!(operators.iter().any(|op| op.symbol == "-"));
        assert!(operators.iter().any(|op| op.symbol == "="));
        assert!(operators.iter().any(|op| op.symbol == "and"));
    }

    #[test]
    fn test_keywords_documentation() {
        let registry = DocumentationRegistry::new();
        let keywords = registry.get_keywords();

        assert!(!keywords.is_empty());

        // Check specific keywords
        assert!(keywords.iter().any(|kw| kw.keyword == "lambda"));
        assert!(keywords.iter().any(|kw| kw.keyword == "let"));
        assert!(keywords.iter().any(|kw| kw.keyword == "if"));
        assert!(keywords.iter().any(|kw| kw.keyword == "match"));
    }

    #[test]
    fn test_documentation_completeness() {
        let registry = DocumentationRegistry::new();
        let all_docs = registry.list_all();

        // Ensure no documentation fields are empty
        for doc in &all_docs {
            assert!(!doc.name.is_empty(), "Empty name found");
            assert!(!doc.syntax.is_empty(), "Empty syntax for {}", doc.name);
            assert!(
                !doc.description.is_empty(),
                "Empty description for {}",
                doc.name
            );

            // Most constructs should have examples
            if doc.category != DocumentationCategory::Operator {
                assert!(!doc.examples.is_empty(), "No examples for {}", doc.name);
            }
        }
    }

    #[test]
    fn test_get_builtins() {
        let registry = DocumentationRegistry::new();
        let builtins = registry.get_builtins();

        assert!(!builtins.is_empty());

        // Check for common built-in functions
        let names: Vec<_> = builtins.iter().map(|b| b.name.as_ref()).collect();
        assert!(names.contains(&"cons"));
        assert!(names.contains(&"car"));
        assert!(names.contains(&"cdr"));
        assert!(names.contains(&"+"));
        assert!(names.contains(&"-"));
        assert!(names.contains(&"*"));
        assert!(names.contains(&"/"));
        // Note: = is an operator, not a built-in function
        assert!(names.contains(&"print"));
        assert!(names.contains(&"string-length"));

        // Verify all builtins have proper documentation
        for builtin in builtins {
            assert!(!builtin.name.is_empty());
            assert!(!builtin.signature.is_empty());
            assert!(!builtin.description.is_empty());
            assert!(!builtin.examples.is_empty());
        }
    }

    #[test]
    fn test_list_user_facing() {
        let registry = DocumentationRegistry::new();
        let user_docs = registry.list_user_facing();
        let all_docs = registry.list_all();

        // User-facing docs should be a subset of all docs
        assert!(user_docs.len() <= all_docs.len());

        // All user-facing docs should have Public visibility
        for doc in &user_docs {
            assert_eq!(
                doc.visibility,
                crate::documentation::DocumentationVisibility::Public
            );
        }

        // Should include common public constructs
        let names: Vec<_> = user_docs.iter().map(|d| d.name.as_str()).collect();
        assert!(names.contains(&"Lambda"));
        assert!(names.contains(&"If"));
        assert!(names.contains(&"List"));
        assert!(names.contains(&"Integer"));
    }

    #[test]
    fn test_search_user_facing() {
        let registry = DocumentationRegistry::new();

        // Search for function-related docs
        let results = registry.search_user_facing("function");

        // All results should be public
        for doc in &results {
            assert_eq!(
                doc.visibility,
                crate::documentation::DocumentationVisibility::Public
            );
        }

        // Should find Lambda and Application at least
        let names: Vec<_> = results.iter().map(|d| d.name.as_str()).collect();
        assert!(names.contains(&"Lambda"));
        assert!(names.contains(&"Application"));

        // Compare with regular search to ensure filtering works
        let all_results = registry.search("function");
        assert!(results.len() <= all_results.len());
    }

    #[test]
    fn test_builtin_documentation_conversion() {
        use crate::documentation::DocumentationCategory;

        let registry = DocumentationRegistry::new();
        let builtins = registry.get_builtins();

        // Test that builtins can be converted to Documentation
        for builtin in builtins {
            let doc = builtin.to_documentation();
            assert_eq!(doc.name, builtin.name);
            assert_eq!(doc.category, DocumentationCategory::Function);
            assert!(!doc.syntax.is_empty());
            assert!(!doc.description.is_empty());
        }
    }
}
