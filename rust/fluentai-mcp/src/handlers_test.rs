//! Unit tests for MCP handlers

#[cfg(test)]
mod tests {
    use crate::handlers::*;
    use crate::server::ServerState;
    use fluentai_core::documentation::DocumentationRegistry;
    use fluentai_stdlib::StdlibRegistry;
    use fluentai_vm::{Bytecode, VM};
    use serde_json::json;

    fn create_test_state() -> ServerState {
        ServerState {
            vm: VM::new(Bytecode::new()),
            docs: DocumentationRegistry::new(),
            stdlib: StdlibRegistry::new(),
        }
    }

    #[tokio::test]
    async fn test_eval_simple() {
        let mut state = create_test_state();
        let args = json!({
            "code": "1 + 2"
        });

        let result = handle_eval(&mut state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert_eq!(content["text"], "3");
    }

    #[tokio::test]
    async fn test_eval_variadic() {
        let mut state = create_test_state();
        let args = json!({
            "code": "1 + 2 + 3 + 4 + 5"
        });

        let result = handle_eval(&mut state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert_eq!(content["text"], "15");
    }

    #[tokio::test]
    async fn test_eval_error_division_by_zero() {
        let mut state = create_test_state();
        let args = json!({
            "code": "1 / 0"
        });

        let result = handle_eval(&mut state, Some(&args)).await;
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        // The actual error message might be different, let's check for common patterns
        assert!(
            err_msg.contains("error") || err_msg.contains("Error") || err_msg.contains("divide")
        );
    }

    #[tokio::test]
    async fn test_eval_missing_code() {
        let mut state = create_test_state();
        let args = json!({});

        let result = handle_eval(&mut state, Some(&args)).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Missing code parameter"));
    }

    #[tokio::test]
    async fn test_eval_code_too_long() {
        let mut state = create_test_state();
        let long_code = "a".repeat(100_001);
        let args = json!({
            "code": long_code
        });

        let result = handle_eval(&mut state, Some(&args)).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("exceeds maximum length"));
    }

    #[tokio::test]
    async fn test_search_docs() {
        let state = create_test_state();
        let args = json!({
            "query": "lambda"
        });

        let result = handle_search_docs(&state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "data");

        let data = &content["data"];
        assert_eq!(data["query"], "lambda");
        assert!(data["results"].is_array());
    }

    #[tokio::test]
    async fn test_search_docs_missing_query() {
        let state = create_test_state();
        let args = json!({});

        let result = handle_search_docs(&state, Some(&args)).await;
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Missing query parameter"));
    }

    #[tokio::test]
    async fn test_get_syntax() {
        let state = create_test_state();
        let args = json!({
            "name": "Lambda"
        });

        let result = handle_get_syntax(&state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "data");

        let data = &content["data"];
        assert_eq!(data["name"], "Lambda");
        assert!(data["syntax"].is_string());
    }

    #[tokio::test]
    async fn test_get_syntax_not_found() {
        let state = create_test_state();
        let args = json!({
            "name": "NonExistentConstruct"
        });

        let result = handle_get_syntax(&state, Some(&args)).await.unwrap();
        assert!(result["isError"].as_bool().unwrap_or(false));
    }

    #[tokio::test]
    async fn test_list_features() {
        let state = create_test_state();

        let result = handle_list_features(&state, None).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "data");

        let data = &content["data"];
        assert!(data["total_features"].is_number());
        assert!(data["categories"].is_object());
    }

    #[tokio::test]
    async fn test_reset_interpreter() {
        let mut state = create_test_state();

        let result = handle_reset_interpreter(&mut state, None).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert!(content["text"].as_str().unwrap().contains("reset"));
    }

    #[tokio::test]
    async fn test_eval_string_operations() {
        let mut state = create_test_state();
        let args = json!({
            "code": "\"hello\" + \" \" + \"world\""
        });

        let result = handle_eval(&mut state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert_eq!(content["text"], "\"hello world\"");
    }

    #[tokio::test]
    async fn test_eval_comparison_operators() {
        let mut state = create_test_state();
        let args = json!({
            "code": "5 == 5"
        });

        let result = handle_eval(&mut state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert_eq!(content["text"], "true");
    }

    #[tokio::test]
    async fn test_eval_logical_operators() {
        let mut state = create_test_state();
        let args = json!({
            "code": "true && true && false"
        });

        let result = handle_eval(&mut state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert_eq!(content["text"], "false");
    }

    #[tokio::test]
    async fn test_eval_nested_expression() {
        let mut state = create_test_state();
        let args = json!({
            "code": "(1 + 2) * (10 - 6)"
        });

        let result = handle_eval(&mut state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "text");
        assert_eq!(content["text"], "12");
    }

    #[tokio::test]
    async fn test_search_docs_multiple_results() {
        let state = create_test_state();
        let args = json!({
            "query": "list"
        });

        let result = handle_search_docs(&state, Some(&args)).await.unwrap();
        let content = &result["content"][0];
        assert_eq!(content["type"], "data");

        let data = &content["data"];
        let results = data["results"].as_array().unwrap();
        assert!(results.len() > 0);

        // Check that results have the expected structure
        for result in results {
            assert!(result["name"].is_string());
            assert!(result["syntax"].is_string());
            assert!(result["description"].is_string());
        }
    }

    #[tokio::test]
    async fn test_list_features_categories() {
        let state = create_test_state();

        let result = handle_list_features(&state, None).await.unwrap();
        let content = &result["content"][0];
        let data = &content["data"];
        let categories = data["categories"].as_object().unwrap();

        // Check that we have expected categories
        assert!(categories.contains_key("Literal"));
        assert!(categories.contains_key("Function"));
        assert!(categories.contains_key("ControlFlow"));

        // Check that each category is an array
        for (_, features) in categories {
            assert!(features.is_array());
        }
    }
}
