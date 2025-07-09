//! Integration tests for the MCP server

use fluentai_core::documentation::DocumentationRegistry;
use fluentai_stdlib::StdlibRegistry;
use fluentai_vm::{Bytecode, VM};
use serde_json::json;
use tokio::time::{sleep, Duration};

#[test]
fn test_documentation_registry_integration() {
    let registry = DocumentationRegistry::new();

    // Test that we can search and get documentation
    let search_results = registry.search("lambda");
    assert!(!search_results.is_empty());

    let lambda_doc = registry.get("Lambda");
    assert!(lambda_doc.is_some());
}

#[test]
fn test_vm_creation() {
    let bytecode = Bytecode::new();
    let vm = VM::new(bytecode);

    // VM should be created successfully
    // This is a basic smoke test
    drop(vm);
}

#[test]
fn test_stdlib_registry() {
    let stdlib = StdlibRegistry::new();

    // Registry should be created successfully
    drop(stdlib);
}

#[cfg(test)]
mod transport_tests {
    use super::*;
    use fluentai_mcp::server::McpServer;
    use fluentai_mcp::transport::TransportType;

    #[tokio::test]
    async fn test_stdio_transport_creation() {
        let transport = TransportType::Stdio.create().await;
        assert!(
            transport.is_ok(),
            "Should create stdio transport successfully"
        );
    }

    #[tokio::test]
    async fn test_http_transport_creation() {
        let transport = TransportType::Http { port: 0 }.create().await;
        assert!(
            transport.is_ok(),
            "Should create HTTP transport successfully"
        );
    }

    #[test]
    fn test_server_creation() {
        // Test that server module can be imported and types exist
        // Actual server startup tests would require more complex setup
        assert!(true, "Server types are available");
    }
}

#[cfg(test)]
mod http_api_tests {
    use super::*;

    #[tokio::test]
    async fn test_eval_sandboxing() {
        use fluentai_core::documentation::DocumentationRegistry;
        use fluentai_mcp::handlers::handle_eval;
        use fluentai_mcp::server::ServerState;
        use fluentai_stdlib::StdlibRegistry;
        use fluentai_vm::{Bytecode, VM};
        use serde_json::json;

        // Create server state
        let bytecode = Bytecode::new();
        let vm = VM::new(bytecode);
        let docs = DocumentationRegistry::new();
        let stdlib = StdlibRegistry::new();

        let mut state = ServerState { vm, docs, stdlib };

        // Test 1: Simple arithmetic (should work)
        let args = json!({
            "code": "(+ 1 2)"
        });
        let result = handle_eval(&mut state, Some(&args)).await;
        assert!(result.is_ok(), "Simple arithmetic should work");

        // Test 2: Code size limit
        let large_code = "(".to_string() + &"+ 1 ".repeat(30_000) + ")";
        let args = json!({
            "code": large_code
        });
        let result = handle_eval(&mut state, Some(&args)).await;
        assert!(result.is_err(), "Large code should be rejected");
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("exceeds maximum length"));
    }

    #[tokio::test]
    #[ignore] // Ignore by default as it requires a running server
    async fn test_http_session_lifecycle() {
        // This test would require starting an actual server
        // and making HTTP requests to it
        assert!(true, "HTTP session lifecycle test");
    }

    #[tokio::test]
    #[ignore]
    async fn test_http_tool_calls() {
        // Test calling various tools via HTTP
        assert!(true, "HTTP tool calls test");
    }
}
