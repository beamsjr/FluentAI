//! Example HTTP client for FluentAi MCP Server
//!
//! This example demonstrates how to connect to the MCP server using HTTP/SSE transport.

use anyhow::Result;
use futures::StreamExt;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};

#[derive(Debug, Serialize, Deserialize)]
struct CreateSessionRequest {
    client_info: ClientInfo,
}

#[derive(Debug, Serialize, Deserialize)]
struct ClientInfo {
    name: String,
    version: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct CreateSessionResponse {
    session_id: String,
    sse_endpoint: String,
}

#[derive(Debug, Serialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    params: Option<JsonValue>,
    id: JsonValue,
}

#[derive(Debug, Deserialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
    id: JsonValue,
}

#[derive(Debug, Deserialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    data: Option<JsonValue>,
}

#[tokio::main]
async fn main() -> Result<()> {
    println!("FluentAi MCP HTTP Client Example");
    println!("================================");

    let base_url = "http://localhost:3000";
    let client = Client::new();

    // Step 1: Create a session
    println!("\n1. Creating session...");
    let session_req = CreateSessionRequest {
        client_info: ClientInfo {
            name: "example-client".to_string(),
            version: "1.0.0".to_string(),
        },
    };

    let response = client
        .post(format!("{}/sessions", base_url))
        .json(&session_req)
        .send()
        .await?;

    let session: CreateSessionResponse = response.json().await?;
    println!("Session created: {}", session.session_id);
    println!("SSE endpoint: {}", session.sse_endpoint);

    // Step 2: Initialize the MCP connection
    println!("\n2. Initializing MCP connection...");
    let init_request = JsonRpcRequest {
        jsonrpc: "2.0".to_string(),
        method: "initialize".to_string(),
        params: Some(json!({
            "protocolVersion": "1.0",
            "clientInfo": {
                "name": "example-client",
                "version": "1.0.0"
            }
        })),
        id: json!("init-1"),
    };

    let response = client
        .post(format!(
            "{}/sessions/{}/messages",
            base_url, session.session_id
        ))
        .json(&init_request)
        .send()
        .await?;

    let init_response: JsonRpcResponse = response.json().await?;
    println!("Initialize response: {:?}", init_response.result);

    // Step 3: List available tools
    println!("\n3. Listing available tools...");
    let list_tools_request = JsonRpcRequest {
        jsonrpc: "2.0".to_string(),
        method: "tools/list".to_string(),
        params: None,
        id: json!("tools-1"),
    };

    let response = client
        .post(format!(
            "{}/sessions/{}/messages",
            base_url, session.session_id
        ))
        .json(&list_tools_request)
        .send()
        .await?;

    let tools_response: JsonRpcResponse = response.json().await?;
    if let Some(result) = tools_response.result {
        if let Some(tools) = result["tools"].as_array() {
            println!("Available tools:");
            for tool in tools {
                println!("  - {} : {}", tool["name"], tool["description"]);
            }
        }
    }

    // Step 4: Execute some FluentAi code
    println!("\n4. Executing FluentAi code...");
    let eval_request = JsonRpcRequest {
        jsonrpc: "2.0".to_string(),
        method: "tools/call".to_string(),
        params: Some(json!({
            "name": "eval",
            "arguments": {
                "code": "(+ 1 2 3)"
            }
        })),
        id: json!("eval-1"),
    };

    let response = client
        .post(format!(
            "{}/sessions/{}/messages",
            base_url, session.session_id
        ))
        .json(&eval_request)
        .send()
        .await?;

    let eval_response: JsonRpcResponse = response.json().await?;
    if let Some(result) = eval_response.result {
        if let Some(content) = result["content"].as_array() {
            if let Some(text) = content[0]["text"].as_str() {
                println!("Result: {}", text);
            }
        }
    }

    // Step 5: Search documentation
    println!("\n5. Searching documentation...");
    let search_request = JsonRpcRequest {
        jsonrpc: "2.0".to_string(),
        method: "tools/call".to_string(),
        params: Some(json!({
            "name": "search_docs",
            "arguments": {
                "query": "lambda"
            }
        })),
        id: json!("search-1"),
    };

    let response = client
        .post(format!(
            "{}/sessions/{}/messages",
            base_url, session.session_id
        ))
        .json(&search_request)
        .send()
        .await?;

    let search_response: JsonRpcResponse = response.json().await?;
    if let Some(result) = search_response.result {
        if let Some(content) = result["content"].as_array() {
            if let Some(text) = content[0]["text"].as_str() {
                // Just print first few lines
                let lines: Vec<&str> = text.lines().take(10).collect();
                println!("Search results (first 10 lines):");
                for line in lines {
                    println!("{}", line);
                }
            }
        }
    }

    println!("\n✓ Example completed successfully!");

    Ok(())
}
