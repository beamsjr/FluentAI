//! MCP Server implementation

use anyhow::Result;
use serde::Serialize;
use serde_json::{json, Value as JsonValue};
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{debug, error, info};

use fluentai_core::documentation::DocumentationRegistry;
use fluentai_vm::{VM, Bytecode};
use fluentai_stdlib::StdlibRegistry;

use crate::handlers::{handle_eval, handle_search_docs, handle_get_syntax, handle_list_features, handle_reset_interpreter};
use crate::transport::{Transport, JsonRpcRequest, JsonRpcResponse, JsonRpcError};

/// MCP Server state
pub struct ServerState {
    pub vm: VM,
    pub docs: DocumentationRegistry,
    #[allow(dead_code)]
    pub stdlib: StdlibRegistry,
}

/// MCP Server
pub struct McpServer {
    state: Arc<Mutex<ServerState>>,
    transport: Box<dyn Transport>,
}

/// Shared request handler
#[derive(Clone)]
pub struct McpRequestHandler {
    state: Arc<Mutex<ServerState>>,
}

#[async_trait::async_trait]
impl crate::transport::RequestHandler for McpRequestHandler {
    async fn handle_request(&self, request: crate::transport::JsonRpcRequest) -> crate::transport::JsonRpcResponse {
        // Call the static handler function directly without creating a server instance
        handle_request_static(&self.state, request).await
    }
}


#[derive(Debug, Serialize)]
struct Tool {
    name: String,
    description: String,
    #[serde(rename = "inputSchema")]
    input_schema: JsonValue,
}

impl McpServer {
    pub async fn new(transport: Box<dyn Transport>) -> Result<Self> {
        // Create VM in a blocking task to avoid async drop issues
        let (vm, docs, stdlib) = tokio::task::spawn_blocking(|| {
            let bytecode = Bytecode::new();
            let vm = VM::new(bytecode);
            let docs = DocumentationRegistry::new();
            let stdlib = StdlibRegistry::new();
            (vm, docs, stdlib)
        })
        .await
        .map_err(|e| anyhow::anyhow!("Failed to create server state: {}", e))?;

        let state = ServerState { vm, docs, stdlib };

        Ok(Self {
            state: Arc::new(Mutex::new(state)),
            transport,
        })
    }

    pub async fn run(mut self) -> Result<()> {
        // Create the request handler
        let handler = McpRequestHandler {
            state: self.state.clone(),
        };
        let handler = Arc::new(handler) as Arc<dyn crate::transport::RequestHandler>;
        
        self.transport.start(handler).await?;
        info!("MCP Server started");

        while self.transport.is_connected() {
            match self.transport.receive_request().await {
                Ok(Some(request)) => {
                    debug!("Received request: {:?}", request);
                    let response = self.handle_request(request).await;
                    self.transport.send_response(response).await?;
                }
                Ok(None) => {
                    // No request available, continue
                    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                }
                Err(e) => {
                    error!("Error receiving request: {}", e);
                    break;
                }
            }
        }

        self.transport.shutdown().await?;
        info!("MCP Server shutdown");
        Ok(())
    }

    async fn handle_request(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        handle_request_static(&self.state, request).await
    }
}

/// Static handler function that doesn't require a full server instance
async fn handle_request_static(state: &Arc<Mutex<ServerState>>, request: JsonRpcRequest) -> JsonRpcResponse {
    let result = match request.method.as_str() {
        "initialize" => handle_initialize_static().await,
        "initialized" => Ok(json!({})),
        "tools/list" => handle_list_tools_static(state).await,
        "tools/call" => handle_tool_call_static(state, request.params).await,
        _ => Err(anyhow::anyhow!("Unknown method: {}", request.method)),
    };

    match result {
        Ok(result) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            result: Some(result),
            error: None,
            id: request.id,
        },
        Err(e) => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            result: None,
            error: Some(JsonRpcError {
                code: -32603,
                message: e.to_string(),
                data: None,
            }),
            id: request.id,
        },
    }
}


// Static handler functions

async fn handle_initialize_static() -> Result<JsonValue> {
    Ok(json!({
        "protocolVersion": "1.0",
        "serverInfo": {
            "name": "FluentAi",
            "version": "0.1.0"
        },
        "capabilities": {
            "tools": true
        }
    }))
}

async fn handle_list_tools_static(_state: &Arc<Mutex<ServerState>>) -> Result<JsonValue> {
    let tools = vec![
        Tool {
            name: "eval".to_string(),
            description: "Execute FluentAi code and return results".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "code": {
                        "type": "string",
                        "description": "FluentAi code to execute"
                    }
                },
                "required": ["code"]
            }),
        },
        Tool {
            name: "search_docs".to_string(),
            description: "Search documentation for a query".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "query": {
                        "type": "string",
                        "description": "Search query"
                    }
                },
                "required": ["query"]
            }),
        },
        Tool {
            name: "get_syntax".to_string(),
            description: "Get documentation for a specific construct".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "name": {
                        "type": "string",
                        "description": "Name of the construct"
                    }
                },
                "required": ["name"]
            }),
        },
        Tool {
            name: "list_features".to_string(),
            description: "List all language features".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {}
            }),
        },
        Tool {
            name: "reset_interpreter".to_string(),
            description: "Clear the interpreter state".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {}
            }),
        },
    ];

    Ok(json!({ "tools": tools }))
}

async fn handle_tool_call_static(state: &Arc<Mutex<ServerState>>, params: Option<JsonValue>) -> Result<JsonValue> {
    let params = params.ok_or_else(|| anyhow::anyhow!("Missing params"))?;
    let tool_name = params.get("name")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow::anyhow!("Missing tool name"))?;
    let arguments = params.get("arguments");

    let mut state_guard = state.lock().await;

    match tool_name {
        "eval" => handle_eval(&mut state_guard, arguments).await,
        "search_docs" => handle_search_docs(&state_guard, arguments).await,
        "get_syntax" => handle_get_syntax(&state_guard, arguments).await,
        "list_features" => handle_list_features(&state_guard, arguments).await,
        "reset_interpreter" => handle_reset_interpreter(&mut state_guard, arguments).await,
        _ => Err(anyhow::anyhow!("Unknown tool: {}", tool_name)),
    }
}
