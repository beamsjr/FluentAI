//! MCP Server implementation

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use tracing::{debug, error, info};

use claudelang_core::documentation::DocumentationRegistry;
use claudelang_vm::{VM, Bytecode};
use claudelang_stdlib::StdlibRegistry;

use crate::handlers::{handle_eval, handle_search_docs, handle_get_syntax, handle_list_features, handle_reset_interpreter};

/// MCP Server state
pub struct ServerState {
    pub vm: VM,
    pub docs: DocumentationRegistry,
    pub stdlib: StdlibRegistry,
}

/// MCP Server
pub struct McpServer {
    state: Arc<Mutex<ServerState>>,
}

#[derive(Debug, Deserialize)]
struct Request {
    jsonrpc: String,
    method: String,
    params: Option<JsonValue>,
    id: JsonValue,
}

#[derive(Debug, Serialize)]
struct Response {
    jsonrpc: String,
    result: Option<JsonValue>,
    error: Option<ErrorResponse>,
    id: JsonValue,
}

#[derive(Debug, Serialize)]
struct ErrorResponse {
    code: i32,
    message: String,
    data: Option<JsonValue>,
}

#[derive(Debug, Serialize)]
struct ServerInfo {
    name: String,
    version: String,
    protocol_version: String,
}

#[derive(Debug, Serialize)]
struct InitializeResult {
    #[serde(rename = "protocolVersion")]
    protocol_version: String,
    capabilities: ServerCapabilities,
    #[serde(rename = "serverInfo")]
    server_info: ServerInfo,
}

#[derive(Debug, Serialize)]
struct ServerCapabilities {
    tools: ToolsCapability,
}

#[derive(Debug, Serialize)]
struct ToolsCapability {}

#[derive(Debug, Serialize)]
struct Tool {
    name: String,
    description: String,
    #[serde(rename = "inputSchema")]
    input_schema: JsonValue,
}

impl McpServer {
    pub fn new() -> Self {
        // Create an empty bytecode for initial VM
        let bytecode = Bytecode::new();
        let vm = VM::new(bytecode);
        let docs = DocumentationRegistry::new();
        let stdlib = StdlibRegistry::new();
        
        let state = ServerState { vm, docs, stdlib };
        
        Self {
            state: Arc::new(Mutex::new(state)),
        }
    }
    
    pub async fn run(&self) -> Result<()> {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let mut reader = BufReader::new(stdin);
        let mut stdout = stdout;
        
        info!("MCP Server listening on stdin/stdout");
        
        let mut line = String::new();
        loop {
            line.clear();
            let bytes_read = reader.read_line(&mut line).await?;
            
            if bytes_read == 0 {
                info!("EOF reached, shutting down");
                break;
            }
            
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            
            debug!("Received: {}", line);
            
            match serde_json::from_str::<Request>(line) {
                Ok(request) => {
                    let response = self.handle_request(request).await;
                    let response_str = serde_json::to_string(&response)?;
                    stdout.write_all(response_str.as_bytes()).await?;
                    stdout.write_all(b"\n").await?;
                    stdout.flush().await?;
                }
                Err(e) => {
                    error!("Failed to parse request: {}", e);
                }
            }
        }
        
        Ok(())
    }
    
    async fn handle_request(&self, request: Request) -> Response {
        let result = match request.method.as_str() {
            "initialize" => self.handle_initialize().await,
            "initialized" => Ok(json!({})),
            "tools/list" => self.handle_list_tools().await,
            "tools/call" => self.handle_tool_call(request.params).await,
            _ => Err(anyhow::anyhow!("Unknown method: {}", request.method)),
        };
        
        match result {
            Ok(result) => Response {
                jsonrpc: "2.0".to_string(),
                result: Some(result),
                error: None,
                id: request.id,
            },
            Err(e) => Response {
                jsonrpc: "2.0".to_string(),
                result: None,
                error: Some(ErrorResponse {
                    code: -32603,
                    message: e.to_string(),
                    data: None,
                }),
                id: request.id,
            },
        }
    }
    
    async fn handle_initialize(&self) -> Result<JsonValue> {
        let result = InitializeResult {
            protocol_version: "0.1.0".to_string(),
            capabilities: ServerCapabilities {
                tools: ToolsCapability {},
            },
            server_info: ServerInfo {
                name: "ClaudeLang MCP Server".to_string(),
                version: "0.1.0".to_string(),
                protocol_version: "0.1.0".to_string(),
            },
        };
        
        Ok(serde_json::to_value(result)?)
    }
    
    async fn handle_list_tools(&self) -> Result<JsonValue> {
        let tools = vec![
            Tool {
                name: "eval".to_string(),
                description: "Execute ClaudeLang code and return results".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "code": {
                            "type": "string",
                            "description": "ClaudeLang code to execute"
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
    
    async fn handle_tool_call(&self, params: Option<JsonValue>) -> Result<JsonValue> {
        let params = params.ok_or_else(|| anyhow::anyhow!("Missing params"))?;
        let tool_name = params["name"].as_str()
            .ok_or_else(|| anyhow::anyhow!("Missing tool name"))?;
        let arguments = params.get("arguments");
        
        let mut state = self.state.lock().await;
        
        match tool_name {
            "eval" => handle_eval(&mut state, arguments).await,
            "search_docs" => handle_search_docs(&state, arguments).await,
            "get_syntax" => handle_get_syntax(&state, arguments).await,
            "list_features" => handle_list_features(&state, arguments).await,
            "reset_interpreter" => handle_reset_interpreter(&mut state, arguments).await,
            _ => Err(anyhow::anyhow!("Unknown tool: {}", tool_name)),
        }
    }
}