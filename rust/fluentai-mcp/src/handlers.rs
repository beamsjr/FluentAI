//! Tool handlers for the MCP server

use anyhow::Result;
use serde_json::{json, Value as JsonValue};
use tracing::debug;

use fluentai_parser::Parser;
use fluentai_vm::security::SecurityPolicy;
use fluentai_vm::Value;
use fluentai_vm::{Bytecode, Compiler, VMBuilder, VM};

use crate::server::ServerState;

/// Handle eval tool - execute FluentAi code
pub async fn handle_eval(_state: &mut ServerState, args: Option<&JsonValue>) -> Result<JsonValue> {
    let args = args.ok_or_else(|| anyhow::anyhow!("Missing arguments"))?;
    let code = args["code"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing code parameter"))?;

    debug!("Evaluating code: {}", code);

    // Validate code length to prevent DoS
    if code.len() > 100_000 {
        return Err(anyhow::anyhow!("Code exceeds maximum length of 100KB"));
    }

    let code = code.to_string();

    // Run VM operations in a blocking task with timeout
    let timeout_secs = std::env::var("FLUENTAI_EXECUTION_TIMEOUT_SECS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(30); // Default: 30 seconds

    let result = tokio::time::timeout(
        tokio::time::Duration::from_secs(timeout_secs),
        tokio::task::spawn_blocking(move || {
            // Parse the code
            let mut parser = Parser::new(&code);
            let ast = parser
                .parse()
                .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

            // Compile the AST to bytecode
            let compiler = Compiler::new();
            let bytecode = compiler
                .compile(&ast)
                .map_err(|e| anyhow::anyhow!("Compile error: {:?}", e))?;

            // Create a sandboxed VM with strict security policy
            let mut security_policy = SecurityPolicy::sandbox();
            // Allow a reasonable amount of memory and instructions for demos
            // These can be configured via environment variables
            security_policy.max_memory = std::env::var("FLUENTAI_MAX_MEMORY_MB")
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .map(|mb| mb * 1024 * 1024)
                .unwrap_or(50 * 1024 * 1024); // Default: 50MB

            security_policy.max_instructions = std::env::var("FLUENTAI_MAX_INSTRUCTIONS")
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .unwrap_or(10_000_000); // Default: 10M instructions

            security_policy.max_allocations = std::env::var("FLUENTAI_MAX_ALLOCATIONS")
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .unwrap_or(50_000); // Default: 50K allocations

            let mut vm = VMBuilder::new()
                .with_bytecode(bytecode)
                .with_security_policy(security_policy)
                .with_sandbox_mode()
                .build()?;

            // Run with timeout (handled by spawn_blocking timeout)
            vm.run()
                .map_err(|e| anyhow::anyhow!("Runtime error: {:?}", e))
        }),
    )
    .await
    .map_err(|_| anyhow::anyhow!("Code execution timed out after 30 seconds"))?
    .map_err(|e| anyhow::anyhow!("Task join error: {}", e))??;

    let result_str = format_value(&result);
    Ok(json!({
        "content": [{
            "type": "text",
            "text": result_str
        }]
    }))
}

/// Handle search_docs tool - search documentation
pub async fn handle_search_docs(
    state: &ServerState,
    args: Option<&JsonValue>,
) -> Result<JsonValue> {
    let args = args.ok_or_else(|| anyhow::anyhow!("Missing arguments"))?;
    let query = args["query"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing query parameter"))?;

    debug!("Searching documentation for: {}", query);

    // Only show user-facing documentation to the LLM
    let results = state.docs.search_user_facing(query);

    // Convert results to JSON objects
    let json_results: Vec<JsonValue> = results
        .iter()
        .take(10)
        .map(|doc| {
            json!({
                "name": doc.name,
                "syntax": doc.syntax,
                "category": format!("{:?}", doc.category),
                "description": doc.description,
                "examples": doc.examples,
                "see_also": doc.see_also
            })
        })
        .collect();

    Ok(json!({
        "content": [{
            "type": "data",
            "data": {
                "query": query,
                "total_results": results.len(),
                "results": json_results
            }
        }]
    }))
}

/// Handle get_syntax tool - get documentation for a specific construct
pub async fn handle_get_syntax(state: &ServerState, args: Option<&JsonValue>) -> Result<JsonValue> {
    let args = args.ok_or_else(|| anyhow::anyhow!("Missing arguments"))?;
    let name = args["name"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing name parameter"))?;

    debug!("Getting syntax for: {}", name);

    if let Some(doc) = state.docs.get(name) {
        Ok(json!({
            "content": [{
                "type": "data",
                "data": {
                    "name": doc.name,
                    "syntax": doc.syntax,
                    "category": format!("{:?}", doc.category),
                    "description": doc.description,
                    "examples": doc.examples,
                    "see_also": doc.see_also
                }
            }]
        }))
    } else {
        Ok(json!({
            "content": [{
                "type": "data",
                "data": {
                    "error": format!("No documentation found for '{}'", name)
                }
            }],
            "isError": true
        }))
    }
}

/// Handle list_features tool - list all language features
pub async fn handle_list_features(
    state: &ServerState,
    _args: Option<&JsonValue>,
) -> Result<JsonValue> {
    debug!("Listing all features");

    // Only show user-facing documentation to the LLM
    let all_docs = state.docs.list_user_facing();

    // Group by category
    let mut by_category: std::collections::HashMap<_, Vec<_>> = std::collections::HashMap::new();
    for doc in all_docs {
        by_category.entry(doc.category).or_default().push(doc);
    }

    // Convert to structured JSON
    let mut categories_json = json!({});

    for (category, mut docs) in by_category {
        docs.sort_by_key(|d| d.name.clone());

        let category_name = format!("{:?}", category);
        let features: Vec<JsonValue> = docs
            .into_iter()
            .map(|doc| {
                json!({
                    "name": doc.name,
                    "syntax": doc.syntax,
                    "description": doc.description
                })
            })
            .collect();

        categories_json[category_name] = json!(features);
    }

    Ok(json!({
        "content": [{
            "type": "data",
            "data": {
                "total_features": state.docs.list_user_facing().len(),
                "categories": categories_json
            }
        }]
    }))
}

/// Handle reset_interpreter tool - clear interpreter state
pub async fn handle_reset_interpreter(
    state: &mut ServerState,
    _args: Option<&JsonValue>,
) -> Result<JsonValue> {
    debug!("Resetting interpreter");

    // Create a new VM instance with empty bytecode
    state.vm = VM::new(Bytecode::new());

    Ok(json!({
        "content": [{
            "type": "text",
            "text": "Interpreter state has been reset."
        }]
    }))
}

/// Format a FluentAi value for display
fn format_value(value: &Value) -> String {
    match value {
        Value::Integer(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Boolean(b) => b.to_string(),
        Value::Nil => "nil".to_string(),
        Value::List(items) => {
            let items_str: Vec<_> = items.iter().map(format_value).collect();
            format!("[{}]", items_str.join(" "))
        }
        Value::Map(map) => {
            let pairs: Vec<_> = map
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
        Value::Function { .. } => "<function>".to_string(),
        Value::Promise(id) => format!("<promise:{}>", id),
        Value::Future { .. } => "<future>".to_string(),
        Value::Channel(id) => format!("<channel:{}>", id),
        Value::Cell(idx) => format!("<cell:{}>", idx),
        Value::Tagged { tag, values } => {
            if values.is_empty() {
                tag.to_string()
            } else {
                let values_str: Vec<_> = values.iter().map(format_value).collect();
                format!("{}({})", tag, values_str.join(", "))
            }
        }
        Value::Module { name, .. } => format!("<module:{}>", name),
        Value::GcHandle(_) => "<gc-handle>".to_string(),
        Value::Symbol(s) => format!(":{}", s),
        Value::Procedure(_) => "<procedure>".to_string(),
        Value::Vector(items) => {
            let items_str: Vec<_> = items.iter().map(format_value).collect();
            format!("#[{}]", items_str.join(" "))
        }
        Value::NativeFunction { name, .. } => format!("<native-function:{}>", name),
        Value::Actor(id) => format!("<actor:{}>", id),
        Value::Error { kind, message, .. } => format!("<error:{}:{}>", kind, message),
    }
}
