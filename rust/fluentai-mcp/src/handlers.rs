//! Tool handlers for the MCP server

use anyhow::Result;
use serde_json::{json, Value as JsonValue};
use tracing::debug;

use fluentai_parser::Parser;
use fluentai_vm::{VM, Compiler, Bytecode};
use fluentai_vm::bytecode::Value;

use crate::server::ServerState;

/// Handle eval tool - execute FluentAi code
pub async fn handle_eval(_state: &mut ServerState, args: Option<&JsonValue>) -> Result<JsonValue> {
    let args = args.ok_or_else(|| anyhow::anyhow!("Missing arguments"))?;
    let code = args["code"].as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing code parameter"))?;
    
    debug!("Evaluating code: {}", code);
    
    // Parse the code
    let mut parser = Parser::new(code);
    let ast = parser.parse()
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
    
    // Compile the AST to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)
        .map_err(|e| anyhow::anyhow!("Compile error: {:?}", e))?;
    
    // Create a new VM with the compiled bytecode and run it
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => {
            let result_str = format_value(&result);
            Ok(json!({
                "content": [{
                    "type": "text",
                    "text": result_str
                }]
            }))
        }
        Err(e) => {
            Ok(json!({
                "content": [{
                    "type": "text",
                    "text": format!("Error: {:?}", e)
                }],
                "isError": true
            }))
        }
    }
}

/// Handle search_docs tool - search documentation
pub async fn handle_search_docs(state: &ServerState, args: Option<&JsonValue>) -> Result<JsonValue> {
    let args = args.ok_or_else(|| anyhow::anyhow!("Missing arguments"))?;
    let query = args["query"].as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing query parameter"))?;
    
    debug!("Searching documentation for: {}", query);
    
    // Only show user-facing documentation to the LLM
    let results = state.docs.search_user_facing(query);
    
    let mut content = String::new();
    content.push_str(&format!("Found {} results for '{}'\n\n", results.len(), query));
    
    for doc in results.iter().take(10) {
        content.push_str(&format!("## {}\n", doc.name));
        content.push_str(&format!("**Syntax:** `{}`\n", doc.syntax));
        content.push_str(&format!("**Category:** {:?}\n", doc.category));
        content.push_str(&format!("{}\n", doc.description));
        
        if !doc.examples.is_empty() {
            content.push_str("\n**Examples:**\n");
            for example in &doc.examples {
                content.push_str(&format!("```claudelang\n{}\n```\n", example));
            }
        }
        
        if !doc.see_also.is_empty() {
            content.push_str(&format!("\n**See also:** {}\n", doc.see_also.join(", ")));
        }
        
        content.push_str("\n---\n\n");
    }
    
    Ok(json!({
        "content": [{
            "type": "text",
            "text": content
        }]
    }))
}

/// Handle get_syntax tool - get documentation for a specific construct
pub async fn handle_get_syntax(state: &ServerState, args: Option<&JsonValue>) -> Result<JsonValue> {
    let args = args.ok_or_else(|| anyhow::anyhow!("Missing arguments"))?;
    let name = args["name"].as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing name parameter"))?;
    
    debug!("Getting syntax for: {}", name);
    
    if let Some(doc) = state.docs.get(name) {
        let mut content = String::new();
        content.push_str(&format!("# {}\n\n", doc.name));
        content.push_str(&format!("**Syntax:** `{}`\n\n", doc.syntax));
        content.push_str(&format!("**Category:** {:?}\n\n", doc.category));
        content.push_str(&format!("{}\n\n", doc.description));
        
        if !doc.examples.is_empty() {
            content.push_str("## Examples\n\n");
            for example in &doc.examples {
                content.push_str(&format!("```claudelang\n{}\n```\n\n", example));
            }
        }
        
        if !doc.see_also.is_empty() {
            content.push_str(&format!("## See Also\n\n{}\n", doc.see_also.join(", ")));
        }
        
        Ok(json!({
            "content": [{
                "type": "text",
                "text": content
            }]
        }))
    } else {
        Ok(json!({
            "content": [{
                "type": "text",
                "text": format!("No documentation found for '{}'", name)
            }],
            "isError": true
        }))
    }
}

/// Handle list_features tool - list all language features
pub async fn handle_list_features(state: &ServerState, _args: Option<&JsonValue>) -> Result<JsonValue> {
    debug!("Listing all features");
    
    // Only show user-facing documentation to the LLM
    let all_docs = state.docs.list_user_facing();
    
    let mut content = String::new();
    content.push_str(&format!("# FluentAi Features ({} total)\n\n", all_docs.len()));
    
    // Group by category
    let mut by_category: std::collections::HashMap<_, Vec<_>> = std::collections::HashMap::new();
    for doc in all_docs {
        by_category.entry(doc.category).or_default().push(doc);
    }
    
    // Sort categories
    let mut categories: Vec<_> = by_category.keys().copied().collect();
    categories.sort_by_key(|c| format!("{:?}", c));
    
    for category in categories {
        content.push_str(&format!("## {:?}\n\n", category));
        
        let mut docs = by_category[&category].clone();
        docs.sort_by_key(|d| d.name.clone());
        
        for doc in docs {
            content.push_str(&format!("- **{}** - `{}` - {}\n", 
                doc.name, doc.syntax, 
                doc.description.split('.').next().unwrap_or(&doc.description)
            ));
        }
        content.push_str("\n");
    }
    
    Ok(json!({
        "content": [{
            "type": "text",
            "text": content
        }]
    }))
}

/// Handle reset_interpreter tool - clear interpreter state
pub async fn handle_reset_interpreter(state: &mut ServerState, _args: Option<&JsonValue>) -> Result<JsonValue> {
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
        Value::Int(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Bool(b) => b.to_string(),
        Value::Nil => "nil".to_string(),
        Value::List(items) => {
            let items_str: Vec<_> = items.iter().map(format_value).collect();
            format!("[{}]", items_str.join(" "))
        }
        Value::Map(map) => {
            let pairs: Vec<_> = map.iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v)))
                .collect();
            format!("{{{}}}", pairs.join(", "))
        }
        Value::Function { .. } => "<function>".to_string(),
        Value::Promise(id) => format!("<promise:{}>", id),
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
    }
}