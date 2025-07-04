//! DOM effect handler for UI operations

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, value::Value, error::Error};
use dashmap::DashMap;
use std::sync::Arc;
use serde_json;

#[derive(Debug, Clone)]
pub struct DomNode {
    pub id: String,
    pub tag: String,
    pub attributes: std::collections::HashMap<String, String>,
    pub children: Vec<String>, // Child node IDs
    pub text_content: Option<String>,
    pub event_listeners: std::collections::HashMap<String, Vec<String>>, // event -> handler IDs
}

pub struct DomHandler {
    nodes: Arc<DashMap<String, DomNode>>,
    root_id: Arc<Mutex<Option<String>>>,
    next_id: Arc<Mutex<u64>>,
}

use tokio::sync::Mutex;

impl DomHandler {
    pub fn new() -> Self {
        Self {
            nodes: Arc::new(DashMap::new()),
            root_id: Arc::new(Mutex::new(None)),
            next_id: Arc::new(Mutex::new(0)),
        }
    }
    
    async fn generate_id(&self) -> String {
        let mut id = self.next_id.lock().await;
        *id += 1;
        format!("node-{}", *id)
    }
}

#[async_trait]
impl EffectHandler for DomHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Dom
    }
    
    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "get_attribute" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(node_id)), Some(Value::String(attr))) = 
                        (args.get(0), args.get(1)) {
                        if let Some(node) = self.nodes.get(node_id) {
                            Ok(node.attributes.get(attr)
                                .map(|v| Value::String(v.clone()))
                                .unwrap_or(Value::Nil))
                        } else {
                            Err(Error::Runtime("Node not found".to_string()))
                        }
                    } else {
                        Err(Error::Runtime("dom:get_attribute requires node ID and attribute name".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:get_attribute requires 2 arguments".to_string()))
                }
            }
            "get_text" => {
                if let Some(Value::String(node_id)) = args.first() {
                    if let Some(node) = self.nodes.get(node_id) {
                        Ok(node.text_content
                            .as_ref()
                            .map(|t| Value::String(t.clone()))
                            .unwrap_or(Value::Nil))
                    } else {
                        Err(Error::Runtime("Node not found".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:get_text requires node ID".to_string()))
                }
            }
            "serialize" => {
                // Serialize DOM to JSON
                let nodes: std::collections::HashMap<String, serde_json::Value> = 
                    self.nodes.iter()
                        .map(|entry| {
                            let node = entry.value();
                            let mut json = serde_json::json!({
                                "id": node.id,
                                "tag": node.tag,
                                "attributes": node.attributes,
                                "children": node.children,
                            });
                            if let Some(text) = &node.text_content {
                                json["text"] = serde_json::Value::String(text.clone());
                            }
                            (entry.key().clone(), json)
                        })
                        .collect();
                
                Ok(Value::String(serde_json::to_string(&nodes)
                    .unwrap_or_else(|_| "{}".to_string())))
            }
            _ => Err(Error::Runtime(format!("Unknown DOM sync operation: {}", operation))),
        }
    }
    
    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "create_element" => {
                if let Some(Value::String(tag)) = args.first() {
                    let node_id = self.generate_id().await;
                    let node = DomNode {
                        id: node_id.clone(),
                        tag: tag.clone(),
                        attributes: std::collections::HashMap::new(),
                        children: Vec::new(),
                        text_content: None,
                        event_listeners: std::collections::HashMap::new(),
                    };
                    
                    self.nodes.insert(node_id.clone(), node);
                    Ok(Value::String(node_id))
                } else {
                    Err(Error::Runtime("dom:create_element requires tag name".to_string()))
                }
            }
            "set_attribute" => {
                if args.len() >= 3 {
                    if let (Some(Value::String(node_id)), 
                            Some(Value::String(attr)), 
                            Some(value)) = (args.get(0), args.get(1), args.get(2)) {
                        if let Some(mut node) = self.nodes.get_mut(node_id) {
                            node.attributes.insert(attr.clone(), value.to_string());
                            Ok(Value::Nil)
                        } else {
                            Err(Error::Runtime("Node not found".to_string()))
                        }
                    } else {
                        Err(Error::Runtime("dom:set_attribute requires node ID, attribute name, and value".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:set_attribute requires 3 arguments".to_string()))
                }
            }
            "set_text" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(node_id)), Some(text)) = 
                        (args.get(0), args.get(1)) {
                        if let Some(mut node) = self.nodes.get_mut(node_id) {
                            node.text_content = Some(text.to_string());
                            Ok(Value::Nil)
                        } else {
                            Err(Error::Runtime("Node not found".to_string()))
                        }
                    } else {
                        Err(Error::Runtime("dom:set_text requires node ID and text".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:set_text requires 2 arguments".to_string()))
                }
            }
            "append_child" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(parent_id)), Some(Value::String(child_id))) = 
                        (args.get(0), args.get(1)) {
                        if self.nodes.contains_key(child_id) {
                            if let Some(mut parent) = self.nodes.get_mut(parent_id) {
                                parent.children.push(child_id.clone());
                                Ok(Value::Nil)
                            } else {
                                Err(Error::Runtime("Parent node not found".to_string()))
                            }
                        } else {
                            Err(Error::Runtime("Child node not found".to_string()))
                        }
                    } else {
                        Err(Error::Runtime("dom:append_child requires parent and child node IDs".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:append_child requires 2 arguments".to_string()))
                }
            }
            "remove_child" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(parent_id)), Some(Value::String(child_id))) = 
                        (args.get(0), args.get(1)) {
                        if let Some(mut parent) = self.nodes.get_mut(parent_id) {
                            parent.children.retain(|id| id != child_id);
                            Ok(Value::Nil)
                        } else {
                            Err(Error::Runtime("Parent node not found".to_string()))
                        }
                    } else {
                        Err(Error::Runtime("dom:remove_child requires parent and child node IDs".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:remove_child requires 2 arguments".to_string()))
                }
            }
            "set_root" => {
                if let Some(Value::String(node_id)) = args.first() {
                    if self.nodes.contains_key(node_id) {
                        let mut root = self.root_id.lock().await;
                        *root = Some(node_id.clone());
                        Ok(Value::Nil)
                    } else {
                        Err(Error::Runtime("Node not found".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:set_root requires node ID".to_string()))
                }
            }
            "get_root" => {
                let root = self.root_id.lock().await;
                Ok(root.as_ref()
                    .map(|id| Value::String(id.clone()))
                    .unwrap_or(Value::Nil))
            }
            "query_selector" => {
                if let Some(Value::String(selector)) = args.first() {
                    // Simplified selector matching (only supports tag names for now)
                    let matching_nodes: Vec<String> = self.nodes.iter()
                        .filter(|entry| entry.value().tag == *selector)
                        .map(|entry| entry.key().clone())
                        .collect();
                    
                    Ok(Value::List(matching_nodes.into_iter()
                        .map(Value::String)
                        .collect()))
                } else {
                    Err(Error::Runtime("dom:query_selector requires selector".to_string()))
                }
            }
            "add_event_listener" => {
                if args.len() >= 3 {
                    if let (Some(Value::String(node_id)), 
                            Some(Value::String(event)), 
                            Some(Value::String(handler_id))) = 
                        (args.get(0), args.get(1), args.get(2)) {
                        if let Some(mut node) = self.nodes.get_mut(node_id) {
                            node.event_listeners
                                .entry(event.clone())
                                .or_insert_with(Vec::new)
                                .push(handler_id.clone());
                            Ok(Value::Nil)
                        } else {
                            Err(Error::Runtime("Node not found".to_string()))
                        }
                    } else {
                        Err(Error::Runtime("dom:add_event_listener requires node ID, event type, and handler ID".to_string()))
                    }
                } else {
                    Err(Error::Runtime("dom:add_event_listener requires 3 arguments".to_string()))
                }
            }
            _ => self.handle_sync(operation, args),
        }
    }
    
    fn is_async_operation(&self, operation: &str) -> bool {
        matches!(operation, 
            "create_element" | "set_attribute" | "set_text" | 
            "append_child" | "remove_child" | "set_root" | 
            "get_root" | "query_selector" | "add_event_listener"
        )
    }
}