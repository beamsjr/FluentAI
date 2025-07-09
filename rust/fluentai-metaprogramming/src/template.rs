//! Template-based code generation

use crate::error::{MetaprogrammingError, Result};
use fluentai_core::ast::{Graph, Node, NodeId};
use fluentai_parser::parse;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

/// A code template
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Template {
    /// Name of the template
    pub name: String,
    /// Template parameters
    pub params: Vec<TemplateParam>,
    /// Template body
    pub body: String,
    /// Optional guards/constraints
    pub guards: Vec<Guard>,
}

/// Template parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateParam {
    pub name: String,
    pub kind: ParamKind,
    pub optional: bool,
    pub default: Option<String>,
}

/// Kind of template parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParamKind {
    /// A single expression
    Expression,
    /// A list of expressions
    ExpressionList,
    /// An identifier
    Identifier,
    /// A pattern
    Pattern,
    /// A literal value
    Literal,
    /// Any value
    Any,
}

/// Guard condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Guard {
    pub condition: String,
    pub message: String,
}

/// Template instantiation context
#[derive(Debug)]
pub struct InstantiationContext {
    pub values: FxHashMap<String, TemplateValue>,
    pub graph: Graph,
}

/// Value bound to a template parameter
#[derive(Debug, Clone)]
pub enum TemplateValue {
    Node(NodeId),
    NodeList(Vec<NodeId>),
    String(String),
    Graph(Graph),
}

/// Template engine
pub struct TemplateEngine {
    templates: FxHashMap<String, Template>,
}

impl TemplateEngine {
    pub fn new() -> Self {
        Self {
            templates: FxHashMap::default(),
        }
    }

    /// Register a template
    pub fn register_template(&mut self, template: Template) {
        self.templates.insert(template.name.clone(), template);
    }

    /// Register built-in templates
    pub fn register_builtins(&mut self) {
        // List comprehension template
        self.register_template(Template {
            name: "list-comp".to_string(),
            params: vec![
                TemplateParam {
                    name: "expr".to_string(),
                    kind: ParamKind::Expression,
                    optional: false,
                    default: None,
                },
                TemplateParam {
                    name: "var".to_string(),
                    kind: ParamKind::Identifier,
                    optional: false,
                    default: None,
                },
                TemplateParam {
                    name: "list".to_string(),
                    kind: ParamKind::Expression,
                    optional: false,
                    default: None,
                },
                TemplateParam {
                    name: "filter".to_string(),
                    kind: ParamKind::Expression,
                    optional: true,
                    default: Some("true".to_string()),
                },
            ],
            body: "(map (lambda ($var) (if $filter $expr nil)) $list)".to_string(),
            guards: vec![],
        });

        // Pattern matching template
        self.register_template(Template {
            name: "match".to_string(),
            params: vec![
                TemplateParam {
                    name: "expr".to_string(),
                    kind: ParamKind::Expression,
                    optional: false,
                    default: None,
                },
                TemplateParam {
                    name: "cases".to_string(),
                    kind: ParamKind::ExpressionList,
                    optional: false,
                    default: None,
                },
            ],
            body: "match-expansion".to_string(), // Special handling
            guards: vec![],
        });

        // Class template
        self.register_template(Template {
            name: "class".to_string(),
            params: vec![
                TemplateParam {
                    name: "name".to_string(),
                    kind: ParamKind::Identifier,
                    optional: false,
                    default: None,
                },
                TemplateParam {
                    name: "fields".to_string(),
                    kind: ParamKind::ExpressionList,
                    optional: false,
                    default: None,
                },
                TemplateParam {
                    name: "methods".to_string(),
                    kind: ParamKind::ExpressionList,
                    optional: true,
                    default: Some("()".to_string()),
                },
            ],
            body: class_template_body(),
            guards: vec![],
        });
    }

    /// Instantiate a template
    pub fn instantiate(
        &self,
        template_name: &str,
        args: FxHashMap<String, TemplateValue>,
    ) -> Result<Graph> {
        let template = self.templates.get(template_name).ok_or_else(|| {
            MetaprogrammingError::TemplateError(format!("Unknown template: {}", template_name))
        })?;

        // Create context
        let mut context = InstantiationContext {
            values: args,
            graph: Graph::new(),
        };

        // Check all required parameters are provided
        for param in &template.params {
            if !param.optional && !context.values.contains_key(&param.name) {
                if let Some(default) = &param.default {
                    // Parse default value
                    let default_graph = parse(default)
                        .map_err(|e| MetaprogrammingError::ParseError(e.to_string()))?;
                    context
                        .values
                        .insert(param.name.clone(), TemplateValue::Graph(default_graph));
                } else {
                    return Err(MetaprogrammingError::TemplateError(format!(
                        "Missing required parameter: {}",
                        param.name
                    )));
                }
            }
        }

        // Check guards
        for guard in &template.guards {
            if !self.check_guard(&context, guard)? {
                return Err(MetaprogrammingError::TemplateError(format!(
                    "Guard failed: {}",
                    guard.message
                )));
            }
        }

        // Special handling for certain templates
        match template.body.as_str() {
            "match-expansion" => self.expand_match(&context),
            _ => self.expand_template_body(&template.body, &context),
        }
    }

    /// Expand template body
    fn expand_template_body(&self, body: &str, context: &InstantiationContext) -> Result<Graph> {
        let mut expanded = body.to_string();

        // Substitute parameters
        for (name, value) in &context.values {
            let substitution = match value {
                TemplateValue::Node(node_id) => self.node_to_string(&context.graph, *node_id)?,
                TemplateValue::NodeList(nodes) => {
                    let node_strs: Result<Vec<_>> = nodes
                        .iter()
                        .map(|&id| self.node_to_string(&context.graph, id))
                        .collect();
                    node_strs?.join(" ")
                }
                TemplateValue::String(s) => s.clone(),
                TemplateValue::Graph(g) => {
                    // Convert graph to string representation
                    if let Some(root) = g.root_id {
                        self.node_to_string(g, root)?
                    } else {
                        "nil".to_string()
                    }
                }
            };

            expanded = expanded.replace(&format!("${}", name), &substitution);
        }

        // Parse expanded template
        parse(&expanded).map_err(|e| MetaprogrammingError::ParseError(e.to_string()))
    }

    /// Convert node to string
    fn node_to_string(&self, graph: &Graph, node_id: NodeId) -> Result<String> {
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Variable { name } => Ok(name.clone()),
                Node::Literal(lit) => Ok(format!("{}", lit)),
                Node::List(items) => {
                    let item_strs: Result<Vec<_>> = items
                        .iter()
                        .map(|&id| self.node_to_string(graph, id))
                        .collect();
                    Ok(format!("({})", item_strs?.join(" ")))
                }
                Node::Lambda { params, body } => {
                    let body_str = self.node_to_string(graph, *body)?;
                    Ok(format!("(lambda ({}) {})", params.join(" "), body_str))
                }
                Node::Application { function, args } => {
                    let func_str = self.node_to_string(graph, *function)?;
                    let arg_strs: Result<Vec<_>> = args
                        .iter()
                        .map(|&id| self.node_to_string(graph, id))
                        .collect();
                    Ok(format!("({} {})", func_str, arg_strs?.join(" ")))
                }
                _ => Ok(format!("<node:{:?}>", node_id)),
            }
        } else {
            Err(MetaprogrammingError::NodeNotFound(node_id))
        }
    }

    /// Check a guard condition
    fn check_guard(&self, _context: &InstantiationContext, _guard: &Guard) -> Result<bool> {
        // Simplified guard checking
        Ok(true)
    }

    /// Expand match template
    fn expand_match(&self, context: &InstantiationContext) -> Result<Graph> {
        let expr = context.values.get("expr").ok_or_else(|| {
            MetaprogrammingError::TemplateError("Match requires 'expr' parameter".to_string())
        })?;

        let cases = context.values.get("cases").ok_or_else(|| {
            MetaprogrammingError::TemplateError("Match requires 'cases' parameter".to_string())
        })?;

        // Build match expression
        let mut result = String::from("(let ((#match-val ");

        // Add expression
        match expr {
            TemplateValue::Node(id) => {
                result.push_str(&self.node_to_string(&context.graph, *id)?);
            }
            _ => {
                return Err(MetaprogrammingError::TemplateError(
                    "Match expr must be a node".to_string(),
                ))
            }
        }

        result.push_str(")) (cond ");

        // Add cases
        if let TemplateValue::NodeList(case_nodes) = cases {
            for &case_id in case_nodes {
                if let Some(Node::List(items)) = context.graph.get_node(case_id) {
                    if items.len() >= 2 {
                        let pattern = self.node_to_string(&context.graph, items[0])?;
                        let body = self.node_to_string(&context.graph, items[1])?;
                        result.push_str(&format!("((match? #match-val {}) {}) ", pattern, body));
                    }
                }
            }
        }

        result.push_str("))");

        parse(&result).map_err(|e| MetaprogrammingError::ParseError(e.to_string()))
    }
}

/// Class template body
fn class_template_body() -> String {
    r#"(let ((constructor (lambda ($fields)
             (let ((self (make-object)))
               ;; Initialize fields
               $fields
               self)))
         (prototype (make-object)))
       ;; Add methods to prototype
       $methods
       ;; Return constructor with prototype
       (set-prototype constructor prototype)
       constructor)"#
        .to_string()
}

/// Template DSL for building templates
pub struct TemplateBuilder {
    name: String,
    params: Vec<TemplateParam>,
    body: String,
    guards: Vec<Guard>,
}

impl TemplateBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            params: Vec::new(),
            body: String::new(),
            guards: Vec::new(),
        }
    }

    pub fn param(mut self, name: impl Into<String>, kind: ParamKind) -> Self {
        self.params.push(TemplateParam {
            name: name.into(),
            kind,
            optional: false,
            default: None,
        });
        self
    }

    pub fn optional_param(
        mut self,
        name: impl Into<String>,
        kind: ParamKind,
        default: Option<String>,
    ) -> Self {
        self.params.push(TemplateParam {
            name: name.into(),
            kind,
            optional: true,
            default,
        });
        self
    }

    pub fn body(mut self, body: impl Into<String>) -> Self {
        self.body = body.into();
        self
    }

    pub fn guard(mut self, condition: impl Into<String>, message: impl Into<String>) -> Self {
        self.guards.push(Guard {
            condition: condition.into(),
            message: message.into(),
        });
        self
    }

    pub fn build(self) -> Template {
        Template {
            name: self.name,
            params: self.params,
            body: self.body,
            guards: self.guards,
        }
    }
}
