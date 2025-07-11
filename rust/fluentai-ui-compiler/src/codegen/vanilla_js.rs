//! Vanilla JavaScript code generation

use crate::error::{CompilerError, Result};
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use std::collections::HashSet;

/// Compile a FluentAi graph to vanilla JavaScript
pub fn compile_graph(
    graph: &Graph,
    imports: &mut HashSet<String>,
    helpers: &mut HashSet<String>,
) -> Result<String> {
    let mut compiler = VanillaJsCompiler::new(graph, imports, helpers);

    if let Some(root_id) = graph.root_id {
        compiler.compile_node(root_id)
    } else {
        Err(CompilerError::CodegenError(
            "No root node in graph".to_string(),
        ))
    }
}

struct VanillaJsCompiler<'a> {
    graph: &'a Graph,
    imports: &'a mut HashSet<String>,
    helpers: &'a mut HashSet<String>,
    indent_level: usize,
}

impl<'a> VanillaJsCompiler<'a> {
    fn new(
        graph: &'a Graph,
        imports: &'a mut HashSet<String>,
        helpers: &'a mut HashSet<String>,
    ) -> Self {
        Self {
            graph,
            imports,
            helpers,
            indent_level: 0,
        }
    }

    fn compile_node(&mut self, node_id: NodeId) -> Result<String> {
        match self.graph.get_node(node_id) {
            Some(node) => self.compile_node_impl(node, node_id),
            None => Err(CompilerError::CodegenError(format!(
                "Node {:?} not found in graph",
                node_id
            ))),
        }
    }

    fn compile_node_impl(&mut self, node: &Node, node_id: NodeId) -> Result<String> {
        match node {
            Node::Literal(lit) => self.compile_literal(lit),
            Node::Variable { name } => Ok(self.compile_variable(name)),
            Node::Lambda { params, body } => self.compile_lambda(params, *body),
            Node::Application { function, args } => self.compile_application(*function, args),
            Node::Let { bindings, body } => self.compile_let(bindings, *body),
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if(*condition, *then_branch, *else_branch),
            Node::List(items) => self.compile_list(items),
            Node::Effect {
                effect_type,
                operation,
                args,
            } => self.compile_effect(&effect_type.to_string(), operation, args),
            _ => Err(CompilerError::UnsupportedFeature(format!(
                "Node type {:?} not yet supported for JS compilation",
                node
            ))),
        }
    }

    fn compile_literal(&self, lit: &Literal) -> Result<String> {
        Ok(match lit {
            Literal::Nil => "null".to_string(),
            Literal::Boolean(b) => b.to_string(),
            Literal::Integer(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s.replace("\"", "\\\"")),
            Literal::Symbol(s) => format!("Symbol.for(\"{}\")", s),
        })
    }

    fn compile_variable(&self, name: &str) -> String {
        // Convert FluentAi variable names to JS-safe names
        name.replace("-", "_").replace("?", "_p").replace("!", "_x")
    }

    fn compile_lambda(&mut self, params: &[String], body: NodeId) -> Result<String> {
        let params_str = params
            .iter()
            .map(|p| self.compile_variable(p))
            .collect::<Vec<_>>()
            .join(", ");

        let body_str = self.compile_node(body)?;

        Ok(format!("({}) => {}", params_str, body_str))
    }

    fn compile_application(&mut self, function: NodeId, args: &[NodeId]) -> Result<String> {
        // Check for special forms
        if let Some(Node::Variable { name }) = self.graph.get_node(function) {
            match name.as_str() {
                "ui:element" => return self.compile_ui_element(args),
                "ui:text" => return self.compile_ui_text(args),
                "ui:bind" => return self.compile_ui_bind(args),
                "ui:on" => return self.compile_ui_event(args),
                "ui:if" => return self.compile_ui_conditional(args),
                "ui:for" => return self.compile_ui_loop(args),
                _ => {}
            }
        }

        // Regular function application
        let func_str = self.compile_node(function)?;
        let args_str = args
            .iter()
            .map(|arg| self.compile_node(*arg))
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        Ok(format!("{}({})", func_str, args_str))
    }

    fn compile_let(&mut self, bindings: &[(String, NodeId)], body: NodeId) -> Result<String> {
        let mut result = String::from("(() => {\n");
        self.indent_level += 1;

        for (name, value) in bindings {
            let indent = self.indent();
            let name_str = self.compile_variable(name);
            let value_str = self.compile_node(*value)?;
            result.push_str(&format!("{}const {} = {};\n", indent, name_str, value_str));
        }

        let indent = self.indent();
        let body_str = self.compile_node(body)?;
        result.push_str(&format!("{}return {};\n", indent, body_str));

        self.indent_level -= 1;
        result.push_str("})()");

        Ok(result)
    }

    fn compile_if(
        &mut self,
        condition: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
    ) -> Result<String> {
        let cond_str = self.compile_node(condition)?;
        let then_str = self.compile_node(then_branch)?;
        let else_str = self.compile_node(else_branch)?;

        Ok(format!("({} ? {} : {})", cond_str, then_str, else_str))
    }

    fn compile_list(&mut self, items: &[NodeId]) -> Result<String> {
        let items_str = items
            .iter()
            .map(|item| self.compile_node(*item))
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        Ok(format!("[{}]", items_str))
    }

    fn compile_effect(
        &mut self,
        effect_type: &str,
        operation: &str,
        args: &[NodeId],
    ) -> Result<String> {
        match effect_type {
            "Dom" => self.compile_dom_effect(operation, args),
            "State" => self.compile_state_effect(operation, args),
            _ => Err(CompilerError::UnsupportedFeature(format!(
                "Effect type {} not supported in JS compilation",
                effect_type
            ))),
        }
    }

    // UI compilation methods
    fn compile_ui_element(&mut self, args: &[NodeId]) -> Result<String> {
        if args.len() < 1 {
            return Err(CompilerError::CodegenError(
                "ui:element requires tag name".to_string(),
            ));
        }

        self.helpers.insert("createElement".to_string());

        let tag = self.compile_node(args[0])?;
        let props = if args.len() > 1 {
            self.compile_node(args[1])?
        } else {
            "{}".to_string()
        };

        let children = if args.len() > 2 {
            args[2..]
                .iter()
                .map(|child| self.compile_node(*child))
                .collect::<Result<Vec<_>>>()?
                .join(", ")
        } else {
            String::new()
        };

        Ok(format!("createElement({}, {}, [{}])", tag, props, children))
    }

    fn compile_ui_text(&mut self, args: &[NodeId]) -> Result<String> {
        if args.is_empty() {
            return Ok("\"\"".to_string());
        }

        let text = self.compile_node(args[0])?;
        Ok(format!("document.createTextNode({})", text))
    }

    fn compile_ui_bind(&mut self, args: &[NodeId]) -> Result<String> {
        if args.len() < 2 {
            return Err(CompilerError::CodegenError(
                "ui:bind requires property and value".to_string(),
            ));
        }

        self.helpers.insert("reactive".to_string());

        let prop = self.compile_node(args[0])?;
        let value = self.compile_node(args[1])?;

        Ok(format!("{{ [{}]: reactive({}) }}", prop, value))
    }

    fn compile_ui_event(&mut self, args: &[NodeId]) -> Result<String> {
        if args.len() < 2 {
            return Err(CompilerError::CodegenError(
                "ui:on requires event and handler".to_string(),
            ));
        }

        let event = self.compile_node(args[0])?;
        let handler = self.compile_node(args[1])?;

        Ok(format!(
            "{{ on{}: {} }}",
            event
                .trim_matches('"')
                .chars()
                .next()
                .unwrap()
                .to_uppercase()
                .collect::<String>()
                + &event.trim_matches('"')[1..],
            handler
        ))
    }

    fn compile_ui_conditional(&mut self, args: &[NodeId]) -> Result<String> {
        if args.len() < 2 {
            return Err(CompilerError::CodegenError(
                "ui:if requires condition and content".to_string(),
            ));
        }

        let condition = self.compile_node(args[0])?;
        let content = self.compile_node(args[1])?;
        let else_content = if args.len() > 2 {
            self.compile_node(args[2])?
        } else {
            "null".to_string()
        };

        Ok(format!("({} ? {} : {})", condition, content, else_content))
    }

    fn compile_ui_loop(&mut self, args: &[NodeId]) -> Result<String> {
        if args.len() < 3 {
            return Err(CompilerError::CodegenError(
                "ui:for requires items, binding, and template".to_string(),
            ));
        }

        let items = self.compile_node(args[0])?;
        let binding = match self.graph.get_node(args[1]) {
            Some(Node::Variable { name }) => self.compile_variable(name),
            _ => {
                return Err(CompilerError::CodegenError(
                    "ui:for binding must be a variable".to_string(),
                ))
            }
        };
        let template = self.compile_node(args[2])?;

        Ok(format!("{}.map(({}) => {})", items, binding, template))
    }

    // Effect compilation methods
    fn compile_dom_effect(&mut self, operation: &str, args: &[NodeId]) -> Result<String> {
        match operation {
            "create-element" => self.compile_ui_element(args),
            "set-attribute" => {
                if args.len() < 3 {
                    return Err(CompilerError::CodegenError(
                        "set-attribute requires element, name, and value".to_string(),
                    ));
                }
                let elem = self.compile_node(args[0])?;
                let name = self.compile_node(args[1])?;
                let value = self.compile_node(args[2])?;
                Ok(format!("{}.setAttribute({}, {})", elem, name, value))
            }
            _ => Err(CompilerError::UnsupportedFeature(format!(
                "DOM operation {} not supported",
                operation
            ))),
        }
    }

    fn compile_state_effect(&mut self, operation: &str, args: &[NodeId]) -> Result<String> {
        self.helpers.insert("reactive".to_string());

        match operation {
            "reactive:ref" => {
                let initial = if args.is_empty() {
                    "null".to_string()
                } else {
                    self.compile_node(args[0])?
                };
                Ok(format!("reactive.ref({})", initial))
            }
            "reactive:get" => {
                if args.is_empty() {
                    return Err(CompilerError::CodegenError(
                        "reactive:get requires ref".to_string(),
                    ));
                }
                let ref_str = self.compile_node(args[0])?;
                Ok(format!("{}.value", ref_str))
            }
            "reactive:set" => {
                if args.len() < 2 {
                    return Err(CompilerError::CodegenError(
                        "reactive:set requires ref and value".to_string(),
                    ));
                }
                let ref_str = self.compile_node(args[0])?;
                let value = self.compile_node(args[1])?;
                Ok(format!("{}.value = {}", ref_str, value))
            }
            _ => Err(CompilerError::UnsupportedFeature(format!(
                "State operation {} not supported",
                operation
            ))),
        }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }
}
