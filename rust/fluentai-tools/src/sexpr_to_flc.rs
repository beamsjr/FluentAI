//! S-Expression to FLC syntax translator

use fluentai_core::ast::{Graph, Literal, Node, NodeId, Pattern};
use std::fmt::Write;

pub struct Translator {
    graph: Graph,
    output: String,
    indent_level: usize,
}

impl Translator {
    pub fn new(graph: Graph) -> Self {
        Self {
            graph,
            output: String::new(),
            indent_level: 0,
        }
    }
    
    /// Translate the AST graph to FLC syntax
    pub fn translate(mut self) -> String {
        if let Some(root) = self.graph.root_id {
            self.translate_node(root);
        }
        self.output
    }
    
    /// Translate a single node
    fn translate_node(&mut self, node_id: NodeId) {
        let node = self.graph.nodes.get(&node_id).cloned();
        match node {
            Some(node) => self.translate_node_inner(node_id, &node),
            None => self.output.push_str("/* unknown node */"),
        }
    }
    
    fn translate_node_inner(&mut self, node_id: NodeId, node: &Node) {
        match node {
            Node::Literal(lit) => self.translate_literal(lit),
            Node::Variable { name } => self.output.push_str(&self.translate_identifier(name)),
            Node::Lambda { params, body } => self.translate_lambda(params, *body),
            Node::Let { bindings, body } => self.translate_let(bindings, *body),
            Node::Letrec { bindings, body } => self.translate_letrec(bindings, *body),
            Node::If { condition, then_branch, else_branch } => {
                self.translate_if(*condition, *then_branch, *else_branch)
            }
            Node::Application { function, args } => self.translate_application(*function, args),
            Node::List(items) => self.translate_list(items),
            Node::Begin(exprs) => self.translate_begin(exprs),
            Node::Define { name, value, exported } => {
                self.translate_define(name, *value, *exported)
            }
            Node::Match { expr, branches } => self.translate_match(*expr, branches),
            Node::Try { body, catch_branches, finally_block } => {
                self.translate_try(*body, catch_branches, finally_block.as_ref().copied())
            }
            Node::Module { name, definitions, exports } => {
                self.translate_module(name, definitions, exports)
            }
            Node::Import { module_path, import_list, import_all } => {
                self.translate_import(module_path, import_list, *import_all)
            }
            Node::Effect { effect_type, operation, args } => {
                self.translate_effect(*effect_type, operation, args)
            }
            Node::Async { body } => self.translate_async(*body),
            Node::Await { expr } => self.translate_await(*expr),
            Node::Spawn { expr } => self.translate_spawn(*expr),
            Node::Channel { capacity } => self.translate_channel(capacity.as_ref().copied()),
            Node::Send { channel, value } => self.translate_send(*channel, *value),
            Node::Receive { channel } => self.translate_receive(*channel),
            Node::Handler { handlers, body } => self.translate_handler(handlers, *body),
            Node::Export { names } => self.translate_export(names),
            Node::ExportDefine { names } => self.translate_export_define(names),
            Node::QualifiedVariable { module_name, variable_name } => {
                self.output.push_str(&format!("{}::{}", module_name, variable_name))
            }
        }
    }
    
    fn translate_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Integer(n) => write!(&mut self.output, "{}", n).unwrap(),
            Literal::Float(f) => write!(&mut self.output, "{}", f).unwrap(),
            Literal::String(s) => write!(&mut self.output, "\"{}\"", escape_string(s)).unwrap(),
            Literal::Boolean(b) => self.output.push_str(if *b { "true" } else { "false" }),
            Literal::Nil => self.output.push_str("nil"),
        }
    }
    
    fn translate_identifier(&self, name: &str) -> String {
        // Convert kebab-case to snake_case
        name.replace('-', "_")
    }
    
    fn translate_lambda(&mut self, params: &[String], body: NodeId) {
        self.output.push_str("{ |");
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.output.push_str(&self.translate_identifier(param));
        }
        self.output.push_str("| ");
        self.translate_node(body);
        self.output.push_str(" }");
    }
    
    fn translate_let(&mut self, bindings: &[(String, NodeId)], body: NodeId) {
        for (name, value) in bindings {
            self.output.push_str("let ");
            self.output.push_str(&self.translate_identifier(name));
            self.output.push_str(" = ");
            self.translate_node(*value);
            self.output.push_str("; ");
        }
        self.translate_node(body);
    }
    
    fn translate_letrec(&mut self, bindings: &[(String, NodeId)], body: NodeId) {
        for (i, (name, value)) in bindings.iter().enumerate() {
            if i == 0 {
                self.output.push_str("let rec ");
            } else {
                self.output.push_str("and ");
            }
            self.output.push_str(&self.translate_identifier(name));
            self.output.push_str(" = ");
            self.translate_node(*value);
            self.output.push_str("; ");
        }
        self.translate_node(body);
    }
    
    fn translate_if(&mut self, condition: NodeId, then_branch: NodeId, else_branch: NodeId) {
        self.output.push_str("if (");
        self.translate_node(condition);
        self.output.push_str(") { ");
        self.translate_node(then_branch);
        self.output.push_str(" }");
        
        // Check if else branch is not nil
        if !self.is_nil_node(else_branch) {
            self.output.push_str(" else { ");
            self.translate_node(else_branch);
            self.output.push_str(" }");
        }
    }
    
    fn translate_application(&mut self, function: NodeId, args: &[NodeId]) {
        // Special handling for binary operators
        if let Some(Node::Variable { name }) = self.graph.nodes.get(&function) {
            if args.len() == 2 && is_binary_operator(name) {
                // Infix operator
                self.translate_node(args[0]);
                self.output.push(' ');
                self.output.push_str(name);
                self.output.push(' ');
                self.translate_node(args[1]);
                return;
            }
            
            // Check for method-like calls
            if args.len() >= 1 && is_method_like(name) {
                // Convert to method call syntax
                self.translate_node(args[0]);
                self.output.push('.');
                self.output.push_str(&self.translate_identifier(name));
                self.output.push('(');
                for (i, arg) in args[1..].iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.translate_node(*arg);
                }
                self.output.push(')');
                return;
            }
        }
        
        // Regular function call
        self.translate_node(function);
        self.output.push('(');
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.translate_node(*arg);
        }
        self.output.push(')');
    }
    
    fn translate_list(&mut self, items: &[NodeId]) {
        self.output.push('[');
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.translate_node(*item);
        }
        self.output.push(']');
    }
    
    fn translate_begin(&mut self, exprs: &[NodeId]) {
        if exprs.len() == 1 {
            self.translate_node(exprs[0]);
        } else {
            self.output.push_str("{\n");
            self.indent_level += 1;
            for expr in exprs {
                self.indent();
                self.translate_node(*expr);
                self.output.push_str(";\n");
            }
            self.indent_level -= 1;
            self.indent();
            self.output.push('}');
        }
    }
    
    fn translate_define(&mut self, name: &str, value: NodeId, exported: bool) {
        self.output.push_str("def ");
        if exported {
            self.output.push_str("pub ");
        }
        
        // Check if value is a lambda for function definition syntax
        if let Some(Node::Lambda { params, body }) = self.graph.nodes.get(&value) {
            self.output.push_str("fn ");
            self.output.push_str(&self.translate_identifier(name));
            self.output.push('(');
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(&self.translate_identifier(param));
            }
            self.output.push_str(") {\n");
            self.indent_level += 1;
            self.indent();
            self.translate_node(*body);
            self.output.push('\n');
            self.indent_level -= 1;
            self.indent();
            self.output.push('}');
        } else {
            self.output.push_str(&self.translate_identifier(name));
            self.output.push_str(" = ");
            self.translate_node(value);
            self.output.push(';');
        }
    }
    
    fn translate_match(&mut self, expr: NodeId, branches: &[(Pattern, NodeId)]) {
        self.translate_node(expr);
        self.output.push_str(".match()");
        for (pattern, result) in branches {
            self.output.push_str(".case(");
            self.translate_pattern(pattern);
            self.output.push_str(", ");
            self.translate_node(*result);
            self.output.push(')');
        }
        self.output.push_str(".run()");
    }
    
    fn translate_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => self.output.push('_'),
            Pattern::Variable(name) => self.output.push_str(&self.translate_identifier(name)),
            Pattern::Literal(lit) => self.translate_literal(lit),
            Pattern::Constructor { name, patterns } => {
                self.output.push_str(name);
                if !patterns.is_empty() {
                    self.output.push('(');
                    for (i, p) in patterns.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        self.translate_pattern(p);
                    }
                    self.output.push(')');
                }
            }
            Pattern::List { patterns, rest } => {
                self.output.push('[');
                for (i, p) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.translate_pattern(p);
                }
                if rest.is_some() {
                    self.output.push_str(", ...");
                }
                self.output.push(']');
            }
        }
    }
    
    fn translate_try(&mut self, body: NodeId, catch_branches: &[(Pattern, NodeId)], finally: Option<NodeId>) {
        self.output.push_str("try {\n");
        self.indent_level += 1;
        self.indent();
        self.translate_node(body);
        self.output.push('\n');
        self.indent_level -= 1;
        self.indent();
        self.output.push('}');
        
        if !catch_branches.is_empty() {
            for (pattern, handler) in catch_branches {
                self.output.push_str(" catch (");
                self.translate_pattern(pattern);
                self.output.push_str(") {\n");
                self.indent_level += 1;
                self.indent();
                self.translate_node(*handler);
                self.output.push('\n');
                self.indent_level -= 1;
                self.indent();
                self.output.push('}');
            }
        }
        
        if let Some(finally_block) = finally {
            self.output.push_str(" finally {\n");
            self.indent_level += 1;
            self.indent();
            self.translate_node(finally_block);
            self.output.push('\n');
            self.indent_level -= 1;
            self.indent();
            self.output.push('}');
        }
    }
    
    fn translate_module(&mut self, name: &str, definitions: &[NodeId], exports: &[String]) {
        self.output.push_str("mod ");
        self.output.push_str(&self.translate_identifier(name));
        self.output.push_str(" {\n");
        self.indent_level += 1;
        
        for def in definitions {
            self.indent();
            self.translate_node(*def);
            self.output.push('\n');
        }
        
        if !exports.is_empty() {
            self.indent();
            self.output.push_str("pub use {");
            for (i, export) in exports.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(&self.translate_identifier(export));
            }
            self.output.push_str("};\n");
        }
        
        self.indent_level -= 1;
        self.output.push('}');
    }
    
    fn translate_import(&mut self, module_path: &[String], imports: &[fluentai_core::ast::ImportItem], import_all: bool) {
        self.output.push_str("use ");
        for (i, segment) in module_path.iter().enumerate() {
            if i > 0 {
                self.output.push_str("::");
            }
            self.output.push_str(&self.translate_identifier(segment));
        }
        
        if import_all {
            self.output.push_str("::*");
        } else if !imports.is_empty() {
            self.output.push_str("::{");
            for (i, item) in imports.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(&self.translate_identifier(&item.name));
                if let Some(alias) = &item.alias {
                    self.output.push_str(" as ");
                    self.output.push_str(&self.translate_identifier(alias));
                }
            }
            self.output.push('}');
        }
        
        self.output.push(';');
    }
    
    fn translate_effect(&mut self, effect_type: NodeId, operation: &str, args: &[NodeId]) {
        self.output.push_str("perform ");
        self.translate_node(effect_type);
        self.output.push_str("::");
        self.output.push_str(operation);
        self.output.push('(');
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.translate_node(*arg);
        }
        self.output.push(')');
    }
    
    fn translate_async(&mut self, body: NodeId) {
        self.output.push_str("async { ");
        self.translate_node(body);
        self.output.push_str(" }");
    }
    
    fn translate_await(&mut self, expr: NodeId) {
        self.translate_node(expr);
        self.output.push_str(".await()");
    }
    
    fn translate_spawn(&mut self, expr: NodeId) {
        self.output.push_str("spawn { ");
        self.translate_node(expr);
        self.output.push_str(" }");
    }
    
    fn translate_channel(&mut self, capacity: Option<NodeId>) {
        self.output.push_str("Channel::new(");
        if let Some(cap) = capacity {
            self.translate_node(cap);
        }
        self.output.push(')');
    }
    
    fn translate_send(&mut self, channel: NodeId, value: NodeId) {
        self.translate_node(channel);
        self.output.push_str(".send(");
        self.translate_node(value);
        self.output.push(')');
    }
    
    fn translate_receive(&mut self, channel: NodeId) {
        self.translate_node(channel);
        self.output.push_str(".recv()");
    }
    
    fn translate_handler(&mut self, handlers: &[(fluentai_core::ast::EffectType, Option<String>, NodeId)], body: NodeId) {
        self.translate_node(body);
        self.output.push_str(".handle {\n");
        self.indent_level += 1;
        
        for (effect_type, operation, handler) in handlers {
            self.indent();
            match effect_type {
                fluentai_core::ast::EffectType::Named(name) => self.output.push_str(name),
                fluentai_core::ast::EffectType::Variable(var) => self.output.push_str(var),
            }
            if let Some(op) = operation {
                self.output.push_str("::");
                self.output.push_str(op);
            }
            self.output.push_str(" => ");
            self.translate_node(*handler);
            self.output.push_str(",\n");
        }
        
        self.indent_level -= 1;
        self.indent();
        self.output.push('}');
    }
    
    fn translate_export(&mut self, names: &[String]) {
        self.output.push_str("pub use {");
        for (i, name) in names.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.output.push_str(&self.translate_identifier(name));
        }
        self.output.push_str("};");
    }
    
    fn translate_export_define(&mut self, names: &[fluentai_core::ast::ExportItem]) {
        for item in names {
            self.output.push_str("pub use ");
            self.output.push_str(&self.translate_identifier(&item.name));
            if let Some(alias) = &item.alias {
                self.output.push_str(" as ");
                self.output.push_str(&self.translate_identifier(alias));
            }
            self.output.push_str(";\n");
        }
    }
    
    // Helper methods
    
    fn is_nil_node(&self, node_id: NodeId) -> bool {
        matches!(
            self.graph.nodes.get(&node_id),
            Some(Node::Literal(Literal::Nil))
        )
    }
    
    fn indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push_str("    ");
        }
    }
}

fn is_binary_operator(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"
    )
}

fn is_method_like(name: &str) -> bool {
    matches!(
        name,
        "map" | "filter" | "fold" | "reduce" | "each" | "for-each" | "flat-map" |
        "take" | "drop" | "append" | "concat" | "reverse" | "sort" | "length" |
        "empty?" | "contains?" | "get" | "set" | "update" | "merge"
    )
}

fn escape_string(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\\' => "\\\\".to_string(),
            '\n' => "\\n".to_string(),
            '\t' => "\\t".to_string(),
            '\r' => "\\r".to_string(),
            c => c.to_string(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_parser::parse;
    
    #[test]
    fn test_translate_simple() {
        let source = "(+ 1 2)";
        let graph = parse(source).unwrap();
        let translator = Translator::new(graph);
        let flc = translator.translate();
        assert_eq!(flc, "1 + 2");
    }
    
    #[test]
    fn test_translate_function() {
        let source = "(define (add x y) (+ x y))";
        let graph = parse(source).unwrap();
        let translator = Translator::new(graph);
        let flc = translator.translate();
        assert!(flc.contains("def fn add(x, y)"));
    }
    
    #[test]
    fn test_translate_lambda() {
        let source = "(lambda (x) (* x 2))";
        let graph = parse(source).unwrap();
        let translator = Translator::new(graph);
        let flc = translator.translate();
        assert_eq!(flc, "{ |x| x * 2 }");
    }
    
    #[test]
    fn test_translate_method_chain() {
        let source = "(filter pred (map f list))";
        let graph = parse(source).unwrap();
        let translator = Translator::new(graph);
        let flc = translator.translate();
        assert_eq!(flc, "list.map(f).filter(pred)");
    }
}