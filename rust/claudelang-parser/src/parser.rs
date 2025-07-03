//! High-performance recursive descent parser for ClaudeLang

use crate::error::ParseError;
use crate::lexer::{Lexer, Token};
use claudelang_core::ast::{EffectType, Graph, Literal, Node, NodeId, Pattern, ImportItem, ExportItem};
use bumpalo::Bump;

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    graph: Graph,
    #[allow(dead_code)]
    arena: Option<&'a Bump>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            graph: Graph::new(),
            arena: None,
        }
    }
    
    pub fn with_arena(source: &'a str, arena: &'a Bump) -> Self {
        Self {
            lexer: Lexer::new(source),
            graph: Graph::new(),
            arena: Some(arena),
        }
    }
    
    pub fn parse(&mut self) -> ParseResult<Graph> {
        // Parse all top-level expressions
        let mut last_node = None;
        
        while self.lexer.peek_token().is_some() {
            let node_id = self.parse_expr()?;
            last_node = Some(node_id);
        }
        
        // Set the last expression as root
        if let Some(root) = last_node {
            self.graph.root_id = Some(root);
        }
        
        Ok(std::mem::replace(&mut self.graph, Graph::new()))
    }
    
    fn parse_expr(&mut self) -> ParseResult<NodeId> {
        match self.lexer.peek_token() {
            Some(Token::LParen) => self.parse_list(),
            Some(Token::LBracket) => self.parse_list_literal(),
            Some(Token::Integer(_)) => self.parse_integer(),
            Some(Token::Float(_)) => self.parse_float(),
            Some(Token::String(_)) => self.parse_string(),
            Some(Token::Boolean(_)) => self.parse_boolean(),
            Some(Token::Symbol(_)) => self.parse_symbol(),
            Some(Token::QualifiedSymbol(_)) => self.parse_qualified_symbol(),
            Some(_) => Err(ParseError::InvalidSyntax("Expected expression".to_string())),
            None => Err(ParseError::UnexpectedEof),
        }
    }
    
    fn parse_list(&mut self) -> ParseResult<NodeId> {
        self.expect_token(Token::LParen)?;
        
        // Check for empty list
        if matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            self.lexer.next_token();
            let node = Node::List(vec![]);
            return Ok(self.graph.add_node(node));
        }
        
        // Check for special forms
        if let Some(Token::Symbol(sym)) = self.lexer.peek_token() {
            match *sym {
                "lambda" => return self.parse_lambda(),
                "let" => return self.parse_let(),
                "letrec" => return self.parse_letrec(),
                "if" => return self.parse_if(),
                "do" => return self.parse_sequence(),
                "effect" => return self.parse_effect(),
                "match" => return self.parse_match(),
                "async" => return self.parse_async(),
                "await" => return self.parse_await(),
                "spawn" => return self.parse_spawn(),
                "chan" => return self.parse_channel(),
                "send!" => return self.parse_send(),
                "recv!" => return self.parse_receive(),
                "module" => return self.parse_module(),
                "import" => return self.parse_import(),
                "export" => return self.parse_export(),
                "spec:contract" => return self.parse_contract(),
                _ => {}
            }
        }
        
        // Regular function application
        self.parse_application()
    }
    
    fn parse_application(&mut self) -> ParseResult<NodeId> {
        let function = self.parse_expr()?;
        let mut args = Vec::new();
        
        while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            args.push(self.parse_expr()?);
        }
        
        self.expect_token(Token::RParen)?;
        
        let node = Node::Application { function, args };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_lambda(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("lambda")?;
        self.expect_token(Token::LParen)?;
        
        // Parse parameters
        let mut params = Vec::new();
        while let Some(Token::Symbol(name)) = self.lexer.peek_token() {
            params.push(name.to_string());
            self.lexer.next_token();
        }
        
        self.expect_token(Token::RParen)?;
        
        // Parse body
        let body = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Lambda { params, body };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_let(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("let")?;
        self.expect_token(Token::LParen)?;
        
        // Parse bindings
        let mut bindings = Vec::new();
        while matches!(self.lexer.peek_token(), Some(Token::LParen)) {
            self.lexer.next_token(); // consume LParen
            
            if let Some(Token::Symbol(name)) = self.lexer.next_token() {
                let value = self.parse_expr()?;
                bindings.push((name.to_string(), value));
                self.expect_token(Token::RParen)?;
            } else {
                return Err(ParseError::InvalidSyntax("Expected binding name".to_string()));
            }
        }
        
        self.expect_token(Token::RParen)?;
        
        // Parse body
        let body = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Let { bindings, body };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_letrec(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("letrec")?;
        self.expect_token(Token::LParen)?;
        
        // Parse bindings
        let mut bindings = Vec::new();
        while matches!(self.lexer.peek_token(), Some(Token::LParen)) {
            self.lexer.next_token(); // consume LParen
            
            if let Some(Token::Symbol(name)) = self.lexer.next_token() {
                let value = self.parse_expr()?;
                bindings.push((name.to_string(), value));
                self.expect_token(Token::RParen)?;
            } else {
                return Err(ParseError::InvalidSyntax("Expected binding name".to_string()));
            }
        }
        
        self.expect_token(Token::RParen)?;
        
        // Parse body
        let body = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Letrec { bindings, body };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_if(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("if")?;
        
        let condition = self.parse_expr()?;
        let then_branch = self.parse_expr()?;
        let else_branch = self.parse_expr()?;
        
        self.expect_token(Token::RParen)?;
        
        let node = Node::If {
            condition,
            then_branch,
            else_branch,
        };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_sequence(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("do")?;
        
        let mut exprs = Vec::new();
        while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            exprs.push(self.parse_expr()?);
        }
        
        self.expect_token(Token::RParen)?;
        
        // Convert to nested let expressions for now
        // TODO: Add Sequence node type
        if exprs.is_empty() {
            let node = Node::Literal(Literal::Nil);
            Ok(self.graph.add_node(node))
        } else if exprs.len() == 1 {
            Ok(exprs[0])
        } else {
            // Create nested structure
            let mut result = exprs.pop().unwrap();
            while let Some(expr) = exprs.pop() {
                let node = Node::Let {
                    bindings: vec![("_".to_string(), expr)],
                    body: result,
                };
                result = self.graph.add_node(node);
            }
            Ok(result)
        }
    }
    
    fn parse_effect(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("effect")?;
        
        // Parse effect type:operation
        if let Some(Token::Symbol(effect_spec)) = self.lexer.next_token() {
            let (effect_type, operation) = if let Some(colon_pos) = effect_spec.find(':') {
                let effect_str = &effect_spec[..colon_pos];
                let operation = &effect_spec[colon_pos + 1..];
                
                let effect_type = match effect_str {
                    "io" => EffectType::IO,
                    "state" => EffectType::State,
                    "error" => EffectType::Error,
                    "time" => EffectType::Time,
                    "network" => EffectType::Network,
                    "random" => EffectType::Random,
                    _ => EffectType::IO,
                };
                
                (effect_type, operation.to_string())
            } else {
                (EffectType::IO, effect_spec.to_string())
            };
            
            // Parse arguments
            let mut args = Vec::new();
            while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
                args.push(self.parse_expr()?);
            }
            
            self.expect_token(Token::RParen)?;
            
            let node = Node::Effect {
                effect_type,
                operation,
                args,
            };
            Ok(self.graph.add_node(node))
        } else {
            Err(ParseError::InvalidSyntax("Expected effect specification".to_string()))
        }
    }
    
    fn parse_match(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("match")?;
        
        let expr = self.parse_expr()?;
        let mut branches = Vec::new();
        
        while matches!(self.lexer.peek_token(), Some(Token::LParen)) {
            self.lexer.next_token(); // consume LParen
            
            let pattern = self.parse_pattern()?;
            let body = self.parse_expr()?;
            branches.push((pattern, body));
            
            self.expect_token(Token::RParen)?;
        }
        
        self.expect_token(Token::RParen)?;
        
        let node = Node::Match { expr, branches };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        match self.lexer.peek_token().cloned() {
            Some(Token::Symbol(name)) if name == "_" => {
                self.lexer.next_token();
                Ok(Pattern::Wildcard)
            }
            Some(Token::Symbol(name)) if !name.is_empty() && name.chars().next().unwrap().is_lowercase() => {
                self.lexer.next_token();
                Ok(Pattern::Variable(name.to_string()))
            }
            Some(Token::Symbol(name)) if !name.is_empty() && name.chars().next().unwrap().is_uppercase() => {
                self.lexer.next_token();
                let constructor_name = name.to_string();
                let mut patterns = Vec::new();
                
                // Parse sub-patterns
                while !matches!(self.lexer.peek_token(), Some(Token::RParen) | Some(Token::LParen)) {
                    patterns.push(self.parse_pattern()?);
                }
                
                Ok(Pattern::Constructor {
                    name: constructor_name,
                    patterns,
                })
            }
            Some(Token::Integer(n)) => {
                self.lexer.next_token();
                Ok(Pattern::Literal(Literal::Integer(n)))
            }
            Some(Token::String(_)) => {
                let s = self.parse_string_literal()?;
                Ok(Pattern::Literal(Literal::String(s)))
            }
            Some(Token::Boolean(b)) => {
                self.lexer.next_token();
                Ok(Pattern::Literal(Literal::Boolean(b)))
            }
            _ => Err(ParseError::InvalidSyntax("Invalid pattern".to_string())),
        }
    }
    
    fn parse_list_literal(&mut self) -> ParseResult<NodeId> {
        self.expect_token(Token::LBracket)?;
        
        let mut elements = Vec::new();
        while !matches!(self.lexer.peek_token(), Some(Token::RBracket)) {
            elements.push(self.parse_expr()?);
        }
        
        self.expect_token(Token::RBracket)?;
        
        // Build list using cons operations
        let nil = Node::List(vec![]);
        let mut result = self.graph.add_node(nil);
        
        for elem in elements.into_iter().rev() {
            // Create cons application
            let cons_node = Node::Variable { name: "cons".to_string() };
            let cons_id = self.graph.add_node(cons_node);
            
            let app = Node::Application {
                function: cons_id,
                args: vec![elem, result],
            };
            result = self.graph.add_node(app);
        }
        
        Ok(result)
    }
    
    fn parse_integer(&mut self) -> ParseResult<NodeId> {
        if let Some(Token::Integer(n)) = self.lexer.next_token() {
            let node = Node::Literal(Literal::Integer(n));
            Ok(self.graph.add_node(node))
        } else {
            Err(ParseError::InvalidSyntax("Expected integer".to_string()))
        }
    }
    
    fn parse_float(&mut self) -> ParseResult<NodeId> {
        if let Some(Token::Float(f)) = self.lexer.next_token() {
            let node = Node::Literal(Literal::Float(f));
            Ok(self.graph.add_node(node))
        } else {
            Err(ParseError::InvalidSyntax("Expected float".to_string()))
        }
    }
    
    fn parse_string(&mut self) -> ParseResult<NodeId> {
        let s = self.parse_string_literal()?;
        let node = Node::Literal(Literal::String(s));
        Ok(self.graph.add_node(node))
    }
    
    fn parse_string_literal(&mut self) -> ParseResult<String> {
        if let Some(Token::String(s)) = self.lexer.next_token() {
            Ok(s)
        } else {
            Err(ParseError::InvalidSyntax("Expected string".to_string()))
        }
    }
    
    fn parse_boolean(&mut self) -> ParseResult<NodeId> {
        if let Some(Token::Boolean(b)) = self.lexer.next_token() {
            let node = Node::Literal(Literal::Boolean(b));
            Ok(self.graph.add_node(node))
        } else {
            Err(ParseError::InvalidSyntax("Expected boolean".to_string()))
        }
    }
    
    fn parse_symbol(&mut self) -> ParseResult<NodeId> {
        if let Some(Token::Symbol(name)) = self.lexer.next_token() {
            let node = Node::Variable { name: name.to_string() };
            Ok(self.graph.add_node(node))
        } else {
            Err(ParseError::InvalidSyntax("Expected symbol".to_string()))
        }
    }
    
    fn expect_token(&mut self, expected: Token) -> ParseResult<()> {
        if let Some(token) = self.lexer.next_token() {
            if std::mem::discriminant(&token) == std::mem::discriminant(&expected) {
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken {
                    position: self.lexer.span().start,
                    expected: format!("{:?}", expected),
                    found: format!("{:?}", token),
                })
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }
    
    fn parse_async(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("async")?;
        let body = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Async { body };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_await(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("await")?;
        let expr = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Await { expr };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_spawn(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("spawn")?;
        let expr = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Spawn { expr };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_channel(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("chan")?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Channel;
        Ok(self.graph.add_node(node))
    }
    
    fn parse_send(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("send!")?;
        let channel = self.parse_expr()?;
        let value = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Send { channel, value };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_receive(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("recv!")?;
        let channel = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Receive { channel };
        Ok(self.graph.add_node(node))
    }
    
    fn expect_symbol(&mut self, expected: &str) -> ParseResult<()> {
        if let Some(Token::Symbol(name)) = self.lexer.next_token() {
            if name == expected {
                Ok(())
            } else {
                Err(ParseError::UnexpectedToken {
                    position: self.lexer.span().start,
                    expected: expected.to_string(),
                    found: name.to_string(),
                })
            }
        } else {
            Err(ParseError::InvalidSyntax(format!("Expected symbol '{}'", expected)))
        }
    }
    
    fn parse_qualified_symbol(&mut self) -> ParseResult<NodeId> {
        if let Some(Token::QualifiedSymbol(qualified)) = self.lexer.next_token() {
            let parts: Vec<&str> = qualified.split('.').collect();
            if parts.len() == 2 {
                let node = Node::QualifiedVariable {
                    module_name: parts[0].to_string(),
                    variable_name: parts[1].to_string(),
                };
                Ok(self.graph.add_node(node))
            } else {
                Err(ParseError::InvalidSyntax("Invalid qualified symbol".to_string()))
            }
        } else {
            Err(ParseError::InvalidSyntax("Expected qualified symbol".to_string()))
        }
    }
    
    fn parse_module(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("module")?;
        
        // Parse module name
        let name = if let Some(Token::Symbol(n)) = self.lexer.next_token() {
            n.to_string()
        } else {
            return Err(ParseError::InvalidSyntax("Expected module name".to_string()));
        };
        
        // Parse optional export list
        let mut exports = Vec::new();
        if matches!(self.lexer.peek_token(), Some(Token::LParen)) {
            // Check if this is an export list
            self.lexer.next_token(); // consume (
            if matches!(self.lexer.peek_token(), Some(Token::Symbol("export"))) {
                self.expect_symbol("export")?;
                
                while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
                    if let Some(Token::Symbol(export_name)) = self.lexer.next_token() {
                        exports.push(export_name.to_string());
                    } else {
                        return Err(ParseError::InvalidSyntax("Expected export name".to_string()));
                    }
                }
                self.expect_token(Token::RParen)?;
            } else {
                // This was the start of the body, put the paren back
                // Since we can't put tokens back, we'll parse the body as a list starting here
                let body = self.parse_application()?;
                self.expect_token(Token::RParen)?;
                
                let node = Node::Module { name, exports, body };
                return Ok(self.graph.add_node(node));
            }
        }
        
        // Parse body
        let body = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        
        let node = Node::Module { name, exports, body };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_import(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("import")?;
        
        // Parse module path (string)
        let module_path = if let Some(Token::String(path)) = self.lexer.next_token() {
            path
        } else {
            return Err(ParseError::InvalidSyntax("Expected module path string".to_string()));
        };
        
        // Parse import list or *
        let mut import_list = Vec::new();
        let mut import_all = false;
        
        if matches!(self.lexer.peek_token(), Some(Token::Symbol("*"))) {
            self.lexer.next_token();
            import_all = true;
        } else if matches!(self.lexer.peek_token(), Some(Token::LParen)) {
            self.lexer.next_token(); // consume (
            
            while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
                if let Some(Token::Symbol(name)) = self.lexer.next_token() {
                    // Check for 'as' syntax
                    let mut alias = None;
                    if matches!(self.lexer.peek_token(), Some(Token::Symbol("as"))) {
                        self.lexer.next_token(); // consume 'as'
                        if let Some(Token::Symbol(alias_name)) = self.lexer.next_token() {
                            alias = Some(alias_name.to_string());
                        } else {
                            return Err(ParseError::InvalidSyntax("Expected alias name".to_string()));
                        }
                    }
                    import_list.push(ImportItem {
                        name: name.to_string(),
                        alias,
                    });
                } else {
                    return Err(ParseError::InvalidSyntax("Expected import name".to_string()));
                }
            }
            self.expect_token(Token::RParen)?;
        }
        
        self.expect_token(Token::RParen)?;
        
        let node = Node::Import {
            module_path,
            import_list,
            import_all,
        };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_export(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("export")?;
        
        let mut export_list = Vec::new();
        
        // Parse export list
        while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            if let Some(Token::Symbol(name)) = self.lexer.next_token() {
                // Check for 'as' syntax
                let mut alias = None;
                if matches!(self.lexer.peek_token(), Some(Token::Symbol("as"))) {
                    self.lexer.next_token(); // consume 'as'
                    if let Some(Token::Symbol(alias_name)) = self.lexer.next_token() {
                        alias = Some(alias_name.to_string());
                    } else {
                        return Err(ParseError::InvalidSyntax("Expected alias name".to_string()));
                    }
                }
                export_list.push(ExportItem {
                    name: name.to_string(),
                    alias,
                });
            } else {
                return Err(ParseError::InvalidSyntax("Expected export name".to_string()));
            }
        }
        
        self.expect_token(Token::RParen)?;
        
        let node = Node::Export { export_list };
        Ok(self.graph.add_node(node))
    }
    
    fn parse_contract(&mut self) -> ParseResult<NodeId> {
        self.expect_symbol("spec:contract")?;
        
        // Parse function name
        let function_name = match self.lexer.next_token() {
            Some(Token::Symbol(name)) => name.to_string(),
            _ => return Err(ParseError::InvalidSyntax("Expected function name in contract".to_string())),
        };
        
        let mut preconditions = Vec::new();
        let mut postconditions = Vec::new();
        let mut invariants = Vec::new();
        let mut complexity = None;
        let mut pure = true; // Default to pure
        
        // Parse contract clauses
        while !matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            match self.lexer.next_token() {
                Some(Token::Keyword(keyword)) => {
                    match keyword {
                        ":requires" | ":pre" => {
                            // Parse preconditions list
                            self.expect_token(Token::LBracket)?;
                            while !matches!(self.lexer.peek_token(), Some(Token::RBracket)) {
                                let expr = self.parse_expr()?;
                                preconditions.push(expr);
                            }
                            self.expect_token(Token::RBracket)?;
                        }
                        ":ensures" | ":post" => {
                            // Parse postconditions list
                            self.expect_token(Token::LBracket)?;
                            while !matches!(self.lexer.peek_token(), Some(Token::RBracket)) {
                                let expr = self.parse_expr()?;
                                postconditions.push(expr);
                            }
                            self.expect_token(Token::RBracket)?;
                        }
                        ":invariant" => {
                            // Parse invariants list
                            self.expect_token(Token::LBracket)?;
                            while !matches!(self.lexer.peek_token(), Some(Token::RBracket)) {
                                let expr = self.parse_expr()?;
                                invariants.push(expr);
                            }
                            self.expect_token(Token::RBracket)?;
                        }
                        ":complexity" => {
                            // Parse complexity string
                            match self.lexer.next_token() {
                                Some(Token::String(s)) => complexity = Some(s.to_string()),
                                _ => return Err(ParseError::InvalidSyntax("Expected string for complexity".to_string())),
                            }
                        }
                        ":pure" => {
                            // Parse purity boolean
                            match self.lexer.next_token() {
                                Some(Token::Boolean(b)) => pure = b,
                                Some(Token::Symbol("true")) => pure = true,
                                Some(Token::Symbol("false")) => pure = false,
                                _ => return Err(ParseError::InvalidSyntax("Expected boolean for :pure".to_string())),
                            }
                        }
                        _ => return Err(ParseError::InvalidSyntax(format!("Unknown contract keyword: {}", keyword))),
                    }
                }
                _ => return Err(ParseError::InvalidSyntax("Expected contract keyword".to_string())),
            }
        }
        
        self.expect_token(Token::RParen)?;
        
        let node = Node::Contract {
            function_name,
            preconditions,
            postconditions,
            invariants,
            complexity,
            pure,
        };
        Ok(self.graph.add_node(node))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    
    #[test]
    fn test_parse_arithmetic() {
        let result = parse("(+ 1 2)").unwrap();
        assert!(result.root_id.is_some());
    }
    
    #[test]
    fn test_parse_lambda() {
        let result = parse("(lambda (x y) (+ x y))").unwrap();
        assert!(result.root_id.is_some());
    }
    
    #[test]
    fn test_parse_let() {
        let result = parse("(let ((x 10) (y 20)) (+ x y))").unwrap();
        assert!(result.root_id.is_some());
    }
    
    #[test]
    fn test_parse_list_literal() {
        let result = parse("[1 2 3]").unwrap();
        assert!(result.root_id.is_some());
    }
    
    #[test]
    fn test_parse_module() {
        let result = parse(r#"(module math (export sin cos) (lambda (x) x))"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Module { name, exports, body: _ } => {
                assert_eq!(name, "math");
                assert_eq!(exports, &vec!["sin".to_string(), "cos".to_string()]);
            }
            _ => panic!("Expected Module node"),
        }
    }
    
    #[test]
    fn test_parse_module_no_exports() {
        let result = parse(r#"(module utils (+ 1 2))"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Module { name, exports, body: _ } => {
                assert_eq!(name, "utils");
                assert!(exports.is_empty());
            }
            _ => panic!("Expected Module node"),
        }
    }
    
    #[test]
    fn test_parse_import_all() {
        let result = parse(r#"(import "math" *)"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Import { module_path, import_list, import_all } => {
                assert_eq!(module_path, "math");
                assert!(import_list.is_empty());
                assert!(*import_all);
            }
            _ => panic!("Expected Import node"),
        }
    }
    
    #[test]
    fn test_parse_import_list() {
        let result = parse(r#"(import "math" (sin cos))"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Import { module_path, import_list, import_all } => {
                assert_eq!(module_path, "math");
                assert_eq!(import_list.len(), 2);
                assert_eq!(import_list[0].name, "sin");
                assert!(import_list[0].alias.is_none());
                assert_eq!(import_list[1].name, "cos");
                assert!(import_list[1].alias.is_none());
                assert!(!*import_all);
            }
            _ => panic!("Expected Import node"),
        }
    }
    
    #[test]
    fn test_parse_import_with_alias() {
        let result = parse(r#"(import "math" (sin as sine))"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Import { module_path, import_list, import_all } => {
                assert_eq!(module_path, "math");
                assert_eq!(import_list.len(), 1);
                assert_eq!(import_list[0].name, "sin");
                assert_eq!(import_list[0].alias, Some("sine".to_string()));
                assert!(!*import_all);
            }
            _ => panic!("Expected Import node"),
        }
    }
    
    #[test]
    fn test_parse_export() {
        let result = parse(r#"(export foo bar)"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Export { export_list } => {
                assert_eq!(export_list.len(), 2);
                assert_eq!(export_list[0].name, "foo");
                assert!(export_list[0].alias.is_none());
                assert_eq!(export_list[1].name, "bar");
                assert!(export_list[1].alias.is_none());
            }
            _ => panic!("Expected Export node"),
        }
    }
    
    #[test]
    fn test_parse_export_with_alias() {
        let result = parse(r#"(export internal-fn as public-fn)"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Export { export_list } => {
                assert_eq!(export_list.len(), 1);
                assert_eq!(export_list[0].name, "internal-fn");
                assert_eq!(export_list[0].alias, Some("public-fn".to_string()));
            }
            _ => panic!("Expected Export node"),
        }
    }
    
    #[test]
    fn test_parse_simple_contract() {
        let result = parse(r#"(spec:contract add
            :requires [(>= x 0) (>= y 0)]
            :ensures [(>= result 0)]
            :complexity "O(1)")"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Contract { function_name, preconditions, postconditions, invariants, complexity, pure } => {
                assert_eq!(function_name, "add");
                assert_eq!(preconditions.len(), 2);
                assert_eq!(postconditions.len(), 1);
                assert!(invariants.is_empty());
                assert_eq!(complexity, &Some("O(1)".to_string()));
                assert_eq!(*pure, true);
            }
            _ => panic!("Expected Contract node"),
        }
    }
    
    #[test]
    fn test_parse_contract_with_pre_post() {
        let result = parse(r#"(spec:contract divide
            :pre [(not= y 0)]
            :post [(= result (/ x y))])"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Contract { function_name, preconditions, postconditions, .. } => {
                assert_eq!(function_name, "divide");
                assert_eq!(preconditions.len(), 1);
                assert_eq!(postconditions.len(), 1);
            }
            _ => panic!("Expected Contract node"),
        }
    }
    
    #[test]
    fn test_parse_impure_contract() {
        let result = parse(r#"(spec:contract read-file
            :requires [(file-exists? path)]
            :ensures [(string? result)]
            :pure false)"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Contract { function_name, pure, .. } => {
                assert_eq!(function_name, "read-file");
                assert_eq!(*pure, false);
            }
            _ => panic!("Expected Contract node"),
        }
    }
    
    #[test]
    fn test_parse_qualified_variable() {
        let result = parse(r#"math.sin"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::QualifiedVariable { module_name, variable_name } => {
                assert_eq!(module_name, "math");
                assert_eq!(variable_name, "sin");
            }
            _ => panic!("Expected QualifiedVariable node"),
        }
    }
    
    #[test]
    fn test_parse_qualified_in_expression() {
        let result = parse(r#"(math.sin 3.14)"#).unwrap();
        assert!(result.root_id.is_some());
        
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Application { function, args } => {
                assert_eq!(args.len(), 1);
                match result.get_node(*function).unwrap() {
                    Node::QualifiedVariable { module_name, variable_name } => {
                        assert_eq!(module_name, "math");
                        assert_eq!(variable_name, "sin");
                    }
                    _ => panic!("Expected QualifiedVariable node"),
                }
            }
            _ => panic!("Expected Application node"),
        }
    }
}