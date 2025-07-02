//! High-performance recursive descent parser for ClaudeLang

use crate::error::ParseError;
use crate::lexer::{Lexer, Token};
use claudelang_core::ast::{EffectType, Graph, Literal, Node, NodeId, Pattern};
use bumpalo::Bump;

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    graph: Graph,
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
                "if" => return self.parse_if(),
                "do" => return self.parse_sequence(),
                "effect" => return self.parse_effect(),
                "match" => return self.parse_match(),
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
}