//! Minimal S-expression parser for test compatibility
//! 
//! This module provides a minimal S-expression parser that is only used
//! for running legacy tests. It is not part of the main FluentAI parser.

use crate::error::{ErrorKind, ParseError};
use fluentai_core::ast::{Graph, Literal, Node, NodeId, Pattern, EffectType};
use std::collections::VecDeque;

pub struct SExpParser {
    tokens: VecDeque<Token>,
    graph: Graph,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Float(f64),
    String(String),
    True,
    False,
    Nil,
}

impl SExpParser {
    pub fn parse(source: &str) -> Result<Graph, ParseError> {
        let tokens = tokenize(source)?;
        let mut parser = SExpParser {
            tokens: tokens.into(),
            graph: Graph::new(),
        };
        
        let mut expressions = Vec::new();
        while !parser.tokens.is_empty() {
            expressions.push(parser.parse_expr()?);
        }
        
        // If multiple expressions, wrap in a block
        let root = if expressions.len() == 1 {
            expressions[0]
        } else {
            parser.graph.add_node(Node::Block { 
                expressions,
                is_async: false 
            }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))?
        };
        
        parser.graph.root_id = Some(root);
        Ok(parser.graph)
    }
    
    fn parse_expr(&mut self) -> Result<NodeId, ParseError> {
        match self.tokens.front() {
            Some(Token::LParen) => self.parse_list(),
            Some(Token::Integer(n)) => {
                let n = *n;
                self.tokens.pop_front();
                self.graph.add_node(Node::Literal(Literal::Integer(n)))
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::Float(f)) => {
                let f = *f;
                self.tokens.pop_front();
                self.graph.add_node(Node::Literal(Literal::Float(f)))
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                self.tokens.pop_front();
                self.graph.add_node(Node::Literal(Literal::String(s)))
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::True) => {
                self.tokens.pop_front();
                self.graph.add_node(Node::Literal(Literal::Boolean(true)))
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::False) => {
                self.tokens.pop_front();
                self.graph.add_node(Node::Literal(Literal::Boolean(false)))
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::Nil) => {
                self.tokens.pop_front();
                self.graph.add_node(Node::Literal(Literal::Nil))
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::Symbol(s)) => {
                let s = s.clone();
                self.tokens.pop_front();
                self.graph.add_node(Node::Variable { name: s })
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
            Some(Token::RParen) => Err(ParseError::InvalidSyntax("Unexpected )".to_string())),
            None => Err(ParseError::InvalidSyntax("Unexpected end of input".to_string())),
        }
    }
    
    fn parse_list(&mut self) -> Result<NodeId, ParseError> {
        self.expect(Token::LParen)?;
        
        if let Some(Token::RParen) = self.tokens.front() {
            self.tokens.pop_front();
            return self.graph.add_node(Node::Literal(Literal::Nil))
                .map_err(|e| ParseError::InvalidSyntax(e.to_string()));
        }
        
        let first = self.tokens.front()
            .ok_or_else(|| ParseError::InvalidSyntax("Expected expression".to_string()))?;
        
        match first {
            Token::Symbol(s) => {
                let s = s.clone();
                self.tokens.pop_front();
                self.parse_special_form(&s)
            }
            _ => {
                // Function application
                let func = self.parse_expr()?;
                let mut args = Vec::new();
                
                while !matches!(self.tokens.front(), Some(Token::RParen)) {
                    args.push(self.parse_expr()?);
                }
                
                self.expect(Token::RParen)?;
                
                self.graph.add_node(Node::Application {
                    function: func,
                    args,
                }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
        }
    }
    
    fn parse_special_form(&mut self, form: &str) -> Result<NodeId, ParseError> {
        match form {
            "lambda" => self.parse_lambda(),
            "let" => self.parse_let(),
            "if" => self.parse_if(),
            "+" | "-" | "*" | "/" | "%" => self.parse_arithmetic(form),
            "=" | "!=" | "<" | ">" | "<=" | ">=" => self.parse_comparison(form),
            "and" => self.parse_and(),
            "or" => self.parse_or(),
            "not" => self.parse_not(),
            "str" => self.parse_string_concat(),
            "!" => self.parse_send(),
            "recv!" => self.parse_recv(),
            "chan" => self.parse_chan(),
            "actor" => self.parse_actor(),
            "receive" => self.parse_actor_receive(),
            "become" => self.parse_become(),
            _ => {
                // Unknown form, treat as function call
                let func = self.graph.add_node(Node::Variable { name: form.to_string() })
                    .map_err(|e| ParseError::InvalidSyntax(e.to_string()))?;
                
                let mut args = Vec::new();
                while !matches!(self.tokens.front(), Some(Token::RParen)) {
                    args.push(self.parse_expr()?);
                }
                
                self.expect(Token::RParen)?;
                
                self.graph.add_node(Node::Application {
                    function: func,
                    args,
                }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
            }
        }
    }
    
    fn parse_lambda(&mut self) -> Result<NodeId, ParseError> {
        // (lambda (params...) body)
        self.expect(Token::LParen)?;
        
        let mut params = Vec::new();
        while !matches!(self.tokens.front(), Some(Token::RParen)) {
            if let Some(Token::Symbol(s)) = self.tokens.front() {
                params.push(s.clone());
                self.tokens.pop_front();
            } else {
                return Err(ParseError::InvalidSyntax("Expected parameter name".to_string()));
            }
        }
        
        self.expect(Token::RParen)?;
        let body = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Lambda { params, body })
            .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_let(&mut self) -> Result<NodeId, ParseError> {
        // (let ((x val) (y val) ...) body)
        self.expect(Token::LParen)?;
        
        let mut bindings = Vec::new();
        
        while !matches!(self.tokens.front(), Some(Token::RParen)) {
            self.expect(Token::LParen)?;
            
            let name = match self.tokens.pop_front() {
                Some(Token::Symbol(s)) => s,
                _ => return Err(ParseError::InvalidSyntax("Expected binding name".to_string())),
            };
            
            let value = self.parse_expr()?;
            self.expect(Token::RParen)?;
            
            bindings.push((name, value));
        }
        
        self.expect(Token::RParen)?;
        let body = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Let { bindings, body })
            .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_if(&mut self) -> Result<NodeId, ParseError> {
        let condition = self.parse_expr()?;
        let then_branch = self.parse_expr()?;
        let else_branch = if matches!(self.tokens.front(), Some(Token::RParen)) {
            self.graph.add_node(Node::Literal(Literal::Nil))
                .map_err(|e| ParseError::InvalidSyntax(e.to_string()))?
        } else {
            self.parse_expr()?
        };
        
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::If {
            condition,
            then_branch,
            else_branch,
        }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_arithmetic(&mut self, op: &str) -> Result<NodeId, ParseError> {
        use fluentai_core::ast::ArithmeticOp::*;
        
        let op_type = match op {
            "+" => Add,
            "-" => Subtract,
            "*" => Multiply,
            "/" => Divide,
            "%" => Modulo,
            _ => unreachable!(),
        };
        
        let left = self.parse_expr()?;
        let right = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Arithmetic {
            op: op_type,
            left,
            right,
        }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_comparison(&mut self, op: &str) -> Result<NodeId, ParseError> {
        use fluentai_core::ast::ComparisonOp::*;
        
        let op_type = match op {
            "=" => Equal,
            "!=" => NotEqual,
            "<" => Less,
            ">" => Greater,
            "<=" => LessEqual,
            ">=" => GreaterEqual,
            _ => unreachable!(),
        };
        
        let left = self.parse_expr()?;
        let right = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Comparison {
            op: op_type,
            left,
            right,
        }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_and(&mut self) -> Result<NodeId, ParseError> {
        let mut operands = Vec::new();
        
        while !matches!(self.tokens.front(), Some(Token::RParen)) {
            operands.push(self.parse_expr()?);
        }
        
        self.expect(Token::RParen)?;
        
        if operands.is_empty() {
            return self.graph.add_node(Node::Literal(Literal::Boolean(true)))
                .map_err(|e| ParseError::InvalidSyntax(e.to_string()));
        }
        
        let mut result = operands.pop().unwrap();
        while let Some(operand) = operands.pop() {
            result = self.graph.add_node(Node::And {
                left: operand,
                right: result,
            }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))?;
        }
        
        Ok(result)
    }
    
    fn parse_or(&mut self) -> Result<NodeId, ParseError> {
        let mut operands = Vec::new();
        
        while !matches!(self.tokens.front(), Some(Token::RParen)) {
            operands.push(self.parse_expr()?);
        }
        
        self.expect(Token::RParen)?;
        
        if operands.is_empty() {
            return self.graph.add_node(Node::Literal(Literal::Boolean(false)))
                .map_err(|e| ParseError::InvalidSyntax(e.to_string()));
        }
        
        let mut result = operands.pop().unwrap();
        while let Some(operand) = operands.pop() {
            result = self.graph.add_node(Node::Or {
                left: operand,
                right: result,
            }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))?;
        }
        
        Ok(result)
    }
    
    fn parse_not(&mut self) -> Result<NodeId, ParseError> {
        let operand = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Not { operand })
            .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_string_concat(&mut self) -> Result<NodeId, ParseError> {
        let mut parts = Vec::new();
        
        while !matches!(self.tokens.front(), Some(Token::RParen)) {
            parts.push(self.parse_expr()?);
        }
        
        self.expect(Token::RParen)?;
        
        // Create nested StringConcat nodes
        if parts.is_empty() {
            return self.graph.add_node(Node::Literal(Literal::String("".to_string())))
                .map_err(|e| ParseError::InvalidSyntax(e.to_string()));
        }
        
        let mut result = parts[0];
        for part in parts.into_iter().skip(1) {
            result = self.graph.add_node(Node::StringConcat {
                left: result,
                right: part,
            }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))?;
        }
        
        Ok(result)
    }
    
    fn parse_send(&mut self) -> Result<NodeId, ParseError> {
        // (! target message)
        let target = self.parse_expr()?;
        let message = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        // Check if target is an actor (we'll determine at runtime)
        // For now, assume it could be either channel or actor
        self.graph.add_node(Node::ActorSend {
            actor: target,
            message,
        }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_recv(&mut self) -> Result<NodeId, ParseError> {
        // (recv! channel)
        let channel = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Receive { channel })
            .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_chan(&mut self) -> Result<NodeId, ParseError> {
        // (chan) or (chan capacity)
        let capacity = if matches!(self.tokens.front(), Some(Token::RParen)) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Channel { capacity })
            .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_actor(&mut self) -> Result<NodeId, ParseError> {
        // (actor initial_state handler)
        let initial_state = self.parse_expr()?;
        let handler = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Actor {
            initial_state,
            handler,
        }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_actor_receive(&mut self) -> Result<NodeId, ParseError> {
        // (receive ((pattern1) handler1) ((pattern2) handler2) ...)
        let mut patterns = Vec::new();
        
        while !matches!(self.tokens.front(), Some(Token::RParen)) {
            self.expect(Token::LParen)?;
            
            // Parse pattern
            self.expect(Token::LParen)?;
            let pattern = self.parse_pattern()?;
            self.expect(Token::RParen)?;
            
            // Parse handler
            let handler = self.parse_expr()?;
            self.expect(Token::RParen)?;
            
            patterns.push((pattern, handler));
        }
        
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::ActorReceive {
            patterns,
            timeout: None,
        }).map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.tokens.front() {
            Some(Token::Symbol(s)) => {
                let s = s.clone();
                self.tokens.pop_front();
                
                // Check for special patterns
                match s.as_str() {
                    "_" => Ok(Pattern::Wildcard),
                    "ping" | "get" | "set" => {
                        // These are constructor patterns
                        Ok(Pattern::Constructor {
                            name: s,
                            patterns: vec![],
                        })
                    }
                    _ => Ok(Pattern::Variable(s)),
                }
            }
            Some(Token::Integer(n)) => {
                let n = *n;
                self.tokens.pop_front();
                Ok(Pattern::Literal(Literal::Integer(n)))
            }
            Some(Token::String(s)) => {
                let s = s.clone();
                self.tokens.pop_front();
                Ok(Pattern::Literal(Literal::String(s)))
            }
            Some(Token::True) => {
                self.tokens.pop_front();
                Ok(Pattern::Literal(Literal::Boolean(true)))
            }
            Some(Token::False) => {
                self.tokens.pop_front();
                Ok(Pattern::Literal(Literal::Boolean(false)))
            }
            Some(Token::Nil) => {
                self.tokens.pop_front();
                Ok(Pattern::Literal(Literal::Nil))
            }
            Some(Token::LParen) => {
                // Constructor pattern like (hello name)
                self.tokens.pop_front();
                
                if let Some(Token::Symbol(constructor)) = self.tokens.front() {
                    let constructor = constructor.clone();
                    self.tokens.pop_front();
                    
                    let mut patterns = Vec::new();
                    while !matches!(self.tokens.front(), Some(Token::RParen)) {
                        patterns.push(self.parse_pattern()?);
                    }
                    
                    self.expect(Token::RParen)?;
                    
                    Ok(Pattern::Constructor {
                        name: constructor,
                        patterns,
                    })
                } else {
                    Err(ParseError::InvalidSyntax("Expected constructor name".to_string()))
                }
            }
            _ => Err(ParseError::InvalidSyntax("Invalid pattern".to_string())),
        }
    }
    
    fn parse_become(&mut self) -> Result<NodeId, ParseError> {
        // (become new_state)
        let new_state = self.parse_expr()?;
        self.expect(Token::RParen)?;
        
        self.graph.add_node(Node::Become { new_state })
            .map_err(|e| ParseError::InvalidSyntax(e.to_string()))
    }
    
    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        match self.tokens.pop_front() {
            Some(token) if std::mem::discriminant(&token) == std::mem::discriminant(&expected) => Ok(()),
            Some(token) => Err(ParseError::InvalidSyntax(
                format!("Expected {:?}, got {:?}", expected, token)
            )),
            None => Err(ParseError::InvalidSyntax("Unexpected end of input".to_string())),
        }
    }
}

fn tokenize(source: &str) -> Result<Vec<Token>, ParseError> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    
    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            ';' => {
                // Skip comments
                chars.next();
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '\n' {
                        break;
                    }
                }
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            '"' => {
                chars.next();
                let mut string = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == '"' {
                        chars.next();
                        break;
                    }
                    if ch == '\\' {
                        chars.next();
                        if let Some(&escaped) = chars.peek() {
                            chars.next();
                            match escaped {
                                'n' => string.push('\n'),
                                't' => string.push('\t'),
                                'r' => string.push('\r'),
                                '\\' => string.push('\\'),
                                '"' => string.push('"'),
                                _ => {
                                    string.push('\\');
                                    string.push(escaped);
                                }
                            }
                        }
                    } else {
                        string.push(ch);
                        chars.next();
                    }
                }
                tokens.push(Token::String(string));
            }
            _ => {
                let mut symbol = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_whitespace() || ch == '(' || ch == ')' || ch == ';' {
                        break;
                    }
                    symbol.push(ch);
                    chars.next();
                }
                
                // Check for special tokens
                match symbol.as_str() {
                    "true" => tokens.push(Token::True),
                    "false" => tokens.push(Token::False),
                    "nil" => tokens.push(Token::Nil),
                    _ => {
                        // Try to parse as number
                        if let Ok(n) = symbol.parse::<i64>() {
                            tokens.push(Token::Integer(n));
                        } else if let Ok(f) = symbol.parse::<f64>() {
                            tokens.push(Token::Float(f));
                        } else {
                            tokens.push(Token::Symbol(symbol));
                        }
                    }
                }
            }
        }
    }
    
    Ok(tokens)
}

#[cfg(test)]
pub fn parse_sexp(source: &str) -> Result<Graph, ParseError> {
    SExpParser::parse(source)
}