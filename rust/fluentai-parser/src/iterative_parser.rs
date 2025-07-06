//! Iterative parser implementation for handling deeply nested structures
//! 
//! This module provides an alternative parsing approach that uses an explicit stack
//! instead of recursion, allowing it to handle arbitrarily deep nesting without
//! risking stack overflow.

use crate::error::ParseError;
use crate::lexer::{Lexer, Token};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};

/// Parsing state for iterative parsing
#[derive(Debug)]
enum ParseState {
    /// Start parsing an expression
    Expression,
    /// Parsing a list, waiting for elements
    List { elements: Vec<NodeId> },
    /// Parsing application arguments
    Application { function: NodeId, args: Vec<NodeId> },
    /// Building a list literal from elements
    ListLiteral { elements: Vec<NodeId> },
    /// Finished parsing, contains the result
    Complete(NodeId),
}

/// Frame in the parsing stack
#[derive(Debug)]
struct ParseFrame {
    state: ParseState,
    /// Position in source where this frame started
    start_pos: usize,
}

/// Iterative parser that uses an explicit stack
pub struct IterativeParser<'a> {
    lexer: Lexer<'a>,
    graph: Graph,
    /// Explicit stack for parsing
    stack: Vec<ParseFrame>,
    /// Maximum stack depth allowed
    max_depth: usize,
}

impl<'a> IterativeParser<'a> {
    /// Default maximum stack depth
    const DEFAULT_MAX_DEPTH: usize = 10000;
    
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            graph: Graph::new(),
            stack: Vec::with_capacity(1024),
            max_depth: Self::DEFAULT_MAX_DEPTH,
        }
    }
    
    pub fn with_max_depth(mut self, max_depth: usize) -> Self {
        self.max_depth = max_depth;
        self
    }
    
    /// Parse the source iteratively
    pub fn parse(mut self) -> Result<Graph, ParseError> {
        let mut results = Vec::new();
        
        // Parse all top-level expressions
        while self.lexer.peek_token().is_some() {
            let node_id = self.parse_expression()?;
            results.push(node_id);
        }
        
        // Set the last expression as root
        if let Some(&root) = results.last() {
            self.graph.root_id = Some(root);
        }
        
        Ok(self.graph)
    }
    
    /// Parse a single expression iteratively
    fn parse_expression(&mut self) -> Result<NodeId, ParseError> {
        // Initialize stack with expression state
        self.stack.clear();
        self.stack.push(ParseFrame {
            state: ParseState::Expression,
            start_pos: self.lexer.span().start,
        });
        
        // Process until we have a complete result
        while let Some(frame) = self.stack.pop() {
            // Check depth limit
            if self.stack.len() >= self.max_depth {
                return Err(ParseError::MaxDepthExceeded {
                    depth: self.stack.len(),
                    max_depth: self.max_depth,
                });
            }
            
            match frame.state {
                ParseState::Expression => {
                    self.handle_expression()?;
                }
                ParseState::List { mut elements } => {
                    self.handle_list(&mut elements)?;
                }
                ParseState::Application { function, mut args } => {
                    self.handle_application(function, &mut args)?;
                }
                ParseState::ListLiteral { mut elements } => {
                    self.handle_list_literal(&mut elements)?;
                }
                ParseState::Complete(node_id) => {
                    // If this is the final result, return it
                    if self.stack.is_empty() {
                        return Ok(node_id);
                    }
                    // Otherwise, pass the result up to the parent frame
                    self.deliver_result(node_id)?;
                }
            }
        }
        
        Err(ParseError::InvalidSyntax("Incomplete expression".to_string()))
    }
    
    /// Handle expression parsing state
    fn handle_expression(&mut self) -> Result<(), ParseError> {
        match self.lexer.peek_token().cloned() {
            Some(Token::LParen) => {
                self.lexer.next_token(); // consume (
                
                // Check for empty list
                if matches!(self.lexer.peek_token(), Some(Token::RParen)) {
                    self.lexer.next_token();
                    let node = Node::List(vec![]);
                    let node_id = self.graph.add_node(node);
                    self.stack.push(ParseFrame {
                        state: ParseState::Complete(node_id),
                        start_pos: 0,
                    });
                    return Ok(());
                }
                
                // Check for special forms
                if let Some(Token::Symbol(sym)) = self.lexer.peek_token() {
                    match *sym {
                        // For now, we'll handle special forms in the recursive parser
                        // In a full implementation, these would also be iterative
                        _ => {
                            // Regular list/application
                            self.stack.push(ParseFrame {
                                state: ParseState::List { elements: Vec::new() },
                                start_pos: self.lexer.span().start,
                            });
                        }
                    }
                } else {
                    // Regular list
                    self.stack.push(ParseFrame {
                        state: ParseState::List { elements: Vec::new() },
                        start_pos: self.lexer.span().start,
                    });
                }
            }
            Some(Token::LBracket) => {
                self.lexer.next_token(); // consume [
                self.stack.push(ParseFrame {
                    state: ParseState::ListLiteral { elements: Vec::new() },
                    start_pos: self.lexer.span().start,
                });
            }
            Some(Token::Integer(n)) => {
                self.lexer.next_token();
                let node = Node::Literal(Literal::Integer(n));
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            }
            Some(Token::Float(f)) => {
                self.lexer.next_token();
                let node = Node::Literal(Literal::Float(f));
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            }
            Some(Token::String(s)) => {
                self.lexer.next_token();
                let node = Node::Literal(Literal::String(s));
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            }
            Some(Token::Boolean(b)) => {
                self.lexer.next_token();
                let node = Node::Literal(Literal::Boolean(b));
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            }
            Some(Token::Symbol(name)) => {
                let name = name.to_string();
                self.lexer.next_token();
                let node = Node::Variable { name };
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            }
            Some(Token::QualifiedSymbol(qualified)) => {
                self.lexer.next_token();
                let parts: Vec<&str> = qualified.split('.').collect();
                if parts.len() == 2 {
                    let node = Node::QualifiedVariable {
                        module_name: parts[0].to_string(),
                        variable_name: parts[1].to_string(),
                    };
                    let node_id = self.graph.add_node(node);
                    self.stack.push(ParseFrame {
                        state: ParseState::Complete(node_id),
                        start_pos: 0,
                    });
                } else {
                    return Err(ParseError::InvalidSyntax("Invalid qualified symbol".to_string()));
                }
            }
            Some(_) => return Err(ParseError::InvalidSyntax("Expected expression".to_string())),
            None => return Err(ParseError::UnexpectedEof),
        }
        Ok(())
    }
    
    /// Handle list parsing state
    fn handle_list(&mut self, elements: &mut Vec<NodeId>) -> Result<(), ParseError> {
        // Check if we're done with the list
        if matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            self.lexer.next_token(); // consume )
            
            if elements.is_empty() {
                // Empty list
                let node = Node::List(vec![]);
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            } else {
                // Convert to application
                let function = elements[0];
                let args = elements[1..].to_vec();
                let node = Node::Application { function, args };
                let node_id = self.graph.add_node(node);
                self.stack.push(ParseFrame {
                    state: ParseState::Complete(node_id),
                    start_pos: 0,
                });
            }
        } else {
            // Need to parse next element
            self.stack.push(ParseFrame {
                state: ParseState::List { elements: elements.clone() },
                start_pos: self.lexer.span().start,
            });
            self.stack.push(ParseFrame {
                state: ParseState::Expression,
                start_pos: self.lexer.span().start,
            });
        }
        Ok(())
    }
    
    /// Handle application parsing state
    fn handle_application(&mut self, function: NodeId, args: &mut Vec<NodeId>) -> Result<(), ParseError> {
        // Check if we're done with arguments
        if matches!(self.lexer.peek_token(), Some(Token::RParen)) {
            self.lexer.next_token(); // consume )
            
            let node = Node::Application { 
                function, 
                args: args.clone() 
            };
            let node_id = self.graph.add_node(node);
            self.stack.push(ParseFrame {
                state: ParseState::Complete(node_id),
                start_pos: 0,
            });
        } else {
            // Need to parse next argument
            self.stack.push(ParseFrame {
                state: ParseState::Application { 
                    function, 
                    args: args.clone() 
                },
                start_pos: self.lexer.span().start,
            });
            self.stack.push(ParseFrame {
                state: ParseState::Expression,
                start_pos: self.lexer.span().start,
            });
        }
        Ok(())
    }
    
    /// Handle list literal parsing state
    fn handle_list_literal(&mut self, elements: &mut Vec<NodeId>) -> Result<(), ParseError> {
        // Check if we're done with the list literal
        if matches!(self.lexer.peek_token(), Some(Token::RBracket)) {
            self.lexer.next_token(); // consume ]
            
            // Build list using cons operations
            let nil = Node::List(vec![]);
            let mut result = self.graph.add_node(nil);
            
            for &elem in elements.iter().rev() {
                // Create cons application
                let cons_node = Node::Variable { name: "cons".to_string() };
                let cons_id = self.graph.add_node(cons_node);
                
                let app = Node::Application {
                    function: cons_id,
                    args: vec![elem, result],
                };
                result = self.graph.add_node(app);
            }
            
            self.stack.push(ParseFrame {
                state: ParseState::Complete(result),
                start_pos: 0,
            });
        } else {
            // Need to parse next element
            self.stack.push(ParseFrame {
                state: ParseState::ListLiteral { elements: elements.clone() },
                start_pos: self.lexer.span().start,
            });
            self.stack.push(ParseFrame {
                state: ParseState::Expression,
                start_pos: self.lexer.span().start,
            });
        }
        Ok(())
    }
    
    /// Deliver a completed result to the parent frame
    fn deliver_result(&mut self, node_id: NodeId) -> Result<(), ParseError> {
        if let Some(parent) = self.stack.last_mut() {
            match &mut parent.state {
                ParseState::List { elements } => {
                    elements.push(node_id);
                }
                ParseState::Application { args, .. } => {
                    args.push(node_id);
                }
                ParseState::ListLiteral { elements } => {
                    elements.push(node_id);
                }
                _ => {
                    return Err(ParseError::InvalidSyntax(
                        "Invalid parser state transition".to_string()
                    ));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_iterative_parse_simple() {
        let parser = IterativeParser::new("42");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_iterative_parse_list() {
        let parser = IterativeParser::new("(+ 1 2)");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_iterative_parse_nested_lists() {
        let parser = IterativeParser::new("(+ (* 2 3) (- 5 1))");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_iterative_parse_list_literal() {
        let parser = IterativeParser::new("[1 2 3]");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_iterative_parse_deeply_nested() {
        // Create a deeply nested expression
        let mut expr = String::new();
        let depth = 1000;
        
        for _ in 0..depth {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push_str(" 2)");
        }
        
        let parser = IterativeParser::new(&expr).with_max_depth(5000);
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_iterative_max_depth() {
        // Create a deeply nested expression
        let mut expr = String::new();
        let depth = 100;
        
        for _ in 0..depth {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push_str(" 2)");
        }
        
        let parser = IterativeParser::new(&expr).with_max_depth(50);
        let result = parser.parse();
        assert!(matches!(result, Err(ParseError::MaxDepthExceeded { .. })));
    }
}