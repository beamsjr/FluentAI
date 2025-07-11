//! Parser for FLC (Fluent Lambda Chain) syntax

use anyhow::{anyhow, Result};
use fluentai_core::ast::{
    Graph, ImportItem, Literal, Node, NodeId, Pattern,
};

use crate::flc_lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    graph: Graph,
    current: Option<Token<'a>>,
    position: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let current = lexer.next_token();
        Self {
            lexer,
            graph: Graph::new(),
            current,
            position: 0,
        }
    }
    
    pub fn parse(mut self) -> Result<Graph> {
        let mut items = vec![];
        
        while self.current.is_some() {
            items.push(self.parse_top_level()?);
        }
        
        if items.is_empty() {
            self.graph.root_id = Some(self.add_node(Node::Literal(Literal::Nil))?);
        } else if items.len() == 1 {
            self.graph.root_id = Some(items[0]);
        } else {
            self.graph.root_id = Some(self.add_node(Node::Begin { exprs: items })?);
        }
        
        Ok(self.graph)
    }
    
    fn parse_top_level(&mut self) -> Result<NodeId> {
        match self.current {
            Some(Token::Use) => self.parse_use_statement(),
            Some(Token::Mod) => self.parse_module(),
            Some(Token::Def) => self.parse_definition(),
            _ => self.parse_expression(),
        }
    }
    
    fn parse_definition(&mut self) -> Result<NodeId> {
        self.consume(Token::Def)?;
        
        let is_public = matches!(self.current, Some(Token::Pub));
        if is_public {
            self.advance();
        }
        
        match self.current {
            Some(Token::Fn) => self.parse_function_definition(is_public),
            Some(Token::Struct) => self.parse_struct_definition(is_public),
            Some(Token::Enum) => self.parse_enum_definition(is_public),
            Some(Token::Trait) => self.parse_trait_definition(is_public),
            Some(Token::Type) => self.parse_type_alias(is_public),
            Some(Token::Actor) => self.parse_actor_definition(is_public),
            Some(Token::Effect) => self.parse_effect_definition(is_public),
            Some(Token::LowerIdent(_)) => self.parse_value_definition(is_public),
            _ => Err(anyhow!("Expected definition after 'def'")),
        }
    }
    
    fn parse_function_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        self.consume(Token::Fn)?;
        
        let name = match self.current {
            Some(Token::LowerIdent(n)) => {
                self.advance();
                n.to_string()
            }
            _ => return Err(anyhow!("Expected function name")),
        };
        
        self.consume(Token::LParen)?;
        let mut params = vec![];
        
        while !matches!(self.current, Some(Token::RParen)) {
            if let Some(Token::LowerIdent(param)) = self.current {
                params.push(param.to_string());
                self.advance();
                
                // Optional type annotation
                if matches!(self.current, Some(Token::Colon)) {
                    self.advance();
                    self.parse_type()?;
                }
                
                if matches!(self.current, Some(Token::Comma)) {
                    self.advance();
                }
            } else {
                return Err(anyhow!("Expected parameter name"));
            }
        }
        
        self.consume(Token::RParen)?;
        
        // Optional return type
        if matches!(self.current, Some(Token::Arrow)) {
            self.advance();
            self.parse_type()?;
        }
        
        self.consume(Token::LBrace)?;
        let body = self.parse_block_expression()?;
        self.consume(Token::RBrace)?;
        
        let lambda = self.add_node(Node::Lambda { params, body })?;
        self.add_node(Node::Define { name, value: lambda })
    }
    
    fn parse_value_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        let name = match self.current {
            Some(Token::LowerIdent(n)) => {
                self.advance();
                n.to_string()
            }
            _ => return Err(anyhow!("Expected variable name")),
        };
        
        self.consume(Token::Eq)?;
        let value = self.parse_expression()?;
        
        if matches!(self.current, Some(Token::Semicolon)) {
            self.advance();
        }
        
        self.add_node(Node::Define { name, value })
    }
    
    fn parse_expression(&mut self) -> Result<NodeId> {
        self.parse_pipe_expression()
    }
    
    fn parse_pipe_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_or_expression()?;
        
        while matches!(self.current, Some(Token::Pipe)) {
            self.advance();
            let right = self.parse_or_expression()?;
            
            // Transform x |> f into (f x)
            left = match self.graph.nodes.get(&right).cloned() {
                Some(Node::Application { function, mut args }) => {
                    // f(args) becomes f(x, args)
                    args.insert(0, left);
                    self.add_node(Node::Application { function, args })?
                }
                _ => {
                    // f becomes (f x)
                    self.add_node(Node::Application { 
                        function: right, 
                        args: vec![left] 
                    })?
                }
            };
        }
        
        Ok(left)
    }
    
    fn parse_or_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_and_expression()?;
        
        while matches!(self.current, Some(Token::OrOr)) {
            self.advance();
            let right = self.parse_and_expression()?;
            let op = self.add_node(Node::Variable { name: "or".to_string() })?;
            left = self.add_node(Node::Application { 
                function: op, 
                args: vec![left, right] 
            })?;
        }
        
        Ok(left)
    }
    
    fn parse_and_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_equality_expression()?;
        
        while matches!(self.current, Some(Token::AndAnd)) {
            self.advance();
            let right = self.parse_equality_expression()?;
            let op = self.add_node(Node::Variable { name: "and".to_string() })?;
            left = self.add_node(Node::Application { 
                function: op, 
                args: vec![left, right] 
            })?;
        }
        
        Ok(left)
    }
    
    fn parse_equality_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_comparison_expression()?;
        
        loop {
            let op_name = match self.current {
                Some(Token::EqEq) => "==",
                Some(Token::NotEq) => "!=",
                _ => break,
            };
            
            self.advance();
            let right = self.parse_comparison_expression()?;
            let op = self.add_node(Node::Variable { name: op_name.to_string() })?;
            left = self.add_node(Node::Application { 
                function: op, 
                args: vec![left, right] 
            })?;
        }
        
        Ok(left)
    }
    
    fn parse_comparison_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_additive_expression()?;
        
        loop {
            let op_name = match self.current {
                Some(Token::Less) => "<",
                Some(Token::Greater) => ">",
                Some(Token::LessEq) => "<=",
                Some(Token::GreaterEq) => ">=",
                _ => break,
            };
            
            self.advance();
            let right = self.parse_additive_expression()?;
            let op = self.add_node(Node::Variable { name: op_name.to_string() })?;
            left = self.add_node(Node::Application { 
                function: op, 
                args: vec![left, right] 
            })?;
        }
        
        Ok(left)
    }
    
    fn parse_additive_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_multiplicative_expression()?;
        
        loop {
            let op_name = match self.current {
                Some(Token::Plus) => "+",
                Some(Token::Minus) => "-",
                _ => break,
            };
            
            self.advance();
            let right = self.parse_multiplicative_expression()?;
            let op = self.add_node(Node::Variable { name: op_name.to_string() })?;
            left = self.add_node(Node::Application { 
                function: op, 
                args: vec![left, right] 
            })?;
        }
        
        Ok(left)
    }
    
    fn parse_multiplicative_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_unary_expression()?;
        
        loop {
            let op_name = match self.current {
                Some(Token::Star) => "*",
                Some(Token::Slash) => "/",
                Some(Token::Percent) => "%",
                _ => break,
            };
            
            self.advance();
            let right = self.parse_unary_expression()?;
            let op = self.add_node(Node::Variable { name: op_name.to_string() })?;
            left = self.add_node(Node::Application { 
                function: op, 
                args: vec![left, right] 
            })?;
        }
        
        Ok(left)
    }
    
    fn parse_unary_expression(&mut self) -> Result<NodeId> {
        match self.current {
            Some(Token::Bang) => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                let not_op = self.add_node(Node::Variable { name: "not".to_string() })?;
                self.add_node(Node::Application { 
                    function: not_op, 
                    args: vec![expr] 
                })
            }
            Some(Token::Minus) => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                let neg_op = self.add_node(Node::Variable { name: "neg".to_string() })?;
                self.add_node(Node::Application { 
                    function: neg_op, 
                    args: vec![expr] 
                })
            }
            _ => self.parse_postfix_expression(),
        }
    }
    
    fn parse_postfix_expression(&mut self) -> Result<NodeId> {
        let mut expr = self.parse_primary_expression()?;
        
        loop {
            match self.current {
                Some(Token::Dot) => {
                    self.advance();
                    match self.current {
                        Some(Token::LowerIdent(method)) => {
                            let method_name = method.to_string();
                            self.advance();
                            
                            if matches!(self.current, Some(Token::LParen)) {
                                // Method call with arguments
                                self.advance();
                                let mut args = vec![expr];
                                
                                while !matches!(self.current, Some(Token::RParen)) {
                                    args.push(self.parse_expression()?);
                                    if matches!(self.current, Some(Token::Comma)) {
                                        self.advance();
                                    }
                                }
                                
                                self.consume(Token::RParen)?;
                                let method = self.add_node(Node::Variable { name: method_name })?;
                                expr = self.add_node(Node::Application { 
                                    function: method, 
                                    args 
                                })?;
                            } else {
                                // Property access or method without parens
                                let method = self.add_node(Node::Variable { name: method_name })?;
                                expr = self.add_node(Node::Application { 
                                    function: method, 
                                    args: vec![expr] 
                                })?;
                            }
                        }
                        _ => return Err(anyhow!("Expected method name after '.'")),
                    }
                }
                Some(Token::LParen) => {
                    // Function call
                    self.advance();
                    let mut args = vec![];
                    
                    while !matches!(self.current, Some(Token::RParen)) {
                        args.push(self.parse_expression()?);
                        if matches!(self.current, Some(Token::Comma)) {
                            self.advance();
                        }
                    }
                    
                    self.consume(Token::RParen)?;
                    expr = self.add_node(Node::Application { 
                        function: expr, 
                        args 
                    })?
                }
                Some(Token::LBracket) => {
                    // Array indexing
                    self.advance();
                    let index = self.parse_expression()?;
                    self.consume(Token::RBracket)?;
                    
                    let get_op = self.add_node(Node::Variable { name: "get".to_string() })?;
                    expr = self.add_node(Node::Application { 
                        function: get_op, 
                        args: vec![expr, index] 
                    })?
                }
                _ => break,
            }
        }
        
        Ok(expr)
    }
    
    fn parse_primary_expression(&mut self) -> Result<NodeId> {
        match self.current.clone() {
            Some(Token::Integer(n)) => {
                self.advance();
                self.add_node(Node::Literal(Literal::Integer(n)))
            }
            Some(Token::Float(f)) => {
                self.advance();
                self.add_node(Node::Literal(Literal::Float(f)))
            }
            Some(Token::String(s)) => {
                self.advance();
                self.add_node(Node::Literal(Literal::String(s.to_string())))
            }
            Some(Token::True) => {
                self.advance();
                self.add_node(Node::Literal(Literal::Boolean(true)))
            }
            Some(Token::False) => {
                self.advance();
                self.add_node(Node::Literal(Literal::Boolean(false)))
            }
            Some(Token::Nil) => {
                self.advance();
                self.add_node(Node::Literal(Literal::Nil))
            }
            Some(Token::LowerIdent(name)) => {
                let name = name.to_string();
                self.advance();
                self.add_node(Node::Variable { name })
            }
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                
                // Check for struct construction
                if matches!(self.current, Some(Token::LBrace)) {
                    self.parse_struct_construction(name)
                } else {
                    self.add_node(Node::Variable { name })
                }
            }
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(Token::RParen)?;
                Ok(expr)
            }
            Some(Token::LBrace) => {
                // Lambda or block
                self.advance();
                
                if matches!(self.current, Some(Token::Or)) {
                    // Lambda
                    self.advance();
                    let mut params = vec![];
                    
                    while !matches!(self.current, Some(Token::Or)) {
                        if let Some(Token::LowerIdent(param)) = self.current {
                            params.push(param.to_string());
                            self.advance();
                            
                            if matches!(self.current, Some(Token::Comma)) {
                                self.advance();
                            }
                        } else {
                            return Err(anyhow!("Expected parameter name"));
                        }
                    }
                    
                    self.consume(Token::Or)?;
                    let body = self.parse_expression()?;
                    self.consume(Token::RBrace)?;
                    
                    self.add_node(Node::Lambda { params, body })
                } else {
                    // Block expression
                    let expr = self.parse_block_expression()?;
                    self.consume(Token::RBrace)?;
                    Ok(expr)
                }
            }
            Some(Token::LBracket) => {
                // List literal
                self.advance();
                let mut items = vec![];
                
                while !matches!(self.current, Some(Token::RBracket)) {
                    items.push(self.parse_expression()?);
                    if matches!(self.current, Some(Token::Comma)) {
                        self.advance();
                    }
                }
                
                self.consume(Token::RBracket)?;
                self.add_node(Node::List(items))
            }
            Some(Token::If) => self.parse_if_expression(),
            Some(Token::Let) => self.parse_let_expression(),
            Some(Token::Match) => self.parse_match_expression(),
            Some(Token::Try) => self.parse_try_expression(),
            _ => Err(anyhow!("Unexpected token in expression: {:?}", self.current)),
        }
    }
    
    fn parse_if_expression(&mut self) -> Result<NodeId> {
        self.consume(Token::If)?;
        self.consume(Token::LParen)?;
        let condition = self.parse_expression()?;
        self.consume(Token::RParen)?;
        
        self.consume(Token::LBrace)?;
        let then_branch = self.parse_expression()?;
        self.consume(Token::RBrace)?;
        
        let else_branch = if matches!(self.current, Some(Token::Else)) {
            self.advance();
            self.consume(Token::LBrace)?;
            let else_expr = self.parse_expression()?;
            self.consume(Token::RBrace)?;
            else_expr
        } else {
            self.add_node(Node::Literal(Literal::Nil))?
        };
        
        self.add_node(Node::If { 
            condition, 
            then_branch, 
            else_branch 
        })
    }
    
    fn parse_let_expression(&mut self) -> Result<NodeId> {
        self.consume(Token::Let)?;
        
        let mut bindings = vec![];
        
        // Check for 'rec' keyword
        let is_rec = matches!(self.current, Some(Token::Rec));
        if is_rec {
            self.advance();
        }
        
        // Parse first binding
        let name = match self.current {
            Some(Token::LowerIdent(n)) => {
                self.advance();
                n.to_string()
            }
            _ => return Err(anyhow!("Expected variable name after 'let'")),
        };
        
        self.consume(Token::Eq)?;
        let value = self.parse_expression()?;
        bindings.push((name, value));
        
        self.consume(Token::Semicolon)?;
        
        // Parse additional bindings for regular let
        while !is_rec && matches!(self.current, Some(Token::Let)) {
            self.advance();
            
            let name = match self.current {
                Some(Token::LowerIdent(n)) => {
                    self.advance();
                    n.to_string()
                }
                _ => return Err(anyhow!("Expected variable name after 'let'")),
            };
            
            self.consume(Token::Eq)?;
            let value = self.parse_expression()?;
            bindings.push((name, value));
            
            self.consume(Token::Semicolon)?;
        }
        
        // Parse body
        let body = self.parse_expression()?;
        
        if is_rec {
            self.add_node(Node::Letrec { bindings, body })
        } else {
            self.add_node(Node::Let { bindings, body })
        }
    }
    
    fn parse_block_expression(&mut self) -> Result<NodeId> {
        let mut stmts = vec![];
        
        while !matches!(self.current, Some(Token::RBrace)) && self.current.is_some() {
            stmts.push(self.parse_expression()?);
            
            // Optional semicolon
            if matches!(self.current, Some(Token::Semicolon)) {
                self.advance();
            }
        }
        
        if stmts.is_empty() {
            self.add_node(Node::Literal(Literal::Nil))
        } else if stmts.len() == 1 {
            Ok(stmts[0])
        } else {
            self.add_node(Node::Begin { exprs: stmts })
        }
    }
    
    // Stub implementations that need to be completed
    
    fn parse_use_statement(&mut self) -> Result<NodeId> {
        // use module::path::{Item1, Item2};
        self.consume(Token::Use)?;
        
        let mut path = vec![];
        
        // Parse module path
        loop {
            let segment = match self.current {
                Some(Token::LowerIdent(name)) => {
                    let name = name.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected module path segment")),
            };
            path.push(segment);
            
            if matches!(self.current, Some(Token::ColonColon)) {
                self.advance();
            } else {
                break;
            }
        }
        
        // Check for wildcard import
        if matches!(self.current, Some(Token::Star)) {
            self.advance();
            self.consume(Token::Semicolon)?;
            return self.add_node(Node::Import {
                module_path: path.join("::"),
                import_list: vec![],
                import_all: true,
            });
        }
        
        // Parse import list
        let mut imports = vec![];
        if matches!(self.current, Some(Token::LBrace)) {
            self.advance();
            
            while !matches!(self.current, Some(Token::RBrace)) {
                let name = match self.current {
                    Some(Token::LowerIdent(n)) | Some(Token::UpperIdent(n)) => {
                        let name = n.to_string();
                        self.advance();
                        name
                    }
                    _ => return Err(anyhow!("Expected import name")),
                };
                
                let alias = if matches!(self.current, Some(Token::As)) {
                    self.advance();
                    match self.current {
                        Some(Token::LowerIdent(n)) | Some(Token::UpperIdent(n)) => {
                            let alias = n.to_string();
                            self.advance();
                            Some(alias)
                        }
                        _ => return Err(anyhow!("Expected alias name after 'as'")),
                    }
                } else {
                    None
                };
                
                imports.push(ImportItem { name, alias });
                
                if matches!(self.current, Some(Token::Comma)) {
                    self.advance();
                }
            }
            
            self.consume(Token::RBrace)?;
        }
        
        self.consume(Token::Semicolon)?;
        
        self.add_node(Node::Import {
            module_path: path.join("::"),
            import_list: imports,
            import_all: false,
        })
    }
    
    fn parse_module(&mut self) -> Result<NodeId> {
        // mod module_name { ... }
        self.consume(Token::Mod)?;
        let name = match self.current {
            Some(Token::LowerIdent(n)) => {
                let name = n.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected module name after 'mod'")),
        };
        
        self.consume(Token::LBrace)?;
        
        let mut definitions = vec![];
        let exports = vec![];
        
        while !matches!(self.current, Some(Token::RBrace)) {
            match self.current {
                Some(Token::Def) => {
                    let def = self.parse_definition()?;
                    definitions.push(def);
                }
                Some(Token::Use) => {
                    let import = self.parse_use_statement()?;
                    definitions.push(import);
                }
                _ => return Err(anyhow!("Expected definition or use statement in module")),
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // Convert definitions to a single body node
        let body = if definitions.is_empty() {
            self.add_node(Node::Literal(Literal::Nil))?
        } else if definitions.len() == 1 {
            definitions[0]
        } else {
            self.add_node(Node::Begin { exprs: definitions })?
        };
        
        self.add_node(Node::Module {
            name,
            exports,
            body,
        })
    }
    
    fn parse_struct_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        // struct StructName { field1: Type1, field2: Type2 }
        self.consume(Token::Struct)?;
        
        let struct_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected struct name after 'struct'")),
        };
        
        self.consume(Token::LBrace)?;
        
        let mut fields = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            // Check for pub modifier
            let _field_public = matches!(self.current, Some(Token::Pub));
            if _field_public {
                self.advance();
            }
            
            // Parse field: name: type
            let field_name = match self.current {
                Some(Token::LowerIdent(n)) => {
                    let name = n.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected field name in struct definition")),
            };
            
            self.consume(Token::Colon)?;
            let _field_type = self.parse_type()?;
            
            fields.push(field_name);
            
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // For now, create a define node with a special struct value
        // In a full implementation, we'd have a Node::Struct variant
        let struct_value = self.add_node(Node::List(vec![]))?;
        self.add_node(Node::Define {
            name: struct_name,
            value: struct_value,
        })
    }
    
    fn parse_enum_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        // enum EnumName { Variant1, Variant2(Type), Variant3 { field: Type } }
        self.consume(Token::Enum)?;
        
        let enum_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected enum name after 'enum'")),
        };
        
        self.consume(Token::LBrace)?;
        
        let mut variants = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            let variant_name = match self.current {
                Some(Token::UpperIdent(name)) => {
                    let name = name.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected variant name in enum definition")),
            };
            
            // Check for variant data
            match self.current {
                Some(Token::LParen) => {
                    // Tuple variant
                    self.advance();
                    while !matches!(self.current, Some(Token::RParen)) {
                        self.parse_type()?;
                        if matches!(self.current, Some(Token::Comma)) {
                            self.advance();
                        }
                    }
                    self.consume(Token::RParen)?;
                }
                Some(Token::LBrace) => {
                    // Struct variant
                    self.advance();
                    while !matches!(self.current, Some(Token::RBrace)) {
                        match self.current {
                            Some(Token::LowerIdent(_)) => {
                                self.advance();
                                self.consume(Token::Colon)?;
                                self.parse_type()?;
                                if matches!(self.current, Some(Token::Comma)) {
                                    self.advance();
                                }
                            }
                            _ => return Err(anyhow!("Expected field name in struct variant")),
                        }
                    }
                    self.consume(Token::RBrace)?;
                }
                _ => {
                    // Unit variant
                }
            }
            
            variants.push(variant_name);
            
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // Create a define node for the enum
        let enum_value = self.add_node(Node::List(vec![]))?;
        self.add_node(Node::Define {
            name: enum_name,
            value: enum_value,
        })
    }
    
    fn parse_trait_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        // trait TraitName { def method(self, args) -> Type; }
        self.consume(Token::Trait)?;
        
        let trait_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected trait name after 'trait'")),
        };
        
        self.consume(Token::LBrace)?;
        
        let mut methods = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            // Parse method signature
            if matches!(self.current, Some(Token::Def)) {
                self.advance();
                
                if matches!(self.current, Some(Token::Fn)) {
                    self.advance();
                }
                
                let method_name = match self.current {
                    Some(Token::LowerIdent(n)) => {
                        let name = n.to_string();
                        self.advance();
                        name
                    }
                    _ => return Err(anyhow!("Expected method name in trait")),
                };
                
                // Parse parameters
                self.consume(Token::LParen)?;
                while !matches!(self.current, Some(Token::RParen)) {
                    match self.current {
                        Some(Token::LowerIdent(_)) => {
                            self.advance();
                            if matches!(self.current, Some(Token::Colon)) {
                                self.advance();
                                self.parse_type()?;
                            }
                        }
                        _ => return Err(anyhow!("Expected parameter name")),
                    }
                    
                    if matches!(self.current, Some(Token::Comma)) {
                        self.advance();
                    }
                }
                self.consume(Token::RParen)?;
                
                // Skip return type if present
                if matches!(self.current, Some(Token::Arrow)) {
                    self.advance();
                    self.parse_type()?;
                }
                
                self.consume(Token::Semicolon)?;
                methods.push(method_name);
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // Create a define node for the trait
        let trait_value = self.add_node(Node::List(vec![]))?;
        self.add_node(Node::Define {
            name: trait_name,
            value: trait_value,
        })
    }
    
    fn parse_impl_definition(&mut self) -> Result<NodeId> {
        // impl TraitName for TypeName { ... } or impl TypeName { ... }
        self.consume(Token::Impl)?;
        
        let first_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected type or trait name after 'impl'")),
        };
        
        let (_trait_name, _type_name) = if matches!(self.current, Some(Token::For)) {
            // impl Trait for Type
            self.advance();
            let type_name = match self.current {
                Some(Token::UpperIdent(name)) => {
                    let name = name.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected type name after 'for'")),
            };
            (Some(first_name), type_name)
        } else {
            // impl Type
            (None, first_name)
        };
        
        self.consume(Token::LBrace)?;
        
        let mut definitions = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            if matches!(self.current, Some(Token::Def)) {
                let def = self.parse_definition()?;
                definitions.push(def);
            } else {
                return Err(anyhow!("Expected definition in impl block"));
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // Create a begin node containing all definitions
        self.add_node(Node::Begin { exprs: definitions })
    }
    
    fn parse_type_alias(&mut self, _is_public: bool) -> Result<NodeId> {
        // type TypeAlias = ExistingType
        self.consume(Token::Type)?;
        
        let alias_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected type alias name after 'type'")),
        };
        
        self.consume(Token::Eq)?;
        
        let type_name = self.parse_type()?;
        
        // Create a define node for the type alias
        let type_value = self.add_node(Node::Variable { name: type_name })?;
        self.add_node(Node::Define {
            name: alias_name,
            value: type_value,
        })
    }
    
    fn parse_actor_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        // actor ActorName { state: Type; def handle MessageType(...) { ... } }
        self.consume(Token::Actor)?;
        
        let actor_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected actor name after 'actor'")),
        };
        
        self.consume(Token::LBrace)?;
        
        let mut definitions = vec![];
        
        while !matches!(self.current, Some(Token::RBrace)) {
            match self.current {
                Some(Token::LowerIdent(_)) => {
                    // State field
                    self.advance();
                    self.consume(Token::Colon)?;
                    self.parse_type()?;
                    
                    // Skip initializer if present
                    if matches!(self.current, Some(Token::Eq)) {
                        self.advance();
                        self.parse_expression()?;
                    }
                    
                    self.consume(Token::Semicolon)?;
                }
                Some(Token::Def) => {
                    // Handler method
                    let def = self.parse_definition()?;
                    definitions.push(def);
                }
                _ => return Err(anyhow!("Expected field or handler in actor")),
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // Create a define node for the actor
        let actor_value = self.add_node(Node::Begin { exprs: definitions })?;
        self.add_node(Node::Define {
            name: actor_name,
            value: actor_value,
        })
    }
    
    fn parse_effect_definition(&mut self, _is_public: bool) -> Result<NodeId> {
        // effect EffectName { def operation(args) -> Type; }
        self.consume(Token::Effect)?;
        
        let effect_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected effect name after 'effect'")),
        };
        
        self.consume(Token::LBrace)?;
        
        let mut operations = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            if matches!(self.current, Some(Token::Def)) {
                self.advance();
                
                let op_name = match self.current {
                    Some(Token::LowerIdent(n)) => {
                        let name = n.to_string();
                        self.advance();
                        name
                    }
                    _ => return Err(anyhow!("Expected operation name in effect")),
                };
                
                // Parse parameters
                self.consume(Token::LParen)?;
                while !matches!(self.current, Some(Token::RParen)) {
                    match self.current {
                        Some(Token::LowerIdent(_)) => {
                            self.advance();
                            if matches!(self.current, Some(Token::Colon)) {
                                self.advance();
                                self.parse_type()?;
                            }
                        }
                        _ => return Err(anyhow!("Expected parameter name")),
                    }
                    
                    if matches!(self.current, Some(Token::Comma)) {
                        self.advance();
                    }
                }
                self.consume(Token::RParen)?;
                
                // Skip return type if present
                if matches!(self.current, Some(Token::Arrow)) {
                    self.advance();
                    self.parse_type()?;
                }
                
                self.consume(Token::Semicolon)?;
                operations.push(op_name);
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // Create a define node for the effect
        let effect_value = self.add_node(Node::List(vec![]))?;
        self.add_node(Node::Define {
            name: effect_name,
            value: effect_value,
        })
    }
    
    fn parse_type(&mut self) -> Result<String> {
        // Simple type parsing for now - just identifiers
        // Full implementation would handle generics, function types, etc.
        match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                
                // Check for generic type parameters
                if matches!(self.current, Some(Token::Less)) {
                    // Skip generic parameters for now
                    let mut depth = 1;
                    self.advance();
                    while depth > 0 && self.current.is_some() {
                        match self.current {
                            Some(Token::Less) => depth += 1,
                            Some(Token::Greater) => depth -= 1,
                            _ => {},
                        }
                        self.advance();
                    }
                }
                
                Ok(name)
            }
            Some(Token::LowerIdent(name)) if matches!(name, "string" | "int" | "float" | "bool") => {
                // Allow primitive types as lowercase
                let name = name.to_string();
                self.advance();
                Ok(name)
            }
            _ => Err(anyhow!("Expected type name")),
        }
    }
    
    fn parse_struct_construction(&mut self, name: String) -> Result<NodeId> {
        // StructName { field1: value1, field2: value2 }
        self.consume(Token::LBrace)?;
        
        let mut fields = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            let field_name = match self.current {
                Some(Token::LowerIdent(n)) => {
                    let name = n.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected field name in struct construction")),
            };
            
            self.consume(Token::Colon)?;
            let value = self.parse_expression()?;
            fields.push((field_name, value));
            
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }
        
        self.consume(Token::RBrace)?;
        
        // For now, represent struct construction as a function call
        let constructor = self.add_node(Node::Variable { name })?;
        let field_values: Vec<NodeId> = fields.into_iter().map(|(_, v)| v).collect();
        self.add_node(Node::Application {
            function: constructor,
            args: field_values,
        })
    }
    
    fn parse_match_expression(&mut self) -> Result<NodeId> {
        // match expr { pattern => result, ... }
        self.consume(Token::Match)?;
        
        let expr = self.parse_expression()?;
        self.consume(Token::LBrace)?;
        
        let mut branches = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            // Parse pattern
            let pattern = self.parse_pattern()?;
            self.consume(Token::FatArrow)?;
            let result = self.parse_expression()?;
            branches.push((pattern, result));
            
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }
        
        self.consume(Token::RBrace)?;
        
        self.add_node(Node::Match { expr, branches })
    }
    
    fn parse_pattern(&mut self) -> Result<Pattern> {
        match &self.current {
            Some(Token::Underscore) => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Some(Token::LowerIdent(name)) => {
                let name = name.to_string();
                self.advance();
                Ok(Pattern::Variable(name))
            }
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                
                // Check for constructor pattern
                if matches!(self.current, Some(Token::LParen)) {
                    self.advance();
                    let mut patterns = vec![];
                    while !matches!(self.current, Some(Token::RParen)) {
                        patterns.push(self.parse_pattern()?);
                        if matches!(self.current, Some(Token::Comma)) {
                            self.advance();
                        }
                    }
                    self.consume(Token::RParen)?;
                    Ok(Pattern::Constructor { name, patterns })
                } else {
                    Ok(Pattern::Constructor { name, patterns: vec![] })
                }
            }
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance();
                Ok(Pattern::Literal(Literal::Integer(n)))
            }
            Some(Token::Float(f)) => {
                let f = *f;
                self.advance();
                Ok(Pattern::Literal(Literal::Float(f)))
            }
            Some(Token::String(s)) => {
                let s = s.to_string();
                self.advance();
                Ok(Pattern::Literal(Literal::String(s)))
            }
            Some(Token::True) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Boolean(false)))
            }
            Some(Token::Nil) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Nil))
            }
            _ => Err(anyhow!("Expected pattern")),
        }
    }
    
    fn parse_try_expression(&mut self) -> Result<NodeId> {
        // try { body } catch (pattern) { handler } finally { cleanup }
        self.consume(Token::Try)?;
        self.consume(Token::LBrace)?;
        let body = self.parse_block_expression()?;
        self.consume(Token::RBrace)?;
        
        let mut catch_branches = vec![];
        while matches!(self.current, Some(Token::Catch)) {
            self.advance();
            self.consume(Token::LParen)?;
            let pattern = self.parse_pattern()?;
            self.consume(Token::RParen)?;
            self.consume(Token::LBrace)?;
            let handler = self.parse_block_expression()?;
            self.consume(Token::RBrace)?;
            catch_branches.push((pattern, handler));
        }
        
        let finally_block = if matches!(self.current, Some(Token::Finally)) {
            self.advance();
            self.consume(Token::LBrace)?;
            let block = self.parse_block_expression()?;
            self.consume(Token::RBrace)?;
            Some(block)
        } else {
            None
        };
        
        self.add_node(Node::Try {
            body,
            catch_branches,
            finally: finally_block,
        })
    }
    
    // Helper methods
    
    fn advance(&mut self) {
        self.position = self.lexer.span().end;
        self.current = self.lexer.next_token();
    }
    
    fn consume(&mut self, expected: Token) -> Result<()> {
        if self.current.as_ref().map(|t| std::mem::discriminant(t)) == Some(std::mem::discriminant(&expected)) {
            self.advance();
            Ok(())
        } else {
            Err(anyhow!("Expected {:?}, found {:?}", expected, self.current))
        }
    }
    
    fn add_node(&mut self, node: Node) -> Result<NodeId> {
        self.graph.add_node(node).map_err(|e| anyhow!("{}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_literals() {
        let parser = Parser::new("42");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_parse_arithmetic() {
        let parser = Parser::new("1 + 2 * 3");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_parse_function_call() {
        let parser = Parser::new("print(\"hello\")");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_parse_method_chain() {
        let parser = Parser::new("list.map(f).filter(pred)");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_parse_lambda() {
        let parser = Parser::new("{ |x| x * 2 }");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
}