//! Parser for FLC (Fluent Lambda Chain) syntax

use anyhow::{anyhow, Result};
use fluentai_core::ast::{
    EffectType, ExportItem, Graph, ImportItem, Literal, Node, NodeId, Pattern, RangePattern,
};

use crate::flc_lexer::{Lexer, Token};

#[derive(Debug, Clone)]
struct ContractInfo {
    function_name: Option<String>,
    preconditions: Vec<NodeId>,
    postconditions: Vec<NodeId>,
    invariants: Vec<NodeId>,
    complexity: Option<String>,
    pure: bool,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    graph: Graph,
    current: Option<Token<'a>>,
    position: usize,
    module_name: Option<String>,
    exports: Vec<ExportItem>,
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
            module_name: None,
            exports: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Graph> {
        let mut items = vec![];

        // Check for optional module declaration at the start
        if matches!(self.current, Some(Token::Mod)) {
            self.parse_module_declaration()?;
        }

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

        // Store module metadata in the graph
        if let Some(module_name) = &self.module_name {
            self.graph
                .graph_metadata
                .insert("module_name".to_string(), module_name.clone());
        }

        // Store exports as metadata
        if !self.exports.is_empty() {
            // Serialize exports to JSON for storage
            let exports_json =
                serde_json::to_string(&self.exports).unwrap_or_else(|_| "[]".to_string());
            self.graph
                .graph_metadata
                .insert("exports".to_string(), exports_json);
        }

        Ok(self.graph)
    }

    fn parse_top_level(&mut self) -> Result<NodeId> {
        let node = match self.current {
            Some(Token::Use) => self.parse_use_statement(),
            Some(Token::Mod) => self.parse_module(),
            Some(Token::Export) => self.parse_export_statement(),
            Some(Token::At) | Some(Token::Private) | Some(Token::Public) => self.parse_definition(),
            Some(Token::UpperIdent(_)) => {
                // Check if this is a trait implementation (Type as Trait)
                if self.peek_ahead_for_as() {
                    self.parse_trait_impl(false)
                } else {
                    self.parse_expression()
                }
            }
            _ => self.parse_statement(),
        }?;

        // Consume optional semicolon at top level
        if matches!(self.current, Some(Token::Semicolon)) {
            self.advance();
        }

        Ok(node)
    }

    fn parse_definition(&mut self) -> Result<NodeId> {
        // Check for contract annotations
        let mut contract_info = None;
        if matches!(self.current, Some(Token::At)) {
            contract_info = Some(self.parse_contract_annotations()?);
        }

        // Determine visibility
        let is_public = matches!(self.current, Some(Token::Public));
        if is_public {
            self.consume(Token::Public)?;
        } else {
            self.consume(Token::Private)?;
        }

        // Parse the definition and collect export name if public
        let (node_id, export_name) = match self.current {
            Some(Token::Async) => {
                self.advance();
                let (node, name) = self.parse_function_definition(is_public, contract_info)?;
                (node, Some(name))
            }
            Some(Token::Function) => {
                let (node, name) = self.parse_function_definition(is_public, contract_info)?;
                (node, Some(name))
            }
            Some(Token::Const) => {
                let (node, name) = self.parse_const_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::Handle) => {
                let (node, name) = self.parse_handler_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::Struct) => {
                let (node, name) = self.parse_struct_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::Enum) => {
                let (node, name) = self.parse_enum_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::Trait) => {
                let (node, name) = self.parse_trait_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::Type) => {
                let (node, name) = self.parse_type_alias(is_public)?;
                (node, Some(name))
            }
            Some(Token::Actor) => {
                let (node, name) = self.parse_actor_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::Effect) => {
                let (node, name) = self.parse_effect_definition(is_public)?;
                (node, Some(name))
            }
            Some(Token::UpperIdent(_)) => {
                let node = self.parse_trait_impl(is_public)?;
                (node, None) // Trait impls don't have names to export
            }
            Some(Token::LowerIdent(_)) => {
                let (node, name) = self.parse_value_definition(is_public)?;
                (node, Some(name))
            }
            _ => return Err(anyhow!("Expected definition after visibility modifier")),
        };

        // If public and has a name, add to exports
        if is_public {
            if let Some(name) = export_name {
                self.exports.push(ExportItem { name, alias: None });
            }
        }

        Ok(node_id)
    }

    fn parse_function_definition(
        &mut self,
        _is_public: bool,
        contract_info: Option<ContractInfo>,
    ) -> Result<(NodeId, String)> {
        self.consume(Token::Function)?;

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
            let param_name = match self.current {
                Some(Token::LowerIdent(param)) => param.to_string(),
                Some(Token::Self_) => "self".to_string(),
                _ => return Err(anyhow!("Expected parameter name")),
            };

            params.push(param_name);
            self.advance();

            // Optional type annotation
            if matches!(self.current, Some(Token::Colon)) {
                self.advance();
                self.parse_type()?;
            }

            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }

        self.consume(Token::RParen)?;

        // Optional return type
        if matches!(self.current, Some(Token::Arrow)) {
            self.advance();
            self.parse_type()?;
        }

        // Optional effect annotation with .with(EffectName)
        let body = if matches!(self.current, Some(Token::Dot)) {
            self.advance();

            // Expect "with"
            match self.current {
                Some(Token::With) => {
                    self.advance();
                }
                _ => return Err(anyhow!("Expected 'with' after '.' in function definition")),
            }

            self.consume(Token::LParen)?;

            // Parse effect name
            let effect_name = match self.current {
                Some(Token::UpperIdent(name)) => {
                    let name = name.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected effect name in .with()")),
            };

            self.consume(Token::RParen)?;

            // Now parse the function body
            self.consume(Token::LBrace)?;
            let inner_body = self.parse_block_expression()?;
            self.consume(Token::RBrace)?;

            // Create an Effect node that wraps the body
            // Map effect name to EffectType
            let effect_type = match effect_name.as_str() {
                "Database" => EffectType::IO, // Database operations are IO effects
                "Network" => EffectType::Network,
                "State" => EffectType::State,
                "Error" => EffectType::Error,
                "Time" => EffectType::Time,
                "Random" => EffectType::Random,
                "Dom" => EffectType::Dom,
                "Async" => EffectType::Async,
                "Concurrent" => EffectType::Concurrent,
                _ => EffectType::IO, // Default to IO for user-defined effects
            };

            // Create an effect node
            self.add_node(Node::Effect {
                effect_type,
                operation: format!("{}_{}", effect_name, name),
                args: vec![inner_body],
            })?
        } else {
            // No effect annotation, parse body normally
            self.consume(Token::LBrace)?;
            let body = self.parse_block_expression()?;
            self.consume(Token::RBrace)?;
            body
        };

        let lambda = self.add_node(Node::Lambda { params, body })?;
        let define_node = self.add_node(Node::Define {
            name: name.clone(),
            value: lambda,
        })?;

        // If we have contract annotations, create a Contract node
        if let Some(contract) = contract_info {
            let contract_node = self.add_node(Node::Contract {
                function_name: contract.function_name.unwrap_or(name.clone()),
                preconditions: contract.preconditions,
                postconditions: contract.postconditions,
                invariants: contract.invariants,
                complexity: contract.complexity,
                pure: contract.pure,
            })?;

            // Return a begin node that includes both the contract and the definition
            let begin_node = self.add_node(Node::Begin {
                exprs: vec![contract_node, define_node],
            })?;
            Ok((begin_node, name))
        } else {
            Ok((define_node, name))
        }
    }

    fn parse_handler_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
        // handle MessageType(param1: Type, param2: Type) { ... }
        self.consume(Token::Handle)?;

        let message_type = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected message type after 'handle'")),
        };

        self.consume(Token::LParen)?;
        let mut params = vec![];

        while !matches!(self.current, Some(Token::RParen)) {
            let param_name = match self.current {
                Some(Token::LowerIdent(n)) => {
                    self.advance();
                    n.to_string()
                }
                _ => return Err(anyhow!("Expected parameter name")),
            };

            // Optional type annotation
            if matches!(self.current, Some(Token::Colon)) {
                self.advance();
                self.parse_type()?;
            }

            params.push(param_name);

            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }
        self.consume(Token::RParen)?;

        // Parse optional return type
        if matches!(self.current, Some(Token::Arrow)) {
            self.advance();
            self.parse_type()?;
        }

        self.consume(Token::LBrace)?;
        let body = self.parse_block_expression()?;
        self.consume(Token::RBrace)?;

        // Create handler as a special function
        let handler_name = format!("handle_{}", message_type);
        let lambda = self.add_node(Node::Lambda { params, body })?;
        let define_node = self.add_node(Node::Define {
            name: handler_name.clone(),
            value: lambda,
        })?;
        Ok((define_node, handler_name))
    }

    fn parse_value_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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

        let node = self.add_node(Node::Define { name: name.clone(), value })?;
        Ok((node, name))
    }

    fn parse_const_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
        self.consume(Token::Const)?;

        let name = match self.current {
            Some(Token::UpperIdent(n)) | Some(Token::ConstIdent(n)) => {
                let name = n.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected constant name (uppercase identifier)")),
        };

        self.consume(Token::Eq)?;
        let value = self.parse_expression()?;

        if matches!(self.current, Some(Token::Semicolon)) {
            self.advance();
        }

        // Constants are just defines with uppercase names
        // The type system or later passes can enforce immutability
        let node = self.add_node(Node::Define { name: name.clone(), value })?;
        Ok((node, name))
    }

    fn parse_expression(&mut self) -> Result<NodeId> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<NodeId> {
        // Parse the left-hand side first
        let left = self.parse_pipe_expression()?;

        // Check if this is an assignment (= for assignment, := for mutation)
        if matches!(self.current, Some(Token::Eq) | Some(Token::ColonEq)) {
            self.advance(); // consume '=' or ':='

            // Parse the right-hand side recursively to ensure right-associativity
            // This allows for chained assignments like a = b = c
            let right = self.parse_assignment_expression()?;

            // Create the assignment node
            // TODO: In the future, we might want to distinguish between = and :=
            // For now, both create Assignment nodes
            self.add_node(Node::Assignment {
                target: left,
                value: right,
            })
        } else {
            // Not an assignment, just return the expression
            Ok(left)
        }
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
                        args: vec![left],
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
            let op = self.add_node(Node::Variable {
                name: "or".to_string(),
            })?;
            left = self.add_node(Node::Application {
                function: op,
                args: vec![left, right],
            })?;
        }

        Ok(left)
    }

    fn parse_and_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_equality_expression()?;

        while matches!(self.current, Some(Token::AndAnd)) {
            self.advance();
            let right = self.parse_equality_expression()?;
            let op = self.add_node(Node::Variable {
                name: "and".to_string(),
            })?;
            left = self.add_node(Node::Application {
                function: op,
                args: vec![left, right],
            })?;
        }

        Ok(left)
    }

    fn parse_equality_expression(&mut self) -> Result<NodeId> {
        let mut left = self.parse_comparison_expression()?;

        loop {
            let op_name = match self.current {
                Some(Token::EqEq) => "=", // Map == to = for compatibility with s-expr builtins
                Some(Token::NotEq) => "!=",
                _ => break,
            };

            self.advance();
            let right = self.parse_comparison_expression()?;
            let op = self.add_node(Node::Variable {
                name: op_name.to_string(),
            })?;
            left = self.add_node(Node::Application {
                function: op,
                args: vec![left, right],
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
            let op = self.add_node(Node::Variable {
                name: op_name.to_string(),
            })?;
            left = self.add_node(Node::Application {
                function: op,
                args: vec![left, right],
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
            let op = self.add_node(Node::Variable {
                name: op_name.to_string(),
            })?;
            left = self.add_node(Node::Application {
                function: op,
                args: vec![left, right],
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
            let op = self.add_node(Node::Variable {
                name: op_name.to_string(),
            })?;
            left = self.add_node(Node::Application {
                function: op,
                args: vec![left, right],
            })?;
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<NodeId> {
        match self.current {
            Some(Token::Bang) => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                let not_op = self.add_node(Node::Variable {
                    name: "not".to_string(),
                })?;
                self.add_node(Node::Application {
                    function: not_op,
                    args: vec![expr],
                })
            }
            Some(Token::Minus) => {
                self.advance();
                let expr = self.parse_unary_expression()?;
                let neg_op = self.add_node(Node::Variable {
                    name: "neg".to_string(),
                })?;
                self.add_node(Node::Application {
                    function: neg_op,
                    args: vec![expr],
                })
            }
            _ => self.parse_postfix_expression(),
        }
    }

    fn parse_postfix_expression(&mut self) -> Result<NodeId> {
        let mut expr = self.parse_primary_expression()?;

        loop {
            match self.current {
                Some(Token::OptionalChain) => {
                    // Optional chaining: obj.?method()
                    self.advance();

                    // Get method/property name
                    let method_name = match self.current {
                        Some(Token::LowerIdent(method)) => {
                            let name = method.to_string();
                            self.advance();
                            name
                        }
                        Some(Token::UpperIdent(method)) => {
                            // Allow PascalCase method names too
                            let name = method.to_string();
                            self.advance();
                            name
                        }
                        _ => return Err(anyhow!("Expected method name after '.?'")),
                    };

                    // Create the optional chain node
                    // For now, we'll represent it as a special function call
                    let optional_chain_fn = self.add_node(Node::Variable {
                        name: format!("optional_chain_{}", method_name),
                    })?;

                    if matches!(self.current, Some(Token::LParen)) {
                        // Method call with arguments: obj.?method(args)
                        self.advance();
                        let mut args = vec![expr];

                        while !matches!(self.current, Some(Token::RParen)) {
                            args.push(self.parse_expression()?);
                            if matches!(self.current, Some(Token::Comma)) {
                                self.advance();
                            }
                        }

                        self.consume(Token::RParen)?;
                        expr = self.add_node(Node::Application {
                            function: optional_chain_fn,
                            args,
                        })?;
                    } else {
                        // Property access: obj.?property
                        expr = self.add_node(Node::Application {
                            function: optional_chain_fn,
                            args: vec![expr],
                        })?;
                    }
                }
                Some(Token::Dot) => {
                    self.advance();

                    // Get method name (can be identifier or certain keywords)
                    let method_name = match self.current {
                        Some(Token::LowerIdent(method)) => {
                            let name = method.to_string();
                            self.advance();
                            name
                        }
                        Some(Token::Match) => {
                            self.advance();
                            "match".to_string()
                        }
                        Some(Token::Await) => {
                            self.advance();
                            "await".to_string()
                        }
                        Some(Token::Case) => {
                            self.advance();
                            "case".to_string()
                        }
                        _ => return Err(anyhow!("Expected method name after '.'")),
                    };

                    if matches!(self.current, Some(Token::LParen)) {
                        // Method call with arguments
                        self.advance();

                        // Special handling for channel operations
                        if method_name == "send" {
                            // channel.send(value) -> Node::Send
                            if matches!(self.current, Some(Token::RParen)) {
                                return Err(anyhow!("send() requires a value argument"));
                            }
                            let value = self.parse_expression()?;
                            self.consume(Token::RParen)?;
                            expr = self.add_node(Node::Send {
                                channel: expr,
                                value,
                            })?;
                        } else if method_name == "receive" {
                            // channel.receive() -> Node::Receive
                            self.consume(Token::RParen)?;
                            expr = self.add_node(Node::Receive { channel: expr })?;
                        } else if method_name == "await" {
                            // expr.await() -> Node::Await
                            self.consume(Token::RParen)?;
                            expr = self.add_node(Node::Await { expr })?;
                        } else if method_name == "case" {
                            // Special handling for case method: case(pattern, value)
                            // The first argument is a pattern, not an expression
                            let pattern = self.parse_pattern()?;

                            // Convert pattern to expression (simplified for now)
                            let pattern_expr = self.pattern_to_expression(pattern)?;

                            let mut args = vec![expr, pattern_expr];

                            // Parse remaining arguments (the value)
                            if matches!(self.current, Some(Token::Comma)) {
                                self.advance();
                                args.push(self.parse_expression()?);
                            }

                            self.consume(Token::RParen)?;
                            let method = self.add_node(Node::Variable { name: method_name })?;
                            expr = self.add_node(Node::Application {
                                function: method,
                                args,
                            })?;
                        } else {
                            // Regular method call
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
                                args,
                            })?;
                        }
                    } else {
                        // Check if the expr is a simple variable and this could be a qualified variable
                        if let Some(Node::Variable { name: module_name }) =
                            self.graph.get_node(expr)
                        {
                            // This is module.variable syntax - create a QualifiedVariable node
                            expr = self.add_node(Node::QualifiedVariable {
                                module_name: module_name.clone(),
                                variable_name: method_name,
                            })?;
                        } else {
                            // Property access or method without parens
                            let method = self.add_node(Node::Variable { name: method_name })?;
                            expr = self.add_node(Node::Application {
                                function: method,
                                args: vec![expr],
                            })?;
                        }
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
                        args,
                    })?
                }
                Some(Token::LBracket) => {
                    // Array indexing
                    self.advance();
                    let index = self.parse_expression()?;
                    self.consume(Token::RBracket)?;

                    let get_op = self.add_node(Node::Variable {
                        name: "get".to_string(),
                    })?;
                    expr = self.add_node(Node::Application {
                        function: get_op,
                        args: vec![expr, index],
                    })?
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<NodeId> {
        #[cfg(test)]
        eprintln!(
            "parse_primary_expression: current token: {:?}",
            self.current
        );

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
            Some(Token::Symbol(s)) => {
                self.advance();
                self.add_node(Node::Literal(Literal::Symbol(s.to_string())))
            }
            Some(Token::FString(s)) => {
                let s = s.to_string();
                self.advance();
                self.parse_fstring(&s)
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

                // Check if this is a lambda (single parameter)
                if matches!(self.current, Some(Token::FatArrow)) {
                    self.advance();
                    let body = self.parse_expression()?;
                    self.add_node(Node::Lambda {
                        params: vec![name],
                        body,
                    })
                } else if name == "channel" && matches!(self.current, Some(Token::LParen)) {
                    // channel() function
                    self.advance(); // consume (

                    // Check for optional capacity argument
                    let capacity = if matches!(self.current, Some(Token::RParen)) {
                        None
                    } else {
                        let cap = self.parse_expression()?;
                        Some(cap)
                    };

                    self.consume(Token::RParen)?;
                    self.add_node(Node::Channel { capacity })
                } else {
                    self.add_node(Node::Variable { name })
                }
            }
            Some(Token::Self_) => {
                self.advance();
                self.add_node(Node::Variable {
                    name: "self".to_string(),
                })
            }
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();

                // Check for struct construction
                if matches!(self.current, Some(Token::LBrace)) {
                    self.parse_struct_construction(name)
                } else if name == "Channel" && matches!(self.current, Some(Token::Dot)) {
                    // Channel.new() syntax
                    self.advance(); // consume dot
                    if let Some(Token::LowerIdent(method)) = self.current {
                        if method == "new" {
                            self.advance(); // consume "new"
                            self.consume(Token::LParen)?;

                            // Check for optional capacity argument
                            let capacity = if matches!(self.current, Some(Token::RParen)) {
                                None
                            } else {
                                let cap = self.parse_expression()?;
                                Some(cap)
                            };

                            self.consume(Token::RParen)?;
                            self.add_node(Node::Channel { capacity })
                        } else {
                            // Unknown method on Channel
                            return Err(anyhow!("Unknown method '{}' on Channel", method));
                        }
                    } else {
                        return Err(anyhow!("Expected method name after Channel."));
                    }
                } else {
                    self.add_node(Node::Variable { name })
                }
            }
            Some(Token::LParen) => {
                self.advance();

                // Check for empty lambda: () => expr
                if matches!(self.current, Some(Token::RParen)) {
                    self.advance();
                    if matches!(self.current, Some(Token::FatArrow)) {
                        // It's an empty parameter lambda
                        self.advance(); // consume =>
                        let body = self.parse_expression()?;
                        return self.add_node(Node::Lambda {
                            params: vec![],
                            body,
                        });
                    } else {
                        return Err(anyhow!("Empty parentheses"));
                    }
                }

                // Try to parse as a lambda parameter list
                // We'll use a more careful approach that doesn't consume tokens unnecessarily
                let checkpoint = self.position;
                let checkpoint_lexer = self.lexer.clone();
                let checkpoint_current = self.current.clone();

                // Try to collect parameter names
                let mut params = vec![];
                let mut could_be_lambda = true;

                // First, check if we have a valid parameter list
                loop {
                    match self.current {
                        Some(Token::LowerIdent(name)) => {
                            params.push(name.to_string());
                            self.advance();

                            match self.current {
                                Some(Token::Comma) => {
                                    self.advance(); // continue to next parameter
                                }
                                Some(Token::RParen) => {
                                    self.advance();
                                    // Check if followed by =>
                                    if matches!(self.current, Some(Token::FatArrow)) {
                                        // It's definitely a lambda!
                                        self.advance(); // consume =>
                                        let body = self.parse_expression()?;
                                        return self.add_node(Node::Lambda { params, body });
                                    } else {
                                        // Not a lambda, break and restore
                                        could_be_lambda = false;
                                        break;
                                    }
                                }
                                _ => {
                                    // Not a valid parameter list
                                    could_be_lambda = false;
                                    break;
                                }
                            }
                        }
                        _ => {
                            // Not starting with an identifier, can't be lambda params
                            could_be_lambda = false;
                            break;
                        }
                    }
                }

                // If it wasn't a lambda, restore and parse as regular expression
                self.position = checkpoint;
                self.lexer = checkpoint_lexer;
                self.current = checkpoint_current;

                let expr = self.parse_expression()?;
                self.consume(Token::RParen)?;
                Ok(expr)
            }
            Some(Token::LBrace) => {
                // Could be a block expression or a map literal
                // Peek ahead to distinguish: if we see string:value pattern, it's a map
                self.advance();

                // Check if this is an empty map {}
                if matches!(self.current, Some(Token::RBrace)) {
                    self.advance();
                    // Create empty map: make_map()
                    let make_map = self.add_node(Node::Variable {
                        name: "make_map".to_string(),
                    })?;
                    return self.add_node(Node::Application {
                        function: make_map,
                        args: vec![],
                    });
                }

                // Look ahead to determine if this is a map literal
                let is_map = self.is_map_literal();

                if is_map {
                    self.parse_map_literal()
                } else {
                    // It's a block expression
                    let expr = self.parse_block_expression()?;
                    self.consume(Token::RBrace)?;
                    Ok(expr)
                }
            }
            Some(Token::Hash) => {
                // Set literal #{...} with spread support
                self.advance();
                self.consume(Token::LBrace)?;

                let mut items = vec![];
                let mut has_spread = false;

                while !matches!(self.current, Some(Token::RBrace)) {
                    if matches!(self.current, Some(Token::DotDotDot)) {
                        // Spread operator: #{...other_set}
                        self.advance();
                        has_spread = true;
                        let spread_expr = self.parse_expression()?;

                        // Mark this as a spread element
                        let spread_fn = self.add_node(Node::Variable {
                            name: "__spread__".to_string(),
                        })?;
                        let spread_item = self.add_node(Node::Application {
                            function: spread_fn,
                            args: vec![spread_expr],
                        })?;
                        items.push(spread_item);
                    } else {
                        items.push(self.parse_expression()?);
                    }

                    if matches!(self.current, Some(Token::Comma)) {
                        self.advance();
                        if matches!(self.current, Some(Token::RBrace)) {
                            // Trailing comma is allowed
                            break;
                        }
                    } else if !matches!(self.current, Some(Token::RBrace)) {
                        return Err(anyhow!("Expected ',' or '}}' in set literal"));
                    }
                }

                self.consume(Token::RBrace)?;

                // Convert to appropriate function call
                let fn_name = if has_spread {
                    "set_with_spread"
                } else {
                    "make_set"
                };
                let set_fn = self.add_node(Node::Variable {
                    name: fn_name.to_string(),
                })?;
                self.add_node(Node::Application {
                    function: set_fn,
                    args: items,
                })
            }
            Some(Token::LBracket) => {
                // List literal with spread support
                self.advance();
                let mut items = vec![];
                let mut has_spread = false;

                while !matches!(self.current, Some(Token::RBracket)) {
                    if matches!(self.current, Some(Token::DotDotDot)) {
                        // Spread operator: [...other_list]
                        self.advance();
                        has_spread = true;
                        let spread_expr = self.parse_expression()?;

                        // Mark this as a spread element by wrapping in a special call
                        let spread_fn = self.add_node(Node::Variable {
                            name: "__spread__".to_string(),
                        })?;
                        let spread_item = self.add_node(Node::Application {
                            function: spread_fn,
                            args: vec![spread_expr],
                        })?;
                        items.push(spread_item);
                    } else {
                        items.push(self.parse_expression()?);
                    }

                    if matches!(self.current, Some(Token::Comma)) {
                        self.advance();
                    }
                }

                self.consume(Token::RBracket)?;

                if has_spread {
                    // If we have spread elements, create a call to list_with_spread
                    let list_fn = self.add_node(Node::Variable {
                        name: "list_with_spread".to_string(),
                    })?;
                    self.add_node(Node::Application {
                        function: list_fn,
                        args: items,
                    })
                } else {
                    // Regular list without spread
                    self.add_node(Node::List(items))
                }
            }
            Some(Token::If) => self.parse_if_expression(),
            Some(Token::Let) => self.parse_let_expression(),
            Some(Token::Match) => self.parse_match_expression(),
            Some(Token::For) => self.parse_for_expression(),
            Some(Token::While) => self.parse_while_expression(),
            Some(Token::Try) => self.parse_try_expression(),
            Some(Token::Spawn) => self.parse_spawn_expression(),
            Some(Token::Perform) => self.parse_perform_expression(),
            Some(Token::Handle) => self.parse_handle_expression(),
            Some(Token::Receive) => self.parse_receive_expression(),
            Some(Token::Dollar) => self.parse_printable(),
            _ => Err(anyhow!(
                "Unexpected token in expression: {:?}",
                self.current
            )),
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
            else_branch,
        })
    }

    fn parse_for_expression(&mut self) -> Result<NodeId> {
        // for item in collection { body }
        self.consume(Token::For)?;

        // Parse the loop variable (pattern)
        let var_name = match self.current {
            Some(Token::LowerIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected variable name after 'for'")),
        };

        self.consume(Token::In)?;

        // Parse the collection to iterate over
        let collection = self.parse_expression()?;

        self.consume(Token::LBrace)?;
        let body = self.parse_block_expression()?;
        self.consume(Token::RBrace)?;

        // Convert to: collection.for_each(var_name => body)
        let for_each_fn = self.add_node(Node::Variable {
            name: "for_each".to_string(),
        })?;
        let lambda = self.add_node(Node::Lambda {
            params: vec![var_name],
            body,
        })?;

        let method_call = self.add_node(Node::Application {
            function: for_each_fn,
            args: vec![collection, lambda],
        })?;

        Ok(method_call)
    }

    fn parse_while_expression(&mut self) -> Result<NodeId> {
        // while condition { body }
        self.consume(Token::While)?;

        let condition = self.parse_expression()?;

        self.consume(Token::LBrace)?;
        let body = self.parse_block_expression()?;
        self.consume(Token::RBrace)?;

        // Convert to a recursive function
        // let loop_fn = rec (unit) => if (condition) { body; loop_fn(unit) } else { nil }
        // loop_fn(unit)

        let unit = self.add_node(Node::Literal(Literal::Nil))?;
        let loop_var = format!("_while_{}", self.graph.nodes.len());

        // Create the recursive call: loop_fn(unit)
        let loop_call_inner = self.add_node(Node::Variable {
            name: loop_var.clone(),
        })?;
        let recursive_call = self.add_node(Node::Application {
            function: loop_call_inner,
            args: vec![unit],
        })?;

        // Create body sequence: body; loop_fn(unit)
        let body_with_recursion = self.add_node(Node::Begin {
            exprs: vec![body, recursive_call],
        })?;

        // Create the if expression
        let if_expr = self.add_node(Node::If {
            condition,
            then_branch: body_with_recursion,
            else_branch: unit,
        })?;

        // Create the recursive lambda
        let loop_lambda = self.add_node(Node::Lambda {
            params: vec!["_".to_string()],
            body: if_expr,
        })?;

        // Create the let-rec binding and call
        let binding = (loop_var.clone(), loop_lambda);
        let loop_call = self.add_node(Node::Variable { name: loop_var })?;
        let final_call = self.add_node(Node::Application {
            function: loop_call,
            args: vec![unit],
        })?;

        self.add_node(Node::Letrec {
            bindings: vec![binding],
            body: final_call,
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

        // Check if this is a destructuring pattern
        if matches!(self.current, Some(Token::LBrace)) {
            // Parse destructuring pattern: let {x, y} = expr
            self.advance(); // consume {

            let mut field_names = vec![];
            while !matches!(self.current, Some(Token::RBrace)) {
                match self.current {
                    Some(Token::LowerIdent(name)) => {
                        field_names.push(name.to_string());
                        self.advance();
                    }
                    _ => return Err(anyhow!("Expected field name in destructuring pattern")),
                }

                if matches!(self.current, Some(Token::Comma)) {
                    self.advance();
                }
            }
            self.consume(Token::RBrace)?;
            self.consume(Token::Eq)?;
            let value = self.parse_expression()?;

            // Generate a temporary variable for the struct value
            let temp_var = format!("_struct{}", self.graph.nodes.len());
            bindings.push((temp_var.clone(), value));

            // Create field access for each destructured field
            for field_name in field_names {
                let struct_var = self.add_node(Node::Variable {
                    name: temp_var.clone(),
                })?;
                let getter_fn = self.add_node(Node::Variable {
                    name: format!("get_{}", field_name),
                })?;
                let field_access = self.add_node(Node::Application {
                    function: getter_fn,
                    args: vec![struct_var],
                })?;
                bindings.push((field_name, field_access));
            }
        } else {
            // Parse regular binding
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
        }

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

    fn parse_block(&mut self) -> Result<NodeId> {
        let mut let_bindings = vec![];
        let mut exprs = vec![];

        while !matches!(self.current, Some(Token::RBrace)) && self.current.is_some() {
            // Debug: print current token before parsing statement
            #[cfg(test)]
            if let Some(ref token) = self.current {
                eprintln!(
                    "parse_block: about to parse statement, current token: {:?}",
                    token
                );
            }

            // Check if this is a let binding
            if matches!(self.current, Some(Token::Let)) {
                // Parse the let binding directly here instead of creating a Let node
                self.advance(); // consume 'let'

                let name = match self.current {
                    Some(Token::LowerIdent(n)) => {
                        self.advance();
                        n.to_string()
                    }
                    _ => return Err(anyhow!("Expected variable name after 'let'")),
                };

                self.consume(Token::Eq)?;
                let value = self.parse_expression()?;

                let_bindings.push((name, value));

                // Optional semicolon after let binding
                if matches!(self.current, Some(Token::Semicolon)) {
                    self.advance();
                }
            } else {
                // Parse as regular statement/expression
                let expr = self.parse_statement()?;
                exprs.push(expr);

                // Optional semicolon (already consumed by parse_statement for statements)
                if matches!(self.current, Some(Token::Semicolon)) {
                    self.advance();
                }
            }
        }

        // Determine the body of the block
        let body = if exprs.is_empty() {
            self.add_node(Node::Literal(Literal::Nil))?
        } else if exprs.len() == 1 {
            exprs[0]
        } else {
            self.add_node(Node::Begin { exprs })?
        };

        // If we have let bindings, wrap the body in a Let node
        if !let_bindings.is_empty() {
            self.add_node(Node::Let {
                bindings: let_bindings,
                body,
            })
        } else {
            Ok(body)
        }
    }

    fn parse_block_expression(&mut self) -> Result<NodeId> {
        // For backward compatibility, delegate to parse_block
        self.parse_block()
    }

    fn parse_statement(&mut self) -> Result<NodeId> {
        // Note: Let bindings are now handled directly in parse_block
        // Assignments are now handled as expressions in parse_assignment_expression

        // Just parse as expression - assignments are now expressions
        self.parse_expression()
    }

    fn parse_let_statement(&mut self) -> Result<NodeId> {
        // This is a simplified version of parse_let_expression that doesn't expect a body
        // Used for let statements in blocks
        self.consume(Token::Let)?;

        let name = match self.current {
            Some(Token::LowerIdent(n)) => {
                self.advance();
                n.to_string()
            }
            _ => return Err(anyhow!("Expected variable name after 'let'")),
        };

        self.consume(Token::Eq)?;
        let value = self.parse_expression()?;

        // Create a let node with the binding and nil as body
        // This effectively makes it a statement that binds the variable
        let nil = self.add_node(Node::Literal(Literal::Nil))?;
        self.add_node(Node::Let {
            bindings: vec![(name, value)],
            body: nil,
        })
    }

    // Stub implementations that need to be completed

    fn parse_use_statement(&mut self) -> Result<NodeId> {
        // use module::path::{Item1, Item2};
        self.consume(Token::Use)?;

        let mut path = vec![];

        // Handle ModulePath token (e.g., "std::collections::HashMap")
        if let Some(Token::ModulePath(module_path)) = self.current {
            path = module_path.split("::").map(|s| s.to_string()).collect();
            self.advance();

            // Check for ::* after ModulePath
            if matches!(self.current, Some(Token::ColonColon)) {
                self.advance();
                if matches!(self.current, Some(Token::Star)) {
                    self.advance();
                    self.consume(Token::Semicolon)?;
                    return self.add_node(Node::Import {
                        module_path: path.join("::"),
                        import_list: vec![],
                        import_all: true,
                    });
                } else {
                    return Err(anyhow!("Expected identifier or '*' after '::'"));
                }
            }
        } else {
            // Parse module path segment by segment
            // First segment is required
            let segment = match self.current {
                Some(Token::LowerIdent(name)) => {
                    let name = name.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected module path segment")),
            };
            path.push(segment);

            // Parse additional segments if :: is present
            while matches!(self.current, Some(Token::ColonColon)) {
                self.advance();

                // Check if this is ::{...} for imports
                if matches!(self.current, Some(Token::LBrace)) {
                    break;
                }

                // Check for wildcard import after ::
                if matches!(self.current, Some(Token::Star)) {
                    self.advance();
                    self.consume(Token::Semicolon)?;
                    return self.add_node(Node::Import {
                        module_path: path.join("::"),
                        import_list: vec![],
                        import_all: true,
                    });
                }

                let segment = match self.current {
                    Some(Token::LowerIdent(name)) => {
                        let name = name.to_string();
                        self.advance();
                        name
                    }
                    _ => return Err(anyhow!("Expected module path segment after ::")),
                };
                path.push(segment);
            }
        }

        // After parsing the full path, check what comes next

        // Check for module alias: use module::path as alias;
        if matches!(self.current, Some(Token::As)) {
            self.advance();
            match self.current {
                Some(Token::LowerIdent(alias)) | Some(Token::UpperIdent(alias)) => {
                    let alias_str = alias.to_string();
                    self.advance();
                    self.consume(Token::Semicolon)?;

                    // For module aliasing, treat the last segment as the import name
                    let import_name = path.last().cloned().unwrap_or_default();
                    return self.add_node(Node::Import {
                        module_path: path.join("::"),
                        import_list: vec![ImportItem {
                            name: import_name,
                            alias: Some(alias_str),
                        }],
                        import_all: false,
                    });
                }
                _ => return Err(anyhow!("Expected alias name after 'as'")),
            }
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
        let mut exports = vec![];

        while !matches!(self.current, Some(Token::RBrace)) {
            match self.current {
                Some(Token::Private) | Some(Token::Public) => {
                    let def = self.parse_definition()?;
                    definitions.push(def);
                }
                Some(Token::Use) => {
                    let import = self.parse_use_statement()?;
                    definitions.push(import);
                }
                Some(Token::Export) => {
                    // Parse export statement and collect exported names
                    let export_node = self.parse_export_statement()?;
                    definitions.push(export_node);

                    // Extract export names from the node for the module's export list
                    if let Node::Export { export_list } = &self.graph.nodes[&export_node] {
                        for item in export_list {
                            exports.push(item.name.clone());
                        }
                    }
                }
                _ => {
                    return Err(anyhow!(
                        "Expected definition, use, or export statement in module"
                    ))
                }
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

    fn parse_export_statement(&mut self) -> Result<NodeId> {
        // export { name1, name2 as alias2, name3 }
        self.consume(Token::Export)?;
        self.consume(Token::LBrace)?;

        let mut export_list = vec![];

        while !matches!(self.current, Some(Token::RBrace)) {
            let name = match self.current {
                Some(Token::LowerIdent(n)) | Some(Token::UpperIdent(n)) => {
                    let name = n.to_string();
                    self.advance();
                    name
                }
                _ => return Err(anyhow!("Expected export name")),
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

            export_list.push(ExportItem { name, alias });

            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }

        self.consume(Token::RBrace)?;
        self.consume(Token::Semicolon)?;

        self.add_node(Node::Export { export_list })
    }

    fn parse_struct_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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
            let _field_public = matches!(self.current, Some(Token::Public));
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

        // Check for .derive() attributes
        let mut derive_traits = vec![];
        if matches!(self.current, Some(Token::Dot)) {
            self.advance();
            if let Some(Token::LowerIdent(method)) = self.current {
                if method == "derive" {
                    self.advance();
                    self.consume(Token::LParen)?;

                    // Parse derive traits
                    while !matches!(self.current, Some(Token::RParen)) {
                        match self.current {
                            Some(Token::UpperIdent(trait_name)) => {
                                derive_traits.push(trait_name.to_string());
                                self.advance();
                            }
                            _ => return Err(anyhow!("Expected trait name in derive")),
                        }

                        if matches!(self.current, Some(Token::Comma)) {
                            self.advance();
                        }
                    }

                    self.consume(Token::RParen)?;
                }
            }
        }

        // For now, create a define node with a special struct value
        // In a full implementation, we'd have a Node::Struct variant
        let struct_value = if derive_traits.is_empty() {
            self.add_node(Node::List(vec![]))?
        } else {
            // Add derive info as metadata
            let derive_list = derive_traits
                .into_iter()
                .map(|t| self.add_node(Node::Literal(Literal::Symbol(t))))
                .collect::<Result<Vec<_>>>()?;
            self.add_node(Node::List(derive_list))?
        };

        let node = self.add_node(Node::Define {
            name: struct_name.clone(),
            value: struct_value,
        })?;
        Ok((node, struct_name))
    }

    fn parse_enum_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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

        // Check for .derive() attributes
        let mut derive_traits = vec![];
        if matches!(self.current, Some(Token::Dot)) {
            self.advance();
            if let Some(Token::LowerIdent(method)) = self.current {
                if method == "derive" {
                    self.advance();
                    self.consume(Token::LParen)?;

                    // Parse derive traits
                    while !matches!(self.current, Some(Token::RParen)) {
                        match self.current {
                            Some(Token::UpperIdent(trait_name)) => {
                                derive_traits.push(trait_name.to_string());
                                self.advance();
                            }
                            _ => return Err(anyhow!("Expected trait name in derive")),
                        }

                        if matches!(self.current, Some(Token::Comma)) {
                            self.advance();
                        }
                    }

                    self.consume(Token::RParen)?;
                }
            }
        }

        // Create a define node for the enum
        let enum_value = if derive_traits.is_empty() {
            self.add_node(Node::List(vec![]))?
        } else {
            // Add derive info as metadata
            let derive_list = derive_traits
                .into_iter()
                .map(|t| self.add_node(Node::Literal(Literal::Symbol(t))))
                .collect::<Result<Vec<_>>>()?;
            self.add_node(Node::List(derive_list))?
        };

        let node = self.add_node(Node::Define {
            name: enum_name.clone(),
            value: enum_value,
        })?;
        Ok((node, enum_name))
    }

    fn parse_trait_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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
            if matches!(self.current, Some(Token::Private))
                || matches!(self.current, Some(Token::Public))
            {
                self.advance();

                if matches!(self.current, Some(Token::Function)) {
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
        let node = self.add_node(Node::Define {
            name: trait_name.clone(),
            value: trait_value,
        })?;
        Ok((node, trait_name))
    }

    fn peek_ahead_for_as(&mut self) -> bool {
        // Save current position
        let saved_lexer = self.lexer.clone();
        let saved_current = self.current.clone();

        // Advance past the type name
        self.advance();

        // Check if next token is 'as'
        let is_as = matches!(self.current, Some(Token::As));

        // Restore position
        self.lexer = saved_lexer;
        self.current = saved_current;

        is_as
    }

    fn parse_trait_impl(&mut self, _is_public: bool) -> Result<NodeId> {
        // Type as Trait { ... }
        let type_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected type name")),
        };

        self.consume(Token::As)?;

        let trait_name = match self.current {
            Some(Token::UpperIdent(name)) => {
                let name = name.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected trait name after 'as'")),
        };

        self.consume(Token::LBrace)?;

        let mut definitions = vec![];
        while !matches!(self.current, Some(Token::RBrace)) {
            // Parse method implementations
            let def = self.parse_definition()?;
            definitions.push(def);
        }

        self.consume(Token::RBrace)?;

        // For now, just return a placeholder - in a real implementation
        // we'd need a proper AST node for trait implementations
        let impl_body = if definitions.is_empty() {
            self.add_node(Node::Literal(Literal::Nil))?
        } else if definitions.len() == 1 {
            definitions[0]
        } else {
            self.add_node(Node::Begin { exprs: definitions })?
        };

        // Create a define node with special naming for trait impl
        self.add_node(Node::Define {
            name: format!("{}@{}", type_name, trait_name),
            value: impl_body,
        })
    }

    fn parse_type_alias(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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
        let node = self.add_node(Node::Define {
            name: alias_name.clone(),
            value: type_value,
        })?;
        Ok((node, alias_name))
    }

    fn parse_actor_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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
                Some(Token::LowerIdent(field_name)) => {
                    // State field
                    let _field_name = field_name.to_string();
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
                Some(Token::Private) | Some(Token::Public) => {
                    // Handler method or other definition
                    let def = self.parse_definition()?;
                    definitions.push(def);
                }
                _ => return Err(anyhow!("Expected field or handler in actor")),
            }
        }

        self.consume(Token::RBrace)?;

        // Create a define node for the actor
        let actor_value = self.add_node(Node::Begin { exprs: definitions })?;
        let node = self.add_node(Node::Define {
            name: actor_name.clone(),
            value: actor_value,
        })?;
        Ok((node, actor_name))
    }

    fn parse_effect_definition(&mut self, _is_public: bool) -> Result<(NodeId, String)> {
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
            if matches!(self.current, Some(Token::Private))
                || matches!(self.current, Some(Token::Public))
            {
                self.advance();

                if matches!(self.current, Some(Token::Function)) {
                    self.advance();
                }

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
        let node = self.add_node(Node::Define {
            name: effect_name.clone(),
            value: effect_value,
        })?;
        Ok((node, effect_name))
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
                            _ => {}
                        }
                        self.advance();
                    }
                }

                Ok(name)
            }
            Some(Token::LowerIdent(name))
                if matches!(name, "string" | "int" | "float" | "bool" | "any") =>
            {
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
        let pattern = self.parse_or_pattern()?;

        // Check for Guard pattern (pattern when condition)
        if matches!(self.current, Some(Token::When)) {
            self.advance(); // consume when
            let condition = self.parse_expression()?;
            return Ok(Pattern::Guard {
                pattern: Box::new(pattern),
                condition,
            });
        }

        Ok(pattern)
    }

    fn parse_or_pattern(&mut self) -> Result<Pattern> {
        let pattern = self.parse_single_pattern()?;

        // Check for Or pattern (pattern1 | pattern2 | ...)
        if matches!(self.current, Some(Token::Or)) {
            let mut patterns = vec![pattern];
            while matches!(self.current, Some(Token::Or)) {
                self.advance(); // consume |
                patterns.push(self.parse_single_pattern()?);
            }
            return Ok(Pattern::Or(patterns));
        }

        Ok(pattern)
    }

    fn parse_single_pattern(&mut self) -> Result<Pattern> {
        let pattern = self.parse_base_pattern()?;

        // Check for As pattern (pattern as binding)
        if matches!(self.current, Some(Token::As)) {
            self.advance(); // consume as
            match self.current {
                Some(Token::LowerIdent(binding)) => {
                    let binding_name = binding.to_string();
                    self.advance();
                    return Ok(Pattern::As {
                        binding: binding_name,
                        pattern: Box::new(pattern),
                    });
                }
                _ => return Err(anyhow!("Expected binding name after 'as' in pattern")),
            }
        }

        Ok(pattern)
    }

    fn parse_base_pattern(&mut self) -> Result<Pattern> {
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
                    Ok(Pattern::Constructor {
                        name,
                        patterns: vec![],
                    })
                }
            }
            Some(Token::Integer(n)) => {
                let n = *n;
                self.advance();

                // Check for range pattern (1..10 or 1..=10)
                if matches!(self.current, Some(Token::DotDot)) {
                    self.advance(); // consume ..
                    let inclusive = if matches!(self.current, Some(Token::Eq)) {
                        self.advance(); // consume =
                        true
                    } else {
                        false
                    };

                    match self.current {
                        Some(Token::Integer(end)) => {
                            let end_val = end;
                            self.advance();
                            Ok(Pattern::Range(RangePattern {
                                start: Literal::Integer(n),
                                end: Literal::Integer(end_val),
                                inclusive,
                            }))
                        }
                        _ => Err(anyhow!("Expected integer after .. in range pattern")),
                    }
                } else {
                    Ok(Pattern::Literal(Literal::Integer(n)))
                }
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
            Some(Token::Symbol(s)) => {
                let s = s.to_string();
                self.advance();
                Ok(Pattern::Literal(Literal::Symbol(s)))
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

    fn parse_printable(&mut self) -> Result<NodeId> {
        self.consume(Token::Dollar)?;
        self.consume(Token::LParen)?;
        let expr = self.parse_expression()?;
        self.consume(Token::RParen)?;

        // Create a printable wrapper node
        // For now, we'll use a special application of a "Printable" constructor
        let printable_constructor = self.add_node(Node::Variable {
            name: "Printable".to_string(),
        })?;
        self.add_node(Node::Application {
            function: printable_constructor,
            args: vec![expr],
        })
    }

    fn parse_perform_expression(&mut self) -> Result<NodeId> {
        // perform IO.print("Hello")
        self.consume(Token::Perform)?;

        #[cfg(test)]
        eprintln!(
            "parse_perform_expression: after consume perform, current token: {:?}",
            self.current
        );

        // Parse the effect type (e.g., IO)
        let effect_type = match self.current {
            Some(Token::UpperIdent(name)) | Some(Token::ConstIdent(name)) => {
                let effect_name = name.to_string();
                self.advance();

                // Convert string to EffectType
                match effect_name.as_str() {
                    "IO" => EffectType::IO,
                    "State" => EffectType::State,
                    "Error" => EffectType::Error,
                    "Async" => EffectType::Async,
                    "Time" => EffectType::Time,
                    "Network" => EffectType::Network,
                    "Random" => EffectType::Random,
                    "Dom" => EffectType::Dom,
                    "Concurrent" => EffectType::Concurrent,
                    _ => return Err(anyhow!("Unknown effect type: {}", effect_name)),
                }
            }
            _ => return Err(anyhow!("Expected effect type after 'perform'")),
        };

        self.consume(Token::Dot)?;

        // Parse the operation name
        let operation = match self.current {
            Some(Token::LowerIdent(op)) => {
                let op_name = op.to_string();
                self.advance();
                op_name
            }
            _ => return Err(anyhow!("Expected operation name after effect type")),
        };

        // Parse arguments
        self.consume(Token::LParen)?;
        let mut args = vec![];

        while !matches!(self.current, Some(Token::RParen)) {
            args.push(self.parse_expression()?);

            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }

        self.consume(Token::RParen)?;

        self.add_node(Node::Effect {
            effect_type,
            operation,
            args,
        })
    }

    fn parse_handle_expression(&mut self) -> Result<NodeId> {
        // handle { body } with { Effect.operation(params) => handler_expr, ... }
        self.consume(Token::Handle)?;
        self.consume(Token::LBrace)?;

        // Parse the body as a block
        let body = self.parse_block()?;
        self.consume(Token::RBrace)?;

        self.consume(Token::With)?;
        self.consume(Token::LBrace)?;

        let mut handlers = vec![];

        while !matches!(self.current, Some(Token::RBrace)) {
            // Parse effect type
            let effect_type = match self.current {
                Some(Token::UpperIdent(name)) | Some(Token::ConstIdent(name)) => {
                    let effect_name = name.to_string();
                    self.advance();

                    match effect_name.as_str() {
                        "IO" => EffectType::IO,
                        "State" => EffectType::State,
                        "Error" => EffectType::Error,
                        "Async" => EffectType::Async,
                        "Time" => EffectType::Time,
                        "Network" => EffectType::Network,
                        "Random" => EffectType::Random,
                        "Dom" => EffectType::Dom,
                        "Concurrent" => EffectType::Concurrent,
                        _ => return Err(anyhow!("Unknown effect type: {}", effect_name)),
                    }
                }
                _ => return Err(anyhow!("Expected effect type in handler")),
            };

            self.consume(Token::Dot)?;

            // Parse operation name
            let operation = match self.current {
                Some(Token::LowerIdent(op)) => {
                    let op_name = op.to_string();
                    self.advance();
                    Some(op_name)
                }
                _ => return Err(anyhow!("Expected operation name after effect type")),
            };

            // Parse parameters
            self.consume(Token::LParen)?;
            let mut params = vec![];

            while !matches!(self.current, Some(Token::RParen)) {
                match self.current {
                    Some(Token::LowerIdent(param)) => {
                        params.push(param.to_string());
                        self.advance();
                    }
                    _ => return Err(anyhow!("Expected parameter name")),
                }

                if matches!(self.current, Some(Token::Comma)) {
                    self.advance();
                }
            }

            self.consume(Token::RParen)?;
            self.consume(Token::FatArrow)?;

            // Parse handler expression as a lambda
            let handler_body = self.parse_expression()?;
            let handler_fn = self.add_node(Node::Lambda {
                params,
                body: handler_body,
            })?;

            handlers.push((effect_type, operation, handler_fn));

            // Check for comma or end of handlers
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }

        self.consume(Token::RBrace)?;

        self.add_node(Node::Handler { handlers, body })
    }

    fn parse_receive_expression(&mut self) -> Result<NodeId> {
        // receive { pattern => handler, ... }
        self.consume(Token::Receive)?;
        self.consume(Token::LBrace)?;

        let mut patterns = vec![];

        while !matches!(self.current, Some(Token::RBrace)) {
            // Parse pattern
            let pattern = self.parse_pattern()?;

            // Expect =>
            self.consume(Token::FatArrow)?;

            // Parse handler expression
            let handler = self.parse_expression()?;

            patterns.push((pattern, handler));

            // Optional comma
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
            }
        }

        self.consume(Token::RBrace)?;

        // For now, create an ActorReceive node without timeout
        self.add_node(Node::ActorReceive {
            patterns,
            timeout: None,
        })
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

    fn parse_spawn_expression(&mut self) -> Result<NodeId> {
        // spawn { expr } or spawn(expr)
        self.consume(Token::Spawn)?;

        let expr = if matches!(self.current, Some(Token::LBrace)) {
            // spawn { expr }
            self.advance(); // consume {
            let expr = self.parse_expression()?;
            self.consume(Token::RBrace)?;
            expr
        } else if matches!(self.current, Some(Token::LParen)) {
            // spawn(expr)
            self.advance(); // consume (
            let expr = self.parse_expression()?;
            self.consume(Token::RParen)?;
            expr
        } else {
            return Err(anyhow!("Expected '{{' or '(' after 'spawn'"));
        };

        self.add_node(Node::Spawn { expr })
    }

    // Helper methods

    fn advance(&mut self) {
        self.position = self.lexer.span().end;
        self.current = self.lexer.next_token();
    }

    fn consume(&mut self, expected: Token) -> Result<()> {
        if self.current.as_ref().map(|t| std::mem::discriminant(t))
            == Some(std::mem::discriminant(&expected))
        {
            self.advance();
            Ok(())
        } else {
            Err(anyhow!("Expected {:?}, found {:?}", expected, self.current))
        }
    }

    fn parse_contract_annotations(&mut self) -> Result<ContractInfo> {
        let mut contract_info = ContractInfo {
            function_name: None,
            preconditions: vec![],
            postconditions: vec![],
            invariants: vec![],
            complexity: None,
            pure: false,
        };

        // Parse multiple @annotations
        while matches!(self.current, Some(Token::At)) {
            self.advance(); // consume @

            match self.current {
                Some(Token::LowerIdent(annotation)) => {
                    let annotation_name = annotation.to_string();
                    self.advance();

                    self.consume(Token::LParen)?;

                    match annotation_name.as_str() {
                        "contract" => {
                            // @contract(function_name)
                            match self.current {
                                Some(Token::LowerIdent(name)) => {
                                    contract_info.function_name = Some(name.to_string());
                                    self.advance();
                                }
                                _ => return Err(anyhow!("Expected function name in @contract")),
                            }
                        }
                        "requires" => {
                            // @requires(condition)
                            let condition = self.parse_expression()?;
                            contract_info.preconditions.push(condition);
                        }
                        "ensures" => {
                            // @ensures(condition)
                            let condition = self.parse_expression()?;
                            contract_info.postconditions.push(condition);
                        }
                        "invariant" => {
                            // @invariant(condition)
                            let condition = self.parse_expression()?;
                            contract_info.invariants.push(condition);
                        }
                        "complexity" => {
                            // @complexity("O(n)")
                            match &self.current {
                                Some(Token::String(s)) => {
                                    contract_info.complexity = Some(s.to_string());
                                }
                                _ => return Err(anyhow!("Expected string in @complexity")),
                            }
                            self.advance();
                        }
                        "pure" => {
                            // @pure(true) or @pure(false)
                            match self.current {
                                Some(Token::True) => {
                                    contract_info.pure = true;
                                    self.advance();
                                }
                                Some(Token::False) => {
                                    contract_info.pure = false;
                                    self.advance();
                                }
                                _ => return Err(anyhow!("Expected true or false in @pure")),
                            }
                        }
                        _ => return Err(anyhow!("Unknown annotation: @{}", annotation_name)),
                    }

                    self.consume(Token::RParen)?;
                }
                _ => return Err(anyhow!("Expected annotation name after @")),
            }
        }

        Ok(contract_info)
    }

    fn is_map_literal(&self) -> bool {
        // Check if the current pattern looks like a map literal
        // Maps start with string keys: "key": value
        matches!(self.current, Some(Token::String(_)))
    }

    fn parse_map_literal(&mut self) -> Result<NodeId> {
        // Parse map literal: {"key1": value1, "key2": value2}
        // Convert to: make_map("key1", value1, "key2", value2)
        let mut args = vec![];

        loop {
            // Parse key (must be a string)
            let key = match &self.current {
                Some(Token::String(s)) => {
                    let key_node = self.add_node(Node::Literal(Literal::String(s.to_string())))?;
                    self.advance();
                    key_node
                }
                _ => return Err(anyhow!("Expected string key in map literal")),
            };

            self.consume(Token::Colon)?;

            // Parse value
            let value = self.parse_expression()?;

            args.push(key);
            args.push(value);

            // Check for comma or end of map
            if matches!(self.current, Some(Token::Comma)) {
                self.advance();
                if matches!(self.current, Some(Token::RBrace)) {
                    // Trailing comma is allowed
                    break;
                }
            } else if matches!(self.current, Some(Token::RBrace)) {
                break;
            } else {
                return Err(anyhow!("Expected ',' or '}}' in map literal"));
            }
        }

        self.consume(Token::RBrace)?;

        // Create make_map(args...)
        let make_map = self.add_node(Node::Variable {
            name: "make_map".to_string(),
        })?;
        self.add_node(Node::Application {
            function: make_map,
            args,
        })
    }

    fn add_node(&mut self, node: Node) -> Result<NodeId> {
        self.graph.add_node(node).map_err(|e| anyhow!("{}", e))
    }

    fn pattern_to_expression(&mut self, pattern: Pattern) -> Result<NodeId> {
        // Convert a pattern to an expression that represents it
        // This is a simplified conversion for now
        match pattern {
            Pattern::Wildcard => self.add_node(Node::Variable {
                name: "_".to_string(),
            }),
            Pattern::Variable(name) => self.add_node(Node::Variable { name }),
            Pattern::Literal(lit) => self.add_node(Node::Literal(lit)),
            Pattern::Constructor { name, patterns } => {
                let constructor = self.add_node(Node::Variable { name })?;
                let mut args = vec![];
                for p in patterns {
                    args.push(self.pattern_to_expression(p)?);
                }
                self.add_node(Node::Application {
                    function: constructor,
                    args,
                })
            }
            Pattern::Or(patterns) => {
                // For or patterns, we'll create a special "OrPattern" node
                // This is a simplification - in a real implementation, we'd need
                // a proper pattern representation in the AST
                let or_fn = self.add_node(Node::Variable {
                    name: "OrPattern".to_string(),
                })?;
                let mut args = vec![];
                for p in patterns {
                    args.push(self.pattern_to_expression(p)?);
                }
                self.add_node(Node::Application {
                    function: or_fn,
                    args,
                })
            }
            Pattern::Guard { pattern, condition } => {
                // Guard pattern: pattern when condition
                let guard_fn = self.add_node(Node::Variable {
                    name: "GuardPattern".to_string(),
                })?;
                let pattern_expr = self.pattern_to_expression(*pattern)?;
                self.add_node(Node::Application {
                    function: guard_fn,
                    args: vec![pattern_expr, condition],
                })
            }
            Pattern::As { binding, pattern } => {
                // As pattern: pattern as binding
                let as_fn = self.add_node(Node::Variable {
                    name: "AsPattern".to_string(),
                })?;
                let pattern_expr = self.pattern_to_expression(*pattern)?;
                let binding_expr = self.add_node(Node::Variable { name: binding })?;
                self.add_node(Node::Application {
                    function: as_fn,
                    args: vec![pattern_expr, binding_expr],
                })
            }
            Pattern::Range(range) => {
                // Range pattern: start..end or start..=end
                let range_fn = self.add_node(Node::Variable {
                    name: if range.inclusive {
                        "RangeInclusive"
                    } else {
                        "RangeExclusive"
                    }
                    .to_string(),
                })?;
                let start = self.add_node(Node::Literal(range.start))?;
                let end = self.add_node(Node::Literal(range.end))?;
                self.add_node(Node::Application {
                    function: range_fn,
                    args: vec![start, end],
                })
            }
            _ => {
                // For other pattern types, just create a generic pattern node
                self.add_node(Node::Variable {
                    name: "UnknownPattern".to_string(),
                })
            }
        }
    }

    fn parse_fstring(&mut self, s: &str) -> Result<NodeId> {
        // Parse f-string with interpolations
        // Format: f"text {expression} more text {expression}"

        #[cfg(test)]
        eprintln!("parse_fstring: input = {:?}", s);

        let mut parts = Vec::new();
        let mut current = String::new();
        let mut chars = s.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '{' {
                // Check for escaped brace
                if chars.peek() == Some(&'{') {
                    chars.next(); // consume second '{'
                    current.push('{');
                } else {
                    // We have an interpolation
                    // First, add any accumulated string literal
                    if !current.is_empty() {
                        let string_node =
                            self.add_node(Node::Literal(Literal::String(current.clone())))?;
                        parts.push(string_node);
                        current.clear();
                    }

                    // Extract the expression between braces
                    let mut expr_str = String::new();
                    let mut brace_depth = 1;

                    while let Some(ch) = chars.next() {
                        if ch == '{' {
                            brace_depth += 1;
                            expr_str.push(ch);
                        } else if ch == '}' {
                            brace_depth -= 1;
                            if brace_depth == 0 {
                                break;
                            }
                            expr_str.push(ch);
                        } else {
                            expr_str.push(ch);
                        }
                    }

                    if brace_depth != 0 {
                        return Err(anyhow!("Unclosed interpolation in f-string"));
                    }

                    // Parse the expression
                    let expr_node = self.parse_interpolated_expression(&expr_str)?;

                    // Convert to string using a to_string application
                    let to_string_var = self.add_node(Node::Variable {
                        name: "to_string".to_string(),
                    })?;
                    let string_expr = self.add_node(Node::Application {
                        function: to_string_var,
                        args: vec![expr_node],
                    })?;

                    parts.push(string_expr);
                }
            } else if ch == '}' {
                // Check for escaped brace
                if chars.peek() == Some(&'}') {
                    chars.next(); // consume second '}'
                    current.push('}');
                } else {
                    return Err(anyhow!("Unexpected '}}' in f-string"));
                }
            } else {
                current.push(ch);
            }
        }

        // Add any remaining string literal
        if !current.is_empty() {
            let string_node = self.add_node(Node::Literal(Literal::String(current)))?;
            parts.push(string_node);
        }

        // If we have no parts, return empty string
        if parts.is_empty() {
            return self.add_node(Node::Literal(Literal::String(String::new())));
        }

        // If we have only one part, return it directly
        if parts.len() == 1 {
            return Ok(parts[0]);
        }

        // Otherwise, concatenate all parts using string-append
        let mut result = parts[0];
        for part in parts.into_iter().skip(1) {
            let append_var = self.add_node(Node::Variable {
                name: "string-append".to_string(),
            })?;
            result = self.add_node(Node::Application {
                function: append_var,
                args: vec![result, part],
            })?;
        }

        Ok(result)
    }

    fn parse_interpolated_expression(&mut self, expr_str: &str) -> Result<NodeId> {
        // Create a sub-parser for the expression
        let mut sub_parser = Parser::new(expr_str);
        let sub_graph = sub_parser.parse()?;

        // Import the nodes from the sub-graph into our main graph
        if let Some(root_id) = sub_graph.root_id {
            self.import_subgraph_node(&sub_graph, root_id)
        } else {
            Err(anyhow!("Empty expression in interpolation"))
        }
    }

    fn import_subgraph_node(&mut self, sub_graph: &Graph, node_id: NodeId) -> Result<NodeId> {
        // Recursively import nodes from the sub-graph
        if let Some(node) = sub_graph.get_node(node_id) {
            match node {
                Node::Literal(lit) => self.add_node(Node::Literal(lit.clone())),
                Node::Variable { name } => self.add_node(Node::Variable { name: name.clone() }),
                Node::Application { function, args } => {
                    let new_function = self.import_subgraph_node(sub_graph, *function)?;
                    let mut new_args = Vec::new();
                    for arg in args {
                        new_args.push(self.import_subgraph_node(sub_graph, *arg)?);
                    }
                    self.add_node(Node::Application {
                        function: new_function,
                        args: new_args,
                    })
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let new_condition = self.import_subgraph_node(sub_graph, *condition)?;
                    let new_then = self.import_subgraph_node(sub_graph, *then_branch)?;
                    let new_else = self.import_subgraph_node(sub_graph, *else_branch)?;
                    self.add_node(Node::If {
                        condition: new_condition,
                        then_branch: new_then,
                        else_branch: new_else,
                    })
                }
                Node::Lambda { params, body } => {
                    let new_body = self.import_subgraph_node(sub_graph, *body)?;
                    self.add_node(Node::Lambda {
                        params: params.clone(),
                        body: new_body,
                    })
                }
                Node::Let { bindings, body } => {
                    let mut new_bindings = Vec::new();
                    for (name, value) in bindings {
                        let new_value = self.import_subgraph_node(sub_graph, *value)?;
                        new_bindings.push((name.clone(), new_value));
                    }
                    let new_body = self.import_subgraph_node(sub_graph, *body)?;
                    self.add_node(Node::Let {
                        bindings: new_bindings,
                        body: new_body,
                    })
                }
                Node::List(items) => {
                    let mut new_items = Vec::new();
                    for item in items {
                        new_items.push(self.import_subgraph_node(sub_graph, *item)?);
                    }
                    self.add_node(Node::List(new_items))
                }
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => self.add_node(Node::QualifiedVariable {
                    module_name: module_name.clone(),
                    variable_name: variable_name.clone(),
                }),
                // Add other node types as needed
                _ => {
                    // For now, just create a placeholder
                    self.add_node(Node::Variable {
                        name: "UnsupportedInterpolation".to_string(),
                    })
                }
            }
        } else {
            Err(anyhow!("Invalid node in subgraph"))
        }
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
        let parser = Parser::new("x => x * 2");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }

    #[test]
    fn test_parse_lambda_multiple_params() {
        let parser = Parser::new("(x, y) => x + y");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }

    #[test]
    fn test_parse_lambda_no_params() {
        let parser = Parser::new("() => 42");
        let graph = parser.parse().unwrap();
        assert!(graph.root_id.is_some());
    }
}

impl<'a> Parser<'a> {
    fn parse_module_declaration(&mut self) -> Result<()> {
        // module ModuleName;
        self.consume(Token::Mod)?;

        let name = match self.current {
            Some(Token::UpperIdent(n)) | Some(Token::LowerIdent(n)) => {
                let name = n.to_string();
                self.advance();
                name
            }
            _ => return Err(anyhow!("Expected module name after 'module'")),
        };

        self.consume(Token::Semicolon)?;
        self.module_name = Some(name);
        Ok(())
    }
}
