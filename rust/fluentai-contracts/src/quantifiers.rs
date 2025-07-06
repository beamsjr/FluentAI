//! Quantifier support for contract specifications
//! 
//! This module provides support for universal (forall) and existential (exists)
//! quantifiers in contract expressions. Since the AST doesn't have native
//! quantifier support, we encode them as special function applications.

use fluentai_core::ast::{Graph, Node, NodeId};
use crate::errors::{ContractError, ContractResult};
use std::collections::HashMap;

/// Represents a quantified expression
#[derive(Debug, Clone, PartialEq)]
pub struct QuantifiedExpression {
    /// Type of quantifier
    pub quantifier: Quantifier,
    /// Bound variables with their types
    pub bound_vars: Vec<(String, QuantifierDomain)>,
    /// The body expression
    pub body: NodeId,
}

/// Types of quantifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quantifier {
    /// Universal quantification (∀)
    ForAll,
    /// Existential quantification (∃)
    Exists,
}

/// Domain for quantified variables
#[derive(Debug, Clone, PartialEq)]
pub enum QuantifierDomain {
    /// Integer range [min, max]
    IntRange(i64, i64),
    /// Natural numbers up to n
    Natural(u64),
    /// Elements of a list
    ListElements(NodeId),
    /// Indices of a list
    ListIndices(NodeId),
    /// Boolean values
    Boolean,
    /// Unspecified (for Z3 to determine)
    Unspecified,
}

/// Parser for quantified expressions in contracts
pub struct QuantifierParser<'a> {
    graph: &'a Graph,
}

impl<'a> QuantifierParser<'a> {
    /// Create a new quantifier parser
    pub fn new(graph: &'a Graph) -> Self {
        Self { graph }
    }
    
    /// Parse a quantified expression from an AST node
    /// 
    /// Expected format:
    /// - (forall ((x Int) (y Int)) body)
    /// - (exists ((x (range 0 10))) body)
    /// - (forall ((x (in lst))) body)
    pub fn parse_quantifier(&self, node_id: NodeId) -> ContractResult<Option<QuantifiedExpression>> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::InvalidExpression(
                format!("Node {} not found", node_id)
            ))?;
        
        match node {
            Node::Application { function, args } => {
                // Check if it's a quantifier function
                let func_name = self.get_function_name(*function)?;
                
                let quantifier = match func_name.as_str() {
                    "forall" | "∀" => Quantifier::ForAll,
                    "exists" | "∃" => Quantifier::Exists,
                    _ => return Ok(None), // Not a quantifier
                };
                
                // Parse quantifier arguments
                if args.len() != 2 {
                    return Err(ContractError::InvalidExpression(
                        format!("{} expects 2 arguments: bindings and body", func_name)
                    ));
                }
                
                let bindings = self.parse_bindings(args[0])?;
                let body = args[1];
                
                Ok(Some(QuantifiedExpression {
                    quantifier,
                    bound_vars: bindings,
                    body,
                }))
            }
            _ => Ok(None),
        }
    }
    
    /// Get function name from a node
    fn get_function_name(&self, node_id: NodeId) -> ContractResult<String> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::InvalidExpression(
                format!("Function node {} not found", node_id)
            ))?;
        
        match node {
            Node::Variable { name } => Ok(name.clone()),
            _ => Err(ContractError::InvalidExpression(
                "Expected function name".to_string()
            )),
        }
    }
    
    /// Parse variable bindings
    /// Expected format: ((var1 domain1) (var2 domain2) ...)
    fn parse_bindings(&self, node_id: NodeId) -> ContractResult<Vec<(String, QuantifierDomain)>> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::InvalidExpression(
                format!("Bindings node {} not found", node_id)
            ))?;
        
        match node {
            Node::List(bindings) => {
                let mut result = Vec::new();
                
                for binding_id in bindings {
                    let binding = self.graph.get_node(*binding_id)
                        .ok_or_else(|| ContractError::InvalidExpression(
                            "Invalid binding".to_string()
                        ))?;
                    
                    match binding {
                        Node::List(pair) if pair.len() == 2 => {
                            let var_name = self.get_variable_name(pair[0])?;
                            let domain = self.parse_domain(pair[1])?;
                            result.push((var_name, domain));
                        }
                        _ => return Err(ContractError::InvalidExpression(
                            "Binding must be (var domain) pair".to_string()
                        )),
                    }
                }
                
                Ok(result)
            }
            _ => Err(ContractError::InvalidExpression(
                "Bindings must be a list".to_string()
            )),
        }
    }
    
    /// Get variable name from a node
    fn get_variable_name(&self, node_id: NodeId) -> ContractResult<String> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::InvalidExpression(
                format!("Variable node {} not found", node_id)
            ))?;
        
        match node {
            Node::Variable { name } => Ok(name.clone()),
            _ => Err(ContractError::InvalidExpression(
                "Expected variable name".to_string()
            )),
        }
    }
    
    /// Parse domain specification
    fn parse_domain(&self, node_id: NodeId) -> ContractResult<QuantifierDomain> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::InvalidExpression(
                format!("Domain node {} not found", node_id)
            ))?;
        
        match node {
            Node::Variable { name } => {
                match name.as_str() {
                    "Int" | "Integer" => Ok(QuantifierDomain::Unspecified),
                    "Bool" | "Boolean" => Ok(QuantifierDomain::Boolean),
                    _ => Err(ContractError::InvalidExpression(
                        format!("Unknown domain type: {}", name)
                    )),
                }
            }
            Node::Application { function, args } => {
                let func_name = self.get_function_name(*function)?;
                
                match func_name.as_str() {
                    "range" => {
                        if args.len() != 2 {
                            return Err(ContractError::InvalidExpression(
                                "range expects 2 arguments".to_string()
                            ));
                        }
                        let min = self.get_integer_literal(args[0])?;
                        let max = self.get_integer_literal(args[1])?;
                        Ok(QuantifierDomain::IntRange(min, max))
                    }
                    "in" => {
                        if args.len() != 1 {
                            return Err(ContractError::InvalidExpression(
                                "in expects 1 argument".to_string()
                            ));
                        }
                        Ok(QuantifierDomain::ListElements(args[0]))
                    }
                    "indices" => {
                        if args.len() != 1 {
                            return Err(ContractError::InvalidExpression(
                                "indices expects 1 argument".to_string()
                            ));
                        }
                        Ok(QuantifierDomain::ListIndices(args[0]))
                    }
                    _ => Err(ContractError::InvalidExpression(
                        format!("Unknown domain function: {}", func_name)
                    )),
                }
            }
            _ => Err(ContractError::InvalidExpression(
                "Invalid domain specification".to_string()
            )),
        }
    }
    
    /// Get integer literal value
    fn get_integer_literal(&self, node_id: NodeId) -> ContractResult<i64> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::InvalidExpression(
                format!("Integer node {} not found", node_id)
            ))?;
        
        match node {
            Node::Literal(fluentai_core::ast::Literal::Integer(n)) => Ok(*n),
            _ => Err(ContractError::InvalidExpression(
                "Expected integer literal".to_string()
            )),
        }
    }
}

/// Helper to build quantified expressions
pub struct QuantifierBuilder {
    pub graph: Graph,
}

impl QuantifierBuilder {
    /// Create a new quantifier builder
    pub fn new(graph: Graph) -> Self {
        Self { graph }
    }
    
    /// Build a forall expression
    pub fn forall(
        &mut self,
        vars: Vec<(&str, QuantifierDomain)>,
        body: impl FnOnce(&mut Self, &HashMap<String, NodeId>) -> NodeId,
    ) -> NodeId {
        self.build_quantifier(Quantifier::ForAll, vars, body)
    }
    
    /// Build an exists expression
    pub fn exists(
        &mut self,
        vars: Vec<(&str, QuantifierDomain)>,
        body: impl FnOnce(&mut Self, &HashMap<String, NodeId>) -> NodeId,
    ) -> NodeId {
        self.build_quantifier(Quantifier::Exists, vars, body)
    }
    
    /// Internal quantifier builder
    fn build_quantifier(
        &mut self,
        quantifier: Quantifier,
        vars: Vec<(&str, QuantifierDomain)>,
        body: impl FnOnce(&mut Self, &HashMap<String, NodeId>) -> NodeId,
    ) -> NodeId {
        // Create variable nodes
        let mut var_map = HashMap::new();
        let mut binding_nodes = Vec::new();
        
        for (var_name, domain) in vars {
            let var_node = self.graph.add_node(Node::Variable { 
                name: var_name.to_string() 
            });
            var_map.insert(var_name.to_string(), var_node);
            
            // Create domain node
            let domain_node = self.create_domain_node(domain);
            
            // Create binding pair
            let binding = self.graph.add_node(Node::List(vec![var_node, domain_node]));
            binding_nodes.push(binding);
        }
        
        // Create bindings list
        let bindings = self.graph.add_node(Node::List(binding_nodes));
        
        // Create body with variable bindings
        let body_node = body(self, &var_map);
        
        // Create quantifier function
        let quant_name = match quantifier {
            Quantifier::ForAll => "forall",
            Quantifier::Exists => "exists",
        };
        let quant_func = self.graph.add_node(Node::Variable { 
            name: quant_name.to_string() 
        });
        
        // Create quantified expression
        self.graph.add_node(Node::Application {
            function: quant_func,
            args: vec![bindings, body_node],
        })
    }
    
    /// Create a domain specification node
    fn create_domain_node(&mut self, domain: QuantifierDomain) -> NodeId {
        match domain {
            QuantifierDomain::Unspecified => {
                self.graph.add_node(Node::Variable { name: "Int".to_string() })
            }
            QuantifierDomain::Boolean => {
                self.graph.add_node(Node::Variable { name: "Bool".to_string() })
            }
            QuantifierDomain::IntRange(min, max) => {
                let range_func = self.graph.add_node(Node::Variable { 
                    name: "range".to_string() 
                });
                let min_node = self.graph.add_node(Node::Literal(
                    fluentai_core::ast::Literal::Integer(min)
                ));
                let max_node = self.graph.add_node(Node::Literal(
                    fluentai_core::ast::Literal::Integer(max)
                ));
                self.graph.add_node(Node::Application {
                    function: range_func,
                    args: vec![min_node, max_node],
                })
            }
            QuantifierDomain::Natural(n) => {
                // Represent as range [0, n]
                self.create_domain_node(QuantifierDomain::IntRange(0, n as i64))
            }
            QuantifierDomain::ListElements(list_id) => {
                let in_func = self.graph.add_node(Node::Variable { 
                    name: "in".to_string() 
                });
                self.graph.add_node(Node::Application {
                    function: in_func,
                    args: vec![list_id],
                })
            }
            QuantifierDomain::ListIndices(list_id) => {
                let indices_func = self.graph.add_node(Node::Variable { 
                    name: "indices".to_string() 
                });
                self.graph.add_node(Node::Application {
                    function: indices_func,
                    args: vec![list_id],
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_quantifier_builder() {
        let mut builder = QuantifierBuilder::new(Graph::new());
        
        // Build: forall x in [0, 10], x >= 0
        let quantified = builder.forall(
            vec![("x", QuantifierDomain::IntRange(0, 10))],
            |builder, vars| {
                let x = vars["x"];
                let zero = builder.graph.add_node(Node::Literal(
                    fluentai_core::ast::Literal::Integer(0)
                ));
                let ge = builder.graph.add_node(Node::Variable {
                    name: ">=".to_string()
                });
                builder.graph.add_node(Node::Application {
                    function: ge,
                    args: vec![x, zero],
                })
            }
        );
        
        // Parse it back
        let parser = QuantifierParser::new(&builder.graph);
        let parsed = parser.parse_quantifier(quantified).unwrap();
        
        assert!(parsed.is_some());
        let quant = parsed.unwrap();
        assert_eq!(quant.quantifier, Quantifier::ForAll);
        assert_eq!(quant.bound_vars.len(), 1);
        assert_eq!(quant.bound_vars[0].0, "x");
    }
}