//! AST evaluator for contract conditions

use std::collections::HashMap;

use fluentai_core::{
    ast::{Graph, Node, NodeId, Literal},
    value::Value,
};
use tracing::{debug, trace};

use crate::errors::{ContractError, ContractResult};

/// Evaluates AST expressions for contract verification
pub struct ConditionEvaluator<'a> {
    /// The AST graph containing expressions
    graph: &'a Graph,
    
    /// Current evaluation context
    bindings: HashMap<String, Value>,
}

impl<'a> ConditionEvaluator<'a> {
    /// Create a new condition evaluator
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            bindings: HashMap::new(),
        }
    }
    
    /// Set up bindings for evaluation
    pub fn with_bindings(mut self, bindings: HashMap<String, Value>) -> Self {
        self.bindings = bindings;
        self
    }
    
    /// Add a single binding
    pub fn bind(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }
    
    /// Evaluate a condition expression to a boolean
    pub fn evaluate_condition(&self, node_id: NodeId) -> ContractResult<bool> {
        let value = self.evaluate(node_id)?;
        match value {
            Value::Boolean(b) => Ok(b),
            _ => Err(ContractError::VerificationError(
                format!("Contract condition must evaluate to boolean, got: {:?}", value)
            )),
        }
    }
    
    /// Evaluate an AST expression to a value
    pub fn evaluate(&self, node_id: NodeId) -> ContractResult<Value> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::VerificationError(
                format!("Invalid node ID in contract: {:?}", node_id)
            ))?;
        
        trace!("Evaluating node {:?}: {:?}", node_id, node);
        
        match node {
            Node::Literal(lit) => self.evaluate_literal(lit),
            Node::Variable { name } => self.evaluate_variable(name),
            Node::Application { function, args } => self.evaluate_application(*function, args),
            Node::If { condition, then_branch, else_branch } => {
                self.evaluate_if(*condition, *then_branch, *else_branch)
            }
            Node::List(elements) => self.evaluate_list(elements),
            _ => Err(ContractError::VerificationError(
                format!("Unsupported node type in contract condition: {:?}", node)
            )),
        }
    }
    
    fn evaluate_literal(&self, lit: &Literal) -> ContractResult<Value> {
        Ok(match lit {
            Literal::Integer(i) => Value::Integer(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Nil => Value::Nil,
        })
    }
    
    fn evaluate_variable(&self, name: &str) -> ContractResult<Value> {
        self.bindings.get(name)
            .cloned()
            .ok_or_else(|| ContractError::VerificationError(
                format!("Undefined variable in contract: {}", name)
            ))
    }
    
    fn evaluate_application(&self, function_id: NodeId, args: &[NodeId]) -> ContractResult<Value> {
        // Get the function name
        let func_name = match self.graph.get_node(function_id) {
            Some(Node::Variable { name }) => name,
            _ => return Err(ContractError::VerificationError(
                "Contract conditions can only call built-in predicates".to_string()
            )),
        };
        
        // Evaluate arguments
        let arg_values: Result<Vec<_>, _> = args.iter()
            .map(|&arg| self.evaluate(arg))
            .collect();
        let arg_values = arg_values?;
        
        // Handle built-in contract predicates
        match func_name.as_str() {
            // Comparison operators
            "=" | "==" => self.builtin_equal(&arg_values),
            "!=" | "not=" => self.builtin_not_equal(&arg_values),
            "<" => self.builtin_less(&arg_values),
            ">" => self.builtin_greater(&arg_values),
            "<=" => self.builtin_less_equal(&arg_values),
            ">=" => self.builtin_greater_equal(&arg_values),
            
            // Arithmetic operators (for use in comparisons)
            "+" => self.builtin_add(&arg_values),
            "-" => self.builtin_subtract(&arg_values),
            "*" => self.builtin_multiply(&arg_values),
            "/" => self.builtin_divide(&arg_values),
            
            // Type predicates
            "int?" | "integer?" => self.builtin_is_integer(&arg_values),
            "float?" => self.builtin_is_float(&arg_values),
            "number?" => self.builtin_is_number(&arg_values),
            "string?" => self.builtin_is_string(&arg_values),
            "list?" => self.builtin_is_list(&arg_values),
            "nil?" => self.builtin_is_nil(&arg_values),
            
            // List operations
            "length" => self.builtin_length(&arg_values),
            "nth" => self.builtin_nth(&arg_values),
            "empty?" => self.builtin_is_empty(&arg_values),
            
            // Logical operators
            "and" => self.builtin_and(&arg_values),
            "or" => self.builtin_or(&arg_values),
            "not" => self.builtin_not(&arg_values),
            
            // Custom predicates (would be defined elsewhere)
            "sorted?" => self.builtin_is_sorted(&arg_values),
            "file-exists?" => self.builtin_file_exists(&arg_values),
            
            _ => Err(ContractError::VerificationError(
                format!("Unknown predicate in contract: {}", func_name)
            )),
        }
    }
    
    fn evaluate_if(&self, condition: NodeId, then_branch: NodeId, else_branch: NodeId) -> ContractResult<Value> {
        let cond_value = self.evaluate_condition(condition)?;
        if cond_value {
            self.evaluate(then_branch)
        } else {
            self.evaluate(else_branch)
        }
    }
    
    fn evaluate_list(&self, elements: &[NodeId]) -> ContractResult<Value> {
        let values: Result<Vec<_>, _> = elements.iter()
            .map(|&elem| self.evaluate(elem))
            .collect();
        Ok(Value::List(values?))
    }
    
    // Built-in predicate implementations
    
    fn builtin_equal(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "= requires exactly 2 arguments".to_string()
            ));
        }
        Ok(Value::Boolean(self.values_equal(&args[0], &args[1])))
    }
    
    fn builtin_not_equal(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "!= requires exactly 2 arguments".to_string()
            ));
        }
        Ok(Value::Boolean(!self.values_equal(&args[0], &args[1])))
    }
    
    fn builtin_less(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "< requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a < b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) < *b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a < (*b as f64))),
            _ => Err(ContractError::VerificationError(
                "< requires numeric arguments".to_string()
            )),
        }
    }
    
    fn builtin_greater(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "> requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a > b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) > *b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a > (*b as f64))),
            _ => Err(ContractError::VerificationError(
                "> requires numeric arguments".to_string()
            )),
        }
    }
    
    fn builtin_less_equal(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "<= requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a <= b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) <= *b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a <= (*b as f64))),
            _ => Err(ContractError::VerificationError(
                "<= requires numeric arguments".to_string()
            )),
        }
    }
    
    fn builtin_greater_equal(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                ">= requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a >= b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Boolean((*a as f64) >= *b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Boolean(*a >= (*b as f64))),
            _ => Err(ContractError::VerificationError(
                ">= requires numeric arguments".to_string()
            )),
        }
    }
    
    fn builtin_add(&self, args: &[Value]) -> ContractResult<Value> {
        if args.is_empty() {
            return Ok(Value::Integer(0));
        }
        
        let mut result = args[0].clone();
        for arg in &args[1..] {
            match (&result, arg) {
                (Value::Integer(a), Value::Integer(b)) => result = Value::Integer(a + b),
                (Value::Float(a), Value::Float(b)) => result = Value::Float(a + b),
                (Value::Integer(a), Value::Float(b)) => result = Value::Float(*a as f64 + b),
                (Value::Float(a), Value::Integer(b)) => result = Value::Float(a + *b as f64),
                _ => return Err(ContractError::VerificationError(
                    "+ requires numeric arguments".to_string()
                )),
            }
        }
        Ok(result)
    }
    
    fn builtin_subtract(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "- requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a - *b as f64)),
            _ => Err(ContractError::VerificationError(
                "- requires numeric arguments".to_string()
            )),
        }
    }
    
    fn builtin_multiply(&self, args: &[Value]) -> ContractResult<Value> {
        if args.is_empty() {
            return Ok(Value::Integer(1));
        }
        
        let mut result = args[0].clone();
        for arg in &args[1..] {
            match (&result, arg) {
                (Value::Integer(a), Value::Integer(b)) => result = Value::Integer(a * b),
                (Value::Float(a), Value::Float(b)) => result = Value::Float(a * b),
                (Value::Integer(a), Value::Float(b)) => result = Value::Float(*a as f64 * b),
                (Value::Float(a), Value::Integer(b)) => result = Value::Float(a * *b as f64),
                _ => return Err(ContractError::VerificationError(
                    "* requires numeric arguments".to_string()
                )),
            }
        }
        Ok(result)
    }
    
    fn builtin_divide(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "/ requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => {
                if *b == 0 {
                    return Err(ContractError::VerificationError("Division by zero".to_string()));
                }
                Ok(Value::Integer(a / b))
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (Value::Integer(a), Value::Float(b)) => Ok(Value::Float(*a as f64 / b)),
            (Value::Float(a), Value::Integer(b)) => Ok(Value::Float(a / *b as f64)),
            _ => Err(ContractError::VerificationError(
                "/ requires numeric arguments".to_string()
            )),
        }
    }
    
    fn builtin_is_integer(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "int? requires exactly 1 argument".to_string()
            ));
        }
        Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
    }
    
    fn builtin_is_float(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "float? requires exactly 1 argument".to_string()
            ));
        }
        Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
    }
    
    fn builtin_is_number(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "number? requires exactly 1 argument".to_string()
            ));
        }
        Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Float(_))))
    }
    
    fn builtin_is_string(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "string? requires exactly 1 argument".to_string()
            ));
        }
        Ok(Value::Boolean(matches!(args[0], Value::String(_))))
    }
    
    fn builtin_is_list(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "list? requires exactly 1 argument".to_string()
            ));
        }
        Ok(Value::Boolean(matches!(args[0], Value::List(_))))
    }
    
    fn builtin_is_nil(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "nil? requires exactly 1 argument".to_string()
            ));
        }
        Ok(Value::Boolean(matches!(args[0], Value::Nil)))
    }
    
    fn builtin_length(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "length requires exactly 1 argument".to_string()
            ));
        }
        match &args[0] {
            Value::List(list) => Ok(Value::Integer(list.len() as i64)),
            Value::String(s) => Ok(Value::Integer(s.len() as i64)),
            _ => Err(ContractError::VerificationError(
                "length requires a list or string".to_string()
            )),
        }
    }
    
    fn builtin_nth(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 2 {
            return Err(ContractError::VerificationError(
                "nth requires exactly 2 arguments".to_string()
            ));
        }
        match (&args[0], &args[1]) {
            (Value::List(list), Value::Integer(idx)) => {
                let idx = *idx as usize;
                if idx < list.len() {
                    Ok(list[idx].clone())
                } else {
                    Err(ContractError::VerificationError(
                        format!("Index {} out of bounds for list of length {}", idx, list.len())
                    ))
                }
            }
            _ => Err(ContractError::VerificationError(
                "nth requires a list and integer index".to_string()
            )),
        }
    }
    
    fn builtin_is_empty(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "empty? requires exactly 1 argument".to_string()
            ));
        }
        match &args[0] {
            Value::List(list) => Ok(Value::Boolean(list.is_empty())),
            Value::String(s) => Ok(Value::Boolean(s.is_empty())),
            _ => Err(ContractError::VerificationError(
                "empty? requires a list or string".to_string()
            )),
        }
    }
    
    fn builtin_and(&self, args: &[Value]) -> ContractResult<Value> {
        for arg in args {
            match arg {
                Value::Boolean(false) => return Ok(Value::Boolean(false)),
                Value::Boolean(true) => continue,
                _ => return Err(ContractError::VerificationError(
                    "and requires boolean arguments".to_string()
                )),
            }
        }
        Ok(Value::Boolean(true))
    }
    
    fn builtin_or(&self, args: &[Value]) -> ContractResult<Value> {
        for arg in args {
            match arg {
                Value::Boolean(true) => return Ok(Value::Boolean(true)),
                Value::Boolean(false) => continue,
                _ => return Err(ContractError::VerificationError(
                    "or requires boolean arguments".to_string()
                )),
            }
        }
        Ok(Value::Boolean(false))
    }
    
    fn builtin_not(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "not requires exactly 1 argument".to_string()
            ));
        }
        match &args[0] {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(ContractError::VerificationError(
                "not requires a boolean argument".to_string()
            )),
        }
    }
    
    fn builtin_is_sorted(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "sorted? requires exactly 1 argument".to_string()
            ));
        }
        match &args[0] {
            Value::List(list) => {
                if list.is_empty() || list.len() == 1 {
                    return Ok(Value::Boolean(true));
                }
                
                // Check if all elements are comparable
                let all_numbers = list.iter().all(|v| matches!(v, Value::Integer(_) | Value::Float(_)));
                if !all_numbers {
                    return Ok(Value::Boolean(false));
                }
                
                // Check if sorted
                for i in 0..list.len() - 1 {
                    if !self.value_less_equal(&list[i], &list[i + 1]) {
                        return Ok(Value::Boolean(false));
                    }
                }
                Ok(Value::Boolean(true))
            }
            _ => Err(ContractError::VerificationError(
                "sorted? requires a list".to_string()
            )),
        }
    }
    
    fn builtin_file_exists(&self, args: &[Value]) -> ContractResult<Value> {
        if args.len() != 1 {
            return Err(ContractError::VerificationError(
                "file-exists? requires exactly 1 argument".to_string()
            ));
        }
        match &args[0] {
            Value::String(path) => {
                // For contract verification, we'll assume file predicates need to be mocked
                // In a real implementation, this would check the filesystem
                debug!("file-exists? called with path: {}", path);
                Ok(Value::Boolean(true)) // Placeholder
            }
            _ => Err(ContractError::VerificationError(
                "file-exists? requires a string path".to_string()
            )),
        }
    }
    
    // Helper methods
    
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::List(a), Value::List(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| self.values_equal(a, b))
            }
            _ => false,
        }
    }
    
    fn value_less_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Integer(a), Value::Integer(b)) => a <= b,
            (Value::Float(a), Value::Float(b)) => a <= b,
            (Value::Integer(a), Value::Float(b)) => (*a as f64) <= *b,
            (Value::Float(a), Value::Integer(b)) => *a <= (*b as f64),
            _ => false,
        }
    }
}