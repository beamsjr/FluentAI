//! Actor definition validation utilities

use anyhow::{anyhow, Result};
use std::collections::HashSet;

pub struct ActorValidator {
    handler_names: HashSet<String>,
    field_names: HashSet<String>,
    has_seen_handler: bool,
}

impl ActorValidator {
    pub fn new() -> Self {
        Self {
            handler_names: HashSet::new(),
            field_names: HashSet::new(),
            has_seen_handler: false,
        }
    }
    
    /// Validate that a state field is being added in the correct position
    pub fn validate_state_field(&mut self, field_name: &str) -> Result<()> {
        // Check if we've already seen a handler
        if self.has_seen_handler {
            return Err(anyhow!(
                "State field '{}' cannot be declared after handlers. All state fields must be declared before any handlers.",
                field_name
            ));
        }
        
        // Check for duplicate field names
        if self.field_names.contains(field_name) {
            return Err(anyhow!(
                "Duplicate state field '{}'. Each state field must have a unique name.",
                field_name
            ));
        }
        
        // Check that field name doesn't conflict with handler names
        if self.handler_names.contains(field_name) {
            return Err(anyhow!(
                "State field '{}' conflicts with handler name. Field and handler names must be unique.",
                field_name
            ));
        }
        
        self.field_names.insert(field_name.to_string());
        Ok(())
    }
    
    /// Validate that a handler is being added correctly
    pub fn validate_handler(&mut self, handler_name: &str) -> Result<()> {
        self.has_seen_handler = true;
        
        // Check for duplicate handler names
        if self.handler_names.contains(handler_name) {
            return Err(anyhow!(
                "Duplicate handler '{}'. Each handler must have a unique name.",
                handler_name
            ));
        }
        
        // Check that handler name doesn't conflict with field names
        if self.field_names.contains(handler_name) {
            return Err(anyhow!(
                "Handler '{}' conflicts with state field name. Field and handler names must be unique.",
                handler_name
            ));
        }
        
        // Validate handler name format
        if handler_name.is_empty() {
            return Err(anyhow!("Handler name cannot be empty"));
        }
        
        self.handler_names.insert(handler_name.to_string());
        Ok(())
    }
    
    /// Validate the overall actor structure
    pub fn validate_complete(&self, actor_name: &str) -> Result<()> {
        // Actors must have at least one handler
        if self.handler_names.is_empty() {
            return Err(anyhow!(
                "Actor '{}' must have at least one handler method",
                actor_name
            ));
        }
        
        Ok(())
    }
    
    /// Check if a field type is valid for actor state
    pub fn validate_field_type(field_type: &str) -> Result<()> {
        // For now, we'll accept any type, but we could add restrictions
        // e.g., no function types, no actor types as fields, etc.
        if field_type.is_empty() {
            return Err(anyhow!("State field type cannot be empty"));
        }
        
        Ok(())
    }
    
    /// Check if handler parameters are valid
    pub fn validate_handler_params(params: &[(String, String)]) -> Result<()> {
        let mut param_names = HashSet::new();
        
        for (param_name, param_type) in params {
            // Check for duplicate parameter names
            if param_names.contains(param_name) {
                return Err(anyhow!(
                    "Duplicate parameter name '{}' in handler",
                    param_name
                ));
            }
            
            // Validate parameter name
            if param_name.is_empty() {
                return Err(anyhow!("Handler parameter name cannot be empty"));
            }
            
            // Validate parameter type
            if param_type.is_empty() {
                return Err(anyhow!("Handler parameter type cannot be empty"));
            }
            
            param_names.insert(param_name.clone());
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_state_field_after_handler() {
        let mut validator = ActorValidator::new();
        
        // Add a handler first
        assert!(validator.validate_handler("inc").is_ok());
        
        // Try to add a state field after handler
        let result = validator.validate_state_field("count");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be declared after handlers"));
    }
    
    #[test]
    fn test_duplicate_handler_names() {
        let mut validator = ActorValidator::new();
        
        // Add first handler
        assert!(validator.validate_handler("process").is_ok());
        
        // Try to add duplicate handler
        let result = validator.validate_handler("process");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Duplicate handler"));
    }
    
    #[test]
    fn test_field_handler_name_conflict() {
        let mut validator = ActorValidator::new();
        
        // Add a field
        assert!(validator.validate_state_field("data").is_ok());
        
        // Try to add handler with same name
        let result = validator.validate_handler("data");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("conflicts with state field"));
    }
    
    #[test]
    fn test_actor_without_handlers() {
        let validator = ActorValidator::new();
        
        // Validate without adding any handlers
        let result = validator.validate_complete("MyActor");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("must have at least one handler"));
    }
    
    #[test]
    fn test_duplicate_parameter_names() {
        let params = vec![
            ("x".to_string(), "int".to_string()),
            ("x".to_string(), "float".to_string()),
        ];
        
        let result = ActorValidator::validate_handler_params(&params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Duplicate parameter name"));
    }
}