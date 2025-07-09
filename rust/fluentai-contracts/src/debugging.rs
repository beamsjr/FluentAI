//! Enhanced contract debugging with visual diagrams and detailed error messages

use crate::{
    Contract, ContractCondition, ContractPredicate, ContractViolation,
    VerificationResult, ViolationType, runtime::EvaluationContext,
};
use fluentai_core::Value;
use std::fmt::{self, Write};
use std::collections::HashMap;

/// Enhanced debugging information for contract violations
#[derive(Debug, Clone)]
pub struct DebugInfo {
    /// The specific condition that failed
    pub failed_condition: ContractCondition,
    /// Evaluation trace showing each step
    pub evaluation_trace: Vec<TraceStep>,
    /// Values of all variables at failure point
    pub variable_values: HashMap<String, Value>,
    /// Suggested fixes based on the failure
    pub suggestions: Vec<String>,
    /// Visual representation of the failure
    pub visual_diagram: String,
}

/// A single step in the evaluation trace
#[derive(Debug, Clone)]
pub struct TraceStep {
    /// The expression being evaluated
    pub expression: String,
    /// The result of evaluation
    pub result: EvalResult,
    /// Depth in the evaluation tree
    pub depth: usize,
    /// Source location if available
    pub location: Option<(usize, usize)>,
}

/// Result of evaluating a subexpression
#[derive(Debug, Clone)]
pub enum EvalResult {
    /// Successfully evaluated to a value
    Success(Value),
    /// Failed with an error
    Error(String),
    /// Skipped due to short-circuit evaluation
    Skipped(String),
}

/// Contract debugger for enhanced error reporting
pub struct ContractDebugger {
    /// Whether to generate visual diagrams
    pub visual_mode: bool,
    /// Maximum depth for trace collection
    pub max_trace_depth: usize,
    /// Whether to collect suggestions
    pub suggest_fixes: bool,
}

impl Default for ContractDebugger {
    fn default() -> Self {
        Self {
            visual_mode: true,
            max_trace_depth: 10,
            suggest_fixes: true,
        }
    }
}

impl ContractDebugger {
    /// Debug a contract violation with enhanced information
    pub fn debug_violation(
        &self,
        violation: &ContractViolation,
        context: &EvaluationContext,
    ) -> DebugInfo {
        let failed_condition = self.extract_failed_condition(violation);
        let evaluation_trace = self.build_evaluation_trace(&failed_condition, context);
        let variable_values = self.collect_variable_values(context);
        let suggestions = if self.suggest_fixes {
            self.generate_suggestions(violation, &failed_condition, &variable_values)
        } else {
            vec![]
        };
        let visual_diagram = if self.visual_mode {
            self.generate_visual_diagram(&failed_condition, &evaluation_trace)
        } else {
            String::new()
        };

        DebugInfo {
            failed_condition,
            evaluation_trace,
            variable_values,
            suggestions,
            visual_diagram,
        }
    }

    /// Extract the specific condition that failed
    fn extract_failed_condition(&self, violation: &ContractViolation) -> ContractCondition {
        // Find the deepest failing condition in the contract
        match &violation.violation_type {
            ViolationType::PreconditionFailed(idx) => {
                violation.contract.preconditions.get(*idx)
                    .cloned()
                    .unwrap_or_else(|| ContractCondition {
                        predicate: ContractPredicate::Bool(false),
                        message: Some("Unknown precondition".to_string()),
                    })
            }
            ViolationType::PostconditionFailed(idx) => {
                violation.contract.postconditions.get(*idx)
                    .cloned()
                    .unwrap_or_else(|| ContractCondition {
                        predicate: ContractPredicate::Bool(false),
                        message: Some("Unknown postcondition".to_string()),
                    })
            }
            ViolationType::InvariantFailed(idx) => {
                violation.contract.invariants.get(*idx)
                    .cloned()
                    .unwrap_or_else(|| ContractCondition {
                        predicate: ContractPredicate::Bool(false),
                        message: Some("Unknown invariant".to_string()),
                    })
            }
            _ => ContractCondition {
                predicate: ContractPredicate::Bool(false),
                message: Some(format!("{:?}", violation.violation_type)),
            },
        }
    }

    /// Build step-by-step evaluation trace
    fn build_evaluation_trace(
        &self,
        condition: &ContractCondition,
        context: &EvaluationContext,
    ) -> Vec<TraceStep> {
        let mut trace = Vec::new();
        self.trace_predicate(&condition.predicate, context, &mut trace, 0);
        trace
    }

    /// Recursively trace predicate evaluation
    fn trace_predicate(
        &self,
        predicate: &ContractPredicate,
        context: &EvaluationContext,
        trace: &mut Vec<TraceStep>,
        depth: usize,
    ) {
        if depth > self.max_trace_depth {
            return;
        }

        let expression = format!("{:?}", predicate);
        let result = match predicate {
            ContractPredicate::Bool(b) => {
                EvalResult::Success(Value::Boolean(*b))
            }
            ContractPredicate::Comparison { left, op, right } => {
                // Trace left side
                self.trace_predicate(left, context, trace, depth + 1);
                // Trace right side
                self.trace_predicate(right, context, trace, depth + 1);
                
                // Evaluate comparison
                match (self.eval_predicate(left, context), self.eval_predicate(right, context)) {
                    (Ok(l), Ok(r)) => {
                        let result = self.compare_values(&l, op, &r);
                        EvalResult::Success(Value::Boolean(result))
                    }
                    (Err(e), _) | (_, Err(e)) => EvalResult::Error(e),
                }
            }
            ContractPredicate::And(predicates) => {
                let mut all_true = true;
                for p in predicates {
                    self.trace_predicate(p, context, trace, depth + 1);
                    match self.eval_predicate(p, context) {
                        Ok(Value::Boolean(false)) => {
                            all_true = false;
                            // Short circuit - remaining predicates are skipped
                            break;
                        }
                        Err(e) => {
                            trace.push(TraceStep {
                                expression: format!("{:?}", p),
                                result: EvalResult::Error(e),
                                depth: depth + 1,
                                location: None,
                            });
                            return;
                        }
                        _ => {}
                    }
                }
                EvalResult::Success(Value::Boolean(all_true))
            }
            ContractPredicate::Or(predicates) => {
                let mut any_true = false;
                for p in predicates {
                    self.trace_predicate(p, context, trace, depth + 1);
                    match self.eval_predicate(p, context) {
                        Ok(Value::Boolean(true)) => {
                            any_true = true;
                            // Short circuit - remaining predicates are skipped
                            break;
                        }
                        Err(e) => {
                            trace.push(TraceStep {
                                expression: format!("{:?}", p),
                                result: EvalResult::Error(e),
                                depth: depth + 1,
                                location: None,
                            });
                            return;
                        }
                        _ => {}
                    }
                }
                EvalResult::Success(Value::Boolean(any_true))
            }
            _ => {
                match self.eval_predicate(predicate, context) {
                    Ok(v) => EvalResult::Success(v),
                    Err(e) => EvalResult::Error(e),
                }
            }
        };

        trace.push(TraceStep {
            expression,
            result,
            depth,
            location: None,
        });
    }

    /// Evaluate a predicate (simplified for example)
    fn eval_predicate(&self, predicate: &ContractPredicate, context: &EvaluationContext) -> Result<Value, String> {
        // This would use the actual evaluation logic from runtime.rs
        // Simplified for demonstration
        Ok(Value::Boolean(true))
    }

    /// Compare values based on operator
    fn compare_values(&self, left: &Value, op: &str, right: &Value) -> bool {
        match (left, op, right) {
            (Value::Integer(l), "=", Value::Integer(r)) => l == r,
            (Value::Integer(l), "<", Value::Integer(r)) => l < r,
            (Value::Integer(l), ">", Value::Integer(r)) => l > r,
            (Value::Integer(l), "<=", Value::Integer(r)) => l <= r,
            (Value::Integer(l), ">=", Value::Integer(r)) => l >= r,
            _ => false,
        }
    }

    /// Collect all variable values from context
    fn collect_variable_values(&self, context: &EvaluationContext) -> HashMap<String, Value> {
        let mut values = HashMap::new();
        
        // Collect from bindings
        for (name, value) in &context.bindings {
            values.insert(name.clone(), value.clone());
        }
        
        // Collect ghost state
        for (name, value) in &context.ghost_state {
            values.insert(format!("@{}", name), value.clone());
        }
        
        values
    }

    /// Generate helpful suggestions based on the failure
    fn generate_suggestions(
        &self,
        violation: &ContractViolation,
        condition: &ContractCondition,
        values: &HashMap<String, Value>,
    ) -> Vec<String> {
        let mut suggestions = Vec::new();

        match &violation.violation_type {
            ViolationType::PreconditionFailed(_) => {
                suggestions.push("Check input validation before calling this function".to_string());
                
                // Analyze the failed condition for specific suggestions
                match &condition.predicate {
                    ContractPredicate::Comparison { left, op, right } => {
                        if let (ContractPredicate::Variable(var), ContractPredicate::Value(val)) = (left.as_ref(), right.as_ref()) {
                            if op == ">" || op == ">=" {
                                suggestions.push(format!(
                                    "Ensure {} is at least {} before calling",
                                    var, val
                                ));
                            }
                        }
                    }
                    ContractPredicate::TypeCheck { value, expected_type } => {
                        suggestions.push(format!(
                            "Convert the value to {} type before passing",
                            expected_type
                        ));
                    }
                    _ => {}
                }
            }
            ViolationType::PostconditionFailed(_) => {
                suggestions.push("The function implementation may have a bug".to_string());
                suggestions.push("Check the function logic against its specification".to_string());
            }
            ViolationType::InvariantFailed(_) => {
                suggestions.push("The object state has become inconsistent".to_string());
                suggestions.push("Review state-modifying operations".to_string());
            }
            _ => {}
        }

        suggestions
    }

    /// Generate a visual diagram of the contract failure
    fn generate_visual_diagram(
        &self,
        condition: &ContractCondition,
        trace: &Vec<TraceStep>,
    ) -> String {
        let mut diagram = String::new();
        
        // Header
        writeln!(&mut diagram, "\n╔══════════════════════════════════════╗").unwrap();
        writeln!(&mut diagram, "║      CONTRACT FAILURE DIAGRAM        ║").unwrap();
        writeln!(&mut diagram, "╚══════════════════════════════════════╝\n").unwrap();

        // Condition that failed
        writeln!(&mut diagram, "Failed Condition:").unwrap();
        writeln!(&mut diagram, "└─ {}", self.format_condition(condition)).unwrap();
        
        if let Some(msg) = &condition.message {
            writeln!(&mut diagram, "   └─ Message: {}", msg).unwrap();
        }

        // Evaluation tree
        writeln!(&mut diagram, "\nEvaluation Trace:").unwrap();
        for (i, step) in trace.iter().enumerate() {
            let indent = "  ".repeat(step.depth);
            let prefix = if i == trace.len() - 1 { "└─" } else { "├─" };
            let status_icon = match &step.result {
                EvalResult::Success(Value::Boolean(true)) => "✓",
                EvalResult::Success(Value::Boolean(false)) => "✗",
                EvalResult::Error(_) => "⚠",
                EvalResult::Skipped(_) => "⊘",
                _ => "•",
            };
            
            writeln!(&mut diagram, "{}{} {} {}", indent, prefix, status_icon, step.expression).unwrap();
            
            match &step.result {
                EvalResult::Success(v) => {
                    writeln!(&mut diagram, "{}   └─ Result: {}", indent, v).unwrap();
                }
                EvalResult::Error(e) => {
                    writeln!(&mut diagram, "{}   └─ Error: {}", indent, e).unwrap();
                }
                EvalResult::Skipped(reason) => {
                    writeln!(&mut diagram, "{}   └─ Skipped: {}", indent, reason).unwrap();
                }
            }
        }

        diagram
    }

    /// Format a condition for display
    fn format_condition(&self, condition: &ContractCondition) -> String {
        // Pretty-print the predicate
        self.format_predicate(&condition.predicate)
    }

    /// Format a predicate for display
    fn format_predicate(&self, predicate: &ContractPredicate) -> String {
        match predicate {
            ContractPredicate::Bool(b) => b.to_string(),
            ContractPredicate::Variable(v) => v.clone(),
            ContractPredicate::Value(v) => format!("{}", v),
            ContractPredicate::Comparison { left, op, right } => {
                format!("({} {} {})", 
                    self.format_predicate(left),
                    op,
                    self.format_predicate(right)
                )
            }
            ContractPredicate::And(preds) => {
                let parts: Vec<_> = preds.iter()
                    .map(|p| self.format_predicate(p))
                    .collect();
                format!("(and {})", parts.join(" "))
            }
            ContractPredicate::Or(preds) => {
                let parts: Vec<_> = preds.iter()
                    .map(|p| self.format_predicate(p))
                    .collect();
                format!("(or {})", parts.join(" "))
            }
            ContractPredicate::Not(pred) => {
                format!("(not {})", self.format_predicate(pred))
            }
            _ => format!("{:?}", predicate),
        }
    }
}

/// Interactive contract debugging REPL
pub struct ContractDebugRepl {
    debugger: ContractDebugger,
    /// History of violations for inspection
    violation_history: Vec<(ContractViolation, DebugInfo)>,
}

impl ContractDebugRepl {
    pub fn new() -> Self {
        Self {
            debugger: ContractDebugger::default(),
            violation_history: Vec::new(),
        }
    }

    /// Add a violation to debug
    pub fn add_violation(&mut self, violation: ContractViolation, context: &EvaluationContext) {
        let debug_info = self.debugger.debug_violation(&violation, context);
        self.violation_history.push((violation, debug_info));
    }

    /// Interactive commands
    pub fn handle_command(&self, cmd: &str) -> String {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        match parts.get(0).map(|s| *s) {
            Some("list") => self.list_violations(),
            Some("show") => {
                if let Some(idx) = parts.get(1).and_then(|s| s.parse::<usize>().ok()) {
                    self.show_violation(idx)
                } else {
                    "Usage: show <index>".to_string()
                }
            }
            Some("trace") => {
                if let Some(idx) = parts.get(1).and_then(|s| s.parse::<usize>().ok()) {
                    self.show_trace(idx)
                } else {
                    "Usage: trace <index>".to_string()
                }
            }
            Some("values") => {
                if let Some(idx) = parts.get(1).and_then(|s| s.parse::<usize>().ok()) {
                    self.show_values(idx)
                } else {
                    "Usage: values <index>".to_string()
                }
            }
            Some("suggest") => {
                if let Some(idx) = parts.get(1).and_then(|s| s.parse::<usize>().ok()) {
                    self.show_suggestions(idx)
                } else {
                    "Usage: suggest <index>".to_string()
                }
            }
            Some("help") => self.show_help(),
            _ => "Unknown command. Type 'help' for available commands.".to_string(),
        }
    }

    fn list_violations(&self) -> String {
        let mut output = String::new();
        writeln!(&mut output, "Contract Violations:").unwrap();
        for (i, (violation, _)) in self.violation_history.iter().enumerate() {
            writeln!(&mut output, "{}: {} - {}", 
                i, 
                violation.contract.name,
                violation.violation_type
            ).unwrap();
        }
        output
    }

    fn show_violation(&self, idx: usize) -> String {
        if let Some((violation, debug_info)) = self.violation_history.get(idx) {
            let mut output = String::new();
            writeln!(&mut output, "Contract: {}", violation.contract.name).unwrap();
            writeln!(&mut output, "Violation: {:?}", violation.violation_type).unwrap();
            writeln!(&mut output, "{}", debug_info.visual_diagram).unwrap();
            output
        } else {
            "Invalid violation index".to_string()
        }
    }

    fn show_trace(&self, idx: usize) -> String {
        if let Some((_, debug_info)) = self.violation_history.get(idx) {
            let mut output = String::new();
            writeln!(&mut output, "Evaluation Trace:").unwrap();
            for step in &debug_info.evaluation_trace {
                let indent = "  ".repeat(step.depth);
                writeln!(&mut output, "{}{} -> {:?}", indent, step.expression, step.result).unwrap();
            }
            output
        } else {
            "Invalid violation index".to_string()
        }
    }

    fn show_values(&self, idx: usize) -> String {
        if let Some((_, debug_info)) = self.violation_history.get(idx) {
            let mut output = String::new();
            writeln!(&mut output, "Variable Values at Failure:").unwrap();
            for (name, value) in &debug_info.variable_values {
                writeln!(&mut output, "  {} = {}", name, value).unwrap();
            }
            output
        } else {
            "Invalid violation index".to_string()
        }
    }

    fn show_suggestions(&self, idx: usize) -> String {
        if let Some((_, debug_info)) = self.violation_history.get(idx) {
            let mut output = String::new();
            writeln!(&mut output, "Suggestions:").unwrap();
            for suggestion in &debug_info.suggestions {
                writeln!(&mut output, "  • {}", suggestion).unwrap();
            }
            output
        } else {
            "Invalid violation index".to_string()
        }
    }

    fn show_help(&self) -> String {
        r#"Contract Debug REPL Commands:
  list           - List all contract violations
  show <idx>     - Show detailed violation with diagram
  trace <idx>    - Show evaluation trace
  values <idx>   - Show variable values at failure
  suggest <idx>  - Show fix suggestions
  help           - Show this help message"#.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contract_debugger() {
        let debugger = ContractDebugger::default();
        let context = EvaluationContext::new();
        
        // Create a sample violation
        let contract = Contract {
            name: "test_contract".to_string(),
            preconditions: vec![
                ContractCondition {
                    predicate: ContractPredicate::Comparison {
                        left: Box::new(ContractPredicate::Variable("x".to_string())),
                        op: ">".to_string(),
                        right: Box::new(ContractPredicate::Value(Value::Integer(0))),
                    },
                    message: Some("x must be positive".to_string()),
                }
            ],
            ..Default::default()
        };
        
        let violation = ContractViolation {
            contract,
            violation_type: ViolationType::PreconditionFailed(0),
            location: None,
            context: "test".to_string(),
        };
        
        let debug_info = debugger.debug_violation(&violation, &context);
        
        assert!(!debug_info.visual_diagram.is_empty());
        assert!(!debug_info.suggestions.is_empty());
    }
}