//! Fast diagnostic computation for FluentAi

use fluentai_core::ast::{Graph, Node, NodeId};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Compute diagnostics for an AST
/// 
/// This function runs in <1ms for typical files to ensure
/// real-time error feedback in the IDE.
pub fn compute_diagnostics(ast: &Graph, _content: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    
    // Check for common issues
    if let Some(root_id) = ast.root_id {
        check_node(&ast, root_id, &mut diagnostics);
    }
    
    // TODO: Add more sophisticated checks:
    // - Type checking
    // - Effect analysis
    // - Contract verification
    // - Unused variables
    // - Unreachable code
    
    diagnostics
}

fn check_node(graph: &Graph, node_id: NodeId, diagnostics: &mut Vec<Diagnostic>) {
    if let Some(node) = graph.get_node(node_id) {
        match node {
            Node::Variable { name } => {
                // Check for undefined variables
                if is_undefined_variable(name) {
                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: Position { line: 0, character: 0 },
                            end: Position { line: 0, character: 0 },
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("Undefined variable: {}", name),
                        ..Default::default()
                    });
                }
            }
            Node::Application { function, args } => {
                check_node(graph, *function, diagnostics);
                for arg in args {
                    check_node(graph, *arg, diagnostics);
                }
            }
            Node::Lambda { params, body } => {
                // Check for duplicate parameters
                let mut seen = std::collections::HashSet::new();
                for param in params {
                    if !seen.insert(param) {
                        diagnostics.push(Diagnostic {
                            range: Range {
                                start: Position { line: 0, character: 0 },
                                end: Position { line: 0, character: 0 },
                            },
                            severity: Some(DiagnosticSeverity::ERROR),
                            message: format!("Duplicate parameter: {}", param),
                            ..Default::default()
                        });
                    }
                }
                check_node(graph, *body, diagnostics);
            }
            Node::Let { bindings, body } => {
                // Check bindings
                for (_name, value) in bindings {
                    check_node(graph, *value, diagnostics);
                }
                check_node(graph, *body, diagnostics);
            }
            Node::If { condition, then_branch, else_branch } => {
                check_node(graph, *condition, diagnostics);
                check_node(graph, *then_branch, diagnostics);
                check_node(graph, *else_branch, diagnostics);
            }
            Node::List(items) => {
                for item in items {
                    check_node(graph, *item, diagnostics);
                }
            }
            _ => {}
        }
    }
}

fn is_undefined_variable(name: &str) -> bool {
    // Built-in functions and special forms are always defined
    !matches!(name, 
        "+" | "-" | "*" | "/" | "%" |
        "=" | "==" | "!=" | "<>" | "<" | "<=" | ">" | ">=" |
        "and" | "or" | "not" |
        "cons" | "list-len" | "list-empty?" |
        "str-len" | "str-concat" | "str-upper" | "str-lower" |
        "lambda" | "let" | "if"
    ) && !name.starts_with("__builtin__")
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_parser::parse;
    
    #[test]
    fn test_no_diagnostics_for_valid_code() {
        let code = "(+ 1 2)";
        let ast = parse(code).unwrap();
        let diagnostics = compute_diagnostics(&ast, code);
        assert!(diagnostics.is_empty());
    }
    
    #[test]
    fn test_undefined_variable_diagnostic() {
        let code = "undefined_var";
        let ast = parse(code).unwrap();
        let diagnostics = compute_diagnostics(&ast, code);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("Undefined variable"));
    }
}