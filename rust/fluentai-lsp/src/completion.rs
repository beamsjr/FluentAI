//! Auto-completion support for FluentAi

use ropey::Rope;
use fluentai_core::ast::Graph;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position};

/// Compute completion items at a given position
/// 
/// Performance target: <5ms for generating suggestions
pub fn compute_completions(
    rope: &Rope,
    _ast: &Option<Graph>,
    position: Position,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();
    
    // Get the current line and character context
    let line = position.line as usize;
    let character = position.character as usize;
    
    if line >= rope.len_lines() {
        return completions;
    }
    
    let line_text = rope.line(line).to_string();
    let prefix = &line_text[..character.min(line_text.len())];
    
    // Determine context - are we after an open paren?
    let in_function_position = prefix.rfind('(').map_or(false, |paren_pos| {
        !prefix[paren_pos..].contains(')')
    });
    
    if in_function_position {
        // Suggest functions and special forms
        add_builtin_functions(&mut completions);
        add_special_forms(&mut completions);
    } else {
        // Suggest everything
        add_builtin_functions(&mut completions);
        add_special_forms(&mut completions);
        add_literals(&mut completions);
    }
    
    // TODO: Add context-aware completions:
    // - Local variables from let bindings
    // - Function parameters
    // - Global definitions
    // - Type-specific methods
    
    completions
}

fn add_builtin_functions(completions: &mut Vec<CompletionItem>) {
    let functions = vec![
        ("+", "Addition", "(+ x y ...)"),
        ("-", "Subtraction", "(- x y ...)"),
        ("*", "Multiplication", "(* x y ...)"),
        ("/", "Division", "(/ x y ...)"),
        ("%", "Modulo", "(% x y)"),
        ("=", "Equality", "(= x y)"),
        ("<", "Less than", "(< x y)"),
        (">", "Greater than", "(> x y)"),
        ("<=", "Less than or equal", "(<= x y)"),
        (">=", "Greater than or equal", "(>= x y)"),
        ("and", "Logical AND", "(and x y ...)"),
        ("or", "Logical OR", "(or x y ...)"),
        ("not", "Logical NOT", "(not x)"),
        ("cons", "Construct list", "(cons head tail)"),
        ("list-len", "List length", "(list-len list)"),
        ("list-empty?", "Check if list is empty", "(list-empty? list)"),
        ("str-concat", "Concatenate strings", "(str-concat str1 str2 ...)"),
        ("str-len", "String length", "(str-len string)"),
        ("str-upper", "Convert to uppercase", "(str-upper string)"),
        ("str-lower", "Convert to lowercase", "(str-lower string)"),
    ];
    
    for (name, doc, detail) in functions {
        completions.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(detail.to_string()),
            documentation: Some(tower_lsp::lsp_types::Documentation::String(doc.to_string())),
            ..Default::default()
        });
    }
}

fn add_special_forms(completions: &mut Vec<CompletionItem>) {
    let special_forms = vec![
        ("lambda", "Define anonymous function", "(lambda (params...) body)"),
        ("let", "Local bindings", "(let ((name value)...) body)"),
        ("if", "Conditional expression", "(if condition then-expr else-expr)"),
        ("define", "Define global binding", "(define name value)"),
        ("quote", "Quote expression", "(quote expr)"),
    ];
    
    for (name, doc, detail) in special_forms {
        completions.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            documentation: Some(tower_lsp::lsp_types::Documentation::String(doc.to_string())),
            ..Default::default()
        });
    }
}

fn add_literals(completions: &mut Vec<CompletionItem>) {
    completions.extend([
        CompletionItem {
            label: "true".to_string(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some("Boolean true".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "false".to_string(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some("Boolean false".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "nil".to_string(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some("Null value".to_string()),
            ..Default::default()
        },
    ]);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_completions_after_paren() {
        let rope = Rope::from_str("(");
        let completions = compute_completions(&rope, &None, Position { line: 0, character: 1 });
        
        // Should include functions
        assert!(completions.iter().any(|c| c.label == "+"));
        assert!(completions.iter().any(|c| c.label == "lambda"));
    }
    
    #[test]
    fn test_completions_at_start() {
        let rope = Rope::from_str("");
        let completions = compute_completions(&rope, &None, Position { line: 0, character: 0 });
        
        // Should include everything
        assert!(completions.iter().any(|c| c.label == "+"));
        assert!(completions.iter().any(|c| c.label == "true"));
    }
}