//! Hover information provider for FluentAi

use ropey::Rope;
use fluentai_core::ast::Graph;
use tower_lsp::lsp_types::{Hover, HoverContents, LanguageString, MarkedString, Position};

/// Compute hover information at a given position
/// 
/// Performance target: <2ms for generating hover info
pub fn compute_hover(
    rope: &Rope,
    _ast: &Option<Graph>,
    position: Position,
) -> Option<Hover> {
    let line = position.line as usize;
    let character = position.character as usize;
    
    if line >= rope.len_lines() {
        return None;
    }
    
    // Get the word at the current position
    let line_text = rope.line(line).to_string();
    let word = get_word_at_position(&line_text, character)?;
    
    // Generate hover information based on the word
    let hover_text = match word.as_str() {
        // Arithmetic operators
        "+" => "**Addition**\n\n`(+ x y ...)`\n\nAdds two or more numbers together.",
        "-" => "**Subtraction**\n\n`(- x y ...)`\n\nSubtracts subsequent numbers from the first.",
        "*" => "**Multiplication**\n\n`(* x y ...)`\n\nMultiplies two or more numbers together.",
        "/" => "**Division**\n\n`(/ x y ...)`\n\nDivides the first number by subsequent numbers.",
        "%" => "**Modulo**\n\n`(% x y)`\n\nReturns the remainder of x divided by y.",
        
        // Comparison operators
        "=" | "==" => "**Equality**\n\n`(= x y)`\n\nReturns true if x equals y.",
        "<" => "**Less Than**\n\n`(< x y)`\n\nReturns true if x is less than y.",
        ">" => "**Greater Than**\n\n`(> x y)`\n\nReturns true if x is greater than y.",
        "<=" => "**Less Than or Equal**\n\n`(<= x y)`\n\nReturns true if x is less than or equal to y.",
        ">=" => "**Greater Than or Equal**\n\n`(>= x y)`\n\nReturns true if x is greater than or equal to y.",
        
        // Boolean operators
        "and" => "**Logical AND**\n\n`(and x y ...)`\n\nReturns true if all arguments are truthy.",
        "or" => "**Logical OR**\n\n`(or x y ...)`\n\nReturns true if any argument is truthy.",
        "not" => "**Logical NOT**\n\n`(not x)`\n\nReturns the logical negation of x.",
        
        // List operations
        "cons" => "**Construct List**\n\n`(cons head tail)`\n\nConstructs a new list with head as the first element and tail as the rest.",
        "list-len" => "**List Length**\n\n`(list-len list)`\n\nReturns the number of elements in the list.",
        "list-empty?" => "**Empty List Check**\n\n`(list-empty? list)`\n\nReturns true if the list is empty.",
        
        // String operations
        "str-concat" => "**String Concatenation**\n\n`(str-concat str1 str2 ...)`\n\nConcatenates multiple strings together.",
        "str-len" => "**String Length**\n\n`(str-len string)`\n\nReturns the length of the string.",
        "str-upper" => "**Uppercase**\n\n`(str-upper string)`\n\nConverts the string to uppercase.",
        "str-lower" => "**Lowercase**\n\n`(str-lower string)`\n\nConverts the string to lowercase.",
        
        // Special forms
        "lambda" => "**Lambda Expression**\n\n`(lambda (params...) body)`\n\nDefines an anonymous function with the given parameters and body.",
        "let" => "**Let Binding**\n\n`(let ((name value)...) body)`\n\nCreates local bindings that are visible in the body.",
        "if" => "**Conditional**\n\n`(if condition then-expr else-expr)`\n\nEvaluates condition and returns then-expr if true, else-expr otherwise.",
        
        // Constants
        "true" => "**Boolean True**\n\nThe boolean value true.",
        "false" => "**Boolean False**\n\nThe boolean value false.",
        "nil" => "**Nil**\n\nThe null/empty value.",
        
        _ => return None,
    };
    
    Some(Hover {
        contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
            language: "markdown".to_string(),
            value: hover_text.to_string(),
        })),
        range: None,
    })
}

/// Extract the word at a given position in a line
fn get_word_at_position(line: &str, position: usize) -> Option<String> {
    // Find word boundaries
    let chars: Vec<char> = line.chars().collect();
    
    if position > chars.len() {
        return None;
    }
    
    // Find start of word
    let mut start = position;
    while start > 0 && is_word_char(chars.get(start - 1)?) {
        start -= 1;
    }
    
    // Find end of word
    let mut end = position;
    while end < chars.len() && is_word_char(chars.get(end)?) {
        end += 1;
    }
    
    if start == end {
        return None;
    }
    
    Some(chars[start..end].iter().collect())
}

fn is_word_char(c: &char) -> bool {
    c.is_alphanumeric() || *c == '-' || *c == '_' || *c == '?' || *c == '!' || 
    *c == '+' || *c == '*' || *c == '/' || *c == '%' || *c == '=' || 
    *c == '<' || *c == '>'
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_get_word_at_position() {
        assert_eq!(get_word_at_position("(+ 1 2)", 1), Some("+".to_string()));
        assert_eq!(get_word_at_position("lambda", 3), Some("lambda".to_string()));
        assert_eq!(get_word_at_position("list-len", 5), Some("list-len".to_string()));
    }
    
    #[test]
    fn test_hover_for_operator() {
        let rope = Rope::from_str("(+ 1 2)");
        let hover = compute_hover(&rope, &None, Position { line: 0, character: 1 });
        assert!(hover.is_some());
    }
}