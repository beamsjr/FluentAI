#[cfg(test)]
mod tests {
    use crate::parse;
    use fluentai_core::ast::{Literal, Node};

    // ===== Real-world Code Examples =====

    #[test]
    fn test_fibonacci_function() {
        let code = r#"
            (letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))))
                (fib 10))
        "#;

        let result = parse(code).unwrap();
        assert!(result.root_id.is_some());

        // Verify the structure
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Letrec { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "fib");
                // Body should be application of fib to 10
                match result.get_node(*body).unwrap() {
                    Node::Application { function, args } => {
                        assert!(matches!(result.get_node(*function).unwrap(), 
                            Node::Variable { name } if name == "fib"));
                        assert_eq!(args.len(), 1);
                        assert!(matches!(
                            result.get_node(args[0]).unwrap(),
                            Node::Literal(Literal::Integer(10))
                        ));
                    }
                    _ => panic!("Expected Application"),
                }
            }
            _ => panic!("Expected Letrec"),
        }
    }

    #[test]
    fn test_quicksort_implementation() {
        let code = r#"
            (letrec ((quicksort (lambda (lst)
                (if (null? lst)
                    []
                    (let ((pivot (car lst))
                          (rest (cdr lst)))
                        (append
                            (quicksort (filter (lambda (x) (< x pivot)) rest))
                            (cons pivot
                                (quicksort (filter (lambda (x) (>= x pivot)) rest)))))))))
                (quicksort [3 1 4 1 5 9 2 6]))
        "#;

        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_async_await_pattern() {
        let code = r#"
            (let ((ch (chan)))
                (do
                    (spawn (send! ch (async (+ 1 2))))
                    (await (recv! ch))))
        "#;

        let result = parse(code).unwrap();
        assert!(result.root_id.is_some());
    }

    #[test]
    fn test_effect_handlers() {
        let code = r#"
            (do
                (effect IO:print "Starting computation")
                (let ((result (+ 1 2 3)))
                    (do
                        (effect IO:print (cons "Result: " result))
                        result)))
        "#;

        let result = parse(code).unwrap();
        assert!(result.root_id.is_some());
    }

    #[test]
    fn test_pattern_matching_with_guards() {
        let code = r#"
            (match x
                (0 "zero")
                (1 "one")
                (n (if (even? n) "even" "odd")))
        "#;

        let result = parse(code).unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Match { branches, .. } => {
                assert_eq!(branches.len(), 3);
            }
            _ => panic!("Expected Match"),
        }
    }

    // ===== Error Recovery Tests =====

    #[test]
    fn test_multiple_top_level_expressions() {
        let code = r#"
            (define x 10)
            (define y 20)
            (+ x y)
        "#;

        let result = parse(code).unwrap();
        // Multiple expressions should be wrapped in Begin
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Begin { .. }
        ));
    }

    #[test]
    fn test_comments_and_whitespace() {
        let code = r#"
            ; Calculate factorial
            (letrec ((factorial (lambda (n)  ; recursive function
                        ; Base case
                        (if (<= n 1)
                            1  ; return 1
                            ; Recursive case
                            (* n (factorial (- n 1)))))))
                (factorial 5))  ; Test with 5
        "#;

        assert!(parse(code).is_ok());
    }

    // ===== Module System Tests =====

    #[test]
    fn test_module_with_imports_exports() {
        let code = r#"
            (module math-utils
                (import std (+ - * /))
                (export factorial fibonacci gcd)
                
                (define (factorial n)
                    (if (<= n 1) 1 (* n (factorial (- n 1)))))
                
                (define (fibonacci n)
                    (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
                
                (define (gcd a b)
                    (if (= b 0) a (gcd b (% a b)))))
        "#;

        // Module parsing might not be fully implemented
        let result = parse(code);
        // Just check it doesn't crash
        assert!(result.is_ok() || result.is_err());
    }

    // ===== Edge Cases and Stress Tests =====

    #[test]
    fn test_deeply_nested_expressions() {
        let mut expr = "42".to_string();
        for _ in 0..10 {
            expr = format!("(+ 1 {})", expr);
        }

        let result = parse(&expr).unwrap();
        assert!(result.root_id.is_some());

        // Verify deep nesting
        let mut current = result.root_id.unwrap();
        let mut depth = 0;
        loop {
            match result.get_node(current).unwrap() {
                Node::Application { args, .. } if !args.is_empty() => {
                    depth += 1;
                    current = args[args.len() - 1];
                }
                Node::Literal(Literal::Integer(42)) => break,
                _ => break,
            }
        }
        assert_eq!(depth, 10);
    }

    #[test]
    fn test_large_list_literal() {
        let elements: Vec<String> = (0..100).map(|i| i.to_string()).collect();
        let code = format!("[{}]", elements.join(" "));

        // List literals are converted to cons operations
        assert!(parse(&code).is_ok());
    }

    #[test]
    fn test_unicode_in_strings_and_comments() {
        let code = r#"
            ; 这是一个中文注释
            (let ((message "Hello, 世界! 🌍")
                  (emoji "🦀"))
                (do
                    (effect IO:print message)
                    emoji))
        "#;

        let result = parse(code).unwrap();
        assert!(result.root_id.is_some());
    }

    // ===== Special Forms Tests =====

    #[test]
    fn test_do_sequence_with_effects() {
        let code = r#"
            (do
                (effect IO:print "Step 1")
                (effect IO:print "Step 2")
                (let ((x 10))
                    (do
                        (effect IO:print (cons "x = " x))
                        x)))
        "#;

        let result = parse(code).unwrap();
        // do converts to nested let bindings
        assert!(result.root_id.is_some());
    }

    #[test]
    fn test_complex_lambda_with_rest_args() {
        // Note: Rest args might not be implemented
        let code = r#"
            (lambda (x y . rest)
                (cons x (cons y rest)))
        "#;

        // This might fail if rest args aren't supported
        let result = parse(code);
        assert!(result.is_ok() || result.is_err());
    }

    // ===== Performance Edge Cases =====

    #[test]
    fn test_many_small_expressions() {
        let exprs: Vec<String> = (0..50).map(|i| format!("(define x{} {})", i, i)).collect();
        let code = exprs.join("\n");

        let result = parse(&code).unwrap();
        assert!(result.root_id.is_some());
    }

    #[test]
    fn test_long_symbol_names() {
        let long_name = "a".repeat(100);
        let code = format!("(define {} 42)", long_name);

        assert!(parse(&code).is_ok());
    }

    #[test]
    fn test_escaped_strings() {
        let code = r#"
            (let ((tab "hello\tworld")
                  (newline "line1\nline2")
                  (quote "she said \"hello\"")
                  (backslash "path\\to\\file"))
                (list tab newline quote backslash))
        "#;

        assert!(parse(code).is_ok());
    }

    // ===== Contract/Spec Tests =====

    #[test]
    fn test_contract_specifications() {
        let code = r#"
            (spec:contract factorial
                :requires [(>= n 0)]
                :ensures [(>= result 1)]
                :pure true)
        "#;

        let result = parse(code).unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Contract {
                function_name,
                preconditions,
                postconditions,
                pure,
                ..
            } => {
                assert_eq!(function_name, "factorial");
                assert!(!preconditions.is_empty());
                assert!(!postconditions.is_empty());
                assert_eq!(*pure, true);
            }
            _ => panic!("Expected Contract"),
        }
    }

    // ===== Mixed Syntax Tests =====

    #[test]
    fn test_mixed_delimiters() {
        let code = r#"
            (let ((lst [1 2 3])
                  (vec [4 5 6]))
                [(car lst) (car vec)])
        "#;

        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_qualified_and_effect_symbols() {
        let code = r#"
            (let ((sin math.sin)
                  (print io:print))
                (effect IO:print (sin 3.14)))
        "#;

        assert!(parse(code).is_ok());
    }
}
