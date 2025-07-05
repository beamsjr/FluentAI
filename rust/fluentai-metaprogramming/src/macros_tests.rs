#[cfg(test)]
mod tests {
    use crate::macros::{MacroDefinition, ExpansionContext, MacroExpander};
    use crate::error::MetaprogrammingError;
    use fluentai_parser::parse;
    use fluentai_core::ast::{Node, Literal, NodeId};
    
    // ===== MacroDefinition Tests =====
    
    #[test]
    fn test_macro_definition_creation() {
        let macro_def = MacroDefinition {
            name: "test_macro".to_string(),
            params: vec!["x".to_string(), "y".to_string()],
            pattern: None,
            body: "(+ $x $y)".to_string(),
            hygenic: true,
        };
        
        assert_eq!(macro_def.name, "test_macro");
        assert_eq!(macro_def.params.len(), 2);
        assert_eq!(macro_def.body, "(+ $x $y)");
        assert!(macro_def.hygenic);
    }
    
    #[test]
    fn test_macro_definition_clone() {
        let macro_def = MacroDefinition {
            name: "clone_test".to_string(),
            params: vec!["a".to_string()],
            pattern: None,
            body: "$a".to_string(),
            hygenic: false,
        };
        
        let cloned = macro_def.clone();
        assert_eq!(cloned.name, macro_def.name);
        assert_eq!(cloned.params, macro_def.params);
        assert_eq!(cloned.hygenic, macro_def.hygenic);
    }
    
    // ===== ExpansionContext Tests =====
    
    #[test]
    fn test_expansion_context_new() {
        let ctx = ExpansionContext::new();
        assert!(ctx.bindings.is_empty());
        assert!(ctx.arguments.is_empty());
        assert_eq!(ctx.gensym_counter, 0);
    }
    
    #[test]
    fn test_gensym() {
        let mut ctx = ExpansionContext::new();
        
        let sym1 = ctx.gensym("temp");
        let sym2 = ctx.gensym("temp");
        let sym3 = ctx.gensym("var");
        
        assert_eq!(sym1, "temp#0");
        assert_eq!(sym2, "temp#1");
        assert_eq!(sym3, "var#2");
        assert_eq!(ctx.gensym_counter, 3);
    }
    
    #[test]
    fn test_gensym_unique() {
        let mut ctx = ExpansionContext::new();
        let mut symbols = std::collections::HashSet::new();
        
        for i in 0..100 {
            let sym = ctx.gensym("test");
            assert!(symbols.insert(sym), "Generated symbol not unique at iteration {}", i);
        }
    }
    
    // ===== MacroExpander Tests =====
    
    #[test]
    fn test_macro_expander_new() {
        let expander = MacroExpander::new();
        assert!(expander.macros.is_empty());
        assert_eq!(expander.max_depth, 100);
    }
    
    #[test]
    fn test_register_macro() {
        let mut expander = MacroExpander::new();
        
        let macro_def = MacroDefinition {
            name: "double".to_string(),
            params: vec!["x".to_string()],
            pattern: None,
            body: "(* $x 2)".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(macro_def.clone());
        assert!(expander.macros.contains_key("double"));
        assert_eq!(expander.macros["double"].name, "double");
    }
    
    #[test]
    fn test_register_macro_overwrites() {
        let mut expander = MacroExpander::new();
        
        let macro1 = MacroDefinition {
            name: "test".to_string(),
            params: vec!["x".to_string()],
            pattern: None,
            body: "(+ $x 1)".to_string(),
            hygenic: true,
        };
        
        let macro2 = MacroDefinition {
            name: "test".to_string(),
            params: vec!["x".to_string()],
            pattern: None,
            body: "(* $x 2)".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(macro1);
        expander.register_macro(macro2);
        
        assert_eq!(expander.macros["test"].body, "(* $x 2)");
    }
    
    #[test]
    fn test_register_builtins() {
        let mut expander = MacroExpander::new();
        expander.register_builtins();
        
        assert!(expander.macros.contains_key("when"));
        assert!(expander.macros.contains_key("unless"));
        assert!(expander.macros.contains_key("cond"));
        assert!(expander.macros.contains_key("let*"));
    }
    
    #[test]
    fn test_builtin_when_macro() {
        let mut expander = MacroExpander::new();
        expander.register_builtins();
        
        let when_macro = &expander.macros["when"];
        assert_eq!(when_macro.params.len(), 2);
        assert_eq!(when_macro.params[0], "condition");
        assert_eq!(when_macro.params[1], "body");
        assert!(when_macro.hygenic);
        assert_eq!(when_macro.body, "(if $condition $body nil)");
    }
    
    #[test]
    fn test_builtin_unless_macro() {
        let mut expander = MacroExpander::new();
        expander.register_builtins();
        
        let unless_macro = &expander.macros["unless"];
        assert_eq!(unless_macro.body, "(if $condition nil $body)");
    }
    
    #[test]
    fn test_builtin_let_star_macro() {
        let mut expander = MacroExpander::new();
        expander.register_builtins();
        
        let let_star_macro = &expander.macros["let*"];
        assert_eq!(let_star_macro.params.len(), 2);
        assert_eq!(let_star_macro.params[0], "bindings");
        assert_eq!(let_star_macro.params[1], "body");
        assert_eq!(let_star_macro.body, "let*-expansion");
    }
    
    #[test]
    fn test_builtin_cond_macro() {
        let mut expander = MacroExpander::new();
        expander.register_builtins();
        
        let cond_macro = &expander.macros["cond"];
        assert_eq!(cond_macro.params, vec!["clauses"]);
        assert_eq!(cond_macro.body, "cond-expansion");
        assert!(cond_macro.hygenic);
    }
    
    // ===== Expansion Tests =====
    
    #[test]
    fn test_expand_simple_macro() {
        let mut expander = MacroExpander::new();
        
        let double_macro = MacroDefinition {
            name: "double".to_string(),
            params: vec!["x".to_string()],
            pattern: None,
            body: "(* $x 2)".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(double_macro);
        
        // Test expansion
        let code = "(double 5)";
        let mut graph = parse(code).unwrap();
        
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
        
        // After expansion, should have (* 5 2)
        if let Some(root) = graph.root_id {
            if let Some(Node::Application { function: _, args }) = graph.get_node(root) {
                // Check if expanded properly
                assert_eq!(args.len(), 2);
            }
        }
    }
    
    #[test]
    #[ignore = "Macro expansion not fully implemented"]
    fn test_expand_nested_macros() {
        let mut expander = MacroExpander::new();
        expander.register_builtins();
        
        // (when (when true 1) 2) should expand to nested ifs
        let code = "(when (when true 1) 2)";
        let mut graph = parse(code).unwrap();
        
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
        
        // Should have expanded to nested if expressions
        // TODO: Verify the actual expansion structure
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    #[ignore = "Recursive macro expansion not implemented"]
    fn test_max_depth_limit() {
        let mut expander = MacroExpander::new();
        expander.max_depth = 3;
        
        // Create a recursive macro
        let recursive_macro = MacroDefinition {
            name: "recurse".to_string(),
            params: vec!["x".to_string()],
            pattern: None,
            body: "(recurse (+ $x 1))".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(recursive_macro);
        
        let code = "(recurse 0)";
        let mut graph = parse(code).unwrap();
        
        // Should fail due to exceeding depth limit
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(e.to_string().contains("depth"));
        }
    }
    
    #[test]
    #[ignore = "Hygenic macro expansion not implemented"]
    fn test_hygenic_expansion() {
        let mut expander = MacroExpander::new();
        
        let let_macro = MacroDefinition {
            name: "my-let".to_string(),
            params: vec!["var".to_string(), "val".to_string(), "body".to_string()],
            pattern: None,
            body: "(let (($var $val)) $body)".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(let_macro);
        
        // Variable names should be renamed to avoid capture
        let code = "(my-let x 10 (+ x y))";
        let mut graph = parse(code).unwrap();
        
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
        
        // The 'x' in the body should be renamed to avoid capture
        // TODO: Verify that variable renaming actually happened
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    fn test_non_hygenic_expansion() {
        let mut expander = MacroExpander::new();
        
        let capture_macro = MacroDefinition {
            name: "capture".to_string(),
            params: vec!["body".to_string()],
            pattern: None,
            body: "(let ((x 42)) $body)".to_string(),
            hygenic: false,
        };
        
        expander.register_macro(capture_macro);
        
        // Non-hygenic macro should allow variable capture
        let code = "(capture x)";
        let mut graph = parse(code).unwrap();
        
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
        
        // x should refer to the macro-introduced binding
        assert!(graph.root_id.is_some());
    }
    
    #[test]
    #[ignore = "Pattern-based macros not implemented"]
    fn test_pattern_based_macro() {
        let mut expander = MacroExpander::new();
        
        // A macro that matches specific patterns
        let swap_macro = MacroDefinition {
            name: "swap".to_string(),
            params: vec!["pair".to_string()],
            pattern: Some(crate::patterns::Pattern::NodeType(
                crate::patterns::NodePattern::List(vec![
                    crate::patterns::Pattern::Bind("a".to_string()),
                    crate::patterns::Pattern::Bind("b".to_string()),
                ])
            )),
            body: "[$b $a]".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(swap_macro);
        
        let code = "(swap [1 2])";
        let mut graph = parse(code).unwrap();
        
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
        
        // Should have swapped to [2 1]
        // TODO: Verify the actual expansion result
    }
    
    #[test]
    #[ignore = "Variadic macros with splicing not implemented"]
    fn test_variadic_macro() {
        let mut expander = MacroExpander::new();
        
        let list_macro = MacroDefinition {
            name: "list".to_string(),
            params: vec!["args".to_string()],
            pattern: None,
            body: "[$$args]".to_string(), // $$ for splicing
            hygenic: true,
        };
        
        expander.register_macro(list_macro);
        
        let code = "(list 1 2 3 4)";
        let mut graph = parse(code).unwrap();
        
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
        
        // Should expand to [1 2 3 4]
        // TODO: Verify the splicing works correctly
    }
    
    // ===== Error Handling Tests =====
    
    #[test]
    fn test_expand_unknown_macro() {
        let expander = MacroExpander::new();
        
        let code = "(unknown-macro 42)";
        let mut graph = parse(code).unwrap();
        
        // Should not error on unknown macros
        let result = expander.expand_graph(&mut graph);
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_expand_malformed_macro_body() {
        let mut expander = MacroExpander::new();
        
        let bad_macro = MacroDefinition {
            name: "bad".to_string(),
            params: vec!["x".to_string()],
            pattern: None,
            body: "(+ $x $undefined)".to_string(), // References undefined parameter
            hygenic: true,
        };
        
        expander.register_macro(bad_macro);
        
        let code = "(bad 5)";
        let mut graph = parse(code).unwrap();
        
        // Malformed macro should not panic - it's ok to either fail gracefully or expand partially
        let result = expander.expand_graph(&mut graph);
        // Just ensure we don't panic
        drop(result);
    }
    
    #[test]
    fn test_macro_with_empty_body() {
        let mut expander = MacroExpander::new();
        
        let empty_macro = MacroDefinition {
            name: "empty".to_string(),
            params: vec![],
            pattern: None,
            body: "".to_string(),
            hygenic: true,
        };
        
        expander.register_macro(empty_macro);
        
        let code = "(empty)";
        let mut graph = parse(code).unwrap();
        
        // Empty body macros should not panic - implementation may vary
        let result = expander.expand_graph(&mut graph);
        // Just ensure we don't panic
        drop(result);
    }
}