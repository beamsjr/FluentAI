//! Compile-time enforcement for built-in function documentation

use crate::documentation::Documentation;

/// Macro to define a built-in function with enforced documentation
#[macro_export]
macro_rules! define_builtin {
    (
        name: $name:expr,
        signature: $signature:expr,
        description: $description:expr,
        examples: [$($example:expr),* $(,)?],
        module: $module:expr
    ) => {
        {
            // Compile-time validation through const assertions
            const _: () = {
                // Ensure name is not empty
                assert!($name.len() > 0, "Built-in name cannot be empty");
                // Ensure signature is not empty  
                assert!($signature.len() > 0, "Built-in signature cannot be empty");
                // Ensure description is not empty
                assert!($description.len() > 0, "Built-in description cannot be empty");
                // Ensure at least one example is provided
                const EXAMPLES: &[&str] = &[$($example),*];
                assert!(EXAMPLES.len() > 0, "Built-in must have at least one example");
            };
            
            $crate::documentation::BuiltinDoc::new($name, $signature, $description, &[$($example),*], $module)
        }
    };
}

/// Registry of all built-in functions with compile-time validation
pub struct BuiltinRegistry;

impl BuiltinRegistry {
    /// Get all built-in functions with validated documentation
    pub fn all_builtins() -> Vec<crate::documentation::BuiltinDoc> {
        vec![
            // Core list operations
            define_builtin!(
                name: "cons",
                signature: "(cons <item> <list>)",
                description: "Constructs a new list by prepending an item to an existing list.",
                examples: ["(cons 1 [2 3])", "(cons \"a\" [])", "(cons x xs)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "car",
                signature: "(car <list>)",
                description: "Returns the first element of a list. Error if list is empty.",
                examples: ["(car [1 2 3])", "(car [\"hello\"])", "(car xs)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "cdr",
                signature: "(cdr <list>)",
                description: "Returns the list without its first element. Error if list is empty.",
                examples: ["(cdr [1 2 3])", "(cdr [\"a\" \"b\"])", "(cdr xs)"],
                module: "core"
            ),
            
            // Basic arithmetic operations
            define_builtin!(
                name: "+",
                signature: "(+ <num1> <num2> ...)",
                description: "Adds two or more numbers together.",
                examples: ["(+ 1 2)", "(+ 1 2 3 4)", "(+ 5.5 2.5)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "-",
                signature: "(- <num1> <num2> ...) | (- <num>)",
                description: "Subtracts numbers. With one argument, negates it.",
                examples: ["(- 5 3)", "(- 10 3 2)", "(- 5)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "*",
                signature: "(* <num1> <num2> ...)",
                description: "Multiplies two or more numbers together.",
                examples: ["(* 2 3)", "(* 2 3 4)", "(* 5.5 2)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "/",
                signature: "(/ <num1> <num2> ...)",
                description: "Divides numbers. Integer division returns a float.",
                examples: ["(/ 10 2)", "(/ 20 4 2)", "(/ 5 2)"],
                module: "core"
            ),
            
            // String operations
            define_builtin!(
                name: "string-length",
                signature: "(string-length <string>)",
                description: "Returns the length of a string.",
                examples: ["(string-length \"hello\")", "(string-length \"\")"],
                module: "strings"
            ),
            
            define_builtin!(
                name: "string-append",
                signature: "(string-append <string1> <string2> ...)",
                description: "Concatenates multiple strings.",
                examples: ["(string-append \"Hello, \" \"World!\")", "(string-append a b c)"],
                module: "strings"
            ),
            
            // I/O operations (via effects)
            define_builtin!(
                name: "print",
                signature: "(print <value> ...)",
                description: "Prints values to standard output with a newline.",
                examples: ["(print \"Hello, World!\")", "(print x y z)"],
                module: "io"
            ),
            
            define_builtin!(
                name: "println",
                signature: "(println <value> ...)",
                description: "Prints values to standard output with a newline.",
                examples: ["(println \"Hello, World!\")", "(println x y z)"],
                module: "io"
            ),
            
            // Additional arithmetic
            define_builtin!(
                name: "%",
                signature: "(% <num1> <num2>)",
                description: "Returns the remainder of dividing the first number by the second.",
                examples: ["(% 10 3)", "(% 7 2)"],
                module: "core"
            ),
            
            // Additional comparison operators are defined in register_operators() to avoid duplicates
            
            // Boolean operations are defined in register_operators() to avoid duplicates
            
            // List operations
            define_builtin!(
                name: "list",
                signature: "(list <item1> <item2> ...)",
                description: "Creates a list from the given arguments.",
                examples: ["(list 1 2 3)", "(list \"a\" \"b\" \"c\")", "(list)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "length",
                signature: "(length <list>)",
                description: "Returns the length of a list.",
                examples: ["(length [1 2 3])", "(length [])", "(length xs)"],
                module: "core"
            ),
            
            define_builtin!(
                name: "empty?",
                signature: "(empty? <list>)",
                description: "Tests if a list is empty.",
                examples: ["(empty? [])", "(empty? [1 2])", "(empty? xs)"],
                module: "core"
            ),
            
            // String operations aliases
            define_builtin!(
                name: "string-upcase",
                signature: "(string-upcase <string>)",
                description: "Converts a string to uppercase.",
                examples: ["(string-upcase \"hello\")", "(string-upcase \"World\")"],
                module: "strings"
            ),
            
            define_builtin!(
                name: "string-downcase",
                signature: "(string-downcase <string>)",
                description: "Converts a string to lowercase.",
                examples: ["(string-downcase \"HELLO\")", "(string-downcase \"World\")"],
                module: "strings"
            ),
        ]
    }
    
    /// Validate that all built-ins have proper documentation
    pub fn validate_all() -> Result<(), &'static str> {
        // This will be checked at compile time by the macro
        Ok(())
    }
}

/// Get documentation for a specific built-in by name
pub fn get_builtin_doc(name: &str) -> Option<Documentation> {
    BuiltinRegistry::all_builtins()
        .into_iter()
        .find(|b| b.name == name)
        .map(|b| b.to_documentation())
}